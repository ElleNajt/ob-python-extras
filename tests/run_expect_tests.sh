#!/usr/bin/env bash

set -euo pipefail
# set -x

update_goldens=false
specific_files=()
failures=()
test_count=0
failed_count=0
test_names=()

mkdir -p staging/plots/babel-formatting
mkdir -p golden/plots/babel-formatting

junit_output="test-results.xml"
echo '<?xml version="1.0" encoding="UTF-8"?>' >"$junit_output"
echo '<testsuites>' >>"$junit_output"

# Parse command line arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
    --update-goldens)
        update_goldens=true
        shift
        ;;
    --file)
        shift
        specific_files=()
        while [[ "$#" -gt 0 && ! "$1" =~ ^-- ]]; do
            specific_files+=("$1")
            shift
        done
        ;;
    --test)
        shift
        test_names=()
        while [[ "$#" -gt 0 && ! "$1" =~ ^-- ]]; do
            test_names+=("$1")
            shift
        done
        continue # Add this to avoid processing the same argument twice
        ;;
    *)
        echo "Unknown parameter: $1"
        exit 1
        ;;
    esac
done

get_emacs_args() {
    local target_file=$1
    local test_names_str=""
    if [ ${#test_names[@]} -gt 0 ]; then
        # Convert test names array to Emacs list string
        test_names_str="'($(
            IFS=' '
            echo "${test_names[*]}"
        ))"
    fi
    cat <<EOF
--batch \
--eval "(setq debug-on-error t)" \
--eval "(defmacro map! (&rest _) nil)" \
--eval "(setq ob-python-extras-python-path (expand-file-name \"../python\"))" \
-l ob \
-l ob-python \
--load "../ob-python-extras.el" \
--eval "
(progn
(setq ob-python-extras/allow-png-deletion t)
  (with-current-buffer (find-file-noselect \"$target_file\")
         (org-babel-map-src-blocks nil
           (when ${test_names_str:-t}  ; If test_names is empty, use t to process all
             (org-babel-remove-result)))
         (save-buffer))
  (setq org-confirm-babel-evaluate nil)
  (setq python-shell-prompt-detect-enabled nil)
  (setq python-shell-completion-native-enable nil)
  (with-current-buffer (find-file-noselect \"$target_file\")
    (message \"Executing %s...\" \"$target_file\")
    (org-babel-map-src-blocks nil
      (when-let ((name (nth 4 (org-babel-get-src-block-info))))
        (message \"Evaluating block: %s\" name)
        (org-babel-execute-src-block)))
    (save-buffer))
    (kill-emacs))" \
"$target_file"
EOF
}

# TODO figure out how to only get the named tests to revaluate
# (if (null ${test_names_str:-nil})
#         (org-babel-execute-buffer)
#       (dolist (name ${test_names_str:-nil})
#         (org-babel-map-src-blocks nil
#           (when (equal (nth 4 (org-babel-get-src-block-info)) name)
#             (org-babel-execute-src-block)))))

compare_test() {
    local test_name="$1"
    local staging_results="$2"
    local test_suite="$3"

    local start_time=$(date +%s)
    local has_failure=0
    local difference=""
    ((test_count += 1))

    if [ ! -f "golden/${test_name}.json" ]; then
        has_failure=1
        difference="No golden file found for test ${test_name}"
    else
        staging_test=$(echo "$staging_results" | jq --arg name "$test_name" 'fromjson | .[$name] | with_entries(select(.value | type == "string" and (contains(".png") | not)))')
        golden_test=$(cat "golden/${test_name}.json" | jq 'with_entries(select(.value | type == "string" and (contains(".png") | not)))')

        # Compare non-PNG content
        if [ "$(echo "$golden_test" | jq -S .)" != "$(echo "$staging_test" | jq -S .)" ]; then
            has_failure=1
            set +e
            difference=$(diff -u <(echo "$golden_test" | jq -S .) <(echo "$staging_test" | jq -S .))
            set -e
        fi

        # Compare PNG if present
        golden_pngs=$(echo "$golden_test" | jq -r 'with_entries(select(.value | type == "string" and (contains(".png") ))) | to_entries | .[] | .value')
        staging_pngs=$(echo "$staging_test" | jq -r 'with_entries(select(.value | type == "string" and (contains(".png") ))) | to_entries | .[] | .value')

        # Convert to arrays
        readarray -t golden_arr <<<"$golden_pngs"
        readarray -t staging_arr <<<"$staging_pngs"

        # Loop through arrays
        for i in "${!golden_arr[@]}"; do
            golden_png="${golden_arr[$i]}"
            staging_png="${staging_arr[$i]}"

            if [[ -n "$golden_png" && -n "$staging_png" ]]; then
                if [ ! -f "golden/$golden_png" ] || [ ! -f "staging/$staging_png" ]; then
                    has_failure=1
                    difference+=$'\n'"PNG missing: golden=$golden_png staging=$staging_png"
                else
                    set +e
                    compare -metric AE "golden/$golden_png" "staging/$staging_png" null: 2>/dev/null
                    if [ $? -ne 0 ]; then
                        has_failure=1
                        difference+=$'\n'"PNG files differ: $golden_png"
                    fi
                    set -e
                fi
            fi
        done
    fi

    local end_time=$(date +%s)
    local duration=$((end_time - start_time))

    if [ $has_failure -eq 1 ]; then
        echo "<testcase name=\"$test_name\" time=\"$duration\">" >>"$junit_output"
        echo "<failure message=\"Test failed\"><![CDATA[$difference]]></failure>" >>"$junit_output"
        echo "</testcase>" >>"$junit_output"
        failures+=("$test_name in $test_suite")
        ((failed_count += 1))
    else
        echo "<testcase name=\"$test_name\" time=\"$duration\" />" >>"$junit_output"
    fi
}

compare_results() {
    local staging_file="$1"
    local test_suite=$(basename "$staging_file" .org)

    # Get structured results
    staging_results=$(emacs --batch -l extract-results.el "$staging_file")

    echo "<testsuite name=\"$test_suite\">" >>"$junit_output"

    # Get list of test names
    if [ ${#test_names[@]} -eq 0 ]; then
        readarray -t test_list < <(echo "$staging_results" | jq -r 'fromjson | keys[]')
    else
        test_list=("${test_names[@]}")
    fi

    for test_name in "${test_list[@]}"; do
        compare_test "$test_name" "$staging_results" "$test_suite"
    done

    echo "</testsuite>" >>"$junit_output"
    return 0
}

process_file() {
    local org_file="$1"
    echo "Processing file: $org_file"

    # Always run the tests in staging
    cp shell*.nix "staging/"
    cp "$org_file" "staging/$org_file"
    eval "emacs $(get_emacs_args "staging/$org_file")"

    if $update_goldens; then
        # Get structured results
        staging_results=$(emacs --batch -l extract-results.el "staging/$org_file")

        if [ ${#test_names[@]} -gt 0 ]; then
            test_list=("${test_names[@]}")
        else
            # Otherwise get all test names
            readarray -t test_list < <(echo "$staging_results" | jq -r 'fromjson | keys[]')
        fi

        # Update each test's golden file
        for test_name in "${test_list[@]}"; do
            echo "$staging_results" | jq --arg name "$test_name" 'fromjson | .[$name]' >"golden/${test_name}.json"

            # Copy any PNGs for this test
            staging_pngs=$(echo "$staging_results" | jq -r --arg name "$test_name" \
                'fromjson | .[$name] | with_entries(select(.value | type == "string" and (contains(".png")))) | to_entries | .[] | .value')

            while IFS= read -r png_file; do
                [[ -z "$png_file" ]] && continue
                if [[ -f "staging/$png_file" ]]; then
                    # Ensure the directory structure exists
                    mkdir -p "$(dirname "golden/$png_file")"
                    cp "staging/$png_file" "golden/$png_file"
                fi
            done <<<"$staging_pngs"
        done
    else
        compare_results "staging/$org_file"
    fi
}

if [ ${#specific_files[@]} -eq 0 ]; then
    echo "No specific files given, processing all org files..."
    for org_file in *.org; do
        if [ -f "$org_file" ] && [ "$org_file" != "readme.org" ]; then
            process_file "$org_file"
            echo "-----------------------------------"
        fi
    done
else
    for org_file in "${specific_files[@]}"; do
        if [ -f "$org_file" ]; then
            process_file "$org_file"
            echo "-----------------------------------"
        else
            echo "File not found: $org_file"
        fi
    done
fi

echo '</testsuites>' >>"$junit_output"

if $update_goldens; then
    echo "Golden files updated."
else
    if [ ${#failures[@]} -eq 0 ]; then
        echo "All ${test_count} tests passed"
        {
            echo "# Last Successful test: $(date)"
            echo "## System Information"
            echo "Emacs version: $(emacs --version | head -n1)"
            echo "Nixpkgs commit: $(nix-instantiate --eval -E '(import <nixpkgs> {}).lib.version' 2>/dev/null || echo "Nixpkgs not found")"
        } >last_successful_test_system_info.md
        exit 0
    else
        echo "Failed tests (${failed_count}/${test_count}):"
        printf '%s\n' "${failures[@]}"
        exit 1
    fi
fi
