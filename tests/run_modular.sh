#!/usr/bin/env bash

set -euo pipefail

update_goldens=false
specific_files=()
failures=()
test_count=0
failed_count=0

mkdir -p staging/plots/babel-formatting
mkdir -p golden/plots/babel-formatting

junit_output="test-results.xml"
echo '<?xml version="1.0" encoding="UTF-8"?>' >"$junit_output"
echo '<testsuites>' >>"$junit_output"

# Parse command line arguments
while [[ "$#" -gt 0 ]]; do
    case $1 in
    --update-goldens) update_goldens=true ;;
    --file)
        shift
        while [[ "$#" -gt 0 && ! "$1" =~ ^-- ]]; do
            specific_files+=("$1")
            shift
        done
        ;;
    *)
        echo "Unknown parameter: $1"
        exit 1
        ;;
    esac
    shift
done

get_emacs_args() {
    local target_file=$1
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
           (org-babel-remove-result))
         (save-buffer))
  (setq org-confirm-babel-evaluate nil)
  (setq python-shell-prompt-detect-enabled nil)
  (setq python-shell-completion-native-enable nil)
  (with-current-buffer (find-file-noselect \"$target_file\")
    (message \"Executing %s...\" \"$target_file\")
    (org-babel-execute-buffer)
    (save-buffer))
    (kill-emacs))" \
"$target_file"
EOF
}

compare_results() {
    local golden_file="$1"
    local staging_file="$2"
    local test_suite=$(basename "$golden_file" .org)

    # Get structured results
    golden_results=$(emacs --batch -l extract-results.el "$golden_file")
    staging_results=$(emacs --batch -l extract-results.el "$staging_file")

    echo "<testsuite name=\"$test_suite\">" >>"$junit_output"

    # Get list of all test names
    test_names=$(echo "$golden_results" | jq -r 'fromjson | keys[]')

    while IFS= read -r test_name; do
        local start_time=$(date +%s)
        local has_failure=0
        local difference=""

        # Compare non-PNG content for this test
        golden_test=$(echo "$golden_results" | jq --arg name "$test_name" 'fromjson | .[$name] | with_entries(select(.value | type == "string" and (contains(".png") | not)))')
        staging_test=$(echo "$staging_results" | jq --arg name "$test_name" 'fromjson | .[$name] | with_entries(select(.value | type == "string" and (contains(".png") | not)))')

        if [ "$(echo "$golden_test" | jq -S .)" != "$(echo "$staging_test" | jq -S .)" ]; then
            has_failure=1
            difference=$(diff -u <(echo "$golden_test" | jq -S .) <(echo "$staging_test" | jq -S .))
        fi

        # Compare PNG if present
        golden_png=$(echo "$golden_results" | jq -r --arg name "$test_name" '.[$name] | with_entries(select(.value | contains(".png"))) | .[].value')
        staging_png=$(echo "$staging_results" | jq -r --arg name "$test_name" '.[$name] | with_entries(select(.value | contains(".png"))) | .[].value')

        if [[ -n "$golden_png" && -n "$staging_png" ]]; then
            if [ ! -f "golden/$golden_png" ] || [ ! -f "staging/$staging_png" ]; then
                has_failure=1
                difference+=$'\n'"PNG missing: golden=$golden_png staging=$staging_png"
            else
                compare -metric AE "golden/$golden_png" "staging/$staging_png" null: 2>/dev/null
                if [ $? -ne 0 ]; then
                    has_failure=1
                    difference+=$'\n'"PNG files differ: $golden_png"
                fi
            fi
        fi

        local end_time=$(date +%s)
        local duration=$((end_time - start_time))

        if [ $has_failure -eq 1 ]; then
            echo "<testcase name=\"$test_name\" time=\"$duration\">" >>"$junit_output"
            echo "<failure message=\"Test failed\"><![CDATA[$difference]]></failure>" >>"$junit_output"
            echo "</testcase>" >>"$junit_output"
            failures+=("$test_name in $test_suite")
        else
            echo "<testcase name=\"$test_name\" time=\"$duration\" />" >>"$junit_output"
        fi

    done <<<"$test_names"

    echo "</testsuite>" >>"$junit_output"
    return 0
}

process_file() {
    local org_file="$1"
    ((test_count++))

    if $update_goldens; then
        cp "$org_file" "golden/$org_file"
        cp shell*.nix "golden/"
        eval "emacs $(get_emacs_args "golden/$org_file")"
    else
        cp shell*.nix "staging/"
        cp "$org_file" "staging/$org_file"
        eval "emacs $(get_emacs_args "staging/$org_file")"
        compare_results "golden/$org_file" "staging/$org_file"
    fi
}

if [ ${#specific_files[@]} -eq 0 ]; then
    for org_file in *.org; do
        if [ -f "$org_file" ]; then
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
            ((failed_count++))
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
            echo "Doom version: $(doom version)"
            echo "Nixpkgs commit: $(nix-instantiate --eval -E '(import <nixpkgs> {}).lib.version' 2>/dev/null || echo "Nixpkgs not found")"
        } >last_successful_test_system_info.md
        exit 0
    else
        echo "Failed tests (${failed_count}/${test_count}):"
        printf '%s\n' "${failures[@]}"
        exit 1
    fi
fi
