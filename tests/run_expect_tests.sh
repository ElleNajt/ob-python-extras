#!/usr/bin/env bash
set -euo pipefail

update_goldens=false
specific_files=()
exit_code=0

mkdir -p staging/plots/babel-formatting
mkdir -p golden/plots/babel-formatting

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
        *) echo "Unknown parameter: $1"; exit 1 ;;
    esac
    shift
done


get_emacs_args() {
    local target_file=$1
    cat << EOF
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

process_file() {
    local org_file="$1"
    echo "Processing $org_file..."

    if $update_goldens; then
        cp "$org_file" "golden/$org_file"
        cp shell*.nix "golden/"
        eval "emacs --batch $(get_emacs_args "golden/$org_file")"
    else
        cp shell*.nix "staging/"
        cp "$org_file" "staging/$org_file"
        eval "emacs --batch $(get_emacs_args "staging/$org_file")"

        # Filter out %expect_skip lines from both files before diffing
        difference=$(diff -u <(sed '/%expect_skip/d' "golden/$org_file") <(sed '/%expect_skip/d' "staging/$org_file") | sed '/^---/d; /^+++/d')

        # Flag to track if we need to return failure
        has_failure=0

        # Check PNG differences
        if [ -n "$difference" ]; then
            if echo "$difference" | grep -q '^-.*\.png\]\]$' && echo "$difference" | grep -q '^+.*\.png\]\]$'; then

                golden_png=$(echo "$difference" | grep '^-.*\.png' | grep -o 'plots/.*\.png')
                staging_png=$(echo "$difference" | grep '^+.*\.png' | grep -o 'plots/.*\.png')


                echo "Found difference in PNG references:"
                echo "Golden:  $golden_png"
                echo "Staging: $staging_png"

                # Check if both PNG files exist
                if [ ! -f "golden/$golden_png" ]; then
                    echo "Error: Golden PNG file not found: golden/$golden_png"
                    return 1
                fi
                if [ ! -f "staging/$staging_png" ]; then
                    echo "Error: Staging PNG file not found: staging/$staging_png"
                    return 1
                fi

                # Compare PNG files using ImageMagick's compare
                # Returns 0 if images are identical, 1 if different, 2 if error occurred
                compare -metric AE "golden/$golden_png" "staging/$staging_png" null: 2>/dev/null
                compare_result=$?

                if [ $compare_result -eq 0 ]; then
                    echo "PNG files are identical despite different filenames."
                elif [ $compare_result -eq 1 ]; then
                    echo "PNG files are different:"
                    echo "golden/$golden_png"
                    echo "staging/$staging_png"
                    has_failure=1
                else
                    echo "Error comparing PNG files"
                    return 2
                fi
            fi
        fi


        # Now check for any other differences
        while IFS= read -r line; do
            if [[ "$line" =~ ^@@ ]]; then
                context=$(echo "$line" | sed -E 's/^@@ .* @@ (.*)/\1/')
                context=$line
                new_context=1
            elif ! [[ "$line" =~ \.png\]\]$ ]] && [[ "$line" =~ ^[+-] ]]; then
                if [ $new_context -eq 1 ]; then
                    echo "$context"
                    new_context=0
                fi
                echo "$line\n"
                has_failure=1
            fi
        done <<< "$difference"

        if [ $has_failure -eq 1 ]; then
            echo "Found differences in $org_file."
            return 1
        else
            echo "No differences found in $org_file."
            return 0
        fi
    fi
}

if [ ${#specific_files[@]} -eq 0 ]; then
    # Process all *.org files if no specific files were provided
    for org_file in *.org; do
        if [ -f "$org_file" ]; then
            process_file "$org_file" || exit_code=1
            echo "-----------------------------------"
        fi
    done
else
    # Process only the specified files
    for org_file in "${specific_files[@]}"; do
        if [ -f "$org_file" ]; then
            process_file "$org_file" || exit_code=1
            echo "-----------------------------------"
        else
            echo "File not found: $org_file"
            exit_code=1
        fi
    done
fi

if $update_goldens; then
    echo "Golden files updated."
else
    if [ $exit_code -eq 0 ]; then
        {
            echo "# Last Successful test: $(date)"
            echo "## System Information"
            echo "Emacs version: $(emacs --version | head -n1)"
            echo "Doom version: $(doom version)"
            echo "Nixpkgs commit: $(nix-instantiate --eval -E '(import <nixpkgs> {}).lib.version' 2>/dev/null || echo "Nixpkgs not found")"
        } > last_successful_test_system_info.md
    else
        echo "All files processed. Differences found in one or more files."
    fi
fi

exit $exit_code
