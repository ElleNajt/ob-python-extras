#!/usr/bin/env sh
#!/bin/bash

update_goldens=false
specific_files=()
exit_code=0

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
  (setq org-confirm-babel-evaluate nil)
  (setq python-shell-prompt-detect-enabled nil)
  (setq python-shell-completion-native-enable nil)
  (with-current-buffer (find-file-noselect \"$target_file\")
    (message \"Executing %s...\" \"$target_file\")
    (org-babel-execute-buffer)
    (save-buffer)))" \
"$target_file"
EOF
}

process_file() {
    local org_file="$1"
    echo "Processing $org_file..."

    if $update_goldens; then
        cp "$org_file" "golden/$org_file"
        cp "shell.nix" "golden/shell.nix"
        eval "emacs --batch $(get_emacs_args "golden/$org_file")"
    else
        cp "shell.nix" "staging/shell.nix"
        cp "$org_file" "staging/$org_file"
        eval "emacs --batch $(get_emacs_args "staging/$org_file")"

        difference=$(diff -u "golden/$org_file" "staging/$org_file")

        if [ -n "$difference" ]; then
            # Check if difference is only in PNG file references
            if echo "$difference" | grep -q '^-.*\.png\]\]$' && echo "$difference" | grep -q '^+.*\.png\]\]$'; then
                # Extract the PNG paths

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
                    return 0
                elif [ $compare_result -eq 1 ]; then
                    echo "PNG files are different:"
                    echo "golden/$golden_png"
                    echo "staging/$staging_png"
                    return 1
                else
                    echo "Error comparing PNG files"
                    return 2
                fi
            else
                # Regular text difference found
                echo "Differences found in $org_file:"
                echo "$difference"
                return 1
            fi
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
        echo "All files processed. No differences found."
    else
        echo "All files processed. Differences found in one or more files."
    fi
fi

exit $exit_code
