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
        cp "$org_file" "staging/$org_file"
        cp "shell.nix" "staging/shell.nix"
        eval "emacs --batch $(get_emacs_args "staging/$org_file")"

        difference=$(diff -u \
            <(cat "golden/$org_file" | sed 's/^[[:space:]]*//' | sed '/^$/d') \
            <(cat "staging/$org_file" | sed 's/^[[:space:]]*//' | sed '/^$/d'))

        if [ -n "$difference" ]; then
            echo "Differences found in $org_file:"
            echo "$difference"
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
        echo "All files processed. No differences found."
    else
        echo "All files processed. Differences found in one or more files."
    fi
fi

exit $exit_code
