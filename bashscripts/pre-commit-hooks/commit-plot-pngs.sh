#!/usr/bin/env bash
set -x
set -euo pipefail

for file in $(git diff --cached --name-only --diff-filter=D); do
    if [[ $file == *.org ]]; then
        basename=$(basename "$file" .org)
        dirname=$(dirname "$file")
        plots_dir="$dirname/plots/$basename"

        if [ -d "$plots_dir" ]; then
            git rm -r "$plots_dir"
        fi
    fi
done

for file in $(git diff --cached --name-only --diff-filter=AM); do
    if [[ $file == *.org ]]; then
        basename=$(basename "$file" .org)
        dirname=$(dirname "$file")
        plots_dir="$dirname/plots/$basename"

        if [ -d "$plots_dir" ]; then
            git add -A "$plots_dir"
        fi
    fi
done
