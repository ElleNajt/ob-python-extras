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
            # Get all referenced PNGs from the org file
            referenced_pngs=$(grep -o '\[\[file:.*\.png\]\]' "$file" | sed 's/\[\[file://' | sed 's/\]\]//' || true)

            # Remove all currently staged PNGs in this plot directory
            # We want this in case we rerun the notebook during staging
            git reset "$plots_dir"/*.png 2>/dev/null

            # Add only referenced PNGs (force-add in case plots dir is gitignored)
            for png in $referenced_pngs; do
                full_path="$dirname/$png"
                if [[ -f "$full_path" ]]; then
                    git add -f "$full_path"
                fi
            done
        fi
    fi
done
