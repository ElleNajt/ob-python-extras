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
            referenced_pngs=$(grep -o '\[\[file:.*\.png\]\]' "$file" | sed 's/\[\[file://' | sed 's/\]\]//')

            # Remove all currently staged PNGs in this plot directory
            # git reset "$plots_dir"/*.png 2>/dev/null

            # Add only referenced PNGs
            for png in $referenced_pngs; do
                if [[ -f "$dirname/$png" ]]; then
                    git add "$dirname/$png"
                fi
            done
        fi
    fi
done
