#!/usr/bin/env sh
set -x
set -euo pipefail

git diff --cached --name-only --diff-filter=D | grep '.org$' | while IFS= read -r org_file; do
    base_name="${file%.org}"
    ipynb_file="${base_name}.ipynb"
    rm -f "$ipynb_file"
    git rm --cached "$ipynb_file"
done

git diff --cached --name-only --diff-filter=AM | grep '.org$' | while IFS= read -r org_file; do
    echo "Processing: $org_file"
    base_name=$(basename "${org_file%.org}")
    file_directory=$(dirname "${org_file}")
    ipynbs_folder_name="auto_generated_ipynbs"
    ipynbs_dir="${file_directory}/${ipynbs_folder_name}"
    ipynb_file="${ipynbs_dir}/${base_name}.ipynb"

    [[ -d "$ipynbs_dir" ]] || mkdir "$ipynbs_dir"

    # The sed Case insensitively change the src block types to jupyter-python
    # for better compatibility with pandoc
    # Add :exports both to make sure images are included in the ipynb
    if [[ "$OSTYPE" == "darwin"* ]]; then
        sed "s/#+begin_src python/#+begin_src jupyter-python :exports both/gI" "$org_file" | pandoc --resource-path="$file_directory" -f org -o "$ipynb_file" -
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        sed 's/#+begin_src python/#+begin_src jupyter-python :exports both/gI' "$org_file" | pandoc --resource-path="$file_directory" -f org -o "$ipynb_file" -
    else
        echo "Unsupported operating system: $OSTYPE"
        exit 1
    fi
    git add "${ipynb_file}"
done
