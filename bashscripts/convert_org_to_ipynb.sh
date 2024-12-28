#!/usr/bin/env zsh
#
set -x
set -euo pipefail
RECURSIVE=0
FORCE=0
while getopts "hrf" flag; do
    case $flag in
    h) # Handle the -h flag
        echo "-f to force overwrite, -r to apply script recursively"
        ;;
    r) # Handle the -r flag
        RECURSIVE=1
        # Enable recursive mode -- all files in all subdirectories
        ;;
    f) # Handle the -force overwrite flag
        FORCE=1
        ;;
    \?)
        echo "Invalid option."
        # Handle invalid options
        ;;
    esac
done

currentdir=$(dirname "$0")

setopt extendedglob

convert_file() {
    local org_file="$1"
    local file_directory=$(dirname "$org_file")
    local base_name=$(basename "${org_file%.org}")
    local ipynb_file="${file_directory}/${base_name}.ipynb"

    if [[ FORCE -eq 1 ]] || ! [ -f "$ipynb_file" ]; then

        if [[ "$OSTYPE" == "darwin"* ]]; then
            sed "s/#+begin_src python/#+begin_src jupyter-python :exports both/gI" "$org_file" | pandoc --resource-path="$file_directory" -f org -o "$ipynb_file" -
        elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
            sed 's/#+begin_src python/#+begin_src jupyter-python :exports both/gI' "$org_file" | pandoc --resource-path="$file_directory" -f org -o "$ipynb_file" -
        else
            echo "Unsupported operating system: $OSTYPE"
            exit 1
        fi

    else
        echo "Error: The file $ipynb_file already exists. Delete it first or call this script with -f to force the overwrite for all ipynbs."
    fi

}

if [[ $RECURSIVE -eq 1 ]]; then
    for notebook in **/*.org; do
        convert_file "$notebook"
    done
else
    for notebook in *.org; do
        convert_file "$notebook"
    done
fi
