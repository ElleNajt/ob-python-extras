#!/usr/bin/env zsh
#
set -x
set -euo pipefail
RECURSIVE=0
FORCE=0
CLEAN=0
while getopts "hrfc" flag; do
    case $flag in
    h) # Handle the -h flag
        echo "-f to force overwrite, -r to apply script recursively, -c to clean UUIDs and property drawers"
        ;;
    r) # Handle the -r flag
        RECURSIVE=1
        # Enable recursive mode -- all files in all subdirectories
        ;;
    f) # Handle the -force overwrite flag
        FORCE=1
        ;;
    c) # Handle the clean flag
        CLEAN=1
        ;;
    \?)
        echo "Invalid option."
        # Handle invalid options
        ;;
    esac
done

currentdir=$(dirname "$0")

if [ -d "${HOME}/.doom.d" ]; then
    doom_dir="${HOME}/.doom.d"
elif [ -d "${HOME}/.config/doom" ]; then
    doom_dir="${HOME}/.config/doom"
else
    echo "Error: Doom private directory not found"
    exit 1
fi
system_specific_header="${doom_dir}/local/$(hostname)-python-env.org"
fall_back_default_header="${currentdir}/default-python-env.org"

if [ -f "$system_specific_header" ]; then
    default_header="$system_specific_header"
elif [ -f "$fall_back_default_header" ]; then
    default_header="$fall_back_default_header"
else
    echo "Error: No header file found"
    exit 1
fi

setopt extendedglob

convert_file() {
    local ipynb_file="$1"
    local file_directory=$(dirname "$ipynb_file")
    local base_name=$(basename "${ipynb_file%.ipynb}")
    local org_file="${file_directory}/${base_name}.org"

    # Convert .ipynb to .org using pandoc
    if [[ FORCE -eq 1 ]] || ! [ -f "$org_file" ]; then
        pandoc "$notebook" -o "$org_file"
    else
        echo "Error: The file $org_file already exists. Delete it first."
    fi
    python_header="$(cat "${default_header}" | sed "s/SESSION_NAME_PLACEHOLDER/${base_name}/")"

    if [[ "$(uname -s)" == "Darwin" ]]; then
        sed -i '' 's/jupyter-python/python/' "$org_file"
    else
        sed -i 's/jupyter-python/python/' "$org_file"
    fi

    # Clean up UUIDs and property drawers if clean flag is set
    if [[ CLEAN -eq 1 ]]; then
        if [[ "$(uname -s)" == "Darwin" ]]; then
            sed -i '' '/^<<.*>>$/d' "$org_file"
            sed -i '' '/:PROPERTIES:/,/:END:/d' "$org_file"
            sed -i '' 's/^%/#%' "$org_file"
        else
            sed -i '/^<<.*>>$/d' "$org_file"
            sed -i '/:PROPERTIES:/,/:END:/d' "$org_file"
            sed -i 's/^%/#%' "$org_file"
        fi
    fi

    # Add the fixed text block to the top of the .org file
    temp_file=$(mktemp)
    mv "$org_file" "$temp_file"

    # Prepend the text block to the .org file
    echo "$python_header" >"$org_file"
    cat "$temp_file" >>"$org_file"

    # Clean up the temporary file
    rm "$temp_file"
}

if [[ $RECURSIVE -eq 1 ]]; then
    for notebook in **/*.ipynb; do
        convert_file "$notebook"
    done
else
    for notebook in *.ipynb; do
        convert_file "$notebook"
    done
fi
