#!/usr/bin/env bash
# Evaluate all Python blocks in an org file using a batch Emacs process.
# Usage: eval-org.sh <file.org> [--lib /path/to/ob-python-extras.el]
#
# Waits for completion and exits with the same code as Emacs.

set -euo pipefail

usage() {
    echo "Usage: $0 <file.org> [--lib /path/to/ob-python-extras.el] [--python-path /path/to/python/scripts]"
    exit 1
}

ORG_FILE=""
LIB=""
PYTHON_PATH=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --lib) LIB="$2"; shift 2 ;;
        --python-path) PYTHON_PATH="$2"; shift 2 ;;
        -h|--help) usage ;;
        *)
            if [[ -z "$ORG_FILE" ]]; then
                ORG_FILE="$1"; shift
            else
                echo "Unknown argument: $1" >&2; usage
            fi
            ;;
    esac
done

[[ -z "$ORG_FILE" ]] && usage
ORG_FILE="$(cd "$(dirname "$ORG_FILE")" && pwd)/$(basename "$ORG_FILE")"
[[ -f "$ORG_FILE" ]] || { echo "File not found: $ORG_FILE" >&2; exit 1; }

# Default: find ob-python-extras via emacsclient if not specified
if [[ -z "$LIB" ]]; then
    LIB="$(emacsclient --eval '(locate-library "ob-python-extras")' 2>/dev/null | tr -d '"')"
fi
[[ -f "$LIB" ]] || { echo "Cannot find ob-python-extras library: $LIB" >&2; exit 1; }

# Default: find python scripts dir
if [[ -z "$PYTHON_PATH" ]]; then
    PYTHON_PATH="$(dirname "$LIB")/python"
    # If that doesn't exist, try the source repo
    if [[ ! -d "$PYTHON_PATH" ]]; then
        PYTHON_PATH="$(emacsclient --eval '(ob-python-extras/find-python-scripts-dir)' 2>/dev/null | tr -d '"')"
    fi
fi
[[ -d "$PYTHON_PATH" ]] || { echo "Cannot find python scripts dir: $PYTHON_PATH" >&2; exit 1; }

echo "Evaluating: $ORG_FILE"
echo "Library:    $LIB"
echo "Python:     $PYTHON_PATH"
echo "---"

emacs --batch \
    --eval "(setq ob-python-extras-python-path \"$PYTHON_PATH\")" \
    -l "$LIB" \
    --eval "(progn
               (require 'ob-python)
               (setq org-confirm-babel-evaluate nil)
               (advice-add 'org-babel-comint-use-async :override #'ignore)
               (find-file \"$ORG_FILE\")
               (org-babel-execute-buffer)
               (save-buffer)
               (message \"Done: $ORG_FILE\")
               (kill-emacs 0))"
