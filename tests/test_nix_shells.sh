#!/usr/bin/env bash

for nix_file in shell*.nix; do
    if [ -f "$nix_file" ]; then
        echo "Testing $nix_file..."
        nix-shell "$nix_file" --run "python -c 'print(\"$nix_file loaded correctly\")'"
    fi
done
