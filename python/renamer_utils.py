#!/usr/bin/env python3

import ast
import tokenize
from io import StringIO
from pathlib import Path

import black


class NameReplacer(ast.NodeTransformer):
    def __init__(self, replacements):
        self.replacements = replacements

    def visit_Name(self, node):
        if node.id in self.replacements:
            return ast.Name(id=self.replacements[node.id], ctx=node.ctx)
        return node


def replace_names_preserving_comments(source_file, replacements):
    # First get the list of replacements we need to make
    with open(source_file, "r") as f:
        tree = ast.parse(f.read())

    transformer = NameReplacer(replacements)
    transformer.visit(tree)

    # Now use tokenize to preserve formatting and comments
    result = []
    with open(source_file, "rb") as f:
        tokens = list(tokenize.tokenize(f.readline))
        for token in tokens:
            if token.type == tokenize.NAME and token.string in replacements:
                result.append((token.type, replacements[token.string]))
            else:
                result.append((token.type, token.string))

    # Convert back to source
    return tokenize.untokenize(result).decode("utf-8")


def replace_names_inplace(source_file, replacements):
    modified_code = replace_names_preserving_comments(source_file, replacements)

    path = Path(source_file)

    with open(path, "w") as f:
        f.write(modified_code)

    black.format_file_in_place(
        Path(path),
        fast=False,
        mode=black.FileMode(),
        write_back=black.WriteBack.YES,
    )


def rename_blocks_in_org(org_file, replacements):
    import re
    import tempfile

    with open(org_file, "r") as f:
        content = f.read()

    # Find Python code blocks
    code_blocks = re.finditer(
        r"#\+begin_src python.*?\n(.*?)\n#\+end_src", content, re.DOTALL
    )

    for block in code_blocks:
        try:
            # Write block to temp file
            with tempfile.NamedTemporaryFile(suffix=".py", mode="w+") as tmp:
                tmp.write(block.group(1))
                tmp.flush()

                # Apply replacements
                replace_names_inplace(tmp.name, replacements)

                # Read modified content
                tmp.seek(0)
                new_code = tmp.read()

                # Replace in original content
                content = content.replace(block.group(1), new_code)
        except Exception as e:
            print(f"Warning: Skipping block due to error: {str(e)}")
            print("Skipped block")
            print(block)
            # Continue with next block, leaving this one unchanged

    # Write back to org file
    with open(org_file, "w") as f:
        f.write(content)


if __name__ == "__main__":
    import json
    import sys

    if len(sys.argv) != 3:
        print("Usage: script.py <org_file> <replacements.json>")
        sys.exit(1)

    org_file = sys.argv[1]
    json_file = sys.argv[2]

    with open(json_file, "r") as f:
        replacements = json.load(f)

    rename_blocks_in_org(org_file, replacements)
