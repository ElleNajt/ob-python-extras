#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3Packages.matplotlib

import os
import gc
import random
import sys
from datetime import datetime
from functools import partial
from unittest.mock import patch

MATPLOTLIB_AVAILABLE = False
try:
    import matplotlib.pyplot as plt
    MATPLOTLIB_AVAILABLE = True
except ImportError:
    pass

class Writer:
    def __init__(self, output_list, output_types):
        self.output_list = output_list
        self.output_types = output_types
        self._stdout = sys.stdout
        self._stderr = sys.stderr

    def write(self, message):
        for line in message.splitlines():
            if line.strip():
                self.output_list.append(line.strip())
                self.output_types.append("Text")

    def flush(self):
        pass

    def isatty(self):
        return True


def start_capturing(outputs_and_file_paths, output_types):
    writer = Writer(outputs_and_file_paths, output_types)
    sys.stdout = writer

    # on second thought, capturing stderr makes too many headaches with dissapearing errors
    # sys.stderr = writer
    return writer


def stop_capturing(list_writer):
    sys.stdout = list_writer._stdout
    sys.stderr = list_writer._stderr


def mock_show(outputs_and_file_paths, output_types, org_babel_file_name):
    directory = os.path.join("plots", org_babel_file_name)
    os.makedirs(directory, exist_ok=True)

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    file_path = os.path.join(
        directory, f"plot_{timestamp}_{random.randint(0, 10000000)}.png"
    )

    plt.savefig(file_path)
    outputs_and_file_paths.append(file_path)
    output_types.append("Image")
    plt.close()
    gc.collect()


def setup(org_babel_file_name):
    outputs_and_file_paths = []
    output_types = []
    if MATPLOTLIB_AVAILABLE:
        patch(
            "matplotlib.pyplot.show",
            new=partial(
                mock_show,
                outputs_and_file_paths=outputs_and_file_paths,
                output_types=output_types,
                org_babel_file_name=org_babel_file_name,
            ),
        ).start()
    list_writer = start_capturing(outputs_and_file_paths, output_types)
    return outputs_and_file_paths, output_types, list_writer


def display(outputs_and_file_paths, output_types, list_writer):
    stop_capturing(list_writer)
    org_babel_output = []
    for item, item_type in zip(outputs_and_file_paths, output_types):
        if item_type == "Text":
            if item.startswith("DF_FLAG:"):
                item = item.replace("DF_FLAG:", "")
            else:
                # If not printing a dataframe, then replace pipes with a slash
                # this is important for printing, e.g. the printSchema method of
                # a spark dataframe
                item = item.replace("|", "\\")
            org_babel_output.append(f"{item}")
        elif item_type == "Image":
            org_babel_output.append(f"[[file:{item}]]")

    print("\n".join(org_babel_output))
