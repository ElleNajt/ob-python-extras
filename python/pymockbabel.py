#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3Packages.matplotlib

import gc
import hashlib
import os
import random
import sys
from datetime import datetime
from functools import partial
from unittest.mock import patch

MATPLOTLIB_AVAILABLE = False
EXTRAS_DO_REPLACEMENTS = True
try:
    import matplotlib.pyplot as plt

    MATPLOTLIB_AVAILABLE = True
except ImportError:
    pass


def set_do_replacements(value: bool):
    global EXTRAS_DO_REPLACEMENTS
    EXTRAS_DO_REPLACEMENTS = value


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


def mock_show(outputs_and_file_paths, output_types, org_babel_file_name, transparent):
    directory = os.path.join("plots", org_babel_file_name)
    os.makedirs(directory, exist_ok=True)

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    file_path = os.path.join(
        directory, f"plot_{timestamp}_{random.randint(0, 10000000)}.png"
    )

    plt.savefig(file_path, transparent=transparent)
    outputs_and_file_paths.append(file_path)
    output_types.append("Image")
    plt.close()
    gc.collect()


def setup(org_babel_file_name, transparent):
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
                transparent=transparent,
            ),
        ).start()
    list_writer = start_capturing(outputs_and_file_paths, output_types)
    return outputs_and_file_paths, output_types, list_writer


def display(outputs_and_file_paths, output_types, list_writer, max_lines=None):
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
                global EXTRAS_DO_REPLACEMENTS
                if EXTRAS_DO_REPLACEMENTS:
                    item = item.replace("|", "\\")
            org_babel_output.append(f"{item}")
        elif item_type == "Image":
            org_babel_output.append(f"[[file:{item}]]")

    if max_lines:
        print("\n".join(org_babel_output[:max_lines]))
        if len(org_babel_output) > max_lines:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            random_hash = hashlib.md5(str(random.random()).encode()).hexdigest()[:8]
            os.makedirs("logs/org_babel_logs/", exist_ok=True)
            log_file = (
                f"logs/org_babel_logs/org_babel_output_{timestamp}_{random_hash}.log"
            )
            with open(log_file, "w") as f:
                f.write("\n".join(org_babel_output))
            print(f"Output Truncated, see [[file:{log_file}][Log File]]")

    else:
        print("\n".join(org_babel_output))
