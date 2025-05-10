#!/usr/bin/env python3

import io
import os
import random
from datetime import datetime
from functools import partial

PANDAS_AVAILABLE = False
_original_repr = {}
_original_str = {}

POLARS_AVAILABLE = False
try:
    import polars as pl

    POLARS_AVAILABLE = True

    def custom_polars_repr(self: pl.DataFrame) -> str:
        shape = str(self.shape)
        return shape + "\n" + self.to_pandas().__repr__()

    pl.DataFrame.__repr__ = custom_polars_repr
    pl.DataFrame.__str__ = custom_polars_repr


except ImportError:
    POLARS_AVAILABLE = False
    pass

PYSPARK_AVAILABLE = False
try:
    from pyspark.sql import DataFrame as SparkDataFrame

    PYSPARK_AVAILABLE = True

    def custom_spark_show(self, n=20):
        self.limit(n).toPandas().print()

    _original_spark_show = SparkDataFrame.show

    def enable_spark():
        if PYSPARK_AVAILABLE:
            SparkDataFrame.show = custom_spark_show

    def disable_spark():
        if PYSPARK_AVAILABLE:
            SparkDataFrame.show = _original_spark_show

except ImportError:
    pass

try:
    import pandas as pd

    pd.options.display.max_rows = 20
    PANDAS_AVAILABLE = True
except ImportError:
    PANDAS_AVAILABLE = False
    pass

try:
    import tabulate

    TABULATE_AVAILABLE = True

except ImportError:
    TABULATE_AVAILABLE = False

try:
    import dataframe_image as dfi

    DATAFRAME_IMAGE_AVAILABLE = True
except ImportError:
    DATAFRAME_IMAGE_AVAILABLE = False


def image_repr(self, org_babel_filename, dpi=400):
    if not DATAFRAME_IMAGE_AVAILABLE:
        raise ImportError("Must have dataframe_image installed.")

    directory = os.path.join("plots", org_babel_filename)
    os.makedirs(directory, exist_ok=True)

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    file_path = os.path.join(
        directory, f"df_plot_{timestamp}_{random.randint(0, 10000000)}.png"
    )

    dfi.export(
        # dfi.export doesn't handle series
        self if self.type == pd.DataFrame else pd.DataFrame(self),
        file_path,
        table_conversion="chrome",
        dpi=dpi,
    )
    return f"[[file:{file_path}]]"


def org_repr(obj):
    # The DF_FLAG: business is to prevent |'s from being replaced with \, which
    # we do elsewhere to prevent things accidentally being parsed as org tables.
    if TABULATE_AVAILABLE:
        markdown = obj.to_markdown()
        lines = [f"DF_FLAG:{line}" for line in markdown.split("\n")]
        return "\n".join(lines)

    float_format = pd.get_option("display.float_format")
    output = io.StringIO()
    if isinstance(obj, pd.DataFrame):
        obj.to_csv(output, sep="|", index=True, float_format=float_format)
    elif isinstance(obj, pd.Series):
        pd.DataFrame(obj).to_csv(
            output,
            sep="|",
            index=True,
            header=[obj.name or ""],
            float_format=float_format,
        )
    output.seek(0)

    table = output.read().strip().split("\n")

    header = table[0].split("|")
    if obj.index.name:
        header[0] = f" {obj.index.name} "
    else:
        header[0] = " idx  "
    table[0] = "|".join(header)

    table = [f"DF_FLAG:| {line.strip()} |" for line in table]

    if len(table) > 1:
        header_width = len(table[0])
        hline = f"DF_FLAG:|{'-' * (header_width - 2)}|"
        table.insert(1, hline)

    # TODO[UXWdAurs7o] Add the head and tail printing like jupyter does

    OFFSET_DUE_TO_HEADER_AND_HLINE = 2
    return "\n".join(
        table[: pd.options.display.max_rows + OFFSET_DUE_TO_HEADER_AND_HLINE]
    )


def enable(repr_type, org_babel_filename=None, dpi=400):
    if repr_type == "org_table":
        global PANDAS_AVAILABLE
        if not PANDAS_AVAILABLE:
            print(
                "Pandas is not available in this environment. Org-mode representation cannot be enabled."
            )
            return

        for obj in [pd.DataFrame, pd.Series]:
            if obj not in _original_repr:
                _original_repr[obj] = obj.__repr__
                _original_str[obj] = obj.__str__

            obj.__str__ = org_repr
            obj.__repr__ = org_repr

    if repr_type == "image":
        for obj in [pd.DataFrame, pd.Series, pd.io.formats.style.Styler]:
            if obj not in _original_repr:
                _original_repr[obj] = obj.__repr__
                _original_str[obj] = obj.__str__

            obj.__str__ = (
                lambda self, org_babel_filename=org_babel_filename, dpi=dpi: image_repr(
                    self, org_babel_filename, dpi
                )
            )
            obj.__repr__ = (
                lambda self, org_babel_filename=org_babel_filename, dpi=dpi: image_repr(
                    self, org_babel_filename, dpi
                )
            )


def disable():
    global PANDAS_AVAILABLE
    if not PANDAS_AVAILABLE:
        print("Pandas is not available in this environment. No changes to revert.")
        return

    for obj in [pd.DataFrame, pd.Series]:
        if obj in _original_repr:
            obj.__repr__ = _original_repr[obj]
            obj.__str__ = _original_str[obj]


def is_enabled():
    return PANDAS_AVAILABLE and pd.DataFrame.__repr__ == org_repr


def is_pandas_available():
    return PANDAS_AVAILABLE


if __name__ == "__main__":
    import numpy as np
    import pandas as pd

    data = {
        "Name": ["Alice", "Bob", "Charlie", "David", "Eva"],
        "Age": [25, 30, 35, 28, 22],
        "City": ["New York", "San Francisco", "London", "Paris", "Tokyo"],
        "Score": [92.5, 88.0, 95.2, 78.9, 90.11111],
    }
    df = pd.DataFrame(data)
    df.index.name = "ID"
    enable(repr_type="org_table")

    print(df)
    print(df.Name)

    pd.options.display.float_format = "{:.1f}".format
    print(df.Score)

    print(df.set_index("Name"))

    # disable()
    # print(df)

    print("testing image")

    enable(repr_type="image", org_babel_filename="test")

    df = pd.DataFrame({"A": [1, 2, 3], "B": [4, 5, 6]})
    styled_df = df.style.background_gradient()
    print(styled_df)
