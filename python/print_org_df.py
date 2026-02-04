#!/usr/bin/env python3

import io
import os
import random
from datetime import datetime
from functools import partial
from typing import Any, Dict, Optional, Union  # for type hints

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

    def custom_spark_show(
        self, n: int = 20, truncate: Union[bool, int] = True, vertical: bool = False
    ) -> None:
        pandas_df = self.limit(n).toPandas()
        total_rows = self.count()

        table = org_repr(pandas_df)
        if n < total_rows:
            table += f"\nonly showing top {n} rows"

        print(table)

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

TORCH_AVAILABLE = False
_torch_original_str = None
_torch_original_repr = None
try:
    import torch

    TORCH_AVAILABLE = True
    # Save original methods immediately at import time to avoid recursion
    _torch_original_str = torch.Tensor.__str__
    _torch_original_repr = torch.Tensor.__repr__
except ImportError:
    TORCH_AVAILABLE = False


RICH_AVAILABLE = False
try:
    from rich.console import Console
    from rich.panel import Panel
    from rich.table import Table
    from rich.text import Text

    RICH_AVAILABLE = True
except ImportError:
    RICH_AVAILABLE = False


def _rich_repr_helper(obj, original_repr):
    """Helper to create rich repr for any object."""
    if not RICH_AVAILABLE:
        return original_repr(obj)

    from io import StringIO

    from rich.console import Console
    from rich.pretty import Pretty

    output = StringIO()
    console = Console(file=output, force_terminal=False, width=120)
    console.print(Pretty(obj, indent_guides=True))
    return output.getvalue()


def tensor_repr(self):
    """Simple rich repr for PyTorch tensors."""
    if not RICH_AVAILABLE:
        return _torch_original_repr(self)

    # Temporarily restore original repr to avoid recursion
    torch.Tensor.__repr__ = _torch_original_repr
    try:
        result = _rich_repr_helper(self, _torch_original_repr)
    finally:
        torch.Tensor.__repr__ = tensor_repr
    return result


def rich_tensor_repr(tensor):
    """Format PyTorch tensor with rich library for better readability."""
    if not RICH_AVAILABLE:
        # Fallback to default repr if rich not available
        if _torch_original_repr is not None:
            return _torch_original_repr(tensor)
        return object.__repr__(tensor)

    console = Console(width=120, force_terminal=False, force_jupyter=False)

    # Use the original string func saved at module load time to avoid recursion
    original_str_func = (
        _torch_original_str if _torch_original_str is not None else object.__str__
    )

    # Capture output to string
    with console.capture() as capture:
        # Create info table
        info_table = Table(show_header=False, box=None, padding=(0, 1))
        info_table.add_column("Property", style="cyan")
        info_table.add_column("Value", style="yellow")

        # Basic info
        info_table.add_row("Shape", str(tuple(tensor.shape)))
        info_table.add_row("Dtype", str(tensor.dtype))
        info_table.add_row("Device", str(tensor.device))

        # Gradient info
        if tensor.requires_grad:
            info_table.add_row("Requires Grad", "True")
            if tensor.grad_fn is not None:
                info_table.add_row("Grad Fn", str(tensor.grad_fn))

        # Statistics for numeric tensors
        if tensor.numel() > 0 and tensor.dtype in [
            torch.float32,
            torch.float64,
            torch.float16,
            torch.bfloat16,
        ]:
            try:
                info_table.add_row("Min", f"{tensor.min().item():.4f}")
                info_table.add_row("Max", f"{tensor.max().item():.4f}")
                info_table.add_row("Mean", f"{tensor.mean().item():.4f}")
                info_table.add_row("Std", f"{tensor.std().item():.4f}")
            except Exception:
                # Skip stats if they fail (NaNs, inf, empty tensors, etc.)
                # Better to show tensor without stats than crash the repr
                pass

        console.print(info_table)

        # Show tensor values (truncated if large)
        console.print("\n[bold]Values:[/bold]")

        # Use torch's ORIGINAL repr for the values, but limit size
        if tensor.numel() <= 1000:
            console.print(original_str_func(tensor))
        else:
            # For large tensors, show a sample
            console.print(f"[dim](showing slice of {tensor.numel()} elements)[/dim]")
            if tensor.ndim == 1:
                console.print(original_str_func(tensor[:10]))
            elif tensor.ndim == 2:
                console.print(original_str_func(tensor[:5, :5]))
            else:
                # For higher dims, show first slice
                console.print(original_str_func(tensor[0]))

    return capture.get()


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
        self if isinstance(self, pd.DataFrame) else pd.DataFrame(self),
        file_path,
        table_conversion="chrome",
        dpi=dpi,
    )
    return f"[[file:{file_path}]]"


# Global setting for index display
_show_index = True


def set_show_index(show: bool):
    """Control whether DataFrame index is shown in org output."""
    global _show_index
    _show_index = show


def org_repr(obj):
    # The DF_FLAG: business is to prevent |'s from being replaced with \, which
    # we do elsewhere to prevent things accidentally being parsed as org tables.
    if TABULATE_AVAILABLE:
        markdown = obj.to_markdown(index=_show_index)
        lines = [f"DF_FLAG:{line}" for line in markdown.split("\n")]
        return "\n".join(lines)

    float_format = pd.get_option("display.float_format")
    output = io.StringIO()
    if isinstance(obj, pd.DataFrame):
        obj.to_csv(output, sep="|", index=_show_index, float_format=float_format)
    elif isinstance(obj, pd.Series):
        pd.DataFrame(obj).to_csv(
            output,
            sep="|",
            index=_show_index,
            header=[obj.name or ""],
            float_format=float_format,
        )
    output.seek(0)

    table = output.read().strip().split("\n")

    if _show_index:
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

        if PYSPARK_AVAILABLE:
            SparkDataFrame.show = custom_spark_show

    # Enable tensor pretty printing
    if TORCH_AVAILABLE and RICH_AVAILABLE:
        torch.Tensor.__repr__ = tensor_repr
        torch.Tensor.__str__ = tensor_repr

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

    # Restore original tensor repr
    if TORCH_AVAILABLE and _torch_original_repr is not None:
        torch.Tensor.__repr__ = _torch_original_repr
        torch.Tensor.__str__ = _torch_original_str


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

    # enable(repr_type="image", org_babel_filename="test")

    # df = pd.DataFrame({"A": [1, 2, 3], "B": [4, 5, 6]})
    # styled_df = df.style.background_gradient()
    # print(styled_df)

    from pyspark.sql import SparkSession

    spark = SparkSession.builder.getOrCreate()
    data = [("Alice", 25), ("Bob", 30), ("Charlie", 35)]
    spark_df = spark.createDataFrame(data, ["name", "age"])

    print("Testing Spark show():")
    spark_df.show()
    spark_df.show(2)
    spark_df.show(1, truncate=False)
