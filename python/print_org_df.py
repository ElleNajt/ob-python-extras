#!/usr/bin/env python3

import io


PANDAS_AVAILABLE = False
_original_repr = {}
_original_str = {}

try:
    import pandas as pd
    pd.options.display.max_rows = 20
    PANDAS_AVAILABLE = True
except ImportError:
    pass

def org_repr(obj):

    float_format = pd.get_option('display.float_format')
    output = io.StringIO()
    if isinstance(obj, pd.DataFrame):
        obj.to_csv(output, sep="|",index = True,

                   float_format = float_format

                   )
    elif isinstance(obj, pd.Series):
        pd.DataFrame(obj).to_csv(output, sep="|", index=True, header=[obj.name or ''], float_format = float_format,)
    output.seek(0)




    table = output.read().strip().split("\n")

    header = table[0].split('|')
    if obj.index.name:
        header[0] = f" {obj.index.name} "
    else:
        header[0] = " idx  "
    table[0] = "|".join(header)


    table = [f"| {line.strip()} |" for line in table]

    if len(table) > 1:
        header_width = len(table[0])
        hline = f"|{'-' * (header_width - 2)}|"
        table.insert(1, hline)

    # TODO Add the head and tail printing like jupyter does

    OFFSET_DUE_TO_HEADER_AND_HLINE = 2
    return "\n".join(table[:pd.options.display.max_rows + OFFSET_DUE_TO_HEADER_AND_HLINE ])


def enable():
    global PANDAS_AVAILABLE
    if not PANDAS_AVAILABLE:
        print("Pandas is not available in this environment. Org-mode representation cannot be enabled.")
        return

    for obj in [pd.DataFrame, pd.Series]:
        if obj not in _original_repr:
            _original_repr[obj] = obj.__repr__
            _original_str[obj] = obj.__str__

        obj.__str__ = org_repr
        obj.__repr__ = org_repr

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
    import pandas as pd
    import numpy as np

    data = {
        'Name': ['Alice', 'Bob', 'Charlie', 'David', 'Eva'],
        'Age': [25, 30, 35, 28, 22],
        'City': ['New York', 'San Francisco', 'London', 'Paris', 'Tokyo'],
        'Score': [92.5, 88.0, 95.2, 78.9, 90.11111]
    }
    df = pd.DataFrame(data)
    df.index.name = 'ID'
    enable()

    print(df)
    print(df.Name)

    pd.options.display.float_format = '{:.1f}'.format
    print(df.Score)


    print(df.set_index("Name"))

    # disable()
    # print(df)
