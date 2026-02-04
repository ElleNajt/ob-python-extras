# CI Test Failure Debugging Notes

## Problem Summary

Tests pass on macOS but fail on Linux CI. The issue is extra columns appearing in org tables on macOS.

## Evidence

### macOS output (golden files):
```
| idx | Name | Age | Score | \r |
|-----+------+-----+-------|----|
| 0   | Joe  | 44  | 92.5  | \r |
```

### Linux output (CI):
```
| idx | Name | Age | Score |
|-----+------+-----+-------|
| 0   | Joe  | 44  | 92.5  |
```

## Where the `\r` comes from

The carriage return is NOT in the Python output. Verified by checking:
```python
.venv/bin/python3 -c "
import pandas as pd
df = pd.DataFrame({'Name': ['Joe'], 'Age': [44]})
print(repr(df.to_markdown()))
"
# Output is clean, no \r
```

The `\r` appears after Emacs processes the output. It's present in the staging org files:
```bash
grep -A 3 "RESULTS: print_table" tests/staging/babel-formatting.org | od -c
# Shows literal \r byte before the final |
```

## Suspects

1. **org-table-align** - Called in `ob-python-extras/adjust-org-babel-results`. May interpret trailing content differently on macOS.

2. **Process output handling** - How Emacs captures Python process stdout may differ between macOS and Linux.

3. **Line ending handling** - macOS may be adding CR characters somewhere in the pipeline.

## Attempted Fixes

### 1. Added tabulate dependency
Tabulate produces cleaner markdown output than the CSV fallback. Didn't fix the issue - the `\r` is added after Python outputs the data.

### 2. Normalization in run_expect_tests.sh
Added jq filters to strip:
- `| \r |` at end of lines
- `|    |` (whitespace) at end of lines  
- `|----|` (separator) at end of lines
- Bare `\r` characters

Partially works but doesn't catch all cases because the separator column can have variable numbers of dashes.

## Potential Solutions

### Option A: Generate golden files on Linux
Run `--update-goldens` in CI or on a Linux machine, then commit those. This would make the "expected" output match what Linux produces.

### Option B: Fix the source of \r
Find where in Emacs/ob-python-extras the carriage returns are being inserted and prevent it. Likely somewhere in:
- `ob-python-extras.el` around line 464 (`ob-python-extras/my-align-advice-function`)
- Or in how babel captures process output

### Option C: More aggressive normalization
Strip ALL trailing table columns that match `| .* |$` pattern, not just specific ones. Risk: might strip legitimate content.

### Option D: Skip table comparison
Only compare non-table lines, or use a fuzzy comparison for tables that ignores trailing columns.

## Files Involved

- `tests/run_expect_tests.sh` - Test runner with normalization logic
- `tests/extract-results.el` - Extracts results from org files to JSON
- `tests/golden/*.json` - Expected outputs (generated on macOS)
- `ob-python-extras.el` - Main package, has org-table-align advice
- `python/print_org_df.py` - DataFrame to org-table conversion
