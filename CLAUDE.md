# ob-python-extras

Emacs package for enhanced Python org-babel support.

## Known Issues

### CI Test Failures (macOS vs Linux)

The GitHub CI tests are failing due to platform differences between macOS (where golden files are generated) and Linux (where CI runs).

**Root cause:** On macOS, `org-table-align` creates an extra empty column in tables. This appears to be caused by carriage return (`\r`) characters being inserted somewhere in the Emacs processing pipeline. The `\r` shows up as content in what becomes an extra column.

**Symptoms:**
- Golden files (macOS) have tables with 6 columns, ending with `| \r |`
- Linux output has tables with 5 columns, no trailing empty column
- Separator rows differ: `|---+---------|----|` (macOS) vs `|---+---------|` (Linux)

**Attempted fixes:**
- Added tabulate dependency for cleaner DataFrame output
- Added normalization in `tests/run_expect_tests.sh` to strip trailing empty columns and CR characters
- The normalization partially works but doesn't fully resolve the issue

**Status:** Tests pass locally on macOS but fail on Linux CI. Need to either:
1. Generate golden files on Linux
2. Find and fix the source of the `\r` characters in the Emacs processing
3. Make normalization more robust

See debugging notes in `tests/CI_DEBUG.md`.

## Running Tests

```bash
cd tests
./run_expect_tests.sh              # Run tests
./run_expect_tests.sh --update-goldens  # Regenerate golden files
```

## Dependencies

Uses uv for Python dependency management. Run `uv sync` to install dependencies.
