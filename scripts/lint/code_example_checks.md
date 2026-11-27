# Executable checks of docstring and CLI help examples

`check_code_examples.py` reports the second, executable slice of #2168.
It never edits prose or fails a build because of example findings.
Startup errors (such as an unreadable Git revision) raise and exit non-zero
immediately; containment or cleanup operational failures during execution also
exit non-zero while preserving diagnostic details in any requested report files.
Nothing is wired into CI.

Run in a fresh virtual environment with the package built from the target
revision installed:

```sh
python scripts/lint/check_code_examples.py --revision HEAD --output report.json --text-output report.txt
```

## Source binding and execution boundary

All docstring and CLI help text, together with file paths and line spans, comes
exclusively from `git show` at the resolved full commit SHA. Dirty worktree
edits and installed docstrings or installed `command.help` strings are never
read as source evidence.

The installed package is treated solely as the code under execution. Its
version and build commit are recorded separately alongside
`installed_matches_revision`.

## Scope and extraction

1. **Python docstrings**: starting from `supy.__all__` (or `--module`), walk
   exported modules, functions, classes, and public class callables and
   `@property` getters defined within `supy`. Members defined outside `supy`
   are excluded before checks run and summarised by count. Each defining
   declaration is checked once.
2. **CLI `--help` text**: walk all registered `suews` Click groups and leaf
   commands. Verify each command's `--help` exits cleanly in its own fresh,
   timeout-bounded, isolated, supervised subprocess with an empty working
   directory and scrubbed environment (non-importable in-memory Click roots are
   reported as `unsupported` rather than invoked in-process), then extract
   literal `help=` / `epilog=` decorator strings (or the callback docstring
   when `help=` is absent, truncated at Click's `\f` marker) and literal
   visible option `help=` strings containing `>>>` from the Git revision AST.
   Dynamic non-literal command help is reported as `unsupported` rather than
   guessed from the installed command.

Within each extracted text block, `doctest.DocTestParser` parses `>>>`
examples into contiguous blocks separated by non-blank prose.

## Isolated execution and prerequisites

Each contiguous `>>>` block that states expected output (either stdout/repr or
an expected traceback) runs in a fresh Python subprocess (`python -P`) with:

- an empty temporary working directory (no repository files present),
- `stdin` redirected to `DEVNULL` so interactive `input()` fails immediately
  with `EOFError` as a missing prerequisite rather than inheriting caller input,
- a scrubbed environment pointing `HOME` / `XDG_CONFIG_HOME` / temp dirs to
  that empty directory and stripping `PYTHONPATH`,
- a dedicated clean `__main__` execution module containing only Python
  builtins (preserving interactive `_` via `sys.displayhook` and accumulating
  `__future__` flags across statements) and whatever the example itself
  imports (never pre-importing `urllib.error`, `socket`, `pathlib`, or
  `doctest` before user code),
- unbuffered Python I/O (`-u`) plus OS file-descriptor capture (`fd 1` and
  `fd 2`) with C stdio flushing (`fflush(NULL)`) and CRLF normalisation so
  Python, buffered C/native and subprocess stdout are captured in order across
  platforms,
- a startup-gated supervised POSIX process group (using a per-block supervisor
  with `PR_SET_CHILD_SUBREAPER` on Linux and publishing the worker PGID before
  user execution so the outer process also signals the worker group if the
  supervisor exits abnormally; processes that explicitly start a new session
  with `setsid` / `start_new_session=True` escape POSIX process-group
  signalling) or a startup-gated Windows Job Object (awaiting process exit
  before handle close and temporary directory cleanup), bounded by a finite
  positive `--timeout` (default `30.0` seconds). Containment setup or cleanup
  failures are recorded under `operational_failures` (without counting
  unstarted blocks in `blocks_executed`) and exit non-zero from `main()`.

Outputs are compared with `doctest.OutputChecker`. Missing prerequisites
(undefined names, missing imports, missing input files or configs, or
unavailable network) stop the block immediately as a `prerequisite-missing`
finding and are never supplied by the checker.

## Statuses and out-of-scope exclusions

Every executed or unsupported item receives one of five statuses:

- `ok`: all statements ran in isolation and matched `doctest.OutputChecker`.
- `mismatch`: execution completed (or raised a different exception) and
  `doctest.OutputChecker` rejected the output; `detail` includes `want`, `got`
  and the doctest diff.
- `prerequisite-missing`: execution stopped on a missing prerequisite
  (`NameError`, `ModuleNotFoundError`/`ImportError`, `FileNotFoundError`,
  permission/directory error, or network error, including chained causes).
- `error`: another unexpected runtime exception, `SyntaxError`, abnormal exit,
  or timeout.
- `unsupported`: malformed doctest syntax, `# doctest: +SKIP` (including blocks
  that partially executed earlier statements before hitting `SKIP`, counted in
  both `blocks_executed` and `blocks_partially_executed`), non-doctest CLI shell
  examples that carry stated output (preserved with their full output span),
  dynamic CLI help text, failed `--help`, or unresolved source declaration.

Per #2168 ("Out of scope"), the following are excluded without execution and
recorded under `excluded_out_of_scope`:

- `>>>` blocks with no stated output (`example has no stated output`),
- non-doctest shell invocations in CLI `--help` with no stated output
  (`cli shell example has no stated output`),
- inherited or framework-synthesised members defined outside `supy`
  (`member defined outside supy`),
- external re-exported callables defined outside `supy`
  (`object defined outside supy`),
- Click command objects re-exported on Python modules
  (`click command checked on cli surface`), which are checked on the CLI
  surface.
