# Static checks of hand-written code text

`check_code_text.py` reports the first, static slice of #2168.
It never edits prose, executes examples or fails a build because of findings.
Operational failures (such as an unreadable revision) still exit non-zero.
Nothing is wired into CI.

Run in a fresh virtual environment with the package built from the target
revision and its development dependencies installed:

```sh
python scripts/lint/check_code_text.py --revision HEAD --output report.json
```

All text and declaration locations come from `git show` at the resolved full
commit SHA; dirty worktree text and installed docstrings are not evidence.
The installed package supplies export identities, signatures, Click definitions
and rendered `--help` output. Its version and build commit are recorded separately,
with `installed_matches_revision` showing whether that commit matches. The caller
must install that revision in the fresh environment. Reports include the source spans read,
complete rendered help, documentation mention lines, counts for all four
statuses and an unabridged mismatch list. Counts describe checks, not objects.

## Scope and parameters

Start at `supy.__all__`, recursively follow exported modules, and inspect exported
callables and public class methods. Where a module has no `__all__`, use its
module-level definitions and re-exports. Private implementation module names
never exclude a re-exported object. Constants and properties have no callable
signature and are outside parameter checks. Aliases are reported at each public
exposure. Missing exports and unresolved source declarations are unsupported.

Use numpydoc 1.9.0's `FunctionDoc` and `Validator.parameter_mismatches`, selecting
PR01 (missing documented parameters) and PR02 (unknown documented parameters).
Parameter order, types, units, descriptions and behaviour are outside this slice.
Numpydoc parses sections and normalises receivers and variadic stars; the adapter
normalises comma spacing in grouped names and initialises only the validator
fields used by parameter checks, avoiding its general installed-docstring reader.
Signature resolution is checked first
because numpydoc itself treats introspection failure as an empty signature.
Decorated, overloaded, inherited and generated constructor signatures are
reported as unsupported rather than assumed to match.

## Bounded prose grammar

A stated default uses **`default: VALUE`** or **`defaults to VALUE`**, ignoring
case, ending at a newline, semicolon or closing parenthesis. A final full stop
and surrounding backticks are removed. VALUE is a Python literal or a bare
single-word string (such as `on`). Python parameter text includes the parsed
parameter type line and description; CLI text is the literal `help=` argument.
Only `ast.literal_eval`-compatible declared defaults are compared. A None
sentinel with a non-None effective default, an expression, or other prose is
`needs-reading`. Other phrasings are not silently interpreted as defaults.

Choice descriptions must explicitly introduce an exhaustive comma/or-separated
list using **`Choices:`**, **`One of:`**, or **`transformations:`** (the latter
covers `suews validate --science-fixes`). Quote/backtick delimiters are removed.
Missing and extra values are reported separately; Click's case sensitivity is
respected. Descriptions containing conditional vocabulary (`if`, `when`, `only`,
`legacy`, `canonical`, `depending`, `subset`) are `needs-reading`. So are help
strings with no exhaustive list; a mention of one value is not a list.
This grammar deliberately favours abstention over guessing natural language.

## Command documentation

Walk all registered `suews` groups and leaf commands, invoke each with `--help`,
and extract option spellings only from rendered option declaration lines.
Compare both directions against option mentions in
`docs/source/api/command-line.rst`, including examples. Headings assign prose to
commands; explicit `suews ...` invocations assign options to the longest matching
registered command path. Short aliases map to their rendered long option names;
negative flags remain separate forms. An explicit `suews <subcommand> --help`
statement covers help on all subcommands. The page links to more detailed pages; those pages
are intentionally not followed, so missing options mean absent from this page,
not absent from all documentation. No claim is made about semantic completeness.
Unscoped overview statements about subcommands are `needs-reading`, rather than
attributed to the root command.
