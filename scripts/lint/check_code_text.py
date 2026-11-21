"""Report static prose drift at a Git revision; never gate a build.

Run in a fresh environment containing the matching installed supy and numpydoc:
    python scripts/lint/check_code_text.py --revision HEAD --output report.json

Source is read exclusively with git show. See code_text_checks.md for the
bounded text grammar, export scope, unsupported cases and report interpretation.
"""

from __future__ import annotations

import argparse
import ast
from collections import Counter
from collections.abc import Iterable, Iterator
from dataclasses import dataclass
import importlib
import inspect
import json
from pathlib import Path
import re
import subprocess
from types import ModuleType

import click
from click.testing import CliRunner
from numpydoc.docscrape import FunctionDoc
from numpydoc.validate import Validator
from pydantic import BaseModel

STATUSES = ("ok", "mismatch", "unsupported", "needs-reading")
DOC_PATH = "docs/source/api/command-line.rst"
DEFAULT = re.compile(r"\bdefault(?:s to|:)\s*([^\n;)]+)", re.IGNORECASE)
OPTION = re.compile(r"(?<![\w-])--[a-z][a-z0-9-]*|(?<![\w-])-[A-Za-z](?!\w)")


def git(repo: Path, *args: str) -> str:
    """Read Git data without consulting installed source files."""
    return subprocess.check_output(
        ["git", "-C", str(repo), *args], text=True, encoding="utf-8"
    )


@dataclass
class Source:
    """A declaration and its exact revision-backed source span."""

    file: str
    name: str
    node: ast.AST
    text: str

    def binding(self) -> dict:
        """Return portable locations, including the actual text read."""
        start = min(
            [self.node.lineno]
            + [d.lineno for d in getattr(self.node, "decorator_list", [])]
        )
        end = self.node.end_lineno
        return {
            "file": self.file,
            "definition": self.name,
            "lines": [start, end],
            "source": "\n".join(self.text.splitlines()[start - 1 : end]),
        }


class Revision:
    """Index declarations from Git, including private implementation modules."""

    def __init__(self, repo: Path, revision: str) -> None:
        self.repo = repo
        self.sha = git(repo, "rev-parse", f"{revision}^{{commit}}").strip()
        self.sources = {}
        self.modules = {}
        paths = git(repo, "ls-tree", "-r", "--name-only", self.sha, "src/supy")
        for path in paths.splitlines():
            if path.endswith(".py"):
                text = self.read(path)
                module = path[4:-3].replace("/", ".").removesuffix(".__init__")
                tree = ast.parse(text)
                self.modules[module] = (path, tree, text)
                self._index(tree, module, path, text)

    def read(self, path: str) -> str:
        """Read a tracked blob at the resolved commit."""
        return git(self.repo, "show", f"{self.sha}:{path}")

    def _index(self, node: ast.AST, prefix: str, path: str, text: str) -> None:
        for child in ast.iter_child_nodes(node):
            if isinstance(child, ast.FunctionDef | ast.AsyncFunctionDef | ast.ClassDef):
                name = f"{prefix}.{child.name}"
                self.sources.setdefault(name, []).append(
                    Source(path, name, child, text)
                )
                self._index(child, name, path, text)
            else:
                self._index(child, prefix, path, text)

    def locate(self, obj: object) -> Source:
        """Resolve by qualified identity, never by an installed source path."""
        obj = inspect.unwrap(obj)
        name = f"{getattr(obj, '__module__', '')}.{getattr(obj, '__qualname__', '')}"
        matches = self.sources.get(name, [])
        if len(matches) != 1:
            raise ValueError("missing, nested or overloaded source declaration")
        return matches[0]


def result(
    revision: str, source: Source, public: str, check: str, status: str, detail: dict
) -> dict:
    """Construct a source-bound finding."""
    return {
        "revision": revision,
        "object": public,
        "check": check,
        "status": status,
        "detail": detail,
        "definition_resolved": isinstance(
            source.node, ast.FunctionDef | ast.AsyncFunctionDef | ast.ClassDef
        ),
        **source.binding(),
    }


class SourceParameterValidator(Validator):
    """Use numpydoc's parameter checks without reading installed docstrings."""

    def __init__(self, obj: object, doc: FunctionDoc) -> None:
        # Validator's general initialiser also reads obj.__doc__ and pydoc.getdoc.
        # Its parameter_mismatches property needs only these two fields. Keep
        # that narrow adapter pinned and tested instead of importing prose from
        # the installed object, even as unused general-validation metadata.
        self.obj = obj
        self.doc = doc


def _parse_parameters(doc: str) -> tuple[FunctionDoc, list[dict]]:
    parsed = FunctionDoc(None, doc=doc)
    formats = []
    for section in ("Parameters", "Other Parameters"):
        entries = []
        for raw_name, raw_kind, desc in parsed[section]:
            name, kind = raw_name, raw_kind
            match = re.fullmatch(r"([*\w]+(?:\s*,\s*[*\w]+)*):\s*(\S.*)", name)
            if match:
                formats.append({"format": "name-colon-type", "text": name})
                name, kind = match.groups()
            entries.append((", ".join(n.strip() for n in name.split(",")), kind, desc))
        parsed[section] = entries
    return parsed, formats


def parameter_check(obj: object, doc: str) -> tuple[str, dict]:
    """Use numpydoc PR01/PR02, after checking introspection really succeeded."""
    try:
        signature = inspect.signature(obj, follow_wrapped=False)
        parsed, formats = _parse_parameters(doc)
        errors = [
            {"code": code, "message": message}
            for code, message in SourceParameterValidator(
                obj, parsed
            ).parameter_mismatches
            if code in {"PR01", "PR02"}
        ]
    except (TypeError, ValueError) as exc:
        return "unsupported", {"reason": type(exc).__name__}
    return ("mismatch" if errors or formats else "ok"), {
        "signature": str(signature),
        "errors": errors,
        "formats": formats,
    }


def _field_description_check(revision: Revision, obj: type) -> tuple[str, dict]:
    declarations = {}
    for owner in reversed(obj.__mro__):
        if not issubclass(owner, BaseModel) or owner is BaseModel:
            continue
        try:
            source = revision.locate(owner)
        except ValueError:
            continue
        for node in source.node.body:
            if isinstance(node, ast.AnnAssign) and isinstance(node.target, ast.Name):
                declarations[node.target.id] = (node, source)
    described, missing, unresolved, bindings = [], [], [], {}
    for name in sorted(obj.model_fields):
        if name not in declarations:
            unresolved.append(name)
            continue
        node, source = declarations[name]
        bindings[name] = Source(source.file, source.name, node, source.text).binding()
        descriptions = [
            keyword.value
            for call in ast.walk(node)
            if isinstance(call, ast.Call)
            and (
                (isinstance(call.func, ast.Name) and call.func.id == "Field")
                or (isinstance(call.func, ast.Attribute) and call.func.attr == "Field")
            )
            for keyword in call.keywords
            if keyword.arg == "description"
        ]
        try:
            values = [ast.literal_eval(value) for value in descriptions]
        except (ValueError, TypeError):
            unresolved.append(name)
            continue
        (
            described
            if any(isinstance(v, str) and v.strip() for v in values)
            else missing
        ).append(name)
    status = "mismatch" if missing else "unsupported" if unresolved else "ok"
    return status, {
        "fields": sorted(obj.model_fields),
        "described": described,
        "missing_descriptions": missing,
        "unresolved_descriptions": unresolved,
        "field_sources": bindings,
    }


def literal(node: ast.AST | str | None) -> object:
    """Accept only Python literals, never evaluate expressions or names."""
    return ast.literal_eval(node)


def default_check(declared: ast.AST | None, text: str) -> tuple[str, dict]:
    """Compare a declared literal with the fixed default phrase, conservatively."""
    matches = DEFAULT.findall(text)
    if len(matches) != 1:
        return "needs-reading", {"reason": "no single fixed default phrase"}
    stated = matches[0].strip().removesuffix(".").strip("`")
    try:
        expected = literal(stated)
    except (ValueError, SyntaxError):
        if re.fullmatch(r"[\w-]+", stated):
            expected = stated
        else:
            return "needs-reading", {
                "reason": "non-literal stated default",
                "text": stated,
            }
    try:
        actual = literal(declared)
    except (ValueError, TypeError, SyntaxError):
        return "needs-reading", {"reason": "non-literal declared default"}
    if actual is None and expected is not None:
        return "needs-reading", {
            "reason": "effective runtime default",
            "stated": expected,
        }
    return ("ok" if actual == expected else "mismatch"), {
        "declared": actual,
        "stated": expected,
    }


def choice_check(
    choices: Iterable[object], text: str, case_sensitive: bool = True
) -> tuple[str, dict]:
    """Compare explicit exhaustive lists, leaving conditional prose to readers."""
    if re.search(r"\b(if|when|only|legacy|canonical|depending|subset)\b", text, re.I):
        return "needs-reading", {"reason": "conditional choice description"}
    match = re.search(r"(?:choices:|one of:|transformations:)\s*(.+)", text, re.I)
    if not match:
        return "needs-reading", {"reason": "no explicit exhaustive choice list"}
    tokens = re.split(r",\s*(?:or\s+)?|\s+or\s+", match[1].rstrip(". "))
    stated = {token.strip(" '\"`{}[]") for token in tokens}
    actual = {str(value) for value in choices}
    if not case_sensitive:
        actual = {v.casefold() for v in actual}
        stated = {v.casefold() for v in stated}
    detail = {
        "missing_from_text": sorted(actual - stated),
        "extra_in_text": sorted(stated - actual),
    }
    return ("mismatch" if actual != stated else "ok"), detail


def exported_names(tree: ast.Module) -> list[str]:
    """Use explicit __all__, otherwise module-level definitions and re-exports."""
    for node in tree.body:
        if isinstance(node, ast.Assign) and any(
            isinstance(t, ast.Name) and t.id == "__all__" for t in node.targets
        ):
            return literal(node.value)
    names = []
    for node in tree.body:
        if isinstance(node, ast.ImportFrom):
            names.extend(alias.asname or alias.name for alias in node.names)
        elif isinstance(node, ast.FunctionDef | ast.ClassDef):
            names.append(node.name)
    return [name for name in names if not name.startswith("_")]


def class_members(
    obj: type, seen: set[tuple[type, str]], excluded: Counter
) -> Iterator[tuple[str, object]]:
    """Resolve ownership before exposing a member to any text check."""
    for member in sorted(dir(obj)):
        if member.startswith("_"):
            continue
        raw = inspect.getattr_static(obj, member)
        target = raw.__func__ if isinstance(raw, staticmethod | classmethod) else raw
        if not callable(target):
            continue
        owner = next(cls for cls in obj.__mro__ if member in vars(cls))
        if not (owner.__module__ == "supy" or owner.__module__.startswith("supy.")):
            excluded["member defined outside supy"] += 1
            continue
        key = (owner, member)
        if key in seen:
            continue
        seen.add(key)
        definition = f"{owner.__module__}.{owner.__qualname__}.{member}"
        yield definition, target


def public_objects(
    revision: Revision,
    module: ModuleType,
    seen: set[str] | None = None,
    members_seen: set[tuple[type, str]] | None = None,
    excluded: Counter | None = None,
) -> Iterator[tuple[str, object, Source, str | None]]:
    """Walk exports, excluding external members and visiting owned definitions once."""
    seen = set() if seen is None else seen
    members_seen = set() if members_seen is None else members_seen
    excluded = Counter() if excluded is None else excluded
    if module.__name__ in seen or module.__name__ not in revision.modules:
        return
    seen.add(module.__name__)
    path, tree, text = revision.modules[module.__name__]
    tree.lineno = 1
    tree.end_lineno = len(text.splitlines())
    anchor = Source(path, module.__name__, tree, text)
    for name in exported_names(tree):
        public = f"{module.__name__}.{name}"
        export = next(
            (
                n
                for n in ast.walk(tree)
                if isinstance(n, ast.Constant) and n.value == name
            ),
            tree,
        )
        anchor = Source(path, module.__name__, export, text)
        try:
            obj = getattr(module, name)
        except Exception as exc:  # Inventory failures must remain visible.
            yield public, None, anchor, f"export lookup failed: {type(exc).__name__}"
            continue
        if isinstance(obj, ModuleType):
            yield from public_objects(revision, obj, seen, members_seen, excluded)
        elif callable(obj):
            yield public, obj, anchor, None
            if inspect.isclass(obj):
                for definition, target in class_members(obj, members_seen, excluded):
                    yield definition, target, anchor, None
        elif obj is None:
            yield public, None, anchor, "export unavailable"


def python_results(
    revision: Revision, module: ModuleType, excluded: Counter | None = None
) -> Iterator[dict]:
    """Check the exported Python surface without executing examples."""
    for public, obj, anchor, initial_reason in public_objects(
        revision, module, excluded=excluded
    ):
        reason = initial_reason
        try:
            source = revision.locate(obj)
        except ValueError:
            source = anchor
            reason = reason or "source declaration cannot be resolved"
        node = source.node
        decorators = getattr(node, "decorator_list", [])
        if hasattr(obj, "__wrapped__") or any(
            ast.unparse(d) not in {"staticmethod", "classmethod"} for d in decorators
        ):
            reason = reason or "decorated declaration"
        if not reason and inspect.isclass(obj) and issubclass(obj, BaseModel):
            status, detail = _field_description_check(revision, obj)
            yield result(
                revision.sha, source, public, "field-descriptions", status, detail
            )
            continue
        if inspect.isclass(obj) and "__init__" not in vars(obj):
            reason = reason or "class constructor may be generated or inherited"
        if reason:
            yield result(
                revision.sha,
                source,
                public,
                "parameters",
                "unsupported",
                {"reason": reason},
            )
            continue
        doc = ast.get_docstring(node) or ""
        status, detail = parameter_check(obj, doc)
        yield result(revision.sha, source, public, "parameters", status, detail)
        if status == "unsupported":
            continue
        parsed, _ = _parse_parameters(doc)
        if inspect.isclass(obj):
            try:
                node = revision.locate(obj.__init__).node
            except ValueError:
                continue  # Generated constructors have no literal source defaults.
        defaults = (
            dict(
                zip(
                    [a.arg for a in node.args.posonlyargs + node.args.args][
                        -len(node.args.defaults) :
                    ],
                    node.args.defaults,
                )
            )
            if node.args.defaults
            else {}
        )
        defaults.update(
            zip([a.arg for a in node.args.kwonlyargs], node.args.kw_defaults)
        )
        for names, kind, desc in parsed["Parameters"] + parsed["Other Parameters"]:
            text = kind + " " + " ".join(desc)
            if not DEFAULT.search(text):
                continue
            for raw_name in names.split(","):
                name = raw_name.strip()
                status, detail = default_check(defaults.get(name), text)
                yield result(
                    revision.sha, source, public, f"default:{name}", status, detail
                )


def commands(
    command: click.Command, path: tuple[str, ...] = ()
) -> Iterator[tuple[tuple[str, ...], click.Command]]:
    """Walk Click's registered tree, including groups themselves."""
    yield path, command
    if isinstance(command, click.Group):
        with click.Context(command) as ctx:
            for name in command.list_commands(ctx):
                yield from commands(command.get_command(ctx, name), (*path, name))


def described_options(text: str) -> dict[tuple[str, ...] | None, dict[str, list[int]]]:
    """Associate RST option mentions with headings or explicit command invocations."""
    described = {}
    section = ()
    lines = text.splitlines()
    for index, line in enumerate(lines):
        if (
            line.startswith("suews")
            and index + 1 < len(lines)
            and lines[index + 1].startswith("~~~")
        ):
            section = tuple(line.split()[1:])
        if not section:
            explicit = set()
            for match in re.finditer(r"``suews\s+(-[^`]+)``", line):
                explicit.update(OPTION.findall(match[1]))
            generic = set()
            for match in re.finditer(r"``suews <subcommand>\s+([^`]+)``", line):
                generic.update(OPTION.findall(match[1]))
            for option in generic:
                described.setdefault(("*",), {}).setdefault(option, []).append(
                    index + 1
                )
            for option in OPTION.findall(line):
                if option in generic and option not in explicit:
                    continue
                target = () if option in explicit else None
                described.setdefault(target, {}).setdefault(option, []).append(
                    index + 1
                )
            continue
        invocation = re.search(r"\bsuews\s+([a-z]+(?:\s+[a-z]+)*)", line)
        target = section
        if invocation:
            words = tuple(invocation[1].split())
            # Commands are resolved against the registered paths by the caller.
            target = words
        for option in OPTION.findall(line):
            described.setdefault(target, {}).setdefault(option, []).append(index + 1)
    return described


def option_difference(
    rendered: Iterable[str], documented: Iterable[str]
) -> dict[str, list[str]]:
    """Compare both directions after aliases have been resolved."""
    return {
        "missing_from_docs": sorted(set(rendered) - set(documented)),
        "extra_in_docs": sorted(set(documented) - set(rendered)),
    }


def rendered_options(help_text: str) -> tuple[set[str], dict[str, str]]:
    """Read declaration rows and map short aliases to their long option names."""
    aliases = {}
    for line in help_text.splitlines():
        if not re.match(r"^  -", line):
            continue
        declaration = re.split(r"\s{2,}", line.strip())[0]
        # A negative flag is a distinct form; do not merge --no-backup into
        # --backup merely because Click renders them on the same row.
        for form in declaration.split("/"):
            spellings = OPTION.findall(form)
            if spellings:
                canonical = next(
                    (s for s in spellings if s.startswith("--")), spellings[0]
                )
                aliases.update(dict.fromkeys(spellings, canonical))
    return set(aliases.values()), aliases


def command_documentation(doc: str, paths: set[tuple[str, ...]]) -> tuple[dict, dict]:
    """Resolve heading, invocation and generic mentions onto registered commands."""
    described = {path: {} for path in paths}
    mentions = described_options(doc)
    ambiguous = mentions.pop(None, {})
    generic = mentions.pop(("*",), {})
    for path in paths:
        if path:
            described[path].update(generic)
    for candidate, options in mentions.items():
        matches = [path for path in paths if candidate[: len(path)] == path]
        target = max(matches, key=len)
        described[target].update(options)
    return described, ambiguous


def cli_results(revision: Revision, root: click.Group) -> Iterator[dict]:
    """Compare source help prose and rendered option records with the RST page."""
    doc = revision.read(DOC_PATH)
    inventory = list(commands(root))
    described, ambiguous = command_documentation(doc, {path for path, _ in inventory})
    for path, command in inventory:
        public = "suews" + (" " + " ".join(path) if path else "")
        source = revision.locate(command.callback)
        if not path and ambiguous:
            yield result(
                revision.sha,
                source,
                public,
                "unscoped-documentation",
                "needs-reading",
                {
                    "docs_file": DOC_PATH,
                    "mentions": ambiguous,
                    "reason": "overview option mentions do not identify a command",
                },
            )
        invocation = CliRunner().invoke(root, [*path, "--help"], terminal_width=240)
        rendered, aliases = rendered_options(invocation.output)
        documented = {aliases.get(option, option) for option in described[path]}
        detail = option_difference(rendered, documented)
        detail.update({
            "help": invocation.output,
            "docs_file": DOC_PATH,
            "docs_lines": [1, len(doc.splitlines())],
            "mentions": described[path],
            "aliases": aliases,
        })
        status = (
            "mismatch"
            if any(detail[k] for k in ("missing_from_docs", "extra_in_docs"))
            else "ok"
        )
        if invocation.exit_code:
            status = "unsupported"
            detail["reason"] = "--help failed"
        yield result(revision.sha, source, public, "documented-options", status, detail)
        yield from cli_prose_results(revision, source, public, command)


def cli_prose_results(
    revision: Revision, source: Source, public: str, command: click.Command
) -> Iterator[dict]:
    """Check literal option prose against declarations and installed choices."""
    options = {
        opt for p in command.params if isinstance(p, click.Option) for opt in p.opts
    }
    for decorator in source.node.decorator_list:
        if not isinstance(decorator, ast.Call) or not ast.unparse(
            decorator.func
        ).endswith(".option"):
            continue
        flags = [
            literal(arg) for arg in decorator.args if isinstance(arg, ast.Constant)
        ]
        flag = next((f for f in flags if f in options), None)
        if flag is None:
            continue
        kwargs = {kw.arg: kw.value for kw in decorator.keywords}
        try:
            help_text = literal(kwargs.get("help", ast.Constant("")))
        except (ValueError, TypeError):
            yield result(
                revision.sha,
                source,
                public,
                f"help:{flag}",
                "needs-reading",
                {"reason": "dynamic help"},
            )
            continue
        if DEFAULT.search(help_text):
            status, detail = default_check(kwargs.get("default"), help_text)
            yield result(
                revision.sha, source, public, f"default:{flag}", status, detail
            )
        param = next(p for p in command.params if flag in p.opts)
        if isinstance(param.type, click.Choice):
            choice_node = kwargs.get("type")
            if (
                not isinstance(choice_node, ast.Call)
                or not ast.unparse(choice_node.func).endswith(".Choice")
                or not choice_node.args
                or not isinstance(choice_node.args[0], ast.List | ast.Tuple)
                or any(
                    not isinstance(item, ast.Constant | ast.Name)
                    for item in choice_node.args[0].elts
                )
            ):
                status, detail = (
                    "needs-reading",
                    {"reason": "dynamic choice declaration"},
                )
            else:
                status, detail = choice_check(
                    param.type.choices, help_text, param.type.case_sensitive
                )
            yield result(
                revision.sha, source, public, f"choices:{flag}", status, detail
            )


def _summarise(findings: list[dict]) -> dict:
    by_check = {}
    unknown = {}
    for finding in findings:
        check = finding["check"].split(":", 1)[0]
        counts = by_check.setdefault(check, dict.fromkeys(STATUSES, 0))
        counts[finding["status"]] += 1
        errors = [e for e in finding["detail"].get("errors", []) if e["code"] == "PR02"]
        if errors:
            unknown[finding["definition"]] = {
                "definition": finding["definition"],
                "file": finding["file"],
                "errors": errors,
            }
    return {
        "counts_by_check": dict(sorted(by_check.items())),
        "unique_definitions": len({
            (f["file"], f["definition"]) for f in findings if f["definition_resolved"]
        }),
        "format_deviations": sum(len(f["detail"].get("formats", [])) for f in findings),
        "format_definitions": len({
            f["definition"] for f in findings if f["detail"].get("formats")
        }),
        "unknown_parameters": [unknown[key] for key in sorted(unknown)],
    }


def main() -> None:
    """Write a complete JSON report; findings do not change the exit status."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--revision", default="HEAD")
    parser.add_argument("--repo", type=Path, default=Path.cwd())
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    revision = Revision(args.repo, args.revision)
    supy = importlib.import_module("supy")
    cli = importlib.import_module("supy.cmd.suews_cli").cli
    version = importlib.import_module("supy._version_scm")
    installed_commit = getattr(version, "__commit_hash__", "unknown")

    excluded = Counter()
    findings = list(python_results(revision, supy, excluded)) + list(
        cli_results(revision, cli)
    )
    counts = Counter(f["status"] for f in findings)
    report = {
        "revision": revision.sha,
        "installed_version": supy.__version__,
        "installed_commit": installed_commit,
        "installed_matches_revision": revision.sha.startswith(installed_commit),
        "counts": {status: counts[status] for status in STATUSES},
        "summary": _summarise(findings),
        "excluded_out_of_scope": {
            "count": sum(excluded.values()),
            "reasons": dict(sorted(excluded.items())),
        },
        "results": findings,
        "mismatches": [f for f in findings if f["status"] == "mismatch"],
    }
    args.output.write_text(
        json.dumps(report, indent=2, ensure_ascii=True, default=repr) + "\n",
        encoding="utf-8",
    )
    print(json.dumps(report["counts"]))
    print(json.dumps(report["summary"], ensure_ascii=True))
    print("excluded_out_of_scope: " + json.dumps(report["excluded_out_of_scope"]))


if __name__ == "__main__":
    main()
