#!/usr/bin/env python3
"""Inventory recorded test debt without importing or executing repository code.

Selectors in test/known_defects.yml use exact attributes or an id_prefix (not
regular expressions). Each stored record must match exactly once. Counts are
records, not unique bugs or passing tests; weighted occurrences and mutation
slices are separate quantities. Issue existence/state is checked by CI, offline
checks validate syntax only. Report mode returns zero even with findings.
"""

import argparse
import ast
from collections import Counter
from dataclasses import dataclass
import json
from pathlib import Path
import re

import yaml

LINKS = "test/known_defects.yml"
SCHEMA = "test/data_model/schema_contract_baseline.json"
MUTATIONS = "test/data_model/known_mutation_gaps.json"
JACOBIAN = "src/suews_bridge/tests/yaml_jacobian.rs"
OBSERVER = "scripts/suews/gen_capi_field_observer.py"
MANIFEST = "src/suews_bridge/test_observer/observer_manifest.rs"
OPTION_SWEEP = "test/physics/test_option_sweep.py"
CLASSES = ("defect", "coverage_gap", "accepted_exception")
STRING = r'"(?:[^"\\]|\\.)*"'
_SAFE_SCALAR_BUILTINS = frozenset({
    "all",
    "any",
    "bool",
    "hash",
    "id",
    "isinstance",
    "len",
    "print",
    "repr",
    "str",
    "type",
})
_SAFE_ELEMENT_BUILTINS = frozenset({
    "dict",
    "enumerate",
    "frozenset",
    "iter",
    "list",
    "max",
    "min",
    "next",
    "reversed",
    "set",
    "sorted",
    "sum",
    "tuple",
    "zip",
})
_SAFE_SCALAR_METHODS = frozenset({
    "count",
    "index",
    "isdisjoint",
    "issubset",
    "issuperset",
    "keys",
})
_SAFE_ELEMENT_METHODS = frozenset({
    "copy",
    "difference",
    "get",
    "intersection",
    "items",
    "symmetric_difference",
    "union",
    "values",
})
_DYNAMIC_EXEC_BUILTINS = frozenset({"eval", "exec"})
_DYNAMIC_NAMESPACE_BUILTINS = frozenset({"globals", "locals", "vars"})
_Taint = tuple[str, bool]


def _unique_pairs(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError(f"duplicate key: {key!r}")
        result[key] = value
    return result


class UniqueLoader(yaml.SafeLoader):
    """Reject duplicate YAML keys instead of silently discarding mappings."""


def _yaml_mapping(loader, node):
    return _unique_pairs(
        (loader.construct_object(key), loader.construct_object(value))
        for key, value in node.value
    )


UniqueLoader.add_constructor(
    yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _yaml_mapping
)


def _load(root, name):
    source = (root / name).read_text(encoding="utf-8")
    if name.endswith(".json"):
        payload = json.loads(source, object_pairs_hook=_unique_pairs)
    else:
        payload = yaml.load(source, Loader=UniqueLoader)
    return _container(payload, dict, name, "<root>")


def _container(value, expected, name, key):
    if not isinstance(value, expected):
        kind = "mapping" if expected is dict else "list"
        raise ValueError(f"{name}: {key} must be a {kind}")
    return value


def _entries(payload, name):
    entries = _container(payload["entries"], list, name, "entries")
    for index, item in enumerate(entries):
        _container(item, dict, name, f"entries/{index}")
    return entries


def _issue(value):
    if type(value) is int and value > 0:
        return value
    if isinstance(value, str) and re.fullmatch(r"#[1-9][0-9]*", value):
        return int(value[1:])
    raise ValueError(f"malformed issue reference: {value!r}")


def _record(ledger, identifier, source, **attributes):
    return {"ledger": ledger, "id": identifier, "source": source, **attributes}


def _python_values(root, name, names):
    """Read named module constants using literals, names and set union only."""
    tree = ast.parse((root / name).read_text(encoding="utf-8"), filename=name)
    definitions = {}
    repeated = set()
    initial_targets = set()
    for node in tree.body:
        if isinstance(node, (ast.Assign, ast.AnnAssign)):
            targets = node.targets if isinstance(node, ast.Assign) else [node.target]
            for target in targets:
                if isinstance(target, ast.Name):
                    if target.id in definitions:
                        repeated.add(target.id)
                    definitions[target.id] = node.value
                    initial_targets.add(target)

    dependencies = set(names)
    dep_values = {}

    def evaluate(node, stack=()):
        if isinstance(node, ast.Name):
            dependencies.add(node.id)
            if node.id in repeated:
                raise ValueError(f"{name}: repeated constant {node.id}")
            if node.id in stack:
                raise ValueError(f"{name}: cyclic constant {node.id}")
            value = evaluate(definitions[node.id], (*stack, node.id))
            dep_values[node.id] = value
            return value
        if isinstance(node, ast.BinOp) and isinstance(node.op, ast.BitOr):
            return evaluate(node.left, stack) | evaluate(node.right, stack)
        if isinstance(node, ast.Dict):
            return _unique_pairs(
                (evaluate(key, stack), evaluate(value, stack))
                for key, value in zip(node.keys, node.values, strict=True)
            )
        if isinstance(node, (ast.List, ast.Tuple, ast.Set)):
            values = [evaluate(item, stack) for item in node.elts]
            if isinstance(node, ast.Set):
                if len(set(values)) != len(values):
                    raise ValueError(f"{name}: duplicate set entry")
                return set(values)
            return tuple(values) if isinstance(node, ast.Tuple) else values
        return ast.literal_eval(node)

    values = {key: evaluate(ast.Name(id=key)) for key in names}
    _reject_constant_mutations(tree, name, dependencies, dep_values, initial_targets)
    return values


def _is_mutable(value) -> bool:
    return isinstance(value, (dict, list, set))


def _has_mutable_elements(value) -> bool:
    if isinstance(value, dict):
        return any(
            _is_mutable(item) or _has_mutable_elements(item)
            for item in (*value.keys(), *value.values())
        )
    if isinstance(value, (list, tuple, set)):
        return any(_is_mutable(item) or _has_mutable_elements(item) for item in value)
    return False


def _merge_taint(first: _Taint | None, second: _Taint | None) -> _Taint | None:
    if first is None:
        return second
    if second is None:
        return first
    return (first[0], first[1] or second[1])


def _collect_target_names(node: ast.AST, out: set[str]) -> None:
    if isinstance(node, ast.Name):
        out.add(node.id)
    elif isinstance(node, (ast.Tuple, ast.List)):
        for elt in node.elts:
            _collect_target_names(elt, out)
    elif isinstance(node, ast.Starred):
        _collect_target_names(node.value, out)


def _collect_pattern_names(pattern: ast.pattern, out: set[str]) -> None:
    if isinstance(pattern, ast.MatchAs):
        if pattern.name:
            out.add(pattern.name)
        if pattern.pattern is not None:
            _collect_pattern_names(pattern.pattern, out)
    elif isinstance(pattern, ast.MatchStar):
        if pattern.name:
            out.add(pattern.name)
    elif isinstance(pattern, ast.MatchMapping):
        if pattern.rest:
            out.add(pattern.rest)
        for sub in pattern.patterns:
            _collect_pattern_names(sub, out)
    elif isinstance(pattern, (ast.MatchSequence, ast.MatchOr)):
        for sub in pattern.patterns:
            _collect_pattern_names(sub, out)
    elif isinstance(pattern, ast.MatchClass):
        for sub in (*pattern.patterns, *pattern.kwd_patterns):
            _collect_pattern_names(sub, out)


def _walk_named_exprs(expr: ast.AST, record) -> None:
    if isinstance(expr, ast.Lambda):
        return
    if isinstance(expr, ast.NamedExpr):
        record(expr.target.id)
    for child in ast.iter_child_nodes(expr):
        _walk_named_exprs(child, record)


def _record_target_bindings(target: ast.AST, record) -> None:
    names: set[str] = set()
    _collect_target_names(target, names)
    for name in names:
        record(name)


def _walk_compound_bindings(stmt: ast.stmt, record, walk_block) -> None:
    if isinstance(stmt, (ast.For, ast.AsyncFor)):
        _record_target_bindings(stmt.target, record)
        _walk_named_exprs(stmt.iter, record)
        walk_block(stmt.body, False)
        walk_block(stmt.orelse, False)
    elif isinstance(stmt, (ast.With, ast.AsyncWith)):
        for item in stmt.items:
            _walk_named_exprs(item.context_expr, record)
            if item.optional_vars is not None:
                _record_target_bindings(item.optional_vars, record)
        walk_block(stmt.body, False)
    elif isinstance(stmt, (ast.If, ast.While)):
        _walk_named_exprs(stmt.test, record)
        walk_block(stmt.body, False)
        walk_block(stmt.orelse, False)
    elif isinstance(stmt, (ast.Try, ast.TryStar)):
        walk_block(stmt.body, False)
        for handler in stmt.handlers:
            if handler.name:
                record(handler.name)
            walk_block(handler.body, False)
        walk_block(stmt.orelse, False)
        walk_block(stmt.finalbody, False)
    elif isinstance(stmt, ast.Match):
        _walk_named_exprs(stmt.subject, record)
        for case in stmt.cases:
            names: set[str] = set()
            _collect_pattern_names(case.pattern, names)
            for name in names:
                record(name)
            if case.guard is not None:
                _walk_named_exprs(case.guard, record)
            walk_block(case.body, False)
    else:
        _walk_named_exprs(stmt, record)


def _record_ann_aug_bindings(stmt: ast.AnnAssign | ast.AugAssign, record) -> None:
    if isinstance(stmt.target, ast.Name):
        record(stmt.target.id)
    if isinstance(stmt, ast.AnnAssign):
        _walk_named_exprs(stmt.annotation, record)
    if stmt.value is not None:
        _walk_named_exprs(stmt.value, record)


def _collect_scope_bindings(
    stmts: list[ast.stmt],
) -> tuple[set[str], dict[str, ast.FunctionDef], set[str], set[str]]:
    """Collect names bound directly in a scope, plus uniquely defined local functions."""
    bound: set[str] = set()
    global_names: set[str] = set()
    nonlocal_names: set[str] = set()
    candidates: dict[str, ast.FunctionDef] = {}
    bind_counts: Counter[str] = Counter()

    def record(name: str) -> None:
        bound.add(name)
        bind_counts[name] += 1

    def walk_block(block: list[ast.stmt], top_level: bool) -> None:
        for stmt in block:
            if isinstance(stmt, ast.FunctionDef):
                record(stmt.name)
                if top_level and not stmt.decorator_list:
                    candidates[stmt.name] = stmt
            elif isinstance(stmt, (ast.AsyncFunctionDef, ast.ClassDef)):
                record(stmt.name)
            elif isinstance(stmt, ast.Assign):
                for target in stmt.targets:
                    _record_target_bindings(target, record)
                _walk_named_exprs(stmt.value, record)
            elif isinstance(stmt, (ast.AnnAssign, ast.AugAssign)):
                _record_ann_aug_bindings(stmt, record)
            elif isinstance(stmt, (ast.Import, ast.ImportFrom)):
                for alias in stmt.names:
                    if alias.name != "*":
                        record(alias.asname or alias.name.split(".", 1)[0])
            elif isinstance(stmt, ast.Global):
                global_names.update(stmt.names)
            elif isinstance(stmt, ast.Nonlocal):
                nonlocal_names.update(stmt.names)
            else:
                _walk_compound_bindings(stmt, record, walk_block)

    walk_block(stmts, True)
    unique_funcs = {
        name: fn for name, fn in candidates.items() if bind_counts[name] == 1
    }
    return bound, unique_funcs, global_names, nonlocal_names


def _function_param_names(args: ast.arguments) -> set[str]:
    names = {arg.arg for arg in (*args.posonlyargs, *args.args, *args.kwonlyargs)}
    if args.vararg is not None:
        names.add(args.vararg.arg)
    if args.kwarg is not None:
        names.add(args.kwarg.arg)
    return names


def _bind_call_taints(
    func_def: ast.FunctionDef,
    call: ast.Call,
    pos_taints: list[tuple[bool, _Taint | None]],
    kw_taints: list[tuple[str | None, _Taint | None]],
) -> dict[str, _Taint]:
    """Map tracked call arguments onto the callee's parameter names."""
    args = func_def.args
    positional = [*args.posonlyargs, *args.args]
    kw_by_name = {arg.arg: arg.arg for arg in (*args.args, *args.kwonlyargs)}
    bound: dict[str, _Taint] = {}

    def add(param: str, taint: _Taint) -> None:
        bound[param] = _merge_taint(bound.get(param), taint)  # type: ignore[assignment]

    seen_star = False
    for index, (is_starred, taint) in enumerate(pos_taints):
        if is_starred:
            seen_star = True
        if taint is None:
            continue
        if is_starred or seen_star:
            deep_taint = (taint[0], True)
            for param in positional:
                add(param.arg, deep_taint)
            if args.vararg is not None:
                add(args.vararg.arg, deep_taint)
        elif index < len(positional):
            add(positional[index].arg, taint)
        elif args.vararg is not None:
            add(args.vararg.arg, (taint[0], True))

    for key, taint in kw_taints:
        if taint is None:
            continue
        if key is None:
            deep_taint = (taint[0], True)
            for param_name in kw_by_name.values():
                add(param_name, deep_taint)
            if args.kwarg is not None:
                add(args.kwarg.arg, deep_taint)
        elif key in kw_by_name:
            add(key, taint)
        elif args.kwarg is not None:
            add(args.kwarg.arg, (taint[0], True))
    return bound


def _has_future_annotations(tree: ast.Module) -> bool:
    stmts = tree.body
    index = 0
    if (
        stmts
        and isinstance(stmts[0], ast.Expr)
        and isinstance(stmts[0].value, ast.Constant)
        and isinstance(stmts[0].value.value, str)
    ):
        index = 1
    while index < len(stmts):
        stmt = stmts[index]
        if (
            isinstance(stmt, ast.ImportFrom)
            and stmt.level == 0
            and stmt.module == "__future__"
        ):
            if any(alias.name == "annotations" for alias in stmt.names):
                return True
            index += 1
            continue
        break
    return False


@dataclass(frozen=True)
class _ScopeCtx:
    local_funcs: dict[str, ast.FunctionDef]
    local_bound: set[str]
    enclosing_funcs: dict[
        str, tuple[ast.FunctionDef, dict[str, _Taint], set[str], dict]
    ]
    enclosing_bound: set[str]
    global_names: set[str]
    nonlocal_names: set[str]
    enclosing_env: dict[str, _Taint] | None
    scope_kind: str
    walrus_env: dict[str, _Taint] | None = None


class _ConstantMutationChecker:
    """Reject mutations, mutating aliases, and unsupported escapes of ledger constants."""

    def __init__(
        self,
        tree: ast.Module,
        name: str,
        dependencies: set[str],
        dep_values: dict[str, object],
        initial_targets: set[ast.Name],
    ) -> None:
        self.tree = tree
        self.name = name
        self.dependencies = dependencies
        self.initial_targets = initial_targets
        self.postponed_annotations = _has_future_annotations(tree)
        self.root_taints: dict[str, _Taint] = {
            dep: (dep, _has_mutable_elements(value))
            for dep, value in dep_values.items()
            if _is_mutable(value) or _has_mutable_elements(value)
        }
        self.module_env: dict[str, _Taint] = dict(self.root_taints)
        self.module_bound, self.module_funcs, _, _ = _collect_scope_bindings(tree.body)
        self.checked_calls: set[tuple[int, tuple[tuple[str, _Taint], ...]]] = set()

    def check(self) -> None:
        """Run a fixed-point pass over the module to reject writes and escapes."""
        ctx = _ScopeCtx(
            local_funcs=self.module_funcs,
            local_bound=self.module_bound,
            enclosing_funcs={},
            enclosing_bound=set(),
            global_names=set(),
            nonlocal_names=set(),
            enclosing_env=None,
            scope_kind="module",
        )
        self._check_block(self.tree.body, self.module_env, ctx)

    def _fail(self, node: ast.AST, message: str) -> None:
        lineno = getattr(node, "lineno", 1)
        raise ValueError(f"{self.name}:{lineno}: {message}")

    def _bind_name(
        self,
        target: str,
        taint: _Taint,
        env: dict[str, _Taint],
        ctx: _ScopeCtx,
    ) -> None:
        merged = _merge_taint(env.get(target), taint)
        assert merged is not None
        env[target] = merged
        if target in ctx.global_names:
            self.module_env[target] = _merge_taint(self.module_env.get(target), merged)  # type: ignore[assignment]
        if target in ctx.nonlocal_names and ctx.enclosing_env is not None:
            ctx.enclosing_env[target] = _merge_taint(
                ctx.enclosing_env.get(target), merged
            )  # type: ignore[assignment]

    def _check_block(
        self, stmts: list[ast.stmt], env: dict[str, _Taint], ctx: _ScopeCtx
    ) -> None:
        while True:
            before = dict(env)
            for stmt in stmts:
                self._check_stmt(stmt, env, ctx)
            if env == before:
                break

    def _check_stmt(
        self, stmt: ast.stmt, env: dict[str, _Taint], ctx: _ScopeCtx
    ) -> None:
        in_class = ctx.scope_kind == "class"
        if isinstance(stmt, ast.Assign):
            self._check_assign(stmt, env, ctx, in_class)
        elif isinstance(stmt, ast.AnnAssign):
            if ctx.scope_kind in {"module", "class"} and not self.postponed_annotations:
                ann_taint = self._eval_expr(stmt.annotation, env, ctx)
                if ann_taint is not None:
                    self._fail(
                        stmt.annotation,
                        f"unsupported escape of constant {ann_taint[0]}",
                    )
            rhs_taint = (
                self._eval_expr(stmt.value, env, ctx)
                if stmt.value is not None
                else None
            )
            self._check_target(stmt.target, rhs_taint, env, ctx, in_class)
        elif isinstance(stmt, ast.AugAssign):
            self._check_aug_assign(stmt, env, ctx)
        elif isinstance(stmt, ast.Delete):
            self._check_delete(stmt, env, ctx)
        elif isinstance(stmt, ast.Return):
            if stmt.value is not None:
                ret_taint = self._eval_expr(stmt.value, env, ctx)
                if ret_taint is not None:
                    self._fail(stmt, f"unsupported escape of constant {ret_taint[0]}")
        elif isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef)):
            self._check_func_def(stmt, env, ctx)
        elif isinstance(stmt, ast.ClassDef):
            self._check_class_def(stmt, env, ctx)
        elif isinstance(stmt, (ast.Import, ast.ImportFrom)):
            for alias in stmt.names:
                bound_name = alias.asname or alias.name.split(".", 1)[0]
                if bound_name in self.dependencies:
                    self._fail(stmt, f"unsupported mutation of constant {bound_name}")
        else:
            self._check_compound_or_simple_stmt(stmt, env, ctx)

    def _check_assign(self, stmt, env, ctx, in_class) -> None:
        parallel = isinstance(stmt.value, (ast.Tuple, ast.List)) and not any(
            isinstance(elt, ast.Starred) for elt in stmt.value.elts
        )
        if parallel:
            elem_taints = [self._eval_expr(elt, env, ctx) for elt in stmt.value.elts]
            seq_taint: _Taint | None = None
            for item in elem_taints:
                if item is not None:
                    seq_taint = _merge_taint(seq_taint, (item[0], True))
            for target in stmt.targets:
                if (
                    isinstance(target, (ast.Tuple, ast.List))
                    and not any(isinstance(elt, ast.Starred) for elt in target.elts)
                    and len(target.elts) == len(elem_taints)
                ):
                    for sub_target, sub_taint in zip(
                        target.elts, elem_taints, strict=True
                    ):
                        self._check_target(sub_target, sub_taint, env, ctx, in_class)
                else:
                    self._check_target(target, seq_taint, env, ctx, in_class)
            return
        rhs_taint = self._eval_expr(stmt.value, env, ctx)
        for target in stmt.targets:
            self._check_target(target, rhs_taint, env, ctx, in_class)

    def _check_aug_assign(self, stmt: ast.AugAssign, env, ctx: _ScopeCtx) -> None:
        rhs_taint = self._eval_expr(stmt.value, env, ctx)
        target = stmt.target
        if isinstance(target, ast.Name):
            if target.id in self.dependencies:
                self._fail(stmt, f"unsupported mutation of constant {target.id}")
            if target.id in env:
                self._fail(
                    stmt, f"unsupported mutation of constant {env[target.id][0]}"
                )
            if rhs_taint is not None:
                self._bind_name(target.id, (rhs_taint[0], True), env, ctx)
        elif isinstance(target, ast.Subscript):
            base_taint = self._eval_expr(target.value, env, ctx)
            self._eval_expr(target.slice, env, ctx)
            if base_taint is not None:
                self._fail(stmt, f"unsupported mutation of constant {base_taint[0]}")
            if rhs_taint is not None:
                self._fail(stmt, f"unsupported escape of constant {rhs_taint[0]}")
        elif isinstance(target, ast.Attribute):
            base_taint = self._eval_expr(target.value, env, ctx)
            if base_taint is not None:
                self._fail(stmt, f"unsupported mutation of constant {base_taint[0]}")
            if target.attr in self.dependencies:
                self._fail(stmt, f"unsupported mutation of constant {target.attr}")
            if rhs_taint is not None:
                self._fail(stmt, f"unsupported escape of constant {rhs_taint[0]}")

    def _check_delete(self, stmt: ast.Delete, env, ctx: _ScopeCtx) -> None:
        for target in stmt.targets:
            if isinstance(target, ast.Name) and target.id in self.dependencies:
                self._fail(stmt, f"unsupported mutation of constant {target.id}")
            elif isinstance(target, ast.Subscript):
                base_taint = self._eval_expr(target.value, env, ctx)
                self._eval_expr(target.slice, env, ctx)
                if base_taint is not None:
                    self._fail(
                        stmt, f"unsupported mutation of constant {base_taint[0]}"
                    )
            elif isinstance(target, ast.Attribute):
                base_taint = self._eval_expr(target.value, env, ctx)
                if base_taint is not None:
                    self._fail(
                        stmt, f"unsupported mutation of constant {base_taint[0]}"
                    )
                if target.attr in self.dependencies:
                    self._fail(stmt, f"unsupported mutation of constant {target.attr}")

    def _check_subscript_target(
        self,
        target: ast.Subscript,
        rhs_taint: _Taint | None,
        env,
        ctx: _ScopeCtx,
        in_class: bool,
    ) -> None:
        base_taint = self._eval_expr(target.value, env, ctx)
        self._eval_expr(target.slice, env, ctx)
        if base_taint is not None:
            self._fail(target, f"unsupported mutation of constant {base_taint[0]}")
        if rhs_taint is None:
            return
        root = target.value
        while isinstance(root, ast.Subscript):
            root = root.value
        if isinstance(root, ast.Name) and not in_class:
            self._bind_name(root.id, (rhs_taint[0], True), env, ctx)
        else:
            self._fail(target, f"unsupported escape of constant {rhs_taint[0]}")

    def _check_target(
        self,
        target: ast.AST,
        rhs_taint: _Taint | None,
        env,
        ctx: _ScopeCtx,
        in_class: bool,
    ) -> None:
        if isinstance(target, ast.Name):
            if target in self.initial_targets and target.id in self.dependencies:
                return
            if target.id in self.dependencies:
                self._fail(target, f"unsupported mutation of constant {target.id}")
            if rhs_taint is not None:
                if in_class:
                    self._fail(target, f"unsupported escape of constant {rhs_taint[0]}")
                self._bind_name(target.id, rhs_taint, env, ctx)
        elif isinstance(target, ast.Subscript):
            self._check_subscript_target(target, rhs_taint, env, ctx, in_class)
        elif isinstance(target, ast.Attribute):
            base_taint = self._eval_expr(target.value, env, ctx)
            if base_taint is not None:
                self._fail(target, f"unsupported mutation of constant {base_taint[0]}")
            if target.attr in self.dependencies:
                self._fail(target, f"unsupported mutation of constant {target.attr}")
            if rhs_taint is not None:
                self._fail(target, f"unsupported escape of constant {rhs_taint[0]}")
        elif isinstance(target, (ast.Tuple, ast.List)):
            unpacked = rhs_taint if (rhs_taint and rhs_taint[1]) else None
            for elt in target.elts:
                self._check_target(elt, unpacked, env, ctx, in_class)
        elif isinstance(target, ast.Starred):
            self._check_target(target.value, rhs_taint, env, ctx, in_class)

    def _check_try_stmt(self, stmt: ast.Try | ast.TryStar, env, ctx: _ScopeCtx) -> None:
        self._check_block(stmt.body, env, ctx)
        for handler in stmt.handlers:
            if handler.type is not None:
                self._eval_expr(handler.type, env, ctx)
            if handler.name in self.dependencies:
                self._fail(handler, f"unsupported mutation of constant {handler.name}")
            self._check_block(handler.body, env, ctx)
        self._check_block(stmt.orelse, env, ctx)
        self._check_block(stmt.finalbody, env, ctx)

    def _check_match_stmt(self, stmt: ast.Match, env, ctx: _ScopeCtx) -> None:
        subj_taint = self._eval_expr(stmt.subject, env, ctx)
        for case in stmt.cases:
            names: set[str] = set()
            _collect_pattern_names(case.pattern, names)
            for bound_name in names:
                if bound_name in self.dependencies:
                    self._fail(stmt, f"unsupported mutation of constant {bound_name}")
                if subj_taint is not None:
                    self._bind_name(bound_name, (subj_taint[0], True), env, ctx)
            if case.guard is not None:
                self._eval_expr(case.guard, env, ctx)
            self._check_block(case.body, env, ctx)

    def _check_compound_or_simple_stmt(
        self, stmt: ast.stmt, env, ctx: _ScopeCtx
    ) -> None:
        in_class = ctx.scope_kind == "class"
        if isinstance(stmt, (ast.For, ast.AsyncFor)):
            iter_taint = self._eval_expr(stmt.iter, env, ctx)
            elem_taint = iter_taint if (iter_taint and iter_taint[1]) else None
            self._check_target(stmt.target, elem_taint, env, ctx, in_class)
            self._check_block(stmt.body, env, ctx)
            self._check_block(stmt.orelse, env, ctx)
        elif isinstance(stmt, (ast.With, ast.AsyncWith)):
            for item in stmt.items:
                ctx_taint = self._eval_expr(item.context_expr, env, ctx)
                if item.optional_vars is not None:
                    self._check_target(
                        item.optional_vars, ctx_taint, env, ctx, in_class
                    )
            self._check_block(stmt.body, env, ctx)
        elif isinstance(stmt, (ast.If, ast.While)):
            self._eval_expr(stmt.test, env, ctx)
            self._check_block(stmt.body, env, ctx)
            self._check_block(stmt.orelse, env, ctx)
        elif isinstance(stmt, (ast.Try, ast.TryStar)):
            self._check_try_stmt(stmt, env, ctx)
        elif isinstance(stmt, ast.Match):
            self._check_match_stmt(stmt, env, ctx)
        elif isinstance(stmt, ast.Expr):
            self._eval_expr(stmt.value, env, ctx)
        elif isinstance(stmt, ast.Assert):
            self._eval_expr(stmt.test, env, ctx)
            if stmt.msg is not None:
                self._eval_expr(stmt.msg, env, ctx)
        elif isinstance(stmt, ast.Raise):
            if stmt.exc is not None:
                self._eval_expr(stmt.exc, env, ctx)
            if stmt.cause is not None:
                self._eval_expr(stmt.cause, env, ctx)

    def _default_param_taints(self, args: ast.arguments, env, ctx) -> dict[str, _Taint]:
        taints: dict[str, _Taint] = {}
        positional = [*args.posonlyargs, *args.args]
        offset = len(positional) - len(args.defaults)
        for index, default in enumerate(args.defaults):
            taint = self._eval_expr(default, env, ctx)
            if taint is not None:
                taints[positional[offset + index].arg] = taint
        for arg, default in zip(args.kwonlyargs, args.kw_defaults, strict=True):
            if default is not None:
                taint = self._eval_expr(default, env, ctx)
                if taint is not None:
                    taints[arg.arg] = taint
        return taints

    def _check_func_annotations(
        self, args: ast.arguments, returns: ast.expr | None, env, ctx
    ) -> None:
        all_args = [*args.posonlyargs, *args.args, *args.kwonlyargs]
        if args.vararg is not None:
            all_args.append(args.vararg)
        if args.kwarg is not None:
            all_args.append(args.kwarg)
        for arg in all_args:
            if arg.annotation is not None:
                ann_taint = self._eval_expr(arg.annotation, env, ctx)
                if ann_taint is not None:
                    self._fail(
                        arg.annotation,
                        f"unsupported escape of constant {ann_taint[0]}",
                    )
        if returns is not None:
            ret_taint = self._eval_expr(returns, env, ctx)
            if ret_taint is not None:
                self._fail(returns, f"unsupported escape of constant {ret_taint[0]}")

    def _child_enclosing_scope(
        self, env: dict[str, _Taint], ctx: _ScopeCtx
    ) -> tuple[
        dict[str, _Taint],
        set[str],
        dict[str, tuple[ast.FunctionDef, dict[str, _Taint], set[str], dict]],
    ]:
        if ctx.scope_kind == "class":
            return (
                ctx.enclosing_env if ctx.enclosing_env is not None else self.module_env,
                ctx.enclosing_bound,
                ctx.enclosing_funcs,
            )
        enclosing_bound = ctx.enclosing_bound | ctx.local_bound
        enclosing_funcs = {
            name: entry
            for name, entry in ctx.enclosing_funcs.items()
            if name not in ctx.local_bound
        }
        for name, fn in ctx.local_funcs.items():
            enclosing_funcs[name] = (fn, env, enclosing_bound, enclosing_funcs)
        return env, enclosing_bound, enclosing_funcs

    def _check_func_def(
        self, stmt: ast.FunctionDef | ast.AsyncFunctionDef, env, ctx: _ScopeCtx
    ) -> None:
        if stmt.name in self.dependencies:
            self._fail(stmt, f"unsupported mutation of constant {stmt.name}")
        for decorator in stmt.decorator_list:
            self._eval_expr(decorator, env, ctx)
        if not self.postponed_annotations:
            self._check_func_annotations(stmt.args, stmt.returns, env, ctx)
        default_taints = self._default_param_taints(stmt.args, env, ctx)
        child_env, child_bound, child_funcs = self._child_enclosing_scope(env, ctx)
        self._check_function_body(
            stmt, default_taints, child_env, child_bound, child_funcs
        )

    def _check_function_body(
        self,
        func: ast.FunctionDef | ast.AsyncFunctionDef,
        param_taints: dict[str, _Taint],
        enclosing_env: dict[str, _Taint],
        enclosing_bound: set[str],
        enclosing_funcs: dict[
            str, tuple[ast.FunctionDef, dict[str, _Taint], set[str], dict]
        ],
    ) -> None:
        param_names = _function_param_names(func.args)
        local_bound, local_funcs, global_names, nonlocal_names = (
            _collect_scope_bindings(func.body)
        )
        local_bound |= param_names
        func_env: dict[str, _Taint] = {}
        for key, value in enclosing_env.items():
            if (
                key in nonlocal_names
                or key not in local_bound
                or (key in self.dependencies and key not in param_names)
            ):
                func_env[key] = value
        for key in global_names:
            if key in self.module_env:
                func_env[key] = _merge_taint(func_env.get(key), self.module_env[key])  # type: ignore[assignment]
        for key, value in param_taints.items():
            func_env[key] = _merge_taint(func_env.get(key), value)  # type: ignore[assignment]
        cache_key = (id(func), tuple(sorted(func_env.items())))
        if cache_key in self.checked_calls:
            return
        self.checked_calls.add(cache_key)
        body_ctx = _ScopeCtx(
            local_funcs=local_funcs,
            local_bound=local_bound,
            enclosing_funcs=enclosing_funcs,
            enclosing_bound=enclosing_bound,
            global_names=global_names,
            nonlocal_names=nonlocal_names,
            enclosing_env=enclosing_env,
            scope_kind="function",
        )
        self._check_block(func.body, func_env, body_ctx)

    def _check_class_def(self, stmt: ast.ClassDef, env, ctx: _ScopeCtx) -> None:
        if stmt.name in self.dependencies:
            self._fail(stmt, f"unsupported mutation of constant {stmt.name}")
        for decorator in stmt.decorator_list:
            self._eval_expr(decorator, env, ctx)
        for base in stmt.bases:
            taint = self._eval_expr(base, env, ctx)
            if taint is not None:
                self._fail(stmt, f"unsupported escape of constant {taint[0]}")
        for keyword in stmt.keywords:
            taint = self._eval_expr(keyword.value, env, ctx)
            if taint is not None:
                self._fail(stmt, f"unsupported escape of constant {taint[0]}")
        class_bound, class_funcs, global_names, nonlocal_names = (
            _collect_scope_bindings(stmt.body)
        )
        class_env = dict(env)
        outer_env, outer_bound, outer_funcs = self._child_enclosing_scope(env, ctx)
        class_ctx = _ScopeCtx(
            local_funcs=class_funcs,
            local_bound=class_bound,
            enclosing_funcs=outer_funcs,
            enclosing_bound=outer_bound,
            global_names=global_names,
            nonlocal_names=nonlocal_names,
            enclosing_env=outer_env,
            scope_kind="class",
        )
        self._check_block(stmt.body, class_env, class_ctx)

    def _eval_expr(self, node: ast.AST, env, ctx) -> _Taint | None:
        result: _Taint | None = None
        if isinstance(node, ast.Name):
            result = env.get(node.id)
        elif isinstance(node, ast.Attribute):
            val_taint = self._eval_expr(node.value, env, ctx)
            if val_taint is not None:
                self._fail(node, f"unsupported escape of constant {val_taint[0]}")
            if node.attr in self.dependencies:
                result = self.root_taints.get(node.attr)
        elif isinstance(node, ast.Subscript):
            val_taint = self._eval_expr(node.value, env, ctx)
            self._eval_expr(node.slice, env, ctx)
            result = val_taint if (val_taint and val_taint[1]) else None
        elif isinstance(node, ast.Call):
            result = self._eval_call(node, env, ctx)
        elif isinstance(node, (ast.Tuple, ast.List, ast.Set)):
            result = self._eval_sequence(node, env, ctx)
        elif isinstance(node, ast.Dict):
            result = self._eval_dict(node, env, ctx)
        elif isinstance(node, ast.IfExp):
            self._eval_expr(node.test, env, ctx)
            result = _merge_taint(
                self._eval_expr(node.body, env, ctx),
                self._eval_expr(node.orelse, env, ctx),
            )
        elif isinstance(node, ast.BoolOp):
            for value in node.values:
                result = _merge_taint(result, self._eval_expr(value, env, ctx))
        else:
            result = self._eval_other_expr(node, env, ctx)
        return result

    def _eval_sequence(self, node, env, ctx) -> _Taint | None:
        merged: _Taint | None = None
        for elt in node.elts:
            if isinstance(elt, ast.Starred):
                taint = self._eval_expr(elt.value, env, ctx)
                if taint is not None and taint[1]:
                    merged = _merge_taint(merged, (taint[0], True))
            else:
                taint = self._eval_expr(elt, env, ctx)
                if taint is not None:
                    merged = _merge_taint(merged, (taint[0], True))
        return merged

    def _eval_dict(self, node: ast.Dict, env, ctx) -> _Taint | None:
        merged: _Taint | None = None
        for key, value in zip(node.keys, node.values, strict=True):
            if key is None:
                taint = self._eval_expr(value, env, ctx)
                if taint is not None and taint[1]:
                    merged = _merge_taint(merged, (taint[0], True))
            else:
                kt = self._eval_expr(key, env, ctx)
                vt = self._eval_expr(value, env, ctx)
                if kt is not None:
                    merged = _merge_taint(merged, (kt[0], True))
                if vt is not None:
                    merged = _merge_taint(merged, (vt[0], True))
        return merged

    def _eval_lambda(self, node: ast.Lambda, env, ctx: _ScopeCtx) -> None:
        default_taints = self._default_param_taints(node.args, env, ctx)
        param_names = _function_param_names(node.args)
        lam_env = {
            key: value
            for key, value in env.items()
            if key not in param_names or key in self.dependencies
        }
        lam_env.update(default_taints)
        outer_env, outer_bound, outer_funcs = self._child_enclosing_scope(env, ctx)
        lam_ctx = _ScopeCtx(
            local_funcs={},
            local_bound=param_names,
            enclosing_funcs=outer_funcs,
            enclosing_bound=outer_bound,
            global_names=set(),
            nonlocal_names=set(),
            enclosing_env=outer_env,
            scope_kind="function",
        )
        body_taint = self._eval_expr(node.body, lam_env, lam_ctx)
        if body_taint is not None:
            self._fail(node, f"unsupported escape of constant {body_taint[0]}")

    def _eval_other_expr(self, node: ast.AST, env, ctx: _ScopeCtx) -> _Taint | None:
        result: _Taint | None = None
        if isinstance(node, ast.BinOp):
            lt = self._eval_expr(node.left, env, ctx)
            rt = self._eval_expr(node.right, env, ctx)
            if isinstance(
                node.op, (ast.BitOr, ast.Add, ast.Sub, ast.BitAnd, ast.BitXor, ast.Mult)
            ):
                if lt is not None and lt[1]:
                    result = (lt[0], True)
                elif rt is not None and rt[1]:
                    result = (rt[0], True)
        elif isinstance(node, ast.NamedExpr):
            vt = self._eval_expr(node.value, env, ctx)
            if node.target.id in self.dependencies:
                self._fail(node, f"unsupported mutation of constant {node.target.id}")
            if vt is not None:
                self._bind_name(node.target.id, vt, env, ctx)
                if ctx.walrus_env is not None:
                    self._bind_name(node.target.id, vt, ctx.walrus_env, ctx)
            result = vt
        elif isinstance(
            node, (ast.ListComp, ast.SetComp, ast.DictComp, ast.GeneratorExp)
        ):
            result = self._eval_comprehension(node, env, ctx)
        elif isinstance(node, ast.Lambda):
            self._eval_lambda(node, env, ctx)
        elif isinstance(node, (ast.Yield, ast.YieldFrom)):
            yt = (
                self._eval_expr(node.value, env, ctx)
                if node.value is not None
                else None
            )
            if yt is not None:
                self._fail(node, f"unsupported escape of constant {yt[0]}")
        elif isinstance(node, ast.Await):
            result = self._eval_expr(node.value, env, ctx)
        else:
            for child in ast.iter_child_nodes(node):
                self._eval_expr(child, env, ctx)
        return result

    def _eval_comprehension(self, node, env, ctx: _ScopeCtx) -> _Taint | None:
        comp_env = dict(env)
        outer_env, outer_bound, outer_funcs = self._child_enclosing_scope(env, ctx)
        walrus_target = ctx.walrus_env if ctx.walrus_env is not None else env
        comp_targets: set[str] = set()
        comp_ctx = ctx
        for index, gen in enumerate(node.generators):
            iter_taint = self._eval_expr(
                gen.iter, env if index == 0 else comp_env, comp_ctx
            )
            elem_taint = iter_taint if (iter_taint and iter_taint[1]) else None
            gen_names: set[str] = set()
            _collect_target_names(gen.target, gen_names)
            comp_targets |= gen_names
            for name in gen_names:
                if name not in self.dependencies:
                    comp_env.pop(name, None)
            comp_ctx = _ScopeCtx(
                local_funcs={},
                local_bound=set(comp_targets),
                enclosing_funcs=outer_funcs,
                enclosing_bound=outer_bound,
                global_names=ctx.global_names,
                nonlocal_names=ctx.nonlocal_names,
                enclosing_env=outer_env,
                scope_kind="function",
                walrus_env=walrus_target,
            )
            self._check_target(gen.target, elem_taint, comp_env, comp_ctx, False)
            for cond in gen.ifs:
                self._eval_expr(cond, comp_env, comp_ctx)
        if isinstance(node, ast.DictComp):
            kt = self._eval_expr(node.key, comp_env, comp_ctx)
            vt = self._eval_expr(node.value, comp_env, comp_ctx)
            merged = _merge_taint(kt, vt)
            return (merged[0], True) if merged is not None else None
        et = self._eval_expr(node.elt, comp_env, comp_ctx)
        return (et[0], True) if et is not None else None

    def _eval_call_args(self, call: ast.Call, env, ctx):
        pos_taints: list[tuple[bool, _Taint | None]] = []
        for arg in call.args:
            if isinstance(arg, ast.Starred):
                pos_taints.append((True, self._eval_expr(arg.value, env, ctx)))
            else:
                pos_taints.append((False, self._eval_expr(arg, env, ctx)))
        kw_taints: list[tuple[str | None, _Taint | None]] = [
            (kw.arg, self._eval_expr(kw.value, env, ctx)) for kw in call.keywords
        ]
        all_taints = [
            taint for _, taint in (*pos_taints, *kw_taints) if taint is not None
        ]
        return pos_taints, kw_taints, all_taints

    def _check_dynamic_builtin_call(self, call: ast.Call, fname: str) -> _Taint | None:
        if fname in _DYNAMIC_EXEC_BUILTINS:
            self._fail(call, f"unsupported dynamic execution via {fname}")
        if fname in _DYNAMIC_NAMESPACE_BUILTINS and not call.args and not call.keywords:
            self._fail(call, f"unsupported dynamic namespace access via {fname}")
        if (
            fname in {"setattr", "delattr"}
            and len(call.args) >= 2
            and isinstance(call.args[1], ast.Constant)
            and call.args[1].value in self.dependencies
        ):
            self._fail(call, f"unsupported mutation of constant {call.args[1].value}")
        if (
            fname == "getattr"
            and len(call.args) >= 2
            and isinstance(call.args[1], ast.Constant)
            and call.args[1].value in self.dependencies
        ):
            return self.root_taints.get(call.args[1].value)
        return None

    def _is_safe_key_callable(self, expr: ast.AST, local_bound: set[str]) -> bool:
        return (
            isinstance(expr, ast.Name)
            and expr.id in _SAFE_SCALAR_BUILTINS
            and expr.id not in local_bound
            and expr.id not in self.module_bound
        )

    def _check_key_callback_escape(
        self,
        call: ast.Call,
        item_taint: _Taint | None,
        local_bound: set[str],
    ) -> None:
        if item_taint is None:
            return
        for kw in call.keywords:
            if kw.arg is None:
                self._fail(call, f"unsupported escape of constant {item_taint[0]}")
            if kw.arg == "key" and not self._is_safe_key_callable(
                kw.value, local_bound
            ):
                self._fail(call, f"unsupported escape of constant {item_taint[0]}")

    @staticmethod
    def _eval_dict_builtin(
        pos_taints: list[tuple[bool, _Taint | None]],
        kw_taints: list[tuple[str | None, _Taint | None]],
    ) -> _Taint | None:
        merged: _Taint | None = None
        for _, taint in pos_taints:
            if taint is not None and taint[1]:
                merged = _merge_taint(merged, (taint[0], True))
        for key, taint in kw_taints:
            if taint is not None and (key is not None or taint[1]):
                merged = _merge_taint(merged, (taint[0], True))
        return merged

    @staticmethod
    def _eval_defaulting_builtin(
        fname: str,
        pos_taints: list[tuple[bool, _Taint | None]],
        kw_taints: list[tuple[str | None, _Taint | None]],
    ) -> _Taint | None:
        merged: _Taint | None = None
        if pos_taints:
            _, first = pos_taints[0]
            if first is not None and first[1]:
                merged = _merge_taint(merged, (first[0], True))
            for is_star, taint in pos_taints[1:]:
                if taint is None:
                    continue
                if not is_star:
                    merged = _merge_taint(merged, taint)
                elif taint[1]:
                    merged = _merge_taint(merged, (taint[0], True))
        for key, taint in kw_taints:
            if taint is None:
                continue
            if fname == "sum" and key == "start":
                merged = _merge_taint(merged, taint)
            elif taint[1]:
                merged = _merge_taint(merged, (taint[0], True))
        return merged

    def _eval_minmax_builtin(
        self,
        call: ast.Call,
        pos_taints: list[tuple[bool, _Taint | None]],
        kw_taints: list[tuple[str | None, _Taint | None]],
        local_bound: set[str],
    ) -> _Taint | None:
        has_star = any(is_star for is_star, _ in pos_taints)
        direct_pos = len(pos_taints) >= 2 or has_star
        item_taint: _Taint | None = None
        for is_star, taint in pos_taints:
            if taint is None:
                continue
            if not is_star and direct_pos:
                item_taint = _merge_taint(item_taint, taint)
            elif taint[1]:
                item_taint = _merge_taint(item_taint, (taint[0], True))
        merged = item_taint
        for key, taint in kw_taints:
            if taint is None:
                continue
            if key == "default":
                merged = _merge_taint(merged, taint)
            elif key == "key":
                self._fail(call, f"unsupported escape of constant {taint[0]}")
            elif taint[1]:
                merged = _merge_taint(merged, (taint[0], True))
        self._check_key_callback_escape(call, item_taint, local_bound)
        return merged

    def _eval_safe_element_builtin(
        self,
        call: ast.Call,
        fname: str,
        pos_taints: list[tuple[bool, _Taint | None]],
        kw_taints: list[tuple[str | None, _Taint | None]],
        all_taints: list[_Taint],
        local_bound: set[str],
    ) -> _Taint | None:
        if fname == "dict":
            return self._eval_dict_builtin(pos_taints, kw_taints)
        if fname in {"next", "sum"}:
            return self._eval_defaulting_builtin(fname, pos_taints, kw_taints)
        if fname in {"min", "max"}:
            return self._eval_minmax_builtin(call, pos_taints, kw_taints, local_bound)
        if fname == "iter":
            if len(pos_taints) >= 2 and pos_taints[0][1] is not None:
                self._fail(
                    call, f"unsupported escape of constant {pos_taints[0][1][0]}"
                )
            deep = next((t for t in all_taints if t[1]), None)
            return (deep[0], True) if deep is not None else None
        deep = next((t for t in all_taints if t[1]), None)
        if fname == "sorted":
            self._check_key_callback_escape(call, deep, local_bound)
        return (deep[0], True) if deep is not None else None

    def _eval_name_call(
        self,
        call: ast.Call,
        fname: str,
        env,
        ctx: _ScopeCtx,
        pos_taints,
        kw_taints,
        all_taints,
    ) -> _Taint | None:
        visible_bound = ctx.local_bound | ctx.enclosing_bound | self.module_bound
        is_builtin = (
            fname not in self.module_bound
            if fname in ctx.global_names
            else fname not in visible_bound
        )
        if is_builtin:
            dyn = self._check_dynamic_builtin_call(call, fname)
            if dyn is not None:
                return dyn
        if not all_taints:
            return None
        if is_builtin and fname in _SAFE_SCALAR_BUILTINS:
            return None
        if is_builtin and fname in _SAFE_ELEMENT_BUILTINS:
            return self._eval_safe_element_builtin(
                call, fname, pos_taints, kw_taints, all_taints, visible_bound
            )
        resolved = self._resolve_callable(fname, env, ctx)
        if resolved is not None:
            target_func, target_env, target_bound, target_funcs = resolved
            param_taints = _bind_call_taints(target_func, call, pos_taints, kw_taints)
            self._check_function_body(
                target_func, param_taints, target_env, target_bound, target_funcs
            )
            return None
        self._fail(call, f"unsupported escape of constant {all_taints[0][0]}")
        return None

    def _resolve_callable(
        self, fname: str, env: dict[str, _Taint], ctx: _ScopeCtx
    ) -> tuple[ast.FunctionDef, dict[str, _Taint], set[str], dict] | None:
        if fname in ctx.global_names:
            mod_fn = self.module_funcs.get(fname)
            return (mod_fn, self.module_env, self.module_bound, {}) if mod_fn else None
        local_fn = ctx.local_funcs.get(fname)
        if local_fn is not None:
            child_env, child_bound, child_funcs = self._child_enclosing_scope(env, ctx)
            return (local_fn, child_env, child_bound, child_funcs)
        if fname not in ctx.local_bound:
            enclosing_entry = ctx.enclosing_funcs.get(fname)
            if enclosing_entry is not None:
                return enclosing_entry
            if fname not in ctx.enclosing_bound:
                mod_fn = self.module_funcs.get(fname)
                if mod_fn is not None:
                    return (mod_fn, self.module_env, self.module_bound, {})
        return None

    def _eval_call(self, call: ast.Call, env, ctx) -> _Taint | None:
        if isinstance(call.func, ast.Attribute):
            recv_taint = self._eval_expr(call.func.value, env, ctx)
            pos_taints, _, all_taints = self._eval_call_args(call, env, ctx)
            if recv_taint is not None:
                return self._eval_tracked_method_call(
                    call, recv_taint, pos_taints, all_taints
                )
            if all_taints:
                self._fail(call, f"unsupported escape of constant {all_taints[0][0]}")
            return None

        pos_taints, kw_taints, all_taints = self._eval_call_args(call, env, ctx)
        if isinstance(call.func, ast.Name):
            return self._eval_name_call(
                call, call.func.id, env, ctx, pos_taints, kw_taints, all_taints
            )

        func_taint = self._eval_expr(call.func, env, ctx)
        if func_taint is not None:
            self._fail(call, f"unsupported escape of constant {func_taint[0]}")
        if all_taints:
            self._fail(call, f"unsupported escape of constant {all_taints[0][0]}")
        return None

    def _eval_tracked_method_call(
        self,
        call: ast.Call,
        recv_taint: _Taint,
        pos_taints: list[tuple[bool, _Taint | None]],
        all_taints: list[_Taint],
    ) -> _Taint | None:
        assert isinstance(call.func, ast.Attribute)
        method = call.func.attr
        if method in _SAFE_SCALAR_METHODS:
            return None
        if method in _SAFE_ELEMENT_METHODS:
            if method == "get" and len(pos_taints) >= 2:
                return _merge_taint(
                    recv_taint if recv_taint[1] else None, pos_taints[1][1]
                )
            if recv_taint[1]:
                return recv_taint
            deep = next((t for t in all_taints if t[1]), None)
            return (deep[0], True) if deep is not None else None
        self._fail(call, f"unsupported mutation of constant {recv_taint[0]}")
        return None


def _reject_constant_mutations(tree, name, dependencies, dep_values, initial_targets):
    """Reject writes, mutating aliases, and unsupported escapes of ledger constants.

    This is conservative across scopes and branches: a possible mutation or
    escape is unsupported, even when static analysis cannot prove that it will
    execute.
    """
    _ConstantMutationChecker(
        tree, name, dependencies, dep_values, initial_targets
    ).check()


def _counter_records(root, name, section, ledger, attribute):
    payload = _load(root, name)
    totals = Counter()
    records = []
    groups = _container(payload[section], dict, name, section)
    for group, counters in groups.items():
        for kind, count in _container(
            counters, dict, name, f"{section}/{group}"
        ).items():
            if type(count) is not int or count <= 0:
                raise ValueError(f"{name}: non-positive counter {group}/{kind}")
            totals[kind] += count
            records.append(
                _record(
                    ledger,
                    f"{group}/{kind}",
                    name,
                    occurrences=count,
                    **{attribute: group},
                )
            )
    if dict(totals) != _container(payload["totals"], dict, name, "totals"):
        raise ValueError(f"{name}: totals disagree with counters")
    return records


def _schema_records(root):
    payload = _load(root, SCHEMA)
    records = []
    groups = _container(payload["groups"], dict, SCHEMA, "groups")
    for group, metadata in groups.items():
        _container(metadata, dict, SCHEMA, f"groups/{group}")
    sections = set(payload) - {
        "description",
        "baseline_commit",
        "groups",
        "validator_warning_swallows",
        "doc_required_when_pending_from_yaml",
    }
    for section in sorted(sections):
        categories = _container(payload[section], dict, SCHEMA, section)
        for category, fields in categories.items():
            _container(fields, dict, SCHEMA, f"{section}/{category}")
            if category == "summary_counts":
                continue
            for field, group in fields.items():
                attributes = {"group": group, "category": category}
                if "owner_issue" in groups[group]:
                    attributes["owner_issue"] = groups[group]["owner_issue"]
                records.append(
                    _record(
                        "S",
                        f"{section}/{category}/{field}",
                        SCHEMA,
                        **attributes,
                    )
                )
    for section in (
        "validator_warning_swallows",
        "doc_required_when_pending_from_yaml",
    ):
        for field, value in _container(payload[section], dict, SCHEMA, section).items():
            attributes = {"group": section}
            if isinstance(value, dict) and "owner_issue" in value:
                attributes["owner_issue"] = value["owner_issue"]
            records.append(_record("S", f"{section}/{field}", SCHEMA, **attributes))
    return records


def _json_records(root):
    records = _counter_records(
        root,
        "scripts/lint/rust_optional_reads_baseline.json",
        "functions",
        "R",
        "function",
    )
    records += _counter_records(
        root,
        "scripts/lint/fortran_literals_baseline.json",
        "files",
        "L",
        "file",
    )
    name = "scripts/lint/fortran_uninit_baseline.json"
    payload = _load(root, name)
    entries = _entries(payload, name)
    if len(entries) != payload["entry_count"]:
        raise ValueError(f"{name}: entry_count disagrees with entries")
    for item in entries:
        identifier = "::".join(item[key] for key in ("file", "proc", "var", "kind"))
        records.append(_record("U", identifier, name, file=item["file"]))
    records += _schema_records(root)
    for item in _entries(_load(root, MUTATIONS), MUTATIONS):
        for key, expected in (("anchors", list), ("mutations", dict)):
            _container(item[key], expected, MUTATIONS, f"entries/{item['id']}/{key}")
        records.append(
            _record(
                "M",
                item["id"],
                MUTATIONS,
                category=item["category"],
                owner_issue=item["owner_issue"],
                slices=len(item["anchors"]) * len(item["mutations"]),
            )
        )
    name = "scripts/lint/fortran_snan_expected_failures.json"
    for item in _entries(_load(root, name), name):
        records.append(_record("N", item["node"], name, owner_issue=item["issue"]))
    return records


def _rust_list(source, name):
    matches = re.findall(
        rf"\bconst\s+{name}\s*:[^=]+?=\s*&\[(.*?)\];",
        source,
        re.DOTALL,
    )
    if len(matches) != 1:
        raise ValueError(f"{JACOBIAN}: expected one literal array {name}")
    # The supported grammar is a comma-separated list of strings or pairs.
    body = re.sub(r"//[^\n]*", "", matches[0]).strip()
    token = rf"(?:{STRING}|\(\s*{STRING}\s*,\s*(?:{STRING}|[0-9]+)\s*,?\s*\))"
    values = []
    while body:
        match = re.match(rf"({token})\s*(?:,\s*|$)", body)
        if not match:
            raise ValueError(f"{JACOBIAN}: unsupported syntax in {name}: {body[:80]}")
        values.append(ast.literal_eval(match[1]))
        body = body[match.end() :]
    return values


def _jacobian_records(root):
    source = (root / JACOBIAN).read_text(encoding="utf-8")
    records = []
    for ledger, name in (
        ("J1", "KNOWN_DEAD_KEYS"),
        ("J2", "KNOWN_CLOBBERED_SPARTACUS_LAYERS"),
        ("J3", "KNOWN_NON_UNIT_DELTAS"),
        ("J0-multi", "REVIEWED_MULTI_OWNER_SLOTS"),
        ("J0-nan", "KNOWN_BASELINE_NAN_SLOTS"),
    ):
        for value in _rust_list(source, name):
            if ledger in {"J2", "J3"}:
                second_type = int if ledger == "J2" else str
                if not (
                    isinstance(value, tuple)
                    and len(value) == 2
                    and isinstance(value[0], str)
                    and type(value[1]) is second_type
                ):
                    raise ValueError(f"{JACOBIAN}: unsupported entry in {name}")
            elif not isinstance(value, str):
                raise ValueError(f"{JACOBIAN}: expected string in {name}")
            identifier = (
                "::".join(map(str, value)) if isinstance(value, tuple) else value
            )
            records.append(_record(ledger, identifier, JACOBIAN))
    return records


def _observer_records(root):
    values = _python_values(
        root,
        OBSERVER,
        (
            "REVIEWED_UNBRIDGED_COMPONENTS",
            "REVIEWED_DRIVER_EXCLUDED_COMPONENTS",
        ),
    )
    expected = Counter()
    records = []
    for name, entries in values.items():
        for (table, field), reason in _container(entries, dict, OBSERVER, name).items():
            records.append(_record("O", f"{table}.{field}", OBSERVER))
            expected[table, field, reason] += 1
            if name == "REVIEWED_UNBRIDGED_COMPONENTS":
                aggregate = {"heat_state": "heatState", "stebbs_state": "stebbsState"}
                expected[
                    "suews_state", f"{aggregate[table]}%{field}".lower(), reason
                ] += 1
    source = (root / MANIFEST).read_text(encoding="utf-8")
    actual = Counter()
    for table, body in re.findall(
        r"pub const OBS_FIELDS_(\w+): &\[ObsFieldDescriptor\] = &\[(.*?)^\];",
        source,
        re.DOTALL | re.MULTILINE,
    ):
        for block in re.findall(r"ObsFieldDescriptor\s*\{(.*?)\},", body, re.DOTALL):
            if not re.search(r"bridged:\s*false", block):
                continue
            path = re.search(rf"path:\s*({STRING})", block)
            reason = re.search(
                rf"unbridged_reason:\s*Some\(\s*({STRING})\s*,?\s*\)", block
            )
            if not path or not reason:
                raise ValueError(f"{MANIFEST}: unsupported excluded descriptor")
            # Fortran identifiers are case-insensitive; the generator keys its
            # exclusions in lower case but preserves source spelling in paths.
            actual[
                table.lower(), json.loads(path[1]).lower(), json.loads(reason[1])
            ] += 1
    if sum(actual.values()) != len(re.findall(r"bridged:\s*false", source)):
        raise ValueError(f"{MANIFEST}: unparsed excluded descriptors")
    if actual != expected:
        raise ValueError(
            f"{MANIFEST}: generated exclusions disagree with canonical source; "
            f"missing={list((expected - actual).elements())}; "
            f"extra={list((actual - expected).elements())}"
        )
    return records


def _python_ledger_records(root):
    records = []
    for ledger, path, name in (
        (
            "T1",
            "test/data_model/test_legacy_table_roundtrip.py",
            "_SENTINEL_CANONICALISED_COLS",
        ),
        ("T2", "test/data_model/test_legacy_table_roundtrip.py", "_MLM_ALLOWED"),
        ("Z", "scripts/lint/check_saved_fortran_locals.py", "ALLOWED"),
        ("X1", OPTION_SWEEP, "OPTION_VALUES_XFAIL"),
    ):
        values = _python_values(root, path, (name,))[name]
        if ledger in {"Q", "T2", "Z", "X1"}:
            _container(values, dict, path, name)
        elif not isinstance(values, (set, list, tuple)):
            raise ValueError(f"{path}: {name} must be a collection")
        if ledger == "T2":
            keys = [
                f"{version}/{field}"
                for version, fields in values.items()
                for field in fields
            ]
        else:
            keys = values
        if ledger == "Z":
            for key in keys:
                if not (
                    isinstance(key, tuple)
                    and len(key) == 2
                    and all(isinstance(part, str) for part in key)
                ):
                    raise ValueError(
                        f"{path}: unsupported entry key in {name}: {key!r}"
                    )
        for key in sorted(keys):
            identifier = "::".join(map(str, key)) if isinstance(key, tuple) else key
            attributes = {"owner_issue": values[key]} if ledger == "Z" else {}
            records.append(_record(ledger, identifier, path, **attributes))
    return records


_UnitEnv = dict[str, set[str]]
_DeferredNode = ast.FunctionDef | ast.AsyncFunctionDef | ast.Lambda
_DeferredMap = dict[int, tuple[_DeferredNode, _UnitEnv]]


def _copy_unittest_env(env: _UnitEnv) -> _UnitEnv:
    return {key: set(values) for key, values in env.items() if values}


def _update_unittest_env(target: _UnitEnv, source: _UnitEnv) -> None:
    for key, values in source.items():
        if values:
            target.setdefault(key, set()).update(values)


def _classify_unittest_expr(expr: ast.AST, env: _UnitEnv) -> set[str]:
    if isinstance(expr, ast.Name) and isinstance(expr.ctx, ast.Load):
        return set(env.get(expr.id, ()))
    if isinstance(expr, ast.Attribute):
        bases = _classify_unittest_expr(expr.value, env)
        out: set[str] = set()
        if "unittest" in bases and expr.attr == "case":
            out.add("unittest_case")
        if ("unittest" in bases or "unittest_case" in bases) and (
            expr.attr == "expectedFailure"
        ):
            out.add("expected_failure")
        return out
    return set()


def _merge_unittest_envs(envs: list[_UnitEnv]) -> _UnitEnv:
    merged: _UnitEnv = {}
    for branch_env in envs:
        _update_unittest_env(merged, branch_env)
    return merged


class _UnittestExpectedFailureFinder:
    """Find unittest.expectedFailure decorator and call registrations by scope."""

    def __init__(self, tree: ast.Module) -> None:
        self.tree = tree
        self.postponed_annotations = _has_future_annotations(tree)
        self.sites: list[ast.AST] = []
        self._seen_sites: set[int] = set()
        self.module_env: _UnitEnv = {}
        self.module_seen_env: _UnitEnv = {}
        self._seen_sinks: list[tuple[int, _UnitEnv]] = []
        self._active_deferred: _DeferredMap = {}
        self._active_scope_kind: str = "module"
        self._active_non_class_env: _UnitEnv = self.module_env

    def _record_site(self, node: ast.AST) -> None:
        if id(node) not in self._seen_sites:
            self._seen_sites.add(id(node))
            self.sites.append(node)

    @staticmethod
    def _queue_deferred(
        node: _DeferredNode, def_env: _UnitEnv, deferred: _DeferredMap
    ) -> None:
        existing = deferred.get(id(node))
        if existing is None:
            deferred[id(node)] = (node, _copy_unittest_env(def_env))
        else:
            _update_unittest_env(existing[1], def_env)

    def _propagate_to_deferred(
        self, env: _UnitEnv, scope_kind: str, deferred: _DeferredMap
    ) -> None:
        if scope_kind == "class":
            return
        for sink_id, sink_env in self._seen_sinks:
            if sink_id == id(deferred):
                _update_unittest_env(sink_env, env)
        for _, live_env in deferred.values():
            _update_unittest_env(live_env, env)
        if scope_kind == "module":
            _update_unittest_env(self.module_seen_env, env)

    def collect(self) -> list[ast.AST]:
        deferred: _DeferredMap = {}
        self._walk_block(
            self.tree.body, self.module_env, "module", self.module_env, deferred
        )
        self._flush_deferred(deferred)
        return self.sites

    def _flush_deferred(self, deferred: _DeferredMap) -> None:
        for node, live_env in deferred.values():
            if isinstance(node, ast.Lambda):
                param_names = _function_param_names(node.args)
                lam_env: _UnitEnv = {
                    k: set(v) for k, v in live_env.items() if k not in param_names
                }
                lam_deferred: _DeferredMap = {}
                prev = (
                    self._active_deferred,
                    self._active_scope_kind,
                    self._active_non_class_env,
                )
                self._active_deferred = lam_deferred
                self._active_scope_kind = "function"
                self._active_non_class_env = lam_env
                try:
                    self._scan_expr(node.body, lam_env)
                finally:
                    (
                        self._active_deferred,
                        self._active_scope_kind,
                        self._active_non_class_env,
                    ) = prev
                self._flush_deferred(lam_deferred)
                continue
            param_names = _function_param_names(node.args)
            local_bound, _, global_names, nonlocal_names = _collect_scope_bindings(
                node.body
            )
            shadowed = param_names | (local_bound - global_names - nonlocal_names)
            child_env: _UnitEnv = {
                k: set(v) for k, v in live_env.items() if k not in shadowed
            }
            for name in global_names:
                if name in self.module_seen_env:
                    child_env.setdefault(name, set()).update(self.module_seen_env[name])
            child_deferred: _DeferredMap = {}
            self._walk_block(
                node.body, child_env, "function", child_env, child_deferred
            )
            self._flush_deferred(child_deferred)

    def _scan_expr(self, expr: ast.AST, env: _UnitEnv) -> None:
        if isinstance(expr, ast.Call):
            if "expected_failure" in _classify_unittest_expr(expr.func, env):
                self._record_site(expr)
            else:
                self._scan_expr(expr.func, env)
            for arg in expr.args:
                self._scan_expr(arg, env)
            for kw in expr.keywords:
                self._scan_expr(kw.value, env)
            return
        if isinstance(expr, ast.Lambda):
            for default in (*expr.args.defaults, *expr.args.kw_defaults):
                if default is not None:
                    self._scan_expr(default, env)
            def_env = (
                self._active_non_class_env
                if self._active_scope_kind == "class"
                else env
            )
            self._queue_deferred(expr, def_env, self._active_deferred)
            return
        if isinstance(
            expr, (ast.ListComp, ast.SetComp, ast.DictComp, ast.GeneratorExp)
        ):
            comp_env = _copy_unittest_env(env)
            for gen in expr.generators:
                self._scan_expr(gen.iter, comp_env)
                bound: set[str] = set()
                _collect_target_names(gen.target, bound)
                for name in bound:
                    comp_env.pop(name, None)
                for cond in gen.ifs:
                    self._scan_expr(cond, comp_env)
            if isinstance(expr, ast.DictComp):
                self._scan_expr(expr.key, comp_env)
                self._scan_expr(expr.value, comp_env)
            else:
                self._scan_expr(expr.elt, comp_env)
            return
        for child in ast.iter_child_nodes(expr):
            self._scan_expr(child, env)

    def _scan_decorators(self, decorators: list[ast.expr], env: _UnitEnv) -> None:
        for dec in decorators:
            if "expected_failure" in _classify_unittest_expr(dec, env):
                self._record_site(dec)
            else:
                self._scan_expr(dec, env)

    def _bind_target(self, target: ast.AST, kinds: set[str], env: _UnitEnv) -> None:
        if isinstance(target, ast.Name):
            if kinds:
                env[target.id] = set(kinds)
            else:
                env.pop(target.id, None)
            return
        if isinstance(target, (ast.Tuple, ast.List)):
            for elt in target.elts:
                self._bind_target(elt, set(), env)
            return
        if isinstance(target, ast.Starred):
            self._bind_target(target.value, set(), env)
            return
        self._scan_expr(target, env)

    @staticmethod
    def _handle_import(stmt: ast.Import, env: _UnitEnv) -> None:
        for alias in stmt.names:
            bound = alias.asname or alias.name.split(".", 1)[0]
            if alias.name == "unittest":
                env[bound] = {"unittest"}
            elif alias.name == "unittest.case":
                env[bound] = {"unittest_case"} if alias.asname else {"unittest"}
            else:
                env.pop(bound, None)

    @staticmethod
    def _handle_import_from(stmt: ast.ImportFrom, env: _UnitEnv) -> None:
        if stmt.level == 0 and stmt.module == "unittest":
            for alias in stmt.names:
                if alias.name == "expectedFailure":
                    env[alias.asname or "expectedFailure"] = {"expected_failure"}
                elif alias.name == "case":
                    env[alias.asname or "case"] = {"unittest_case"}
                elif alias.name == "*":
                    env["expectedFailure"] = {"expected_failure"}
                    env["case"] = {"unittest_case"}
                else:
                    env.pop(alias.asname or alias.name, None)
        elif stmt.level == 0 and stmt.module == "unittest.case":
            for alias in stmt.names:
                if alias.name in {"expectedFailure", "*"}:
                    env[alias.asname or "expectedFailure"] = {"expected_failure"}
                else:
                    env.pop(alias.asname or alias.name, None)
        else:
            for alias in stmt.names:
                if alias.name != "*":
                    env.pop(alias.asname or alias.name, None)

    def _handle_assign(self, stmt: ast.Assign, env: _UnitEnv) -> None:
        self._scan_expr(stmt.value, env)
        parallel = isinstance(stmt.value, (ast.Tuple, ast.List)) and not any(
            isinstance(elt, ast.Starred) for elt in stmt.value.elts
        )
        if parallel:
            kinds_list = [_classify_unittest_expr(elt, env) for elt in stmt.value.elts]
            for target in stmt.targets:
                if (
                    isinstance(target, (ast.Tuple, ast.List))
                    and not any(isinstance(elt, ast.Starred) for elt in target.elts)
                    and len(target.elts) == len(kinds_list)
                ):
                    for sub_target, sub_kinds in zip(
                        target.elts, kinds_list, strict=True
                    ):
                        self._bind_target(sub_target, sub_kinds, env)
                else:
                    self._bind_target(target, set(), env)
            return
        kinds = _classify_unittest_expr(stmt.value, env)
        for target in stmt.targets:
            self._bind_target(target, kinds, env)

    def _handle_func_def(
        self,
        stmt: ast.FunctionDef | ast.AsyncFunctionDef,
        env: _UnitEnv,
        scope_kind: str,
        non_class_env: _UnitEnv,
        deferred: _DeferredMap,
    ) -> None:
        self._scan_decorators(stmt.decorator_list, env)
        for default in (*stmt.args.defaults, *stmt.args.kw_defaults):
            if default is not None:
                self._scan_expr(default, env)
        if not self.postponed_annotations:
            for arg in (
                *stmt.args.posonlyargs,
                *stmt.args.args,
                *stmt.args.kwonlyargs,
                *(a for a in (stmt.args.vararg, stmt.args.kwarg) if a is not None),
            ):
                if arg.annotation is not None:
                    self._scan_expr(arg.annotation, env)
            if stmt.returns is not None:
                self._scan_expr(stmt.returns, env)
        env.pop(stmt.name, None)
        def_env = non_class_env if scope_kind == "class" else env
        self._queue_deferred(stmt, def_env, deferred)

    def _handle_class_def(
        self,
        stmt: ast.ClassDef,
        env: _UnitEnv,
        scope_kind: str,
        non_class_env: _UnitEnv,
        deferred: _DeferredMap,
    ) -> None:
        self._scan_decorators(stmt.decorator_list, env)
        for base in stmt.bases:
            self._scan_expr(base, env)
        for kw in stmt.keywords:
            self._scan_expr(kw.value, env)
        env.pop(stmt.name, None)
        class_env = _copy_unittest_env(env)
        outer_non_class = non_class_env if scope_kind == "class" else env
        self._walk_block(stmt.body, class_env, "class", outer_non_class, deferred)

    def _walk_try_stmt(
        self,
        stmt: ast.Try | ast.TryStar,
        env: _UnitEnv,
        scope_kind: str,
        non_class_env: _UnitEnv,
        deferred: _DeferredMap,
    ) -> None:
        in_class = scope_kind == "class"
        body_env = _copy_unittest_env(env)
        try_seen = _copy_unittest_env(env)
        self._seen_sinks.append((id(deferred), try_seen))
        try:
            self._walk_block(
                stmt.body,
                body_env,
                scope_kind,
                non_class_env if in_class else body_env,
                deferred,
            )
        finally:
            self._seen_sinks.pop()
        orelse_env = _copy_unittest_env(body_env)
        self._walk_block(
            stmt.orelse,
            orelse_env,
            scope_kind,
            non_class_env if in_class else orelse_env,
            deferred,
        )
        branch_envs = [orelse_env]
        for handler in stmt.handlers:
            if handler.type is not None:
                self._scan_expr(handler.type, try_seen)
            h_env = _copy_unittest_env(try_seen)
            if handler.name:
                h_env.pop(handler.name, None)
            self._walk_block(
                handler.body,
                h_env,
                scope_kind,
                non_class_env if in_class else h_env,
                deferred,
            )
            branch_envs.append(h_env)
        env.clear()
        env.update(_merge_unittest_envs(branch_envs))
        self._propagate_to_deferred(env, scope_kind, deferred)
        self._walk_block(
            stmt.finalbody,
            env,
            scope_kind,
            non_class_env if in_class else env,
            deferred,
        )

    def _walk_match_stmt(
        self,
        stmt: ast.Match,
        env: _UnitEnv,
        scope_kind: str,
        non_class_env: _UnitEnv,
        deferred: _DeferredMap,
    ) -> None:
        in_class = scope_kind == "class"
        self._scan_expr(stmt.subject, env)
        branch_envs = [_copy_unittest_env(env)]
        for case in stmt.cases:
            case_env = _copy_unittest_env(env)
            bound: set[str] = set()
            _collect_pattern_names(case.pattern, bound)
            for name in bound:
                case_env.pop(name, None)
            if case.guard is not None:
                self._scan_expr(case.guard, case_env)
            self._walk_block(
                case.body,
                case_env,
                scope_kind,
                non_class_env if in_class else case_env,
                deferred,
            )
            branch_envs.append(case_env)
        env.clear()
        env.update(_merge_unittest_envs(branch_envs))
        self._propagate_to_deferred(env, scope_kind, deferred)

    def _walk_loop_stmt(
        self,
        stmt: ast.For | ast.AsyncFor | ast.While,
        env: _UnitEnv,
        scope_kind: str,
        non_class_env: _UnitEnv,
        deferred: _DeferredMap,
    ) -> None:
        in_class = scope_kind == "class"
        if isinstance(stmt, (ast.For, ast.AsyncFor)):
            self._scan_expr(stmt.iter, env)
        else:
            self._scan_expr(stmt.test, env)
        entry_env = _copy_unittest_env(env)
        iter_env = _copy_unittest_env(env)
        while True:
            before = _copy_unittest_env(iter_env)
            body_env = _copy_unittest_env(iter_env)
            if isinstance(stmt, (ast.For, ast.AsyncFor)):
                self._bind_target(stmt.target, set(), body_env)
            else:
                self._scan_expr(stmt.test, body_env)
            self._walk_block(
                stmt.body,
                body_env,
                scope_kind,
                non_class_env if in_class else body_env,
                deferred,
            )
            merged_iter = _merge_unittest_envs([before, body_env])
            if merged_iter == before:
                break
            iter_env = merged_iter
        else_env = _merge_unittest_envs([entry_env, iter_env])
        self._walk_block(
            stmt.orelse,
            else_env,
            scope_kind,
            non_class_env if in_class else else_env,
            deferred,
        )
        env.clear()
        env.update(_merge_unittest_envs([entry_env, iter_env, else_env]))
        self._propagate_to_deferred(env, scope_kind, deferred)

    def _walk_compound_stmt(
        self,
        stmt: ast.stmt,
        env: _UnitEnv,
        scope_kind: str,
        non_class_env: _UnitEnv,
        deferred: _DeferredMap,
    ) -> None:
        in_class = scope_kind == "class"
        if isinstance(stmt, ast.If):
            self._scan_expr(stmt.test, env)
            body_env, else_env = _copy_unittest_env(env), _copy_unittest_env(env)
            self._walk_block(
                stmt.body,
                body_env,
                scope_kind,
                non_class_env if in_class else body_env,
                deferred,
            )
            self._walk_block(
                stmt.orelse,
                else_env,
                scope_kind,
                non_class_env if in_class else else_env,
                deferred,
            )
            env.clear()
            env.update(_merge_unittest_envs([body_env, else_env]))
            self._propagate_to_deferred(env, scope_kind, deferred)
        elif isinstance(stmt, (ast.For, ast.AsyncFor, ast.While)):
            self._walk_loop_stmt(stmt, env, scope_kind, non_class_env, deferred)
        elif isinstance(stmt, (ast.With, ast.AsyncWith)):
            for item in stmt.items:
                self._scan_expr(item.context_expr, env)
                if item.optional_vars is not None:
                    self._bind_target(item.optional_vars, set(), env)
            self._walk_block(
                stmt.body,
                env,
                scope_kind,
                non_class_env if in_class else env,
                deferred,
            )
        elif isinstance(stmt, (ast.Try, ast.TryStar)):
            self._walk_try_stmt(stmt, env, scope_kind, non_class_env, deferred)
        elif isinstance(stmt, ast.Match):
            self._walk_match_stmt(stmt, env, scope_kind, non_class_env, deferred)
        else:
            for child in ast.iter_child_nodes(stmt):
                self._scan_expr(child, env)

    def _walk_stmt(
        self,
        stmt: ast.stmt,
        env: _UnitEnv,
        scope_kind: str,
        non_class_env: _UnitEnv,
        deferred: _DeferredMap,
    ) -> None:
        if isinstance(stmt, ast.Import):
            self._handle_import(stmt, env)
            self._propagate_to_deferred(env, scope_kind, deferred)
        elif isinstance(stmt, ast.ImportFrom):
            self._handle_import_from(stmt, env)
            self._propagate_to_deferred(env, scope_kind, deferred)
        elif isinstance(stmt, ast.Assign):
            self._handle_assign(stmt, env)
            self._propagate_to_deferred(env, scope_kind, deferred)
        elif isinstance(stmt, ast.AnnAssign):
            if scope_kind in {"module", "class"} and not self.postponed_annotations:
                self._scan_expr(stmt.annotation, env)
            if stmt.value is not None:
                self._scan_expr(stmt.value, env)
                self._bind_target(
                    stmt.target, _classify_unittest_expr(stmt.value, env), env
                )
                self._propagate_to_deferred(env, scope_kind, deferred)
        elif isinstance(stmt, ast.AugAssign):
            self._scan_expr(stmt.value, env)
            self._bind_target(stmt.target, set(), env)
        elif isinstance(stmt, ast.Delete):
            for target in stmt.targets:
                if isinstance(target, ast.Name):
                    env.pop(target.id, None)
                else:
                    self._scan_expr(target, env)
        elif isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef)):
            self._handle_func_def(stmt, env, scope_kind, non_class_env, deferred)
        elif isinstance(stmt, ast.ClassDef):
            self._handle_class_def(stmt, env, scope_kind, non_class_env, deferred)
        else:
            self._walk_compound_stmt(stmt, env, scope_kind, non_class_env, deferred)

    def _walk_block(
        self,
        stmts: list[ast.stmt],
        env: _UnitEnv,
        scope_kind: str,
        non_class_env: _UnitEnv,
        deferred: _DeferredMap,
    ) -> None:
        prev = (
            self._active_deferred,
            self._active_scope_kind,
            self._active_non_class_env,
        )
        self._active_deferred = deferred
        self._active_scope_kind = scope_kind
        self._active_non_class_env = non_class_env
        try:
            for stmt in stmts:
                self._walk_stmt(stmt, env, scope_kind, non_class_env, deferred)
        finally:
            (
                self._active_deferred,
                self._active_scope_kind,
                self._active_non_class_env,
            ) = prev


def _suppression_nodes(tree, parents):
    """Return each Python suppression once, in source order."""
    sites: list[ast.AST] = []
    seen: set[int] = set()

    def add(node: ast.AST) -> None:
        if id(node) not in seen:
            seen.add(id(node))
            sites.append(node)

    for node in ast.walk(tree):
        if isinstance(node, ast.Call):
            func = node.func
            is_xfail = (isinstance(func, ast.Attribute) and func.attr == "xfail") or (
                isinstance(func, ast.Name) and func.id == "xfail"
            )
        else:
            # Bare marks work in decorators, pytestmark containers and
            # pytest.param(marks=...). The function of a call is already
            # counted above, so never count it twice.
            parent = parents.get(node)
            is_call_func = isinstance(parent, ast.Call) and parent.func is node
            is_bare_pytest = (
                isinstance(node, ast.Attribute)
                and node.attr == "xfail"
                and isinstance(node.value, ast.Attribute)
                and node.value.attr == "mark"
                and isinstance(node.value.value, ast.Name)
                and node.value.value.id == "pytest"
            )
            is_xfail = is_bare_pytest and not is_call_func
        if is_xfail:
            add(node)
    for node in _UnittestExpectedFailureFinder(tree).collect():
        add(node)
    return sorted(sites, key=lambda node: (node.lineno, node.col_offset))


def _suppression_records(root):
    records = []
    option_sites = []
    for path in sorted({
        *(root / "test").rglob("*.py"),
        *(root / "benchmark").rglob("*.py"),
    }):
        name = path.relative_to(root).as_posix()
        tree = ast.parse(path.read_text(encoding="utf-8"), filename=name)
        parents = {
            child: node
            for node in ast.walk(tree)
            for child in ast.iter_child_nodes(node)
        }
        counts = Counter()
        for node in _suppression_nodes(tree, parents):
            scopes = []
            parent = parents.get(node)
            while parent is not None:
                if isinstance(
                    parent, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)
                ):
                    scopes.append(parent.name)
                parent = parents.get(parent)
            scope = ".".join(reversed(scopes)) or "<module>"
            counts[scope] += 1
            identifier = f"{name}::{scope}::{counts[scope]}"
            if name == OPTION_SWEEP and scope == "_sweep_params":
                option_sites.append(node)
            else:
                records.append(_record("X", identifier, name))
    if len(option_sites) != 1:
        raise ValueError(f"{OPTION_SWEEP}: expected one OPTION_VALUES_XFAIL consumer")
    for path in sorted((root / "src/suews_bridge").rglob("*.rs")):
        source = re.sub(
            r"//[^\n]*|/\*.*?\*/", "", path.read_text(encoding="utf-8"), flags=re.DOTALL
        )
        # Include cfg_attr(..., ignore) as well as unconditional attributes.
        for index, _match in enumerate(
            re.finditer(r"#\s*\[[^\]]*\bignore\b[^\]]*\]", source), 1
        ):
            name = path.relative_to(root).as_posix()
            records.append(_record("rust-ignore", f"{name}::{index}", name))
    return records


def extract_records(root: Path) -> tuple[list[dict], list[str]]:
    """Read every registered ledger and independently discover suppressions."""
    records, errors = [], []
    for extractor in (
        _json_records,
        _jacobian_records,
        _observer_records,
        _python_ledger_records,
        _suppression_records,
    ):
        try:
            records.extend(extractor(root))
        except (
            OSError,
            UnicodeError,
            ValueError,
            TypeError,
            KeyError,
            SyntaxError,
            yaml.YAMLError,
        ) as exc:
            errors.append(f"{extractor.__name__}: {exc}")
    counts = Counter((item["ledger"], item["id"]) for item in records)
    errors.extend(
        f"duplicate ledger entry: {key}" for key, count in counts.items() if count > 1
    )
    return sorted(records, key=lambda item: (item["ledger"], item["id"])), errors


def _matches(record, selector):
    return all(
        record["id"].startswith(value)
        if key == "id_prefix"
        else record.get(key) == value
        for key, value in selector.items()
    )


def _mapping_link(mapping, contracts):
    _container(mapping, dict, LINKS, "mappings item")
    allowed = {
        "ledger",
        "id",
        "id_prefix",
        "file",
        "function",
        "group",
        "category",
        "owner_issue",
    }
    if set(mapping) != {"selector", "contract", "issue", "classification"}:
        raise ValueError("expected selector, contract, issue and classification")
    selector = mapping["selector"]
    if not isinstance(selector, dict) or "ledger" not in selector:
        raise ValueError("selector needs a ledger")
    if set(selector) - allowed or not all(
        isinstance(v, str) and v for v in selector.values()
    ):
        raise ValueError("unsupported selector attributes")
    issue = _issue(mapping["issue"])
    contract = mapping["contract"]
    if contract not in contracts:
        raise ValueError(f"unknown contract id: {contract!r}")
    if mapping["classification"] not in CLASSES:
        raise ValueError("unknown classification")
    return {
        "contract": contract,
        "issue": issue,
        "classification": mapping["classification"],
    }


def link_records(
    records: list[dict], mappings: list[dict], contracts: set[str]
) -> tuple[list[dict], list[str]]:
    """Require one valid, live mapping per record, agreeing with native owners."""
    errors = []
    matched = [[] for _ in records]
    for index, mapping in enumerate(mappings):
        label = f"{LINKS}: mapping {index + 1}"
        try:
            link = _mapping_link(mapping, contracts)
        except (ValueError, TypeError, KeyError) as exc:
            errors.append(f"{label}: {exc}")
            continue
        selector = mapping["selector"]
        hits = [i for i, record in enumerate(records) if _matches(record, selector)]
        if not hits:
            errors.append(f"{label}: stale selector {selector}")
        for i in hits:
            matched[i].append(link)
    linked = []
    for record, matches in zip(records, matched, strict=True):
        label = f"{record['ledger']}:{record['id']}"
        if len(matches) != 1:
            errors.append(
                f"{label}: {'unmapped entry' if not matches else 'overlapping selectors'}"
            )
            continue
        link = matches[0]
        if "owner_issue" in record:
            try:
                if _issue(record["owner_issue"]) != link["issue"]:
                    errors.append(f"{label}: owner_issue disagrees with mapping")
            except ValueError as exc:
                errors.append(f"{label}: {exc}")
        linked.append({**record, **link})
    return linked, errors


def _load_links(root):
    contracts = set()
    name = "test/contracts.yml"
    registry = _container(_load(root, name)["contracts"], list, name, "contracts")
    for index, contract in enumerate(registry):
        _container(contract, dict, name, f"contracts/{index}")
        identifier = contract["id"]
        if not isinstance(identifier, str) or not re.fullmatch(
            r"C-[A-Z0-9-]+", identifier
        ):
            raise ValueError(f"invalid contract id: {identifier!r}")
        if identifier in contracts:
            raise ValueError(f"duplicate contract id: {identifier}")
        contracts.add(identifier)
    if not contracts:
        raise ValueError("empty contract registry")
    payload = _load(root, LINKS)
    mappings = _container(payload["mappings"], list, LINKS, "mappings")
    if type(payload["version"]) is not int or payload["version"] != 1:
        raise ValueError("unsupported link schema")
    return contracts, mappings


def check_tree(root: Path) -> dict:
    """Return the inventory, mappings, grouped counts and all diagnostics."""
    records, errors = extract_records(root)
    contracts, mappings = set(), []
    try:
        contracts, mappings = _load_links(root)
    except (
        OSError,
        UnicodeError,
        ValueError,
        TypeError,
        KeyError,
        yaml.YAMLError,
    ) as exc:
        errors.append(f"registry/links: {exc}")
    linked, link_errors = link_records(records, mappings, contracts)
    errors.extend(link_errors)
    counts = {key: dict.fromkeys(CLASSES, 0) for key in sorted(contracts)}
    groups = {}
    for record in linked:
        counts[record["contract"]][record["classification"]] += 1
        key = (
            record["contract"],
            record["issue"],
            record["ledger"],
            record["classification"],
        )
        group = groups.setdefault(key, {"records": 0, "occurrences": 0, "slices": 0})
        group["records"] += 1
        group["occurrences"] += record.get("occurrences", 0)
        group["slices"] += record.get("slices", 0)
    return {
        "summary": {
            "records": len(records),
            "mapped": len(linked),
            "errors": len(errors),
            **{kind: sum(c[kind] for c in counts.values()) for kind in CLASSES},
        },
        "contracts": counts,
        "groups": [
            {
                "contract": key[0],
                "issue": key[1],
                "ledger": key[2],
                "classification": key[3],
                **value,
            }
            for key, value in sorted(groups.items())
        ],
        "issues": sorted({item["issue"] for item in linked}),
        "records": linked,
        "errors": errors,
    }


def render_report(report: dict) -> str:
    """Render ASCII Markdown; a green inventory is not scientific validation."""
    summary = report["summary"]
    status = "FAILED" if report["errors"] else "green"
    lines = [
        "# Recorded test debt",
        "",
        f"Inventory {status} with {summary['defect']} recorded defects; "
        f"{summary['coverage_gap']} coverage gaps; {summary['accepted_exception']} accepted exceptions.",
        f"{summary['mapped']}/{summary['records']} records mapped; {summary['errors']} errors.",
        "",
        "Counts are stored records, not unique bugs or evidence that physics passes.",
        "Occurrence weights apply only to optional-read/literal counters; slices only to mutation records.",
        "",
        "| Contract | Defects | Coverage gaps | Accepted exceptions |",
        "| --- | ---: | ---: | ---: |",
    ]
    for contract, counts in report["contracts"].items():
        lines.append(
            f"| {contract} | {counts['defect']} | {counts['coverage_gap']} | {counts['accepted_exception']} |"
        )
    lines += [
        "",
        "## Ledger and owner breakdown",
        "",
        "| Contract | Issue | Ledger | Classification | Records | Occurrences | Slices |",
        "| --- | --- | --- | --- | ---: | ---: | ---: |",
    ]
    for group in report["groups"]:
        lines.append(
            "| {contract} | #{issue} | {ledger} | {classification} | {records} | {occurrences} | {slices} |".format(
                **group
            )
        )
    lines += ["", "## Errors", ""]
    lines.extend(f"- {error}" for error in report["errors"])
    if not report["errors"]:
        lines.append("None.")
    return "\n".join(lines).encode("ascii", errors="backslashreplace").decode("ascii")


def main(argv: list[str] | None = None) -> int:
    """Print Markdown, optionally persist JSON, and fail findings in strict mode."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--root", type=Path, default=Path(__file__).resolve().parents[2]
    )
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--report", action="store_true", help="report only (default)")
    mode.add_argument(
        "--strict", action="store_true", help="fail on inventory/link errors"
    )
    parser.add_argument(
        "--json", type=Path, help="also write a machine-readable report"
    )
    args = parser.parse_args(argv)
    report = check_tree(args.root)
    print(render_report(report))
    if args.json:
        args.json.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    return int(args.strict and bool(report["errors"]))


if __name__ == "__main__":
    raise SystemExit(main())
