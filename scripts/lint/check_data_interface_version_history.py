#!/usr/bin/env python3
"""Check that forcing, output, and forcing-manifest histories remain append-only.

This audit enforces append-only immutability for the version registries in
``src/supy/data_model/{forcing,output}/version.py`` and the pinned forcing
manifest digest map ``FORCING_PUBLISHED_MANIFEST_DIGESTS`` in
``src/supy/data_model/contracts.py``.
"""

from __future__ import annotations

import argparse
import ast
import os
from pathlib import Path
import re
import subprocess
import sys
from typing import Literal, NamedTuple

from version_audit import (
    extract_literal_assignments,
    read_file_at_ref,
    resolve_merge_base,
)

type InterfaceKind = Literal["forcing", "output"]
type AuditTarget = InterfaceKind | Literal["forcing-manifest"]


class VersionState(NamedTuple):
    """Literal values read from one interface version module."""

    current: str | None
    versions: tuple[tuple[str, str], ...]


_INTERFACES: dict[InterfaceKind, tuple[str, str, str]] = {
    "forcing": (
        "src/supy/data_model/forcing/version.py",
        "CURRENT_FORCING_VERSION",
        "FORCING_VERSIONS",
    ),
    "output": (
        "src/supy/data_model/output/version.py",
        "CURRENT_OUTPUT_VERSION",
        "OUTPUT_VERSIONS",
    ),
}
_FORCING_MANIFEST_PATH = "src/supy/data_model/contracts.py"
_FORCING_MANIFEST_SYMBOL = "FORCING_PUBLISHED_MANIFEST_DIGESTS"
_READONLY_DICT_METHODS = frozenset({"copy", "get", "items", "keys", "values"})
_SEMVER = re.compile(r"^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$")
_DIGEST = re.compile(r"^sha256:[0-9a-f]{64}$")


class AuditFailure(RuntimeError):
    """Raised for a version-history violation that a contributor can fix."""


class _MissingSymbolError(ValueError):
    """Raised when a target symbol is absent from a module."""


def _parse_state(source: str, current_name: str, versions_name: str) -> VersionState:
    assignments = extract_literal_assignments(source, (current_name, versions_name))
    current = assignments[current_name]
    versions = assignments[versions_name]
    if current is not None and not isinstance(current, str):
        raise ValueError(f"{current_name} must be a string or None")
    if not isinstance(versions, dict) or any(
        not isinstance(version, str) or not isinstance(digest, str)
        for version, digest in versions.items()
    ):
        raise ValueError(f"{versions_name} must be a string-to-string dict literal")
    return VersionState(current, tuple(versions.items()))


def _build_parent_map(tree: ast.AST) -> dict[ast.AST, ast.AST]:
    parents: dict[ast.AST, ast.AST] = {}
    for parent in ast.walk(tree):
        for child in ast.iter_child_nodes(parent):
            parents[child] = parent
    return parents


def _collect_param_names(args: ast.arguments) -> set[str]:
    names = {arg.arg for arg in (*args.posonlyargs, *args.args, *args.kwonlyargs)}
    if args.vararg is not None:
        names.add(args.vararg.arg)
    if args.kwarg is not None:
        names.add(args.kwarg.arg)
    return names


def _bound_import_names(node: ast.Import | ast.ImportFrom) -> set[str]:
    names: set[str] = set()
    for alias in node.names:
        if alias.name == "*":
            continue
        names.add(
            alias.asname if alias.asname is not None else alias.name.split(".", 1)[0]
        )
    return names


def _collect_target_names(node: ast.AST) -> set[str]:
    names: set[str] = set()
    if isinstance(node, ast.Name):
        names.add(node.id)
    elif isinstance(node, (ast.Tuple, ast.List)):
        for elt in node.elts:
            names.update(_collect_target_names(elt))
    elif isinstance(node, ast.Starred):
        names.update(_collect_target_names(node.value))
    return names


def _unconditionally_binds_in_scope(stmt: ast.stmt, symbol_name: str) -> bool:
    if isinstance(stmt, ast.Assign):
        return any(
            symbol_name in _collect_target_names(target) for target in stmt.targets
        )
    if isinstance(stmt, ast.AnnAssign):
        return (
            stmt.value is not None
            and isinstance(stmt.target, ast.Name)
            and stmt.target.id == symbol_name
        )
    if isinstance(stmt, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
        return stmt.name == symbol_name
    if isinstance(stmt, (ast.Import, ast.ImportFrom)):
        return symbol_name in _bound_import_names(stmt)
    if isinstance(stmt, ast.TypeAlias):
        return isinstance(stmt.name, ast.Name) and stmt.name.id == symbol_name
    return False


def _may_unbind_in_scope(node: ast.AST, symbol_name: str) -> bool:
    """Return True if ``node`` may delete/unbind ``symbol_name`` in the current scope."""
    if isinstance(
        node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Lambda)
    ):
        return False
    if (
        isinstance(node, ast.Name)
        and isinstance(node.ctx, ast.Del)
        and node.id == symbol_name
    ):
        return True
    if isinstance(node, ast.ExceptHandler) and node.name == symbol_name:
        return True
    return any(
        _may_unbind_in_scope(child, symbol_name) for child in ast.iter_child_nodes(node)
    )


def _has_wildcard_import(node: ast.Import | ast.ImportFrom) -> bool:
    return isinstance(node, ast.ImportFrom) and any(
        alias.name == "*" for alias in node.names
    )


def _collect_scope_bindings(
    nodes: list[ast.stmt] | list[ast.AST],
) -> tuple[set[str], set[str]]:
    """Collect names bound or declared ``global`` directly in one scope."""
    bound: set[str] = set()
    globals_: set[str] = set()

    def _visit_comprehension_namedexpr(node: ast.AST) -> None:
        if isinstance(
            node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Lambda)
        ):
            return
        if isinstance(node, ast.NamedExpr) and isinstance(node.target, ast.Name):
            bound.add(node.target.id)
        for child in ast.iter_child_nodes(node):
            _visit_comprehension_namedexpr(child)

    def _visit_function_header(node: ast.FunctionDef | ast.AsyncFunctionDef) -> None:
        for expr in (
            *node.decorator_list,
            *node.args.defaults,
            *(d for d in node.args.kw_defaults if d is not None),
        ):
            _visit(expr)
        for arg in (
            *node.args.posonlyargs,
            *node.args.args,
            *node.args.kwonlyargs,
            *(a for a in (node.args.vararg, node.args.kwarg) if a is not None),
        ):
            if arg.annotation is not None:
                _visit(arg.annotation)
        if node.returns is not None:
            _visit(node.returns)

    def _visit(node: ast.AST) -> None:
        if isinstance(node, ast.Global):
            globals_.update(node.names)
        elif isinstance(node, ast.Name):
            if isinstance(node.ctx, (ast.Store, ast.Del)):
                bound.add(node.id)
        elif isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            bound.add(node.name)
            _visit_function_header(node)
        elif isinstance(node, ast.ClassDef):
            bound.add(node.name)
            for expr in (*node.decorator_list, *node.bases):
                _visit(expr)
            for kw in node.keywords:
                _visit(kw.value)
        elif isinstance(node, ast.Lambda):
            for expr in (
                *node.args.defaults,
                *(d for d in node.args.kw_defaults if d is not None),
            ):
                _visit(expr)
        elif isinstance(
            node, (ast.ListComp, ast.SetComp, ast.DictComp, ast.GeneratorExp)
        ):
            _visit(node.generators[0].iter)
            _visit_comprehension_namedexpr(node)
        elif isinstance(node, (ast.Import, ast.ImportFrom)):
            bound.update(_bound_import_names(node))
        else:
            if (
                isinstance(node, (ast.ExceptHandler, ast.MatchAs, ast.MatchStar))
                and node.name is not None
            ):
                bound.add(node.name)
            elif isinstance(node, ast.MatchMapping) and node.rest is not None:
                bound.add(node.rest)
            for child in ast.iter_child_nodes(node):
                _visit(child)

    for item in nodes:
        _visit(item)
    return bound, globals_


def _is_allowed_readonly_load(
    name_node: ast.Name,
    parents: dict[ast.AST, ast.AST],
) -> bool:
    parent = parents.get(name_node)
    if (
        isinstance(parent, ast.Attribute)
        and parent.value is name_node
        and parent.attr in _READONLY_DICT_METHODS
    ):
        grandparent = parents.get(parent)
        return isinstance(grandparent, ast.Call) and grandparent.func is parent
    if (
        isinstance(parent, ast.Subscript)
        and parent.value is name_node
        and isinstance(parent.ctx, ast.Load)
    ):
        return True
    if (
        isinstance(parent, ast.Compare)
        and len(parent.ops) == 1
        and isinstance(parent.ops[0], (ast.In, ast.NotIn))
        and parent.comparators[0] is name_node
    ):
        return True
    return (
        isinstance(parent, (ast.For, ast.AsyncFor, ast.comprehension))
        and parent.iter is name_node
    )


def _find_single_top_level_assignment(
    tree: ast.Module,
    symbol_name: str,
) -> tuple[ast.Name, ast.expr]:
    found: tuple[ast.Name, ast.expr] | None = None
    for node in tree.body:
        if isinstance(node, ast.Assign):
            matches = [
                target
                for target in node.targets
                if isinstance(target, ast.Name) and target.id == symbol_name
            ]
            if not matches:
                continue
            if len(node.targets) != 1 or len(matches) != 1 or found is not None:
                raise ValueError(
                    f"{symbol_name} must be assigned exactly once as a single-target top-level dict literal"
                )
            found = (matches[0], node.value)
        elif (
            isinstance(node, ast.AnnAssign)
            and isinstance(node.target, ast.Name)
            and node.target.id == symbol_name
        ):
            if node.value is None or found is not None:
                raise ValueError(
                    f"{symbol_name} must be assigned exactly once as a single-target top-level dict literal"
                )
            found = (node.target, node.value)

    if found is None:
        module_bound, _ = _collect_scope_bindings(tree.body)
        if symbol_name in module_bound:
            raise ValueError(
                f"{symbol_name} must be assigned once as a single-target top-level dict literal"
            )
        raise _MissingSymbolError(f"could not read {symbol_name}")
    return found


class _ScopeAuditor:
    """Scope-aware auditor ensuring a module constant is never rebound or mutated."""

    def __init__(
        self,
        symbol_name: str,
        definition_target: ast.Name,
        parents: dict[ast.AST, ast.AST],
    ) -> None:
        self.symbol_name = symbol_name
        self.definition_target = definition_target
        self.parents = parents

    def _reject_rebind(self) -> None:
        raise ValueError(f"{self.symbol_name} must not be reassigned or deleted")

    def audit_block(
        self,
        nodes: list[ast.stmt] | list[ast.AST],
        *,
        rebind_targets_module: bool,
        load_targets_module: bool,
        enclosing_shadowed: bool,
        is_class_body: bool = False,
        lazy_class_fallback: bool = False,
    ) -> None:
        """Audit statements in one lexical scope."""
        class_bound_seen = False
        for stmt in nodes:
            if is_class_body and _may_unbind_in_scope(stmt, self.symbol_name):
                class_bound_seen = False
            active_load = load_targets_module and not class_bound_seen
            if is_class_body and isinstance(stmt, (ast.Assign, ast.AnnAssign)):
                self._visit_class_assignment(
                    stmt,
                    rebind_targets_module=rebind_targets_module,
                    load_targets_module=active_load,
                    enclosing_shadowed=enclosing_shadowed,
                    lazy_class_fallback=lazy_class_fallback,
                )
                if _unconditionally_binds_in_scope(stmt, self.symbol_name):
                    class_bound_seen = True
                continue

            self._visit(
                stmt,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=active_load,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
            if (
                is_class_body
                and isinstance(stmt, ast.stmt)
                and _unconditionally_binds_in_scope(stmt, self.symbol_name)
                and not _may_unbind_in_scope(stmt, self.symbol_name)
            ):
                class_bound_seen = True

    def _visit_class_assignment(
        self,
        stmt: ast.Assign | ast.AnnAssign,
        *,
        rebind_targets_module: bool,
        load_targets_module: bool,
        enclosing_shadowed: bool,
        lazy_class_fallback: bool,
    ) -> None:
        if stmt.value is not None:
            self._visit(
                stmt.value,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        if isinstance(stmt, ast.Assign):
            for target in stmt.targets:
                self._visit_binding_target(
                    target,
                    rebind_targets_module=rebind_targets_module,
                    load_targets_module=load_targets_module,
                    enclosing_shadowed=enclosing_shadowed,
                    lazy_class_fallback=lazy_class_fallback,
                )
            return
        if stmt.annotation is not None:
            self._visit(
                stmt.annotation,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module or lazy_class_fallback,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        self._visit_binding_target(
            stmt.target,
            rebind_targets_module=rebind_targets_module,
            load_targets_module=load_targets_module,
            enclosing_shadowed=enclosing_shadowed,
            lazy_class_fallback=lazy_class_fallback,
        )

    def _visit_binding_target(
        self,
        target: ast.AST,
        *,
        rebind_targets_module: bool,
        load_targets_module: bool,
        enclosing_shadowed: bool,
        lazy_class_fallback: bool,
    ) -> None:
        """Audit an assignment/comprehension target without skipping subscript writes."""
        if isinstance(target, ast.Name):
            if rebind_targets_module and target.id == self.symbol_name:
                self._reject_rebind()
            return
        if isinstance(target, (ast.Tuple, ast.List)):
            for elt in target.elts:
                self._visit_binding_target(
                    elt,
                    rebind_targets_module=rebind_targets_module,
                    load_targets_module=load_targets_module,
                    enclosing_shadowed=enclosing_shadowed,
                    lazy_class_fallback=lazy_class_fallback,
                )
            return
        if isinstance(target, ast.Starred):
            self._visit_binding_target(
                target.value,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
            return
        self._visit(
            target,
            rebind_targets_module=rebind_targets_module,
            load_targets_module=load_targets_module,
            enclosing_shadowed=enclosing_shadowed,
            lazy_class_fallback=lazy_class_fallback,
        )

    def _visit_function(
        self,
        node: ast.FunctionDef | ast.AsyncFunctionDef,
        *,
        rebind_targets_module: bool,
        load_targets_module: bool,
        enclosing_shadowed: bool,
        lazy_class_fallback: bool,
    ) -> None:
        if rebind_targets_module and node.name == self.symbol_name:
            self._reject_rebind()
        for expr in (
            *node.decorator_list,
            *node.args.defaults,
            *(d for d in node.args.kw_defaults if d is not None),
        ):
            self._visit(
                expr,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        lazy_load = load_targets_module or lazy_class_fallback
        for expr in node.type_params:
            self._visit(
                expr,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=lazy_load,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        for arg in (
            *node.args.posonlyargs,
            *node.args.args,
            *node.args.kwonlyargs,
            *(a for a in (node.args.vararg, node.args.kwarg) if a is not None),
        ):
            if arg.annotation is not None:
                self._visit(
                    arg.annotation,
                    rebind_targets_module=rebind_targets_module,
                    load_targets_module=lazy_load,
                    enclosing_shadowed=enclosing_shadowed,
                    lazy_class_fallback=lazy_class_fallback,
                )
        if node.returns is not None:
            self._visit(
                node.returns,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=lazy_load,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )

        local_bound, local_globals = _collect_scope_bindings(node.body)
        params = _collect_param_names(node.args)
        if self.symbol_name in local_globals:
            child_rebind = True
            child_load = True
            child_shadowed = False
        elif self.symbol_name in params or self.symbol_name in local_bound:
            child_rebind = False
            child_load = False
            child_shadowed = True
        else:
            child_rebind = False
            child_load = not enclosing_shadowed
            child_shadowed = enclosing_shadowed
        self.audit_block(
            node.body,
            rebind_targets_module=child_rebind,
            load_targets_module=child_load,
            enclosing_shadowed=child_shadowed,
        )

    def _visit_class(
        self,
        node: ast.ClassDef,
        *,
        rebind_targets_module: bool,
        load_targets_module: bool,
        enclosing_shadowed: bool,
        lazy_class_fallback: bool,
    ) -> None:
        if rebind_targets_module and node.name == self.symbol_name:
            self._reject_rebind()
        for expr in (*node.decorator_list, *node.bases):
            self._visit(
                expr,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        lazy_load = load_targets_module or lazy_class_fallback
        for expr in node.type_params:
            self._visit(
                expr,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=lazy_load,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        for kw in node.keywords:
            self._visit(
                kw.value,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        class_bound, class_globals = _collect_scope_bindings(node.body)
        is_global = self.symbol_name in class_globals
        binds_in_class = self.symbol_name in class_bound
        # Per Python's class-block name resolution, binding a name anywhere in a
        # class body suppresses enclosing-function cell lookup and falls back
        # directly to module globals whenever the class entry is unbound.
        class_load = True if (is_global or binds_in_class) else not enclosing_shadowed
        self.audit_block(
            node.body,
            rebind_targets_module=is_global,
            load_targets_module=class_load,
            enclosing_shadowed=enclosing_shadowed,
            is_class_body=not is_global,
            lazy_class_fallback=class_load,
        )

    def _visit_lambda(
        self,
        node: ast.Lambda,
        *,
        rebind_targets_module: bool,
        load_targets_module: bool,
        enclosing_shadowed: bool,
        lazy_class_fallback: bool,
    ) -> None:
        for expr in (
            *node.args.defaults,
            *(d for d in node.args.kw_defaults if d is not None),
        ):
            self._visit(
                expr,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        params = _collect_param_names(node.args)
        local_bound, _ = _collect_scope_bindings([node.body])
        shadowed = self.symbol_name in params or self.symbol_name in local_bound
        self.audit_block(
            [node.body],
            rebind_targets_module=False,
            load_targets_module=False if shadowed else not enclosing_shadowed,
            enclosing_shadowed=True if shadowed else enclosing_shadowed,
        )

    def _visit_comprehension(
        self,
        node: ast.ListComp | ast.SetComp | ast.DictComp | ast.GeneratorExp,
        *,
        rebind_targets_module: bool,
        load_targets_module: bool,
        enclosing_shadowed: bool,
        lazy_class_fallback: bool,
    ) -> None:
        # Outermost iterable evaluates in the enclosing statement's scope (including class attrs);
        # comprehension targets, subsequent iterables, filters, and body resolve lexically
        # (skipping class namespaces per Python's execution model).
        self._visit(
            node.generators[0].iter,
            rebind_targets_module=rebind_targets_module,
            load_targets_module=load_targets_module,
            enclosing_shadowed=enclosing_shadowed,
            lazy_class_fallback=lazy_class_fallback,
        )
        comp_targets: set[str] = set()
        for gen in node.generators:
            comp_targets.update(_collect_target_names(gen.target))
        shadowed = self.symbol_name in comp_targets
        comp_load = False if shadowed else not enclosing_shadowed
        comp_shadowed = True if shadowed else enclosing_shadowed
        for idx, gen in enumerate(node.generators):
            if idx > 0:
                self._visit(
                    gen.iter,
                    rebind_targets_module=rebind_targets_module,
                    load_targets_module=comp_load,
                    enclosing_shadowed=comp_shadowed,
                    lazy_class_fallback=False,
                )
            self._visit_binding_target(
                gen.target,
                rebind_targets_module=False,
                load_targets_module=comp_load,
                enclosing_shadowed=comp_shadowed,
                lazy_class_fallback=False,
            )
            for if_expr in gen.ifs:
                self._visit(
                    if_expr,
                    rebind_targets_module=rebind_targets_module,
                    load_targets_module=comp_load,
                    enclosing_shadowed=comp_shadowed,
                    lazy_class_fallback=False,
                )
        body_exprs = (
            (node.key, node.value) if isinstance(node, ast.DictComp) else (node.elt,)
        )
        for expr in body_exprs:
            self._visit(
                expr,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=comp_load,
                enclosing_shadowed=comp_shadowed,
                lazy_class_fallback=False,
            )

    def _visit_type_alias(
        self,
        node: ast.TypeAlias,
        *,
        rebind_targets_module: bool,
        load_targets_module: bool,
        enclosing_shadowed: bool,
        lazy_class_fallback: bool,
    ) -> None:
        if (
            rebind_targets_module
            and isinstance(node.name, ast.Name)
            and node.name.id == self.symbol_name
        ):
            self._reject_rebind()
        # TypeAlias values and type-parameter bounds are evaluated lazily after
        # class execution, when a class-local binding may have been deleted.
        lazy_load = load_targets_module or lazy_class_fallback
        for param in node.type_params:
            self._visit(
                param,
                rebind_targets_module=False,
                load_targets_module=lazy_load,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        self._visit(
            node.value,
            rebind_targets_module=False,
            load_targets_module=lazy_load,
            enclosing_shadowed=enclosing_shadowed,
            lazy_class_fallback=lazy_class_fallback,
        )

    def _visit_name(
        self,
        node: ast.Name,
        *,
        rebind_targets_module: bool,
        load_targets_module: bool,
    ) -> None:
        if node.id != self.symbol_name:
            return
        if isinstance(node.ctx, (ast.Store, ast.Del)):
            if rebind_targets_module and node is not self.definition_target:
                self._reject_rebind()
        elif (
            isinstance(node.ctx, ast.Load)
            and load_targets_module
            and not _is_allowed_readonly_load(node, self.parents)
        ):
            raise ValueError(
                f"{self.symbol_name} must only be used in read-only lookups"
            )

    def _visit(
        self,
        node: ast.AST,
        *,
        rebind_targets_module: bool,
        load_targets_module: bool,
        enclosing_shadowed: bool,
        lazy_class_fallback: bool,
    ) -> None:
        if isinstance(node, ast.Name):
            self._visit_name(
                node,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module,
            )
        elif isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef)):
            self._visit_function(
                node,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        elif isinstance(node, ast.ClassDef):
            self._visit_class(
                node,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        elif isinstance(node, ast.Lambda):
            self._visit_lambda(
                node,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        elif isinstance(
            node, (ast.ListComp, ast.SetComp, ast.DictComp, ast.GeneratorExp)
        ):
            self._visit_comprehension(
                node,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        elif isinstance(node, ast.TypeAlias):
            self._visit_type_alias(
                node,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        elif isinstance(node, ast.NamedExpr):
            if (
                rebind_targets_module
                and isinstance(node.target, ast.Name)
                and node.target.id == self.symbol_name
            ):
                self._reject_rebind()
            self._visit(
                node.value,
                rebind_targets_module=rebind_targets_module,
                load_targets_module=load_targets_module,
                enclosing_shadowed=enclosing_shadowed,
                lazy_class_fallback=lazy_class_fallback,
            )
        elif isinstance(node, (ast.Import, ast.ImportFrom)):
            if rebind_targets_module and _has_wildcard_import(node):
                raise ValueError(
                    f"{self.symbol_name} cannot be verified alongside wildcard imports"
                )
            if rebind_targets_module and self.symbol_name in _bound_import_names(node):
                self._reject_rebind()
        else:
            if rebind_targets_module and (
                (
                    isinstance(node, (ast.ExceptHandler, ast.MatchAs, ast.MatchStar))
                    and node.name == self.symbol_name
                )
                or (
                    isinstance(node, ast.MatchMapping) and node.rest == self.symbol_name
                )
            ):
                self._reject_rebind()
            for child in ast.iter_child_nodes(node):
                self._visit(
                    child,
                    rebind_targets_module=rebind_targets_module,
                    load_targets_module=load_targets_module,
                    enclosing_shadowed=enclosing_shadowed,
                    lazy_class_fallback=lazy_class_fallback,
                )


def _ensure_no_reassignment_or_mutation(
    tree: ast.Module,
    symbol_name: str,
    definition_target: ast.Name,
) -> None:
    auditor = _ScopeAuditor(symbol_name, definition_target, _build_parent_map(tree))
    auditor.audit_block(
        tree.body,
        rebind_targets_module=True,
        load_targets_module=True,
        enclosing_shadowed=False,
    )


def _parse_string_dict_literal(
    value_node: ast.expr,
    symbol_name: str,
) -> tuple[tuple[str, str], ...]:
    if not isinstance(value_node, ast.Dict):
        raise ValueError(f"{symbol_name} must be a string-to-string dict literal")
    entries: list[tuple[str, str]] = []
    seen: set[str] = set()
    for key_node, val_node in zip(value_node.keys, value_node.values, strict=True):
        if key_node is None:
            raise ValueError(f"{symbol_name} must be a string-to-string dict literal")
        version = ast.literal_eval(key_node)
        digest = ast.literal_eval(val_node)
        if not isinstance(version, str) or not isinstance(digest, str):
            raise ValueError(f"{symbol_name} must be a string-to-string dict literal")
        if version in seen:
            raise ValueError(
                f"{symbol_name} contains duplicate version key {version!r}"
            )
        seen.add(version)
        entries.append((version, digest))
    return tuple(entries)


def _parse_manifest_digests(
    source: str,
    symbol_name: str = _FORCING_MANIFEST_SYMBOL,
) -> tuple[tuple[str, str], ...]:
    tree = ast.parse(source)
    definition_target, value_node = _find_single_top_level_assignment(tree, symbol_name)
    _ensure_no_reassignment_or_mutation(tree, symbol_name, definition_target)
    return _parse_string_dict_literal(value_node, symbol_name)


def _validate_version_entries(
    label: str,
    versions: tuple[tuple[str, str], ...],
) -> None:
    previous: tuple[int, int, int] | None = None
    for index, (version, digest) in enumerate(versions):
        match = _SEMVER.fullmatch(version)
        if match is None:
            raise AuditFailure(
                f"{label}: version {version!r} must use stable MAJOR.MINOR.PATCH SemVer"
            )
        parsed = tuple(int(part) for part in match.groups())
        if index == 0 and parsed != (1, 0, 0):
            raise AuditFailure(f"{label}: the first public version must be '1.0.0'")
        if previous is not None and parsed <= previous:
            raise AuditFailure(f"{label}: version history must increase monotonically")
        if _DIGEST.fullmatch(digest) is None:
            raise AuditFailure(
                f"{label}: version {version!r} needs a lowercase SHA-256 digest"
            )
        previous = parsed


def _validate_state(kind: InterfaceKind, state: VersionState) -> None:
    if not state.versions:
        if state.current is not None:
            raise AuditFailure(f"{kind}: an empty history requires current=None")
        return

    if state.current != state.versions[-1][0]:
        raise AuditFailure(f"{kind}: current must be the last registered version")

    _validate_version_entries(kind, state.versions)


def _validate_transition(
    kind: InterfaceKind,
    base: VersionState,
    current: VersionState,
) -> bool:
    base_length = len(base.versions)
    if (
        len(current.versions) < base_length
        or current.versions[:base_length] != base.versions
    ):
        raise AuditFailure(f"{kind}: released version history is append-only")

    appended = len(current.versions) > base_length
    current_changed = current.current != base.current
    if appended != current_changed:
        raise AuditFailure(
            f"{kind}: current version and version history must change together"
        )
    return appended


def _validate_manifest_digests(digests: tuple[tuple[str, str], ...]) -> None:
    if not digests:
        return
    _validate_version_entries("forcing-manifest", digests)


def _validate_manifest_digests_transition(
    base: tuple[tuple[str, str], ...],
    current: tuple[tuple[str, str], ...],
) -> bool:
    base_length = len(base)
    if len(current) < base_length or current[:base_length] != base:
        raise AuditFailure(
            "forcing-manifest: FORCING_PUBLISHED_MANIFEST_DIGESTS is append-only"
        )
    return len(current) > base_length


def _validate_manifest_digests_parity(
    forcing_versions: tuple[tuple[str, str], ...],
    manifest_digests: tuple[tuple[str, str], ...],
) -> None:
    forcing_keys = tuple(version for version, _ in forcing_versions)
    manifest_keys = tuple(version for version, _ in manifest_digests)
    if manifest_keys != forcing_keys:
        raise AuditFailure(
            "forcing-manifest: FORCING_PUBLISHED_MANIFEST_DIGESTS versions must "
            "match FORCING_VERSIONS"
        )


def _state_at_base(
    merge_base: str,
    path: str,
    current_name: str,
    versions_name: str,
) -> VersionState:
    source = read_file_at_ref(merge_base, path)
    if source is None:
        return VersionState(None, ())
    return _parse_state(source, current_name, versions_name)


def _manifest_digests_at_base(
    merge_base: str,
) -> tuple[bool, tuple[tuple[str, str], ...]]:
    source = read_file_at_ref(merge_base, _FORCING_MANIFEST_PATH)
    if source is None:
        return False, ()
    try:
        return True, _parse_manifest_digests(source)
    except _MissingSymbolError:
        return False, ()


def _audit(base_ref: str) -> list[AuditTarget]:
    merge_base = resolve_merge_base(base_ref)
    changed: list[AuditTarget] = []
    current_states: dict[InterfaceKind, VersionState] = {}
    for kind, (path, current_name, versions_name) in _INTERFACES.items():
        base = _state_at_base(merge_base, path, current_name, versions_name)
        current = _parse_state(
            Path(path).read_text(encoding="utf-8"),
            current_name,
            versions_name,
        )
        _validate_state(kind, current)
        if _validate_transition(kind, base, current):
            changed.append(kind)
        current_states[kind] = current

    contracts_in_base, base_manifests = _manifest_digests_at_base(merge_base)
    manifest_path = Path(_FORCING_MANIFEST_PATH)
    if not manifest_path.exists():
        if contracts_in_base or current_states["forcing"].versions:
            raise AuditFailure(f"forcing-manifest: {_FORCING_MANIFEST_PATH} is missing")
    else:
        current_manifests = _parse_manifest_digests(
            manifest_path.read_text(encoding="utf-8")
        )
        appended = _validate_manifest_digests_transition(
            base_manifests, current_manifests
        )
        _validate_manifest_digests(current_manifests)
        _validate_manifest_digests_parity(
            current_states["forcing"].versions, current_manifests
        )
        if appended:
            changed.append("forcing-manifest")
    return changed


def main(argv: list[str] | None = None) -> int:
    """Run the forcing/output version-history audit."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--base",
        default="origin/master",
        help="Git ref to compare against (default: origin/master).",
    )
    args = parser.parse_args(argv)

    try:
        changed = _audit(args.base)
    except AuditFailure as exc:
        print(f"[data-interface-version-history] FAILED: {exc}", file=sys.stderr)
        return 1
    except (OSError, SyntaxError, subprocess.CalledProcessError, ValueError) as exc:
        print(f"[data-interface-version-history] ERROR: {exc}", file=sys.stderr)
        return 2

    if changed:
        print(
            "[data-interface-version-history] appended version history: "
            + ", ".join(changed)
        )
    else:
        print("[data-interface-version-history] no version-history changes.")
    return 0


if __name__ == "__main__":
    os.chdir(Path(__file__).resolve().parents[2])
    sys.exit(main())
