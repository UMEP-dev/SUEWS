"""Tests for the SUEWS unified CLI JSON envelope.

The envelope is the single contract every `suews` subcommand emits and every
MCP tool consumes. Shape: ``{status, data, errors, warnings, meta}``.
"""

from __future__ import annotations

import json

import pytest

from supy.cmd.json_envelope import EXIT_OK, EXIT_USER_ERROR, Envelope

pytestmark = pytest.mark.api


class TestEnvelopeStatus:
    def test_success_when_no_errors_no_warnings(self) -> None:
        env = Envelope.success(data={"x": 1}, command="suews validate")
        assert env.status == "success"
        assert env.errors == []
        assert env.warnings == []

    def test_warning_when_no_errors_but_warnings_present(self) -> None:
        env = Envelope.success(
            data={"x": 1},
            command="suews validate",
            warnings=["forcing has gaps"],
        )
        assert env.status == "warning"
        assert env.warnings == ["forcing has gaps"]

    def test_error_when_errors_present(self) -> None:
        env = Envelope.error(
            errors=["schema mismatch"],
            command="suews validate",
        )
        assert env.status == "error"
        # String errors are coerced to {"message": str}; see TestErrorCoercion.
        assert env.errors == [{"message": "schema mismatch"}]
        assert env.data == {}


class TestEnvelopeMeta:
    def test_meta_has_required_keys(self) -> None:
        env = Envelope.success(data={}, command="suews validate")
        meta = env.meta
        for key in (
            "schema_version",
            "suews_version",
            "supy_version",
            "git_commit",
            "command",
            "started_at",
            "ended_at",
        ):
            assert key in meta, f"meta missing key {key!r}"

    def test_meta_command_round_trip(self) -> None:
        env = Envelope.success(data={}, command="suews validate")
        assert env.meta["command"] == "suews validate"

    def test_meta_schema_version_matches_module(self) -> None:
        from supy.data_model.schema.version import CURRENT_SCHEMA_VERSION

        env = Envelope.success(data={}, command="suews schema info")
        assert env.meta["schema_version"] == CURRENT_SCHEMA_VERSION

    def test_meta_supy_version_matches_module(self) -> None:
        import supy

        env = Envelope.success(data={}, command="suews validate")
        assert env.meta["supy_version"] == supy.__version__

    def test_meta_git_commit_is_string_or_none(self) -> None:
        env = Envelope.success(data={}, command="suews validate")
        commit = env.meta["git_commit"]
        assert commit is None or isinstance(commit, str)

    def test_meta_started_at_is_iso_8601(self) -> None:
        env = Envelope.success(data={}, command="suews validate")
        # ISO 8601 with trailing Z (UTC)
        assert env.meta["started_at"].endswith("Z")
        assert "T" in env.meta["started_at"]

    def test_meta_ended_at_at_least_started_at(self) -> None:
        env = Envelope.success(data={}, command="suews validate")
        assert env.meta["ended_at"] >= env.meta["started_at"]


class TestEnvelopeSerialisation:
    def test_to_dict_has_five_top_level_keys(self) -> None:
        env = Envelope.success(data={"x": 1}, command="suews validate")
        d = env.to_dict()
        assert set(d.keys()) == {"status", "data", "errors", "warnings", "meta"}

    def test_to_json_round_trip(self) -> None:
        env = Envelope.success(
            data={"x": 1, "y": [1, 2, 3]},
            command="suews validate",
            warnings=["watch out"],
        )
        text = env.to_json()
        parsed = json.loads(text)
        assert parsed["status"] == "warning"
        assert parsed["data"] == {"x": 1, "y": [1, 2, 3]}
        assert parsed["warnings"] == ["watch out"]
        assert parsed["meta"]["command"] == "suews validate"

    def test_to_json_uses_utf8_safely(self) -> None:
        env = Envelope.success(data={"note": "résumé"}, command="suews inspect")
        text = env.to_json()
        # ensure_ascii must be False so non-ASCII passes through unescaped.
        assert "résumé" in text


class TestEnvelopeEmit:
    def test_emit_writes_json_to_stream(self) -> None:
        import io

        buf = io.StringIO()
        env = Envelope.success(data={"x": 1}, command="suews validate")
        env.emit(stream=buf)
        parsed = json.loads(buf.getvalue())
        assert parsed["status"] == "success"
        assert parsed["data"] == {"x": 1}


class TestErrorCoercion:
    def test_string_errors_coerced_to_dict_with_message(self) -> None:
        env = Envelope.error(
            errors=["schema mismatch", "missing field"],
            command="suews validate",
        )
        assert env.errors == [
            {"message": "schema mismatch"},
            {"message": "missing field"},
        ]

    def test_dict_errors_passed_through(self) -> None:
        err = {"code": "E001", "message": "bad", "field": "albedo"}
        env = Envelope.error(errors=[err], command="suews validate")
        assert env.errors == [err]

    def test_string_warnings_coerced_similarly(self) -> None:
        env = Envelope.success(
            data={},
            command="suews validate",
            warnings=["heads up"],
        )
        assert env.warnings == ["heads up"]


class TestExitCodes:
    def test_exit_code_constants_defined(self) -> None:
        # Exit codes per the plan: 0 ok, 1 user error, 2 schema/migration, 3 run, 4 env
        assert EXIT_OK == 0
        assert EXIT_USER_ERROR == 1
