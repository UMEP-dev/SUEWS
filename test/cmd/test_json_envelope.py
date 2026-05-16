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

    def test_meta_git_commit_falls_back_to_baked_hash(
        self, monkeypatch: pytest.MonkeyPatch, tmp_path
    ) -> None:
        """Wheel installs lack a ``.git`` directory; the envelope must
        still carry a non-null ``git_commit`` by reading
        ``supy._version_scm.__commit_hash__`` (gh#1401).
        """
        import supy
        from supy.cmd import json_envelope as je

        # Pretend supy is installed in tmp_path so the .git walk fails.
        fake_pkg_init = tmp_path / "supy" / "__init__.py"
        fake_pkg_init.parent.mkdir()
        fake_pkg_init.write_text("# fake supy", encoding="utf-8")
        monkeypatch.setattr(supy, "__file__", str(fake_pkg_init))

        # Inject a known build-time commit hash into _version_scm.
        from supy import _version_scm

        monkeypatch.setattr(_version_scm, "__commit_hash__", "abc1234", raising=False)

        commit = je._git_commit()
        assert commit == "abc1234"

    def test_meta_git_commit_returns_none_when_baked_unknown(
        self, monkeypatch: pytest.MonkeyPatch, tmp_path
    ) -> None:
        """``"unknown"`` (the build-time sentinel for "git was not
        available during the build") must surface as ``None`` rather
        than the literal string (gh#1401).
        """
        import supy
        from supy.cmd import json_envelope as je

        fake_pkg_init = tmp_path / "supy" / "__init__.py"
        fake_pkg_init.parent.mkdir()
        fake_pkg_init.write_text("# fake supy", encoding="utf-8")
        monkeypatch.setattr(supy, "__file__", str(fake_pkg_init))

        from supy import _version_scm

        monkeypatch.setattr(_version_scm, "__commit_hash__", "unknown", raising=False)

        assert je._git_commit() is None

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

    def test_emit_falls_back_to_ascii_when_stream_cannot_encode_unicode(self) -> None:
        import io

        class Cp1252Stream:
            def __init__(self) -> None:
                self.buf = io.StringIO()

            def write(self, text: str) -> int:
                text.encode("cp1252")
                return self.buf.write(text)

            def flush(self) -> None:
                pass

        stream = Cp1252Stream()
        env = Envelope.success(data={"symbol": "lambda λ"}, command="suews inspect")
        env.emit(stream=stream)

        text = stream.buf.getvalue()
        assert "\\u03bb" in text
        assert json.loads(text)["data"]["symbol"] == "lambda λ"


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
