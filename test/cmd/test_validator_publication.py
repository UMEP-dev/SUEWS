"""Force overlapping publication and rollback without scheduler sleeps."""

import errno
import multiprocessing
import os
from pathlib import Path
from unittest.mock import patch

import pytest

pytestmark = [pytest.mark.api, pytest.mark.cfg]


def _publisher(outputs, workspace, report_path, connection, fail=False, observe=False):
    """Run a real publisher, with pipe barriers around the injected fault."""
    from supy.data_model.validation.pipeline import orchestrator

    original_replace = os.replace

    def controlled_replace(source, destination):
        if fail and Path(source).name == "publish_2":
            raise OSError("injected publication failure")
        original_replace(source, destination)
        if fail and Path(source).name == "publish_1":
            connection.send("partial")
            if not connection.poll(60) or connection.recv() != "continue":
                raise TimeoutError("publication control was not released")

    # The contender first probes the *real* native lock non-blockingly. Its
    # contention message proves it reached a held lock; elapsed time is not
    # used as evidence that it was blocked. It then waits normally.
    if os.name == "nt":
        import msvcrt

        native_lock = msvcrt.locking

        def observed_lock(fd, operation, length):
            if operation != msvcrt.LK_LOCK:
                return native_lock(fd, operation, length)
            try:
                native_lock(fd, msvcrt.LK_NBLCK, length)
            except OSError as exc:
                if exc.errno not in (errno.EACCES, errno.EAGAIN, errno.EDEADLK):
                    raise
                connection.send("waiting")
                while True:
                    try:
                        native_lock(fd, operation, length)
                        break
                    except OSError as retry:
                        if retry.errno != errno.EDEADLK:
                            raise
            connection.send("locked")

        lock_patch = patch.object(msvcrt, "locking", observed_lock if observe else native_lock)
    else:
        import fcntl

        native_lock = fcntl.flock

        def observed_lock(fd, operation):
            if operation != fcntl.LOCK_EX:
                return native_lock(fd, operation)
            try:
                native_lock(fd, operation | fcntl.LOCK_NB)
            except BlockingIOError:
                connection.send("waiting")
                native_lock(fd, operation)
            connection.send("locked")

        lock_patch = patch.object(fcntl, "flock", observed_lock if observe else native_lock)

    try:
        with patch.object(os, "replace", controlled_replace), lock_patch:
            orchestrator._publish_abc_outputs(outputs, workspace, report_path)
        connection.send("published")
    except Exception as exc:
        connection.send((type(exc).__name__, str(exc)))
    finally:
        connection.close()


def _receive(connection):
    assert connection.poll(60), "publisher did not reach its next control point"
    return connection.recv()


def _staged_outputs(directory, workspace, stem, generation, suffix=".yml"):
    workspace.mkdir()
    outputs = {}
    for name in (f"updated_{stem}{suffix}", f"report_{stem}.txt", f"report_{stem}.json"):
        source = workspace / name
        source.write_bytes(f"{generation}: {name}\n".encode())
        outputs[directory / name] = source
    return outputs


@pytest.mark.parametrize("suffix", [".yml", ".yaml"])
def test_publication_serialises_rollback_but_other_inputs_progress(tmp_path, suffix):
    """The failed publisher rolls back before its waiting successor can publish.

    Also cover different input extensions sharing canonical report names.
    A third, independent report publishes while the first still holds its lock.
    """
    context = multiprocessing.get_context("spawn")
    report_path = tmp_path / "report_sample.txt"
    older_workspace = tmp_path / "older"
    newer_workspace = tmp_path / "newer"
    independent_workspace = tmp_path / "independent"
    older = _staged_outputs(tmp_path, older_workspace, "sample", "older")
    newer = _staged_outputs(tmp_path, newer_workspace, "sample", "newer", suffix)
    independent = _staged_outputs(tmp_path, independent_workspace, "other", "other")
    initial = {}
    for destination in dict.fromkeys([*older, *newer]):
        initial[destination] = f"initial: {destination.name}\n".encode()
        destination.write_bytes(initial[destination])

    processes = []
    connections = []

    def start(outputs, workspace, report, **kwargs):
        parent, child = context.Pipe()
        process = context.Process(
            target=_publisher,
            args=(outputs, workspace, report, child),
            kwargs=kwargs,
        )
        process.start()
        child.close()
        processes.append(process)
        connections.append(parent)
        return parent

    try:
        old_connection = start(older, older_workspace, report_path, fail=True)
        assert _receive(old_connection) == "partial"
        assert report_path.read_bytes() == older[report_path].read_bytes()

        new_connection = start(newer, newer_workspace, report_path, observe=True)
        assert _receive(new_connection) == "waiting"
        # No backup may be taken from the older invocation's partial output.
        assert not list(newer_workspace.glob("previous_*"))

        other_connection = start(
            independent, independent_workspace, tmp_path / "report_other.txt"
        )
        assert _receive(other_connection) == "published"
        assert all(path.read_bytes() == source.read_bytes() for path, source in independent.items())

        old_connection.send("continue")
        assert _receive(old_connection) == ("OSError", "injected publication failure")
        assert _receive(new_connection) == "locked"
        assert _receive(new_connection) == "published"
        assert all(path.read_bytes() == source.read_bytes() for path, source in newer.items())
        assert (newer_workspace / "previous_1").read_bytes() == initial[report_path]
        assert all(source.is_file() for source in older.values())
        if suffix == ".yaml":
            old_yaml = tmp_path / "updated_sample.yml"
            assert old_yaml.read_bytes() == initial[old_yaml]
        for process in processes:
            process.join(60)
            assert process.exitcode == 0
    finally:
        for process in processes:
            if process.is_alive():
                process.terminate()
            process.join(10)
        for connection in connections:
            connection.close()
