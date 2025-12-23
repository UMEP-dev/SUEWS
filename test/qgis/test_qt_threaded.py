#!/usr/bin/env python3
"""
Threaded Qt test for SUEWS ERROR handling.

Tests if error handling works when SUEWS runs in a worker thread
(like QGIS does for long-running tasks).

This is the closest simulation to how QGIS actually runs SUEWS.
"""
import sys
import os

# Must set display for headless GUI
os.environ.setdefault('QT_QPA_PLATFORM', 'offscreen')

from PyQt5.QtCore import QThread, pyqtSignal, QObject
from PyQt5.QtWidgets import QApplication

class SUEWSWorker(QObject):
    """Worker that runs SUEWS simulation in a separate thread."""
    finished = pyqtSignal()
    error = pyqtSignal(str)
    result = pyqtSignal(str)

    def __init__(self):
        super().__init__()

    def run(self):
        """Run the SUEWS simulation that triggers an error."""
        try:
            import supy as sp
            from supy.data_model import init_config_from_yaml
            import warnings
            warnings.filterwarnings('ignore', category=DeprecationWarning)

            print(f"[Worker thread] supy version: {sp.__version__}")

            # Use benchmark config
            config_path = os.path.join(
                os.path.dirname(__file__),
                '..', 'fixtures', 'benchmark1', 'benchmark1_short.yml'
            )

            config = init_config_from_yaml(config_path)
            df_state = config.to_df_state()
            grid = df_state.index[0]

            # Load forcing
            df_forcing = sp.load_forcing_grid(config_path, grid, df_state_init=df_state)
            df_forcing_short = df_forcing.head(288)

            # Modify to trigger error
            z_col = ('z', '0')
            zdm_col = ('zdm_in', '0')
            df_state.loc[grid, z_col] = 5.0
            df_state.loc[grid, zdm_col] = 15.0

            print("[Worker thread] Running simulation with z < zdm...")

            # This should trigger error
            df_output, df_state_final = sp.run_supy(df_forcing_short, df_state, chunk_day=1)

            self.result.emit(f"Output shape: {df_output.shape}")

        except RuntimeError as e:
            print(f"[Worker thread] RuntimeError caught: {str(e)[:100]}...")
            self.error.emit(f"RuntimeError: {str(e)[:100]}")

        except Exception as e:
            print(f"[Worker thread] Exception: {type(e).__name__}: {e}")
            self.error.emit(f"{type(e).__name__}: {e}")

        finally:
            self.finished.emit()


class TestRunner(QObject):
    """Main test controller."""

    def __init__(self, app):
        super().__init__()
        self.app = app
        self.error_caught = False

    def run_test(self):
        """Start the worker thread."""
        print("\n=== Test: SUEWS error in Qt worker thread ===")

        # Create thread and worker
        self.thread = QThread()
        self.worker = SUEWSWorker()
        self.worker.moveToThread(self.thread)

        # Connect signals
        self.thread.started.connect(self.worker.run)
        self.worker.finished.connect(self.thread.quit)
        self.worker.finished.connect(self.on_finished)
        self.worker.error.connect(self.on_error)
        self.worker.result.connect(self.on_result)

        # Start thread
        print("[Main thread] Starting worker thread...")
        self.thread.start()

    def on_error(self, msg):
        """Called when worker emits error signal."""
        print(f"[Main thread] Got error signal: {msg[:80]}...")
        self.error_caught = True

    def on_result(self, msg):
        """Called when worker emits result signal."""
        print(f"[Main thread] Got result: {msg}")

    def on_finished(self):
        """Called when worker finishes."""
        print("\n[Main thread] Worker finished")

        if self.error_caught:
            print("SUCCESS: Error was caught in worker thread!")
            print("Qt did NOT crash when error occurred in thread!")
        else:
            print("WARNING: No error was triggered in worker thread.")

        print("\n=== Test completed ===")
        print("If you see this message, Qt threading did NOT crash!")

        self.app.quit()


def main():
    app = QApplication(sys.argv)
    print("QApplication initialized (offscreen mode)")

    runner = TestRunner(app)

    # Use single-shot timer to start test after event loop begins
    from PyQt5.QtCore import QTimer
    QTimer.singleShot(0, runner.run_test)

    return app.exec_()


if __name__ == '__main__':
    sys.exit(main())
