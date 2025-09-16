"""Performance tests for SUEWS resource usage monitoring."""

import pytest
import asyncio
import time
import psutil
import threading
import pandas as pd
import numpy as np
import yaml
import os
import gc
from pathlib import Path
from unittest.mock import Mock, AsyncMock, patch
from typing import Dict, List, Any, Optional
from collections import deque
import sys

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from suews_mcp.server import SUEWSMCPServer
from suews_mcp.handlers import SUEWSMCPHandlers
from suews_mcp.config import MCPServerConfig


class ResourceMonitor:
    """Real-time resource monitoring for performance tests."""

    def __init__(self, interval: float = 0.1):
        self.interval = interval
        self.process = psutil.Process()
        self.monitoring = False
        self.data: deque = deque(maxlen=10000)  # Keep last 10k measurements
        self._monitor_thread: Optional[threading.Thread] = None

    def start_monitoring(self):
        """Start resource monitoring in background thread."""
        if self.monitoring:
            return

        self.monitoring = True
        self.data.clear()
        self._monitor_thread = threading.Thread(target=self._monitor_loop, daemon=True)
        self._monitor_thread.start()

    def stop_monitoring(self):
        """Stop resource monitoring."""
        self.monitoring = False
        if self._monitor_thread:
            self._monitor_thread.join(timeout=1.0)

    def _monitor_loop(self):
        """Main monitoring loop running in background thread."""
        while self.monitoring:
            try:
                timestamp = time.time()

                # CPU and memory info
                cpu_percent = self.process.cpu_percent()
                memory_info = self.process.memory_info()
                memory_mb = memory_info.rss / 1024 / 1024
                memory_peak_mb = (
                    memory_info.peak_wss / 1024 / 1024
                    if hasattr(memory_info, "peak_wss")
                    else memory_mb
                )

                # System-wide resources
                system_cpu = psutil.cpu_percent()
                system_memory = psutil.virtual_memory()

                # File handles and threads
                try:
                    num_fds = (
                        self.process.num_fds()
                        if hasattr(self.process, "num_fds")
                        else 0
                    )
                except (psutil.AccessDenied, AttributeError):
                    num_fds = 0

                try:
                    num_threads = self.process.num_threads()
                except (psutil.AccessDenied, AttributeError):
                    num_threads = 0

                self.data.append(
                    {
                        "timestamp": timestamp,
                        "cpu_percent": cpu_percent,
                        "memory_mb": memory_mb,
                        "memory_peak_mb": memory_peak_mb,
                        "system_cpu": system_cpu,
                        "system_memory_percent": system_memory.percent,
                        "system_memory_available_mb": system_memory.available
                        / 1024
                        / 1024,
                        "num_fds": num_fds,
                        "num_threads": num_threads,
                    }
                )

            except Exception as e:
                # Don't let monitoring errors crash the test
                pass

            time.sleep(self.interval)

    def get_stats(self, metric: str) -> Dict[str, float]:
        """Get statistics for a specific metric."""
        if not self.data:
            return {}

        values = [d[metric] for d in self.data if metric in d]
        if not values:
            return {}

        return {
            "min": min(values),
            "max": max(values),
            "mean": sum(values) / len(values),
            "count": len(values),
        }

    def get_peak_usage(self) -> Dict[str, float]:
        """Get peak resource usage during monitoring period."""
        if not self.data:
            return {}

        return {
            "peak_cpu": max(d.get("cpu_percent", 0) for d in self.data),
            "peak_memory_mb": max(d.get("memory_mb", 0) for d in self.data),
            "peak_threads": max(d.get("num_threads", 0) for d in self.data),
            "peak_fds": max(d.get("num_fds", 0) for d in self.data),
        }

    def get_monitoring_duration(self) -> float:
        """Get total monitoring duration."""
        if len(self.data) < 2:
            return 0.0
        return self.data[-1]["timestamp"] - self.data[0]["timestamp"]


class TestResourceUsage:
    """Test resource usage patterns and limits."""

    @pytest.fixture
    def resource_config(self):
        """Configuration for resource usage testing."""
        return MCPServerConfig(
            server_name="resource-test-server",
            server_version="0.1.0-resource",
            log_level="WARNING",  # Reduce logging overhead
            enable_debug=False,
            suews_timeout=1800,
            max_concurrent_simulations=8,
            enable_simulation_tool=True,
            enable_validation_tool=True,
            enable_analysis_tool=True,
            max_memory_mb=1024,  # Set memory limit for testing
            max_simulation_time_hours=48.0,
        )

    @pytest.fixture
    def resource_monitor(self):
        """Resource monitoring fixture."""
        monitor = ResourceMonitor()
        yield monitor
        monitor.stop_monitoring()

    @pytest.fixture
    def large_dataset_generator(self):
        """Generate large datasets for resource testing."""

        def generate_large_forcing(n_hours: int, n_variables: int = 20) -> pd.DataFrame:
            """Generate large forcing dataset."""
            np.random.seed(42)  # Reproducible data

            base_data = {
                "iy": [2024] * n_hours,
                "id": [(i // 24) + 1 for i in range(n_hours)],
                "it": [i % 24 for i in range(n_hours)],
                "imin": [0] * n_hours,
            }

            # Add many meteorological variables
            variable_names = [
                "qn",
                "qh",
                "qe",
                "qs",
                "qf",
                "U",
                "RH",
                "Tair",
                "pres",
                "rain",
                "kdown",
                "ldown",
                "fcld",
                "wuh",
                "xsmd",
                "lai_hp",
                "lai_dp",
                "albedo_hp",
                "albedo_dp",
                "z0m",
                "z0v",
                "porosity",
            ]

            for i, var in enumerate(variable_names[:n_variables]):
                if var in ["qn", "qh", "qe", "qs"]:
                    # Energy fluxes
                    base_val = 100 - i * 10
                    values = np.maximum(0, base_val + np.random.normal(0, 20, n_hours))
                elif var in ["Tair"]:
                    # Temperature
                    values = (
                        15
                        + 10 * np.sin(2 * np.pi * np.arange(n_hours) / 24)
                        + np.random.normal(0, 2, n_hours)
                    )
                elif var in ["U"]:
                    # Wind speed
                    values = np.maximum(0.5, 5.0 + np.random.normal(0, 2, n_hours))
                elif var in ["RH"]:
                    # Humidity
                    values = np.clip(70 + np.random.normal(0, 15, n_hours), 20, 100)
                else:
                    # Generic variables
                    values = np.random.normal(50, 15, n_hours)

                base_data[var] = values

            return pd.DataFrame(base_data)

        return generate_large_forcing

    @pytest.mark.performance
    @pytest.mark.resource
    @pytest.mark.asyncio
    async def test_memory_usage_baseline(self, resource_config, resource_monitor):
        """Test baseline memory usage of server components."""

        # Initial memory measurement
        initial_memory = psutil.Process().memory_info().rss / 1024 / 1024

        resource_monitor.start_monitoring()

        # Create server and handlers
        server = SUEWSMCPServer(resource_config)
        handlers = server.handlers

        # Let monitoring run for a few seconds
        await asyncio.sleep(2.0)

        # Perform basic operations
        init_result = await handlers.handle_initialize(
            {
                "protocol_version": "2024-11-05",
                "client_info": {"name": "resource-test", "version": "1.0.0"},
            }
        )

        tools_result = await handlers.handle_list_tools()
        health_result = await handlers.handle_call_tool("health_check", {})

        await asyncio.sleep(1.0)

        resource_monitor.stop_monitoring()

        # Analyze resource usage
        memory_stats = resource_monitor.get_stats("memory_mb")
        peak_usage = resource_monitor.get_peak_usage()

        final_memory = psutil.Process().memory_info().rss / 1024 / 1024
        memory_increase = final_memory - initial_memory

        print(f"Memory usage analysis:")
        print(f"  Initial: {initial_memory:.1f} MB")
        print(f"  Final: {final_memory:.1f} MB")
        print(f"  Increase: {memory_increase:.1f} MB")
        print(f"  Peak during monitoring: {peak_usage['peak_memory_mb']:.1f} MB")

        # Memory usage should be reasonable
        assert (
            memory_increase < 50
        ), f"Server initialization uses too much memory: {memory_increase:.1f} MB"
        assert (
            peak_usage["peak_memory_mb"] < 200
        ), f"Peak memory too high: {peak_usage['peak_memory_mb']:.1f} MB"

        # Cleanup
        await server.cleanup()

    @pytest.mark.performance
    @pytest.mark.resource
    @pytest.mark.asyncio
    async def test_memory_scaling_with_simulations(
        self, resource_config, resource_monitor, large_dataset_generator, tmp_path
    ):
        """Test how memory usage scales with number of active simulations."""

        handlers = SUEWSMCPHandlers(resource_config)

        memory_measurements = []

        # Test with increasing numbers of active simulations
        simulation_counts = [0, 1, 5, 10, 20, 50]

        for n_sims in simulation_counts:
            # Clear existing simulations
            handlers._active_simulations.clear()

            # Force garbage collection
            gc.collect()

            # Baseline measurement
            baseline_memory = psutil.Process().memory_info().rss / 1024 / 1024

            resource_monitor.start_monitoring()

            # Add simulations with realistic data structures
            for i in range(n_sims):
                simulation_data = {
                    "status": "running",
                    "config_file": f"/test/config_{i}.yml",
                    "start_time": time.time() - 3600,
                    "simulation_id": f"memory_scale_test_{i}",
                    "output_data": large_dataset_generator(
                        168, 15
                    ),  # 1 week, 15 variables
                    "metadata": {
                        "timesteps": 168,
                        "sites": 3,
                        "variables": ["QN", "QH", "QE", "QS", "QF", "T2", "RH2", "U10"],
                        "forcing_data_size": 168 * 15 * 8,  # Approx bytes
                        "config_complexity": "medium",
                    },
                    "progress": {
                        "completed_timesteps": i * 10,
                        "total_timesteps": 168,
                        "current_phase": "main_calculation",
                    },
                }
                handlers._active_simulations[f"sim_{i}"] = simulation_data

            # Let system stabilize
            await asyncio.sleep(0.5)

            # Run health check to trigger data access
            health_result = await handlers.handle_call_tool("health_check", {})

            await asyncio.sleep(1.0)

            resource_monitor.stop_monitoring()

            current_memory = psutil.Process().memory_info().rss / 1024 / 1024
            memory_increase = current_memory - baseline_memory
            peak_usage = resource_monitor.get_peak_usage()

            measurement = {
                "n_simulations": n_sims,
                "baseline_memory": baseline_memory,
                "current_memory": current_memory,
                "memory_increase": memory_increase,
                "peak_memory": peak_usage["peak_memory_mb"],
                "memory_per_sim": memory_increase / max(1, n_sims),
            }

            memory_measurements.append(measurement)

            print(
                f"{n_sims} simulations: {memory_increase:.1f} MB increase "
                f"({measurement['memory_per_sim']:.2f} MB per sim)"
            )

        # Analysis
        print("\nMemory scaling analysis:")
        for measurement in memory_measurements:
            n_sims = measurement["n_simulations"]
            increase = measurement["memory_increase"]
            per_sim = measurement["memory_per_sim"]

            print(f"  {n_sims:2d} sims: +{increase:5.1f} MB ({per_sim:4.2f} MB/sim)")

        # Performance assertions
        max_per_sim = max(
            m["memory_per_sim"] for m in memory_measurements if m["n_simulations"] > 0
        )
        assert (
            max_per_sim < 10.0
        ), f"Memory per simulation too high: {max_per_sim:.2f} MB/sim"

        # Memory should scale reasonably linearly
        if len(memory_measurements) >= 3:
            large_sim_mem = next(
                m for m in memory_measurements if m["n_simulations"] == 50
            )
            medium_sim_mem = next(
                m for m in memory_measurements if m["n_simulations"] == 10
            )

            scaling_ratio = (
                large_sim_mem["memory_increase"] / medium_sim_mem["memory_increase"]
            )
            expected_ratio = 50 / 10  # Should be roughly 5x

            assert (
                scaling_ratio < expected_ratio * 1.5
            ), f"Memory scaling worse than linear: {scaling_ratio:.1f}x vs {expected_ratio}x expected"

    @pytest.mark.performance
    @pytest.mark.resource
    @pytest.mark.asyncio
    async def test_cpu_usage_patterns(self, resource_config, resource_monitor):
        """Test CPU usage patterns during various operations."""

        handlers = SUEWSMCPHandlers(resource_config)

        # Test different operation types
        operations = [
            (
                "initialization",
                lambda: handlers.handle_initialize(
                    {
                        "protocol_version": "2024-11-05",
                        "client_info": {"name": "cpu-test", "version": "1.0.0"},
                    }
                ),
            ),
            ("list_tools", lambda: handlers.handle_list_tools()),
            ("health_check", lambda: handlers.handle_call_tool("health_check", {})),
            (
                "concurrent_health_checks",
                lambda: asyncio.gather(
                    *[handlers.handle_call_tool("health_check", {}) for _ in range(10)]
                ),
            ),
        ]

        cpu_results = {}

        for operation_name, operation_func in operations:
            print(f"Testing CPU usage for: {operation_name}")

            # Pre-operation baseline
            baseline_cpu = psutil.Process().cpu_percent()
            await asyncio.sleep(0.1)  # Let CPU measurement stabilize

            resource_monitor.start_monitoring()

            # Execute operation multiple times to get measurable CPU usage
            for _ in range(5 if "concurrent" in operation_name else 20):
                result = await operation_func()
                await asyncio.sleep(0.01)  # Small delay between operations

            await asyncio.sleep(0.5)  # Let monitoring capture peak usage

            resource_monitor.stop_monitoring()

            # Analyze CPU usage
            cpu_stats = resource_monitor.get_stats("cpu_percent")
            peak_usage = resource_monitor.get_peak_usage()

            cpu_results[operation_name] = {
                "baseline_cpu": baseline_cpu,
                "peak_cpu": peak_usage["peak_cpu"],
                "avg_cpu": cpu_stats.get("mean", 0),
                "max_cpu": cpu_stats.get("max", 0),
            }

            print(f"  Peak CPU: {peak_usage['peak_cpu']:.1f}%")
            print(f"  Average CPU: {cpu_stats.get('mean', 0):.1f}%")

        # Performance assertions
        # Most operations should have low CPU usage
        for op_name, stats in cpu_results.items():
            if "concurrent" not in op_name:
                assert (
                    stats["peak_cpu"] < 20.0
                ), f"{op_name} CPU usage too high: {stats['peak_cpu']:.1f}%"
            else:
                # Concurrent operations can use more CPU
                assert (
                    stats["peak_cpu"] < 50.0
                ), f"{op_name} CPU usage too high: {stats['peak_cpu']:.1f}%"

    @pytest.mark.performance
    @pytest.mark.resource
    @pytest.mark.asyncio
    async def test_file_handle_usage(self, resource_config, resource_monitor, tmp_path):
        """Test file handle usage patterns."""

        handlers = SUEWSMCPHandlers(resource_config)

        # Get initial file handle count
        try:
            initial_fds = (
                psutil.Process().num_fds()
                if hasattr(psutil.Process(), "num_fds")
                else 0
            )
        except (psutil.AccessDenied, AttributeError):
            initial_fds = 0

        resource_monitor.start_monitoring()

        # Create many temporary files (simulating large datasets)
        temp_files = []
        for i in range(50):
            temp_file = tmp_path / f"test_file_{i}.txt"
            with open(temp_file, "w") as f:
                f.write("test data" * 1000)  # Write some data
            temp_files.append(temp_file)

        # Simulate configuration validation operations that might open files
        for i in range(10):
            config_args = {
                "config_file": str(temp_files[i % len(temp_files)]),
                "strict": False,
            }

            with patch.object(handlers, "_call_validation_tool") as mock_validate:
                mock_validate.return_value = {
                    "content": [{"type": "text", "text": f"File {i} validated"}],
                    "is_error": False,
                }

                result = await handlers.handle_call_tool(
                    "validate_suews_config", config_args
                )

            # Small delay to let file operations complete
            await asyncio.sleep(0.01)

        await asyncio.sleep(1.0)

        resource_monitor.stop_monitoring()

        # Final file handle count
        try:
            final_fds = (
                psutil.Process().num_fds()
                if hasattr(psutil.Process(), "num_fds")
                else 0
            )
        except (psutil.AccessDenied, AttributeError):
            final_fds = 0

        fd_stats = resource_monitor.get_stats("num_fds")
        peak_usage = resource_monitor.get_peak_usage()

        fd_increase = final_fds - initial_fds

        print(f"File handle usage:")
        print(f"  Initial: {initial_fds}")
        print(f"  Final: {final_fds}")
        print(f"  Increase: {fd_increase}")
        print(f"  Peak: {peak_usage.get('peak_fds', 0)}")

        # File handles should not leak significantly
        assert fd_increase < 20, f"Too many file handles opened: {fd_increase}"

        # Peak usage should be reasonable
        if peak_usage.get("peak_fds", 0) > 0:
            assert (
                peak_usage["peak_fds"] < initial_fds + 50
            ), f"Peak file handle usage too high: {peak_usage['peak_fds']}"

        # Cleanup
        for temp_file in temp_files:
            temp_file.unlink(missing_ok=True)

    @pytest.mark.performance
    @pytest.mark.resource
    @pytest.mark.asyncio
    async def test_thread_usage_patterns(self, resource_config, resource_monitor):
        """Test thread creation and management patterns."""

        # Get initial thread count
        try:
            initial_threads = psutil.Process().num_threads()
        except (psutil.AccessDenied, AttributeError):
            initial_threads = 1

        resource_monitor.start_monitoring()

        # Create multiple server instances (simulating scaling)
        servers = []
        for i in range(5):
            server = SUEWSMCPServer(resource_config)
            servers.append(server)
            await asyncio.sleep(0.1)  # Small delay between creations

        # Run concurrent operations across servers
        tasks = []
        for server in servers:
            tasks.append(server.handlers.handle_call_tool("health_check", {}))
            tasks.append(server.handlers.handle_list_tools())

        results = await asyncio.gather(*tasks)

        await asyncio.sleep(1.0)

        # Cleanup servers
        for server in servers:
            await server.cleanup()

        await asyncio.sleep(0.5)

        resource_monitor.stop_monitoring()

        # Final thread count
        try:
            final_threads = psutil.Process().num_threads()
        except (psutil.AccessDenied, AttributeError):
            final_threads = 1

        thread_stats = resource_monitor.get_stats("num_threads")
        peak_usage = resource_monitor.get_peak_usage()

        thread_increase = final_threads - initial_threads

        print(f"Thread usage:")
        print(f"  Initial: {initial_threads}")
        print(f"  Final: {final_threads}")
        print(f"  Increase: {thread_increase}")
        print(f"  Peak: {peak_usage.get('peak_threads', 0)}")

        # Thread count should not grow excessively
        assert thread_increase < 10, f"Too many threads created: {thread_increase}"

        # Peak thread usage should be reasonable
        if peak_usage.get("peak_threads", 0) > 0:
            expected_max_threads = initial_threads + 20  # Conservative limit
            assert (
                peak_usage["peak_threads"] < expected_max_threads
            ), f"Peak thread usage too high: {peak_usage['peak_threads']} > {expected_max_threads}"

    @pytest.mark.performance
    @pytest.mark.resource
    @pytest.mark.slow
    @pytest.mark.asyncio
    async def test_long_running_resource_stability(
        self, resource_config, resource_monitor, large_dataset_generator, tmp_path
    ):
        """Test resource stability during long-running operations."""

        # Reduce concurrency for stability test
        resource_config.max_concurrent_simulations = 2
        handlers = SUEWSMCPHandlers(resource_config)

        resource_monitor.start_monitoring()

        # Simulate long-running server operation
        start_time = time.time()
        operation_duration = 30.0  # 30 seconds

        # Create some background "simulations"
        for i in range(5):
            handlers._active_simulations[f"long_running_sim_{i}"] = {
                "status": "running",
                "config_file": f"/test/long_config_{i}.yml",
                "start_time": start_time,
                "simulation_id": f"stability_test_{i}",
                "output_data": large_dataset_generator(
                    8760, 10
                ),  # 1 year, 10 variables
                "metadata": {
                    "timesteps": 8760,
                    "estimated_completion": start_time + 3600,  # 1 hour estimate
                },
            }

        # Run periodic operations while monitoring resources
        operation_count = 0
        while time.time() - start_time < operation_duration:
            # Mix of different operations
            operations = [
                handlers.handle_call_tool("health_check", {}),
                handlers.handle_list_tools(),
            ]

            # Occasionally run validation
            if operation_count % 10 == 0:
                config_file = tmp_path / f"stability_config_{operation_count}.yml"
                with open(config_file, "w") as f:
                    yaml.dump({"test": "config", "id": operation_count}, f)

                with patch.object(handlers, "_call_validation_tool") as mock_validate:
                    mock_validate.return_value = {
                        "content": [
                            {"type": "text", "text": "Stability test validation"}
                        ],
                        "is_error": False,
                    }
                    operations.append(
                        handlers.handle_call_tool(
                            "validate_suews_config", {"config_file": str(config_file)}
                        )
                    )

            # Execute operations
            results = await asyncio.gather(*operations)

            # Verify all operations succeeded
            for result in results:
                if isinstance(result, dict):
                    assert not result.get("is_error", False)

            operation_count += 1

            # Small delay between operation cycles
            await asyncio.sleep(0.5)

        resource_monitor.stop_monitoring()

        # Analyze resource stability
        memory_stats = resource_monitor.get_stats("memory_mb")
        cpu_stats = resource_monitor.get_stats("cpu_percent")

        monitoring_duration = resource_monitor.get_monitoring_duration()

        print(f"Long-running stability test ({monitoring_duration:.1f}s):")
        print(f"  Operations executed: {operation_count}")
        print(
            f"  Memory - Min: {memory_stats.get('min', 0):.1f} MB, "
            f"Max: {memory_stats.get('max', 0):.1f} MB, "
            f"Avg: {memory_stats.get('mean', 0):.1f} MB"
        )
        print(
            f"  CPU - Min: {cpu_stats.get('min', 0):.1f}%, "
            f"Max: {cpu_stats.get('max', 0):.1f}%, "
            f"Avg: {cpu_stats.get('mean', 0):.1f}%"
        )

        # Stability assertions
        memory_range = memory_stats.get("max", 0) - memory_stats.get("min", 0)
        assert (
            memory_range < 100
        ), f"Memory usage too variable: {memory_range:.1f} MB range"

        # CPU should not be consistently high
        assert (
            cpu_stats.get("mean", 0) < 15.0
        ), f"Average CPU too high: {cpu_stats.get('mean', 0):.1f}%"

        # Should have completed many operations
        operations_per_second = operation_count / monitoring_duration
        assert (
            operations_per_second > 1.0
        ), f"Operation rate too low: {operations_per_second:.2f} ops/sec"

    @pytest.mark.performance
    @pytest.mark.resource
    def test_resource_limits_enforcement(self, resource_config):
        """Test that resource limits are properly enforced."""

        # Test memory limit enforcement
        original_limit = resource_config.max_memory_mb
        resource_config.max_memory_mb = 100  # Very low limit

        handlers = SUEWSMCPHandlers(resource_config)

        # Verify configuration reflects the limit
        assert handlers.config.max_memory_mb == 100

        # Reset for other tests
        resource_config.max_memory_mb = original_limit

        # Test concurrent simulation limit
        original_concurrent = resource_config.max_concurrent_simulations
        resource_config.max_concurrent_simulations = 3

        handlers = SUEWSMCPHandlers(resource_config)

        # Verify semaphore is properly configured
        assert handlers._simulation_semaphore._value == 3

        # Test that we can't exceed the limit
        permits_acquired = 0
        for i in range(5):  # Try to acquire more than limit
            if handlers._simulation_semaphore.acquire_nowait():
                permits_acquired += 1
            else:
                break

        assert permits_acquired == 3, f"Acquired {permits_acquired} permits, expected 3"

        # Release permits
        for _ in range(permits_acquired):
            handlers._simulation_semaphore.release()

        # Reset for other tests
        resource_config.max_concurrent_simulations = original_concurrent

    @pytest.mark.performance
    @pytest.mark.resource
    def test_resource_monitoring_overhead(self, resource_config):
        """Test that resource monitoring itself has minimal overhead."""

        # Test without monitoring
        start_time = time.perf_counter()

        handlers = SUEWSMCPHandlers(resource_config)

        # Run operations without monitoring
        for _ in range(100):
            # Simulate light operations
            len(handlers._active_simulations)
            handlers.config.server_name

        no_monitor_time = time.perf_counter() - start_time

        # Test with monitoring
        monitor = ResourceMonitor(interval=0.01)  # High frequency monitoring
        monitor.start_monitoring()

        start_time = time.perf_counter()

        # Same operations with monitoring
        for _ in range(100):
            len(handlers._active_simulations)
            handlers.config.server_name

        with_monitor_time = time.perf_counter() - start_time

        monitor.stop_monitoring()

        # Monitoring overhead should be minimal
        overhead_ratio = (
            with_monitor_time / no_monitor_time if no_monitor_time > 0 else 1
        )

        print(f"Resource monitoring overhead:")
        print(f"  Without monitoring: {no_monitor_time:.4f}s")
        print(f"  With monitoring: {with_monitor_time:.4f}s")
        print(f"  Overhead ratio: {overhead_ratio:.2f}x")

        assert (
            overhead_ratio < 1.1
        ), f"Resource monitoring overhead too high: {overhead_ratio:.2f}x"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
