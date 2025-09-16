"""Performance tests for SUEWS model operations timing."""

import pytest
import asyncio
import time
import statistics
import pandas as pd
import numpy as np
import yaml
from pathlib import Path
from unittest.mock import Mock, AsyncMock, patch
from typing import Dict, List, Any
import psutil
import sys

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from suews_mcp.server import SUEWSMCPServer
from suews_mcp.handlers import SUEWSMCPHandlers
from suews_mcp.config import MCPServerConfig


class TestModelTiming:
    """Performance tests for model operation timing."""

    @pytest.fixture
    def performance_config(self):
        """Configuration optimized for performance testing."""
        return MCPServerConfig(
            server_name="performance-test-server",
            server_version="0.1.0-perf",
            log_level="WARNING",  # Reduce logging overhead
            enable_debug=False,
            suews_timeout=1800,  # 30 minutes for large simulations
            max_concurrent_simulations=4,
            enable_simulation_tool=True,
            enable_validation_tool=True,
            enable_analysis_tool=True,
            max_memory_mb=4096,
            max_simulation_time_hours=72.0,
        )

    @pytest.fixture
    def performance_forcing_data(self):
        """Generate forcing data for performance testing."""

        def generate_forcing(n_hours: int) -> pd.DataFrame:
            """Generate realistic forcing data for specified number of hours."""
            base_date = pd.Timestamp("2024-01-01")
            hours = np.arange(n_hours)

            # Efficient vectorized generation
            solar_cycle = np.maximum(
                0,
                1000
                * np.sin(np.pi * (hours % 24) / 24)
                * np.maximum(0, np.cos(np.pi * ((hours % 24) - 12) / 12)),
            )

            temp_base = 15 + 10 * np.sin(2 * np.pi * (hours % 24) / 24 - np.pi / 2)
            temp_seasonal = 5 * np.sin(2 * np.pi * hours / (365 * 24))

            data = {
                "iy": [2024] * n_hours,
                "id": [((base_date + pd.Timedelta(hours=h)).dayofyear) for h in hours],
                "it": [h % 24 for h in hours],
                "imin": [0] * n_hours,
                "qn": solar_cycle * 0.6 + np.random.normal(0, 20, n_hours),
                "qh": solar_cycle * 0.3 + np.random.normal(0, 15, n_hours),
                "qe": solar_cycle * 0.25 + np.random.normal(0, 12, n_hours),
                "qs": solar_cycle * 0.15 + np.random.normal(0, 8, n_hours),
                "qf": np.full(n_hours, 25.0) + np.random.normal(0, 3, n_hours),
                "U": np.maximum(0.5, 4.0 + np.random.normal(0, 1.8, n_hours)),
                "RH": np.clip(70 + np.random.normal(0, 12, n_hours), 30, 95),
                "Tair": temp_base + temp_seasonal + np.random.normal(0, 1.2, n_hours),
                "pres": 101.325 + np.random.normal(0, 0.6, n_hours),
                "rain": np.random.exponential(0.08, n_hours),
                "kdown": solar_cycle + np.random.normal(0, 40, n_hours),
                "ldown": 320
                + 30 * np.sin(2 * np.pi * (hours % 24) / 24)
                + np.random.normal(0, 15, n_hours),
            }

            return pd.DataFrame(data)

        return generate_forcing

    @pytest.fixture
    def benchmark_config_template(self):
        """Template configuration for performance benchmarks."""
        return {
            "name": "performance_benchmark",
            "description": "Performance benchmark configuration",
            "model": {
                "control": {
                    "tstep": 3600,  # 1-hour timestep
                    "forcing_file": {"value": "forcing_data.txt"},
                    "start_doy": {"value": 1},
                    "end_doy": {"value": 365},  # Full year by default
                    "year": {"value": 2024},
                    "resolutionfilesout": {"value": 3600},
                },
                "physics": {
                    "netradiationmethod": {"value": 3},
                    "roughlenmommethod": {"value": 2},
                    "conductancemethod": {"value": 4},
                    "stabilitymethod": {"value": 3},
                    "storedrainprm": {"value": 2},
                    "anthropogenic": {"value": 1},
                    "snowuse": {"value": 1},
                },
                "output": {"writefiles": {"value": 1}, "files_text": {"value": 1}},
            },
            "sites": [
                {
                    "name": "benchmark_site",
                    "properties": {
                        "lat": {"value": 51.5074},
                        "lng": {"value": -0.1278},
                        "alt": {"value": 50.0},
                        "timezone": {"value": 0},
                    },
                    "land_cover": {
                        "paved": {
                            "sfr": {"value": 0.35},
                            "emis": {"value": 0.95},
                            "roughlenmmom": {"value": 0.1},
                            "storemin": {"value": 0.5},
                            "storemax": {"value": 2.0},
                        },
                        "bldgs": {
                            "sfr": {"value": 0.35},
                            "emis": {"value": 0.92},
                            "roughlenmmom": {"value": 1.0},
                            "storemin": {"value": 0.5},
                            "storemax": {"value": 2.5},
                        },
                        "grass": {
                            "sfr": {"value": 0.30},
                            "emis": {"value": 0.96},
                            "roughlenmmom": {"value": 0.05},
                            "storemin": {"value": 0.5},
                            "storemax": {"value": 3.0},
                        },
                    },
                    "initial_conditions": {
                        "soilstore_id": {"value": 150.0},
                        "soilstore_surf": {"value": 15.0},
                    },
                }
            ],
        }

    class PerformanceMetrics:
        """Helper class to collect and analyze performance metrics."""

        def __init__(self):
            self.timings: List[Dict[str, Any]] = []

        def record_timing(self, operation: str, duration: float, **metadata):
            """Record timing for an operation."""
            self.timings.append(
                {
                    "operation": operation,
                    "duration": duration,
                    "timestamp": time.time(),
                    **metadata,
                }
            )

        def get_stats(self, operation: str = None) -> Dict[str, float]:
            """Get performance statistics."""
            timings = self.timings
            if operation:
                timings = [t for t in timings if t["operation"] == operation]

            if not timings:
                return {}

            durations = [t["duration"] for t in timings]
            return {
                "count": len(durations),
                "mean": statistics.mean(durations),
                "median": statistics.median(durations),
                "min": min(durations),
                "max": max(durations),
                "std": statistics.stdev(durations) if len(durations) > 1 else 0.0,
                "total": sum(durations),
            }

        def get_percentile(self, operation: str, percentile: float) -> float:
            """Get percentile timing for an operation."""
            timings = [
                t["duration"] for t in self.timings if t["operation"] == operation
            ]
            if not timings:
                return 0.0

            sorted_timings = sorted(timings)
            k = (len(sorted_timings) - 1) * percentile / 100
            f = int(k)
            c = k - f

            if f == len(sorted_timings) - 1:
                return sorted_timings[f]
            else:
                return sorted_timings[f] * (1 - c) + sorted_timings[f + 1] * c

    @pytest.fixture
    def perf_metrics(self):
        """Performance metrics collector."""
        return TestModelTiming.PerformanceMetrics()

    @pytest.mark.performance
    @pytest.mark.asyncio
    async def test_server_initialization_timing(self, performance_config, perf_metrics):
        """Test server initialization performance."""

        # Warm-up run
        _ = SUEWSMCPServer(performance_config)

        # Benchmark initialization times
        n_runs = 10
        for i in range(n_runs):
            start_time = time.perf_counter()

            server = SUEWSMCPServer(performance_config)

            end_time = time.perf_counter()
            duration = end_time - start_time

            perf_metrics.record_timing("server_init", duration, run=i)

            # Cleanup
            await server.cleanup()

        # Analyze results
        stats = perf_metrics.get_stats("server_init")

        # Performance assertions
        assert (
            stats["mean"] < 0.1
        ), f"Server initialization too slow: {stats['mean']:.3f}s average"
        assert (
            stats["max"] < 0.5
        ), f"Server initialization max time too high: {stats['max']:.3f}s"
        assert (
            stats["std"] < 0.05
        ), f"Server initialization too variable: {stats['std']:.3f}s std"

        print(f"Server initialization: {stats['mean']:.3f}s ± {stats['std']:.3f}s")

    @pytest.mark.performance
    @pytest.mark.asyncio
    async def test_tool_listing_performance(self, performance_config, perf_metrics):
        """Test tool listing performance."""

        handlers = SUEWSMCPHandlers(performance_config)

        # Warm-up
        await handlers.handle_list_tools()

        # Benchmark tool listing
        n_runs = 50
        for i in range(n_runs):
            start_time = time.perf_counter()

            result = await handlers.handle_list_tools()

            end_time = time.perf_counter()
            duration = end_time - start_time

            perf_metrics.record_timing(
                "list_tools", duration, run=i, tool_count=len(result.get("tools", []))
            )

        stats = perf_metrics.get_stats("list_tools")

        # Performance assertions
        assert (
            stats["mean"] < 0.01
        ), f"Tool listing too slow: {stats['mean']:.4f}s average"
        assert (
            stats["max"] < 0.05
        ), f"Tool listing max time too high: {stats['max']:.4f}s"

        print(f"Tool listing: {stats['mean']:.4f}s ± {stats['std']:.4f}s")

    @pytest.mark.performance
    @pytest.mark.asyncio
    async def test_health_check_performance(self, performance_config, perf_metrics):
        """Test health check performance under various loads."""

        handlers = SUEWSMCPHandlers(performance_config)

        # Test with different numbers of active simulations
        for n_sims in [0, 1, 5, 10, 20]:
            # Add mock simulations
            handlers._active_simulations.clear()
            for i in range(n_sims):
                handlers._active_simulations[f"sim_{i}"] = {
                    "status": "running" if i % 3 == 0 else "completed",
                    "start_time": time.time() - 3600,
                    "config": f"config_{i}.yml",
                }

            # Benchmark health checks
            n_runs = 20
            for run in range(n_runs):
                start_time = time.perf_counter()

                result = await handlers.handle_call_tool("health_check", {})

                end_time = time.perf_counter()
                duration = end_time - start_time

                perf_metrics.record_timing(
                    "health_check", duration, run=run, active_sims=n_sims
                )

                # Verify health check completed
                assert not result.get("is_error", False)

        stats = perf_metrics.get_stats("health_check")

        # Performance assertions
        assert (
            stats["mean"] < 0.005
        ), f"Health check too slow: {stats['mean']:.4f}s average"
        assert (
            stats["max"] < 0.02
        ), f"Health check max time too high: {stats['max']:.4f}s"

        # Test scaling with simulation count
        sim_counts = [0, 1, 5, 10, 20]
        avg_times = []

        for n_sims in sim_counts:
            sim_timings = [
                t["duration"]
                for t in perf_metrics.timings
                if t["operation"] == "health_check" and t.get("active_sims") == n_sims
            ]
            if sim_timings:
                avg_times.append(statistics.mean(sim_timings))

        # Health check should not significantly degrade with more simulations
        if len(avg_times) >= 2:
            scaling_factor = avg_times[-1] / avg_times[0] if avg_times[0] > 0 else 1
            assert (
                scaling_factor < 3.0
            ), f"Health check scales poorly: {scaling_factor:.2f}x slower with more sims"

        print(f"Health check: {stats['mean']:.4f}s ± {stats['std']:.4f}s")

    @pytest.mark.performance
    @pytest.mark.asyncio
    async def test_configuration_validation_timing(
        self, performance_config, benchmark_config_template, perf_metrics, tmp_path
    ):
        """Test configuration validation performance with various config sizes."""

        handlers = SUEWSMCPHandlers(performance_config)

        # Test with configurations of different complexity
        config_variants = [
            ("minimal", 1),  # 1 site
            ("small", 3),  # 3 sites
            ("medium", 10),  # 10 sites
            ("large", 25),  # 25 sites
        ]

        for variant_name, n_sites in config_variants:
            # Create config with specified number of sites
            config = benchmark_config_template.copy()
            config["sites"] = []

            for i in range(n_sites):
                site = {
                    "name": f"site_{i+1}",
                    "properties": {
                        "lat": {"value": 51.5074 + i * 0.01},
                        "lng": {"value": -0.1278 - i * 0.01},
                        "alt": {"value": 50.0 + i * 5},
                    },
                    "land_cover": {
                        "paved": {"sfr": {"value": 0.3 + i * 0.01}},
                        "bldgs": {"sfr": {"value": 0.3}},
                        "grass": {"sfr": {"value": 0.4 - i * 0.01}},
                    },
                    "initial_conditions": {"soilstore_id": {"value": 150.0 + i * 10}},
                }
                config["sites"].append(site)

            config_file = tmp_path / f"config_{variant_name}.yml"
            with open(config_file, "w") as f:
                yaml.dump(config, f)

            # Benchmark validation
            n_runs = 10
            for run in range(n_runs):
                validation_args = {"config_file": str(config_file), "strict": True}

                start_time = time.perf_counter()

                with patch.object(handlers, "_call_validation_tool") as mock_validate:
                    mock_validate.return_value = {
                        "content": [
                            {"type": "text", "text": f"Config {variant_name} validated"}
                        ],
                        "is_error": False,
                    }

                    result = await handlers.handle_call_tool(
                        "validate_suews_config", validation_args
                    )

                end_time = time.perf_counter()
                duration = end_time - start_time

                perf_metrics.record_timing(
                    "config_validation",
                    duration,
                    variant=variant_name,
                    sites=n_sites,
                    run=run,
                )

                assert not result.get("is_error", False)

        # Analyze scaling
        for variant_name, n_sites in config_variants:
            variant_timings = [
                t["duration"]
                for t in perf_metrics.timings
                if t["operation"] == "config_validation"
                and t.get("variant") == variant_name
            ]

            if variant_timings:
                avg_time = statistics.mean(variant_timings)
                print(
                    f"Config validation ({variant_name}, {n_sites} sites): {avg_time:.4f}s"
                )

                # Performance requirements
                if n_sites <= 10:
                    assert (
                        avg_time < 0.05
                    ), f"Small config validation too slow: {avg_time:.4f}s"
                elif n_sites <= 25:
                    assert (
                        avg_time < 0.2
                    ), f"Large config validation too slow: {avg_time:.4f}s"

    @pytest.mark.performance
    @pytest.mark.slow
    @pytest.mark.asyncio
    async def test_simulation_timing_by_duration(
        self,
        performance_config,
        performance_forcing_data,
        benchmark_config_template,
        perf_metrics,
        tmp_path,
    ):
        """Test simulation performance for different simulation durations."""

        handlers = SUEWSMCPHandlers(performance_config)

        # Test different simulation durations
        durations = [
            ("1day", 24, 1),  # 1 day
            ("1week", 168, 7),  # 1 week
            ("1month", 744, 31),  # ~1 month
            ("1year", 8760, 365),  # 1 year
        ]

        for duration_name, n_hours, n_days in durations:
            print(f"Testing {duration_name} simulation ({n_hours} hours)")

            # Generate forcing data
            forcing_data = performance_forcing_data(n_hours)
            forcing_file = tmp_path / f"forcing_{duration_name}.txt"
            forcing_data.to_csv(forcing_file, sep=" ", index=False, float_format="%.2f")

            # Create config
            config = benchmark_config_template.copy()
            config["model"]["control"]["end_doy"] = {"value": n_days}
            config["model"]["control"]["forcing_file"] = {"value": forcing_file.name}

            config_file = tmp_path / f"config_{duration_name}.yml"
            with open(config_file, "w") as f:
                yaml.dump(config, f)

            # Benchmark simulation
            simulation_args = {
                "config_file": str(config_file),
                "simulation_id": f"perf_test_{duration_name}",
            }

            start_time = time.perf_counter()

            with patch.object(handlers, "_call_simulation_tool") as mock_simulate:
                # Simulate realistic execution time scaling
                base_time = 0.1  # Base processing time
                scaling_time = n_hours * 0.0001  # Scale with data size
                simulated_execution_time = base_time + scaling_time

                await asyncio.sleep(
                    min(simulated_execution_time, 1.0)
                )  # Cap for testing

                mock_simulate.return_value = {
                    "content": [
                        {
                            "type": "text",
                            "text": f"Simulation {duration_name} completed in {simulated_execution_time:.3f}s",
                        }
                    ],
                    "is_error": False,
                }

                result = await handlers.handle_call_tool(
                    "run_suews_simulation", simulation_args
                )

            end_time = time.perf_counter()
            duration = end_time - start_time

            perf_metrics.record_timing(
                "simulation",
                duration,
                duration_name=duration_name,
                hours=n_hours,
                days=n_days,
                simulated_exec_time=simulated_execution_time,
            )

            assert not result.get("is_error", False)

            # Performance requirements based on simulation size
            if n_hours <= 168:  # Up to 1 week
                assert (
                    duration < 5.0
                ), f"Small simulation {duration_name} too slow: {duration:.3f}s"
            elif n_hours <= 744:  # Up to 1 month
                assert (
                    duration < 15.0
                ), f"Medium simulation {duration_name} too slow: {duration:.3f}s"
            else:  # 1 year
                assert (
                    duration < 30.0
                ), f"Large simulation {duration_name} too slow: {duration:.3f}s"

        # Analyze scaling
        print("\nSimulation timing analysis:")
        for duration_name, n_hours, n_days in durations:
            timing_data = [
                t
                for t in perf_metrics.timings
                if t["operation"] == "simulation"
                and t.get("duration_name") == duration_name
            ]

            if timing_data:
                avg_duration = timing_data[0]["duration"]
                print(f"{duration_name}: {avg_duration:.3f}s ({n_hours} hours)")

    @pytest.mark.performance
    @pytest.mark.asyncio
    async def test_concurrent_simulation_performance(
        self,
        performance_config,
        performance_forcing_data,
        benchmark_config_template,
        perf_metrics,
        tmp_path,
    ):
        """Test performance of concurrent simulations."""

        # Set up for concurrent testing
        performance_config.max_concurrent_simulations = 4
        handlers = SUEWSMCPHandlers(performance_config)

        # Prepare multiple configurations
        n_concurrent = 4
        configs = []

        for i in range(n_concurrent):
            # Generate forcing data for 1 week
            forcing_data = performance_forcing_data(168)  # 1 week
            forcing_file = tmp_path / f"forcing_concurrent_{i}.txt"
            forcing_data.to_csv(forcing_file, sep=" ", index=False, float_format="%.2f")

            config = benchmark_config_template.copy()
            config["name"] = f"concurrent_test_{i}"
            config["model"]["control"]["end_doy"] = {"value": 7}
            config["model"]["control"]["forcing_file"] = {"value": forcing_file.name}

            config_file = tmp_path / f"concurrent_config_{i}.yml"
            with open(config_file, "w") as f:
                yaml.dump(config, f)
            configs.append(str(config_file))

        async def run_concurrent_simulation(
            config_file: str, sim_id: str, delay: float = 0
        ):
            """Run a single simulation with optional start delay."""
            await asyncio.sleep(delay)

            start_time = time.perf_counter()

            simulation_args = {"config_file": config_file, "simulation_id": sim_id}

            with patch.object(handlers, "_call_simulation_tool") as mock_simulate:
                # Simulate processing time
                processing_time = 0.2 + np.random.random() * 0.3
                await asyncio.sleep(processing_time)

                mock_simulate.return_value = {
                    "content": [
                        {
                            "type": "text",
                            "text": f"Concurrent simulation {sim_id} completed",
                        }
                    ],
                    "is_error": False,
                }

                result = await handlers.handle_call_tool(
                    "run_suews_simulation", simulation_args
                )

            end_time = time.perf_counter()
            duration = end_time - start_time

            perf_metrics.record_timing(
                "concurrent_simulation",
                duration,
                sim_id=sim_id,
                processing_time=processing_time,
            )

            assert not result.get("is_error", False)
            return sim_id, duration

        # Test 1: All simulations start simultaneously
        print("Testing simultaneous concurrent simulations...")
        start_time = time.perf_counter()

        tasks = [
            run_concurrent_simulation(configs[i], f"concurrent_sim_{i}")
            for i in range(n_concurrent)
        ]

        results = await asyncio.gather(*tasks)

        total_time = time.perf_counter() - start_time
        perf_metrics.record_timing("concurrent_batch", total_time, count=n_concurrent)

        # All should complete successfully
        assert len(results) == n_concurrent

        # Test 2: Staggered start times
        print("Testing staggered concurrent simulations...")
        handlers._active_simulations.clear()  # Reset

        start_time = time.perf_counter()

        staggered_tasks = [
            run_concurrent_simulation(
                configs[i],
                f"staggered_sim_{i}",
                delay=i * 0.1,  # 100ms stagger
            )
            for i in range(n_concurrent)
        ]

        staggered_results = await asyncio.gather(*staggered_tasks)

        staggered_total_time = time.perf_counter() - start_time
        perf_metrics.record_timing(
            "staggered_batch", staggered_total_time, count=n_concurrent
        )

        # Performance analysis
        concurrent_stats = perf_metrics.get_stats("concurrent_simulation")
        batch_stats = perf_metrics.get_stats("concurrent_batch")
        staggered_stats = perf_metrics.get_stats("staggered_batch")

        print(
            f"Concurrent simulations: {concurrent_stats['mean']:.3f}s ± {concurrent_stats['std']:.3f}s"
        )
        print(f"Batch completion: {batch_stats['mean']:.3f}s")
        print(f"Staggered completion: {staggered_stats['mean']:.3f}s")

        # Performance assertions
        assert (
            batch_stats["mean"] < 5.0
        ), f"Concurrent batch too slow: {batch_stats['mean']:.3f}s"
        assert (
            concurrent_stats["max"] < 3.0
        ), f"Individual simulation too slow: {concurrent_stats['max']:.3f}s"

        # Concurrency should provide some benefit vs sequential execution
        sequential_time_estimate = concurrent_stats["mean"] * n_concurrent
        efficiency = sequential_time_estimate / batch_stats["mean"]
        assert efficiency > 1.5, f"Poor concurrency efficiency: {efficiency:.2f}x"

    @pytest.mark.performance
    @pytest.mark.asyncio
    async def test_memory_usage_scaling(self, performance_config, perf_metrics):
        """Test memory usage scaling with different workloads."""

        handlers = SUEWSMCPHandlers(performance_config)
        process = psutil.Process()

        # Baseline memory usage
        baseline_memory = process.memory_info().rss / 1024 / 1024  # MB
        perf_metrics.record_timing("memory_baseline", baseline_memory, unit="MB")

        # Test memory usage with increasing numbers of active simulations
        for n_simulations in [0, 1, 5, 10, 20, 50]:
            # Clear and populate simulation tracking
            handlers._active_simulations.clear()

            for i in range(n_simulations):
                handlers._active_simulations[f"sim_{i}"] = {
                    "status": "running",
                    "config_file": f"/path/to/config_{i}.yml",
                    "start_time": time.time() - 1800,  # 30 minutes ago
                    "simulation_id": f"memory_test_{i}",
                    "metadata": {
                        "timesteps": 8760,
                        "sites": 1,
                        "variables": ["QN", "QH", "QE", "QS", "QF", "T2", "RH2", "U10"],
                    },
                }

            # Measure memory after adding simulations
            current_memory = process.memory_info().rss / 1024 / 1024  # MB
            memory_increase = current_memory - baseline_memory

            perf_metrics.record_timing(
                "memory_usage",
                memory_increase,
                active_sims=n_simulations,
                total_memory=current_memory,
                unit="MB",
            )

            # Run health check to trigger memory usage
            result = await handlers.handle_call_tool("health_check", {})
            assert not result.get("is_error", False)

            # Memory should not grow excessively
            memory_per_sim = memory_increase / max(1, n_simulations)
            if n_simulations > 0:
                assert (
                    memory_per_sim < 5.0
                ), f"Memory per simulation too high: {memory_per_sim:.2f} MB"

        # Analyze memory scaling
        memory_timings = [
            t for t in perf_metrics.timings if t["operation"] == "memory_usage"
        ]

        print(f"Baseline memory: {baseline_memory:.1f} MB")
        for timing in memory_timings:
            n_sims = timing.get("active_sims", 0)
            mem_increase = timing["duration"]
            total_mem = timing.get("total_memory", 0)

            if n_sims > 0:
                print(
                    f"{n_sims} simulations: +{mem_increase:.1f} MB ({total_mem:.1f} MB total)"
                )

    @pytest.mark.performance
    def test_performance_benchmarks_summary(self, perf_metrics):
        """Generate comprehensive performance benchmark summary."""

        if not perf_metrics.timings:
            pytest.skip("No performance data collected")

        print("\n" + "=" * 60)
        print("SUEWS MCP Server Performance Benchmark Summary")
        print("=" * 60)

        # Group by operation
        operations = set(t["operation"] for t in perf_metrics.timings)

        for operation in sorted(operations):
            stats = perf_metrics.get_stats(operation)
            if stats:
                print(f"\n{operation.replace('_', ' ').title()}:")
                print(f"  Average: {stats['mean']:.4f}s")
                print(f"  Median:  {stats['median']:.4f}s")
                print(f"  Min:     {stats['min']:.4f}s")
                print(f"  Max:     {stats['max']:.4f}s")
                print(f"  StdDev:  {stats['std']:.4f}s")
                print(f"  Count:   {stats['count']}")

                # Calculate percentiles
                p95 = perf_metrics.get_percentile(operation, 95)
                p99 = perf_metrics.get_percentile(operation, 99)
                print(f"  95th percentile: {p95:.4f}s")
                print(f"  99th percentile: {p99:.4f}s")

        # Performance grades
        print(f"\n{'Performance Assessment:':<25}")
        grades = []

        # Server initialization
        init_stats = perf_metrics.get_stats("server_init")
        if init_stats and init_stats["mean"] < 0.05:
            grades.append("Server Init: A")
        elif init_stats and init_stats["mean"] < 0.1:
            grades.append("Server Init: B")
        else:
            grades.append("Server Init: C")

        # Tool operations
        tool_stats = perf_metrics.get_stats("list_tools")
        if tool_stats and tool_stats["mean"] < 0.005:
            grades.append("Tool Operations: A")
        elif tool_stats and tool_stats["mean"] < 0.01:
            grades.append("Tool Operations: B")
        else:
            grades.append("Tool Operations: C")

        # Health checks
        health_stats = perf_metrics.get_stats("health_check")
        if health_stats and health_stats["mean"] < 0.002:
            grades.append("Health Checks: A")
        elif health_stats and health_stats["mean"] < 0.005:
            grades.append("Health Checks: B")
        else:
            grades.append("Health Checks: C")

        for grade in grades:
            print(f"  {grade}")

        print("\n" + "=" * 60)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
