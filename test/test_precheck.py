import pytest
from supy.data_model import SUEWSConfig

def test_precheck_invocation(capsys):
    yaml_input = {
        "name": "test",
        "model": {
            "control": {
                "start_time": "2012-01-01",
                "end_time": "2012-12-31",
                "forcing_file": {"value": "dummy.txt"},
            },
            "physics": {
                "diagmethod": 2,
                "stabilitymethod": 3,
                "storageheatmethod": 2,
            },
        },
        "sites": [{}],
    }

    try:
        SUEWSConfig(**yaml_input)
    except Exception:
        pass  # precheck is tested, don't care about rest

    captured = capsys.readouterr()
    assert "Starting precheck procedure..." in captured.out
    assert "Precheck complete." in captured.out
