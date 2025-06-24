import pytest
from copy import deepcopy
from supy.data_model import SUEWSConfig
from supy.data_model.core import precheck_start_end_date

def test_precheck_start_end_date():
    data = {}
    updated_data, model_year, start_date, end_date = precheck_start_end_date(data)

    assert updated_data == data
    assert start_date == "2011-01-22"
    assert end_date == "2011-02-22"
    assert model_year == 2011
