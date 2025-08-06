"""
Test delimiter handling in table conversion.

This test ensures that CSV/table writing during conversion
doesn't have delimiter conflicts that could cause parsing errors.
"""

import pytest
import tempfile
from pathlib import Path
import pandas as pd
import numpy as np
from io import StringIO

# Import the converter module
from supy.util._converter import add_var


class TestDelimiterHandling:
    """Test that CSV delimiter handling is correct in converters."""

    def test_add_var_csv_output(self):
        """Test that add_var produces correctly formatted CSV output."""
        with tempfile.TemporaryDirectory() as tmpdir:
            # Create a test file similar to SUEWS tables
            test_file = Path(tmpdir) / "test_table.txt"
            
            # Create initial content with space-separated values
            initial_content = """Code Name Value
1 2 3
100 Test 1.5
200 Sample 2.5
-9 -9 -9
-9 -9 -9"""
            
            test_file.write_text(initial_content)
            
            # Add a new variable using add_var
            add_var(str(test_file), "NewVar", "4", "999")
            
            # Read the modified file
            modified_content = test_file.read_text()
            
            # Check that the file can be parsed correctly
            # SUEWS tables use whitespace as separator
            df = pd.read_csv(
                StringIO(modified_content),
                sep=r"\s+",
                header=[0, 1],  # Multi-level header
                comment="!",
            )
            
            # Verify the new column was added
            assert df.shape[1] == 4  # Original 3 + 1 new column
            
            # Check that no problematic quote characters are in the output
            assert '","' not in modified_content, "Found problematic ',' quote pattern"
            assert '" "' not in modified_content, "Found problematic space quote pattern"
            
            # Verify the values are preserved correctly
            # The last two rows should be -9 (missing value indicator)
            assert df.iloc[-1, 0] == -9
            assert df.iloc[-2, 0] == -9

    def test_no_quote_conflicts(self):
        """Test that there are no quote/delimiter conflicts in output."""
        with tempfile.TemporaryDirectory() as tmpdir:
            test_file = Path(tmpdir) / "test_spaces.txt"
            
            # Create content with values that might cause issues
            # if delimiters are incorrectly configured
            initial_content = """Code Description Value
1 2 3
100 "Test_with_quotes" 1.5
200 'Single_quotes' 2.5
300 Space_in_name 3.5
-9 -9 -9
-9 -9 -9"""
            
            test_file.write_text(initial_content)
            
            # Add a variable with a value that includes special characters
            add_var(str(test_file), "Special", "4", "Test_Value")
            
            # Read back the file
            modified_content = test_file.read_text()
            
            # Parse the modified file
            df = pd.read_csv(
                StringIO(modified_content),
                sep=r"\s+",
                header=[0, 1],
                comment="!",
            )
            
            # Should be able to parse without errors
            assert df.shape[0] == 5  # 5 data rows
            assert df.shape[1] == 4  # 4 columns after addition
            
            # Verify no CSV-style quoting was introduced
            # (SUEWS uses space-separated format without quotes)
            lines = modified_content.split("\n")
            for line in lines:
                # Check that values are space-separated, not comma-separated
                if line and not line.startswith("!"):
                    # Should not have commas as delimiters
                    assert "," not in line or line.startswith("#"), \
                        f"Found comma in data line: {line}"

    def test_float_formatting_consistency(self):
        """Test that float values are formatted consistently."""
        with tempfile.TemporaryDirectory() as tmpdir:
            test_file = Path(tmpdir) / "test_floats.txt"
            
            # Create content with float values
            initial_content = """Code Value1 Value2
1 2 3
100 1.234567 9.876543
200 0.000001 999999.9
-9 -9 -9
-9 -9 -9"""
            
            test_file.write_text(initial_content)
            
            # Add a float variable
            add_var(str(test_file), "FloatCol", "4", "3.14159")
            
            # Read the modified content
            modified_content = test_file.read_text()
            
            # Check float formatting
            lines = modified_content.split("\n")
            data_lines = [l for l in lines[2:] if l.strip()]  # Skip headers
            
            for line in data_lines[:3]:  # Check non-missing value rows
                values = line.split()
                for val in values[1:]:  # Skip code column
                    try:
                        float_val = float(val)
                        # Check that floats are formatted with 4 decimal places
                        if "." in val:
                            decimal_places = len(val.split(".")[1])
                            assert decimal_places <= 4, \
                                f"Float {val} has more than 4 decimal places"
                    except ValueError:
                        # Not a float, that's okay
                        pass

    def test_multiindex_header_preservation(self):
        """Test that multi-index headers are preserved correctly."""
        with tempfile.TemporaryDirectory() as tmpdir:
            test_file = Path(tmpdir) / "test_header.txt"
            
            # Create content with multi-level header (as SUEWS tables have)
            initial_content = """   1     2     3
Code Type Value
100 A 1.5
200 B 2.5
-9 -9 -9
-9 -9 -9"""
            
            test_file.write_text(initial_content)
            
            # Add a new variable
            add_var(str(test_file), "NewCol", "4", "TestVal")
            
            # Read back and verify structure
            modified_content = test_file.read_text()
            lines = modified_content.split("\n")
            
            # Should have two header lines
            # First line should be column indices (1, 2, 3, 4)
            # Second line should be column names
            assert len([l for l in lines if l.strip()]) >= 6  # 2 headers + 4 data rows
            
            # Verify the column indices are present
            first_line = lines[0].strip()
            # Should contain sequential numbers
            assert "1" in first_line
            assert "2" in first_line
            assert "3" in first_line
            assert "4" in first_line  # New column index