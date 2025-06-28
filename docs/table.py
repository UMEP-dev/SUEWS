#!/bin/python3
# coding: utf-8

"""
Convert RST grid tables to list-tables.

Basic usage
-----------

#. Convert grid tables in a file to list-tables. The result is output
   to stdout::

      $ python tables.py input.rst

#. Convert several files::

      $ python tables.py input1.rst input2.rst

      $ python tables.py *.rst

Options
-------

-c, --create    create new files ``*.rst.new`` with the converted tables.
                Original files are unchanged.
-r, --replace   modify input files, replacing grid tables with list-tables

.. warning::

    The script does not perform any tests on the conversion nor does it
    rollback if it encounters an error. Using the ``--replace`` option
    replaces the source text completely. To prevent data loss, use the
    ``--create`` option and confirm the conversion is correct before removing
    the original source file.

.. important::

    Always build your document and compare the rendered list-table to the
    original rendered grid table. It is very possible that some errors may
    occur that require manual fixes, especially when converting complex tables.

    The script does not handle cells that span multiple rows or columns. If
    you convert a table with these types of cells you may receive a parsing
    error when running sphinx-build::

       ERROR: Error parsing content block for the "list-table" directive:
              uniform two-level bullet list expected, but row 2 does not
              contain the same number of items as row 1 (4 vs 3)

    This indicates that the list-table needs manual clean-up. Look for lines
    like this in the source::

       * - Alarms
         - Acknowledge an alarm    * - hello
         -
         - Yes

    Compare with the original table to determine the correct structure of the
    broken row.

Notes
-----

- The script does not create titles for tables. After conversion, you may
  want to manually add titles.

- The script sets all columns to the same width: ``100 / col_num``. After
  conversion, you may want to manually edit ``:width:``.

- The script automatically uses the first row of the table as a header.
  After conversion, you may want to manually edit ``:header-rows:``.

- The script requires a blank line after each table. If the blank line is at
  the end of the file, you must add an extra line temporarily for the script
  to process the table correctly.
"""

import argparse
import logging
from os import path

# number of tables converted during a run
numtables = 0


def readfile(infile):
    """
    Read data from file and return text as a string.

    :param infile: file to read
    :type infile: str
    :returns: text (Success), 1 (Failure)
    :rtype: str or int
    """
    infile = path.realpath(infile)
    try:
        with open(infile, "r") as f:
            data = f.read()
        return data
    except Exception as e:
        logging.error("File error (readData): " + str(e))
        return 1


def writefile(outfile, data):
    """
    Write data to file.

    :param outfile: file to write
    :type files: str
    :returns: 0 (Success), 1 (Failure)
    :rtype: int
    """
    outfile = path.realpath(outfile)
    try:
        with open(outfile, "w") as f:
            f.write(data)
        return 0
    except Exception as e:
        logging.error("File error (writeData): " + str(e))
        return 1


def adjustrow(row):
    """
    Convert a grid row to a list-table row.

    :param row: a row of grid table text
    :type row: str
    :return: a row of list-table text
    :rtype: str
    """
    if row.startswith("+") is True:
        return "\n"
    row = row.split("|")
    new_row = []
    for entry in row:
        new_row.append(entry)
        try:
            new_row.pop(new_row.index(""))
        except:
            pass
    convert = []
    convert.append("   * - " + new_row[0].strip())
    for entry in new_row[1:]:
        convert.append(("\n     - " + entry.strip()).rstrip())
    result = "".join(convert)
    return result


def buildtable(gridtable):
    """
    Build an RST list-table from an RST grid table.

    :param gridtable: an RST grid table
    :type gridtable: list
    :return: an RST list-table
    :rtype: str
    """
    col_num = gridtable[0].count("+") - 1
    col_width = str(int(100 / col_num))
    col_width = (" " + col_width) * col_num

    output = []
    for line in gridtable:
        row = adjustrow(line)
        output.append(row)
    result = "".join(output)
    list_table = """.. list-table::\n   :widths:%s\n   :header-rows: 1\n%s""" % (
        col_width,
        result,
    )
    global numtables
    numtables += 1
    return list_table


def dofile(infile, replace, create):
    """Process file to convert grid tables to list-tables."""
    data = readfile(infile)
    data = data.splitlines()
    grid = False
    insert = False
    gridtable = []
    content = []
    for line in data:
        if line.startswith("+--") is True or line.startswith("+==") is True:
            grid = True
            insert = True
            gridtable.append(line)
        elif grid is True and line.startswith("|") is True:
            gridtable.append(line)
        else:
            grid = False
        if grid is False:
            if insert is True:
                insert = False
                newtable = buildtable(gridtable)
                content.append(newtable + "\n")
                gridtable = []
            else:
                content.append(line + "\n")
    content = "".join(content)
    if replace is True:
        writefile(infile, content)
    elif create is True:
        writefile(infile + ".new", content)
    else:
        print(content)
    return


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="table",
        description="""Convert RST grid table
                                     to list-table.""",
    )
    parser.add_argument("INPUT", type=str, nargs="+", help="RST file(s)")
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "-c",
        "--create",
        action="store_true",
        help="""create new files (*.rst.new) with the converted
                       tables. Original files are unchanged.""",
    )
    group.add_argument(
        "-r",
        "--replace",
        action="store_true",
        help="""modify input files, replacing grid tables with
                       list-tables""",
    )
    args = parser.parse_args()
    for file in args.INPUT:
        dofile(file, args.replace, args.create)
    print("\033[92mTables converted: " + str(numtables) + "\033[0m")
