.. _beta-testing:

Beta Testing Development Versions
=================================

This guide explains how to install and test development versions of SuPy from `test.pypi.org <https://test.pypi.org/project/supy/>`_.

.. important::
   Development versions are pre-release builds for testing new features. They may contain bugs or incomplete features.
   Use stable releases from `PyPI <https://pypi.org/project/supy/>`_ for production work.

Quick Start with uv
-------------------

The recommended method uses ``uv`` for fast, reliable installation:

.. code-block:: bash

   # Install uv
   curl -LsSf https://astral.sh/uv/install.sh | sh

   # Create environment
   uv venv .venv-beta
   source .venv-beta/bin/activate

   # Install development version
   uv pip install --index-url https://test.pypi.org/simple/ \
                 --extra-index-url https://pypi.org/simple/ \
                 supy

Why Use uv?
-----------

Standard pip installation from test.pypi.org often fails with dependency errors:

- Test PyPI doesn't mirror all dependencies
- Package name mismatches between indices
- Complex dependency resolution issues

``uv`` provides:

- Robust dependency resolution
- Efficient multi-index handling  
- 10-100x faster installation
- Better error messages

Detailed Instructions
---------------------

1. Install uv
~~~~~~~~~~~~~

**macOS/Linux:**

.. code-block:: bash

   curl -LsSf https://astral.sh/uv/install.sh | sh

**Windows (PowerShell):**

.. code-block:: powershell

   irm https://astral.sh/uv/install.ps1 | iex

2. Create Test Environment
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   # Create isolated environment
   uv venv .venv-beta
   
   # Activate environment
   # macOS/Linux:
   source .venv-beta/bin/activate
   # Windows:
   .venv-beta\Scripts\activate

3. Install Development Version
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   uv pip install --index-url https://test.pypi.org/simple/ \
                 --extra-index-url https://pypi.org/simple/ \
                 supy

The ``--extra-index-url`` ensures dependencies are fetched from PyPI when not available on Test PyPI.

4. Verify Installation
~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: python

   import supy as sp
   print(sp.__version__)  # Should show e.g. 2025.9.16.dev0

Managing Versions
-----------------

Install Specific Version
~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bash

   uv pip install --index-url https://test.pypi.org/simple/ \
                 --extra-index-url https://pypi.org/simple/ \
                 supy==2025.9.16.dev0

Switch to Stable
~~~~~~~~~~~~~~~~

.. code-block:: bash

   uv pip uninstall supy
   uv pip install supy

List Available Versions
~~~~~~~~~~~~~~~~~~~~~~~

Visit `test.pypi.org/project/supy <https://test.pypi.org/project/supy/>`_ to see all development versions.

Troubleshooting
---------------

Dependency Errors
~~~~~~~~~~~~~~~~~

If packages can't be found, ensure you're using both indices:

.. code-block:: bash

   uv pip install --index-url https://test.pypi.org/simple/ \
                 --extra-index-url https://pypi.org/simple/ \
                 supy

Wrong Version Installed
~~~~~~~~~~~~~~~~~~~~~~~

Clear the cache and reinstall:

.. code-block:: bash

   uv pip uninstall supy
   uv pip cache clean
   uv pip install --index-url https://test.pypi.org/simple/ \
                 --extra-index-url https://pypi.org/simple/ \
                 supy

Import Errors
~~~~~~~~~~~~~

Install with all optional dependencies:

.. code-block:: bash

   uv pip install --index-url https://test.pypi.org/simple/ \
                 --extra-index-url https://pypi.org/simple/ \
                 "supy[all]"

Reporting Issues
----------------

When reporting beta version issues:

1. **Include Version Information**

   .. code-block:: python

      import supy as sp
      import sys
      import platform
      
      print(f"SuPy version: {sp.__version__}")
      print(f"Python version: {sys.version}")
      print(f"Platform: {platform.platform()}")

2. **Describe Installation Method**

   - Installation commands used
   - Any errors during installation
   - Environment details (conda, venv, etc.)

3. **Provide Complete Error Messages**

   - Full stack traces
   - Steps to reproduce
   - Minimal example code

4. **Create GitHub Issue**

   - Visit: https://github.com/UMEP-dev/SUEWS/issues/new
   - Use label: ``beta-testing``
   - Reference the specific dev version

Alternative: Using pip
----------------------

If ``uv`` is not available, you can try pip with careful index ordering:

.. code-block:: bash

   pip install --index-url https://test.pypi.org/simple/ \
               --extra-index-url https://pypi.org/simple/ \
               --upgrade \
               supy

.. warning::
   This method is more prone to the dependency resolution issues described in `Issue #652 <https://github.com/UMEP-dev/SUEWS/issues/652>`_.

See Also
--------

- :doc:`installation` - Standard installation guide
- `SuPy on Test PyPI <https://test.pypi.org/project/supy/>`_
- `SuPy on PyPI <https://pypi.org/project/supy/>`_
- `uv documentation <https://github.com/astral-sh/uv>`_