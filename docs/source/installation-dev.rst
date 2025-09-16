Installing Development Versions
================================

.. warning::
   
   **⚠️ CAUTION: Development versions are unstable!**
   
   - Development versions are **pre-release** and may contain bugs
   - Features may change or break without notice
   - Not recommended for production use or research publications
   - Only use if you need to test new features or help with development
   - Always note the exact version used in your work

.. note::
   
   For stable installations, see :doc:`installation` and use the standard ``pip install supy`` command.

Testing Pre-release Versions
-----------------------------

Development versions are published to `test.pypi.org <https://test.pypi.org/project/supy/>`_ and can be installed for testing new features before official release.

Prerequisites
~~~~~~~~~~~~~

- Python 3.9 or later
- ``venv`` module (included with Python)
- Internet connection to access test.pypi.org

Installation Steps
~~~~~~~~~~~~~~~~~~

**Step 1: Create an isolated environment**

Always use a separate environment for development versions to avoid conflicts:

.. code-block:: bash

   python3 -m venv .venv-dev
   source .venv-dev/bin/activate  # Linux/macOS
   # or
   .venv-dev\Scripts\activate      # Windows

**Step 2: Install uv package manager**

The ``uv`` tool handles test package dependencies better than standard pip:

.. code-block:: bash

   pip install uv

**Step 3: Check available development versions**

Visit https://test.pypi.org/project/supy/ to find the latest development version.

Look for versions with format ``YYYY.M.D.dev0`` (e.g., ``2025.9.16.dev0``).

**Step 4: Install specific development version**

.. code-block:: bash

   # Replace 2025.9.16.dev0 with the latest version from Step 3
   uv pip install --extra-index-url https://test.pypi.org/simple/ \
                 --index-strategy unsafe-best-match \
                 supy==2025.9.16.dev0

.. important::
   
   - The ``--index-strategy unsafe-best-match`` flag is **required** for correct dependency resolution
   - Always specify the exact version (e.g., ``supy==2025.9.16.dev0``)
   - Without these, you may get the stable version instead

**Step 5: Verify installation**

.. code-block:: bash

   python -c "import supy; print(f'SuPy version: {supy.__version__}')"

This should display the development version (e.g., ``2025.9.16.dev0``).

**Step 6: Test functionality**

.. code-block:: bash

   python -c "import supy as sp; sp.load_sample_data(); print('✓ Installation successful')"

Troubleshooting
~~~~~~~~~~~~~~~

**Wrong version installed**
   If you get a stable version (e.g., ``2025.7.6``) instead of a dev version:
   
   - Ensure you used ``--index-strategy unsafe-best-match``
   - Verify you specified the exact version with ``==``
   - Check the version exists at https://test.pypi.org/project/supy/

**Dependency errors**
   The ``uv`` tool with proper flags should resolve most issues. If problems persist:
   
   - Try uninstalling and reinstalling: ``uv pip uninstall supy``
   - Create a fresh environment
   - Report the issue on GitHub

**Import errors**
   Development versions may have different dependencies. Try:
   
   .. code-block:: bash
   
      uv pip install --extra-index-url https://test.pypi.org/simple/ \
                    --index-strategy unsafe-best-match \
                    --upgrade \
                    supy==2025.9.16.dev0

Reporting Issues
~~~~~~~~~~~~~~~~

When reporting issues with development versions:

1. **Always include the exact version**:
   
   .. code-block:: python
   
      import supy as sp
      import sys
      print(f"SuPy: {sp.__version__}")
      print(f"Python: {sys.version}")

2. **Describe your installation method** (the exact commands used)

3. **Include full error messages** and stack traces

4. **Create a GitHub issue** with label ``beta-testing``

Switching Back to Stable
~~~~~~~~~~~~~~~~~~~~~~~~

To return to the stable version:

.. code-block:: bash

   # In your regular environment (not .venv-dev)
   pip uninstall supy
   pip install supy

Or simply use a different virtual environment for stable work.

Why Standard pip Fails
~~~~~~~~~~~~~~~~~~~~~~~

Standard ``pip install`` from test.pypi.org often fails because:

- Test PyPI doesn't mirror all dependencies from main PyPI
- Package name mismatches between indices
- Complex dependency resolution across multiple indices

The ``uv`` tool with ``--index-strategy unsafe-best-match`` solves these issues by:

- Better handling of multiple package indices
- More robust dependency resolution
- Faster installation (10-100x faster than pip)

See Also
~~~~~~~~

- `Test PyPI SuPy releases <https://test.pypi.org/project/supy/>`_
- `GitHub Issues <https://github.com/UMEP-dev/SUEWS/issues>`_
- :doc:`installation` - For stable version installation
- :doc:`troubleshooting` - For general troubleshooting