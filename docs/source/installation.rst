.. _installation:


Installation
============



Formal releases
---------------

Since 2023, SUEWS is available as a command line tool via its Python wrapper package `SuPy (SUEWS in Python) <SuPy>`_ on `PyPI`_.

.. note::

    The Fortran-based binaries build prior to 2023 are still available at the `SUEWS download page`_.
    However, they are not maintained anymore so users are encouraged to use the Python-based packages instead.


Installing Python
*****************
We recommend `uv <https://docs.astral.sh/uv/>`_, a fast Python package and environment manager written in Rust.

To install ``uv``, follow the `official installation instructions <https://docs.astral.sh/uv/getting-started/installation/>`__::

    # macOS/Linux
    curl -LsSf https://astral.sh/uv/install.sh | sh

    # Windows (PowerShell)
    powershell -c "irm https://astral.sh/uv/install.ps1 | iex"

``uv`` can automatically download and manage Python versions, so a separate Python installation is not required.


Installing SuPy
***************

One can install ``supy`` using ``pip``:


.. code-block:: shell

  python3 -m pip install supy --upgrade





.. _PyPI: https://pypi.org/project/supy/
.. _SuPy: :ref:`supy_index`



Testing Development Versions
-----------------------------

.. warning::
   
   **⚠️ CAUTION: Development versions are unstable!**
   
   - Development versions are **pre-release** and may contain bugs
   - Features may change or break without notice  
   - Not recommended for production use or research publications
   - Only use if you need to test new features or help with development

Development versions are published to `test.pypi.org <https://test.pypi.org/project/supy/>`_ for testing new features before official release.

**Installation Steps:**

1. **Install uv** (one-time setup)::

    # macOS/Linux
    curl -LsSf https://astral.sh/uv/install.sh | sh
    
    # Windows (PowerShell)
    powershell -c "irm https://astral.sh/uv/install.ps1 | iex"

.. note::

   ``uv`` is a fast Python package and environment manager written in Rust. It replaces ``pip``, ``venv``, and other tools with a single, faster solution.

2. **Create an isolated environment**::

    uv venv .venv-dev
    source .venv-dev/bin/activate  # Linux/macOS
    # or: .venv-dev\Scripts\activate  # Windows
    
    # You'll see (.venv-dev) in your terminal prompt when activated

.. tip::

   ``uv venv`` is 80x faster than ``python -m venv`` and automatically manages Python versions.

3. **Check latest version** at https://test.pypi.org/project/supy/ (format: ``YYYY.M.D.dev0``)

4. **Install development version**::

    # Replace 2025.9.16.dev0 with latest version from step 3
    uv pip install --extra-index-url https://test.pypi.org/simple/ \
                  --index-strategy unsafe-best-match \
                  supy==2025.9.16.dev0

5. **Verify installation**::

    python -c "import supy; print(f'SUEWS version: {supy.__version__}')"
    # Should show: 2025.9.16.dev0

**For future use:**

Always activate the virtual environment before working with the development version::

    source .venv-dev/bin/activate  # Linux/macOS
    # or: .venv-dev\Scripts\activate  # Windows

To deactivate when finished::

    deactivate

**Why uv?**

- Creates virtual environments 80x faster than standard tools
- Handles test.pypi.org dependencies correctly with ``--index-strategy unsafe-best-match``
- Single tool for both environment and package management
- Can automatically download and manage Python versions


Alternative: pip-only Approach
******************************

If you cannot use ``uv`` (e.g., in managed Python environments like OSGeo4W/QGIS, or corporate environments), use this two-step approach.

.. note::

   **Why two steps?** Using ``pip install --extra-index-url https://test.pypi.org/simple/`` can cause dependency issues where pip pulls source tarballs from test.pypi.org instead of pre-built wheels from PyPI. The two-step approach ensures only ``supy`` comes from test.pypi.org while all dependencies come from regular PyPI.

**Steps:**

1. **Check available versions** at https://test.pypi.org/project/supy/#history

   .. warning::

      TestPyPI regularly removes old packages to free up space, so version numbers in examples below may no longer exist. Always use a version currently listed on the page above.

2. **Download the wheel from test.pypi.org** (supy only, no dependencies)::

    # Replace 2025.11.25.dev0 with an available version from step 1
    pip download --no-deps -i https://test.pypi.org/simple/ supy==2025.11.25.dev0

3. **Install from the downloaded wheel** (dependencies from regular PyPI)::

    pip install --find-links=. supy==2025.11.25.dev0

4. **Verify installation**::

    python -c "import supy; print(f'SUEWS version: {supy.__version__}')"


OSGeo4W / UMEP Users
********************

If you use SUEWS via `UMEP <https://umep-docs.readthedocs.io/>`_ in QGIS, use the **pip-only approach** above with these UMEP-specific requirements:

.. important::

   **Use dev1 versions only.** UMEP users should install versions ending in ``dev1`` (e.g., ``2025.11.25.dev1``), which include UMEP-specific compatibility fixes. Do not use ``dev0`` versions.

.. important::

   **Use the OSGeo4W Shell** (not PowerShell or CMD). OSGeo4W's Python requires environment variables that are only set in this shell. Find it via Start menu → "OSGeo4W Shell".

**UMEP-specific notes:**

- No virtual environment is needed — OSGeo4W manages QGIS's Python environment
- The development version will replace any existing ``supy`` installation in QGIS
- After installation, **restart QGIS** before using the new version
- To verify, open the QGIS Python Console and run::

    import supy; print(supy.__version__)


Development build
-----------------

.. warning::

   The development build can be highly unstable and is not recommended for production use.
   However, it is automatically constructed every week for testing purposes and we are happy to receive feedback on the development build.


To install the development build of SUEWS, you need to install ``supy`` in the development mode:

1. Clone the repository::

    git clone https://github.com/UMEP-dev/SUEWS.git
    cd SUEWS

2. Run ``make`` to see quick start instructions and available commands::

    make

3. Follow the quick start workflow shown (activate a virtual environment, then ``make dev``)


