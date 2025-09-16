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
These instructions will set you up with `mamba`_, which makes it easy to install and manage Python packages.

To install the ``mamba`` Python distribution follow `the mamba installation instructions <https://mamba.readthedocs.io/en/latest/installation.html>`__.

This makes installing ``supy`` and many other packages in the scientific Python ecosystem much easier and quicker.
It also provides many pre-compiled binaries that are not available on PyPI.

.. tip::

    ``mamba`` is a drop-in replacement for ``conda`` (another widely used Python package manager):
    ``mamba`` is faster and solves some common problems with ``conda``.
    More details about ``mamba`` can be found at `mamba`_.


Installing SuPy
***************

One can install ``supy`` using ``pip``:


.. code-block:: shell

  python3 -m pip install supy --upgrade

.. comment out the following section for now as supy is not yet available on conda-forge.
.. or ``mamba``:

.. .. code-block:: bash

..     mamba install -c conda-forge supy





.. _PyPI: https://pypi.org/project/supy/
.. _mamba: https://github.com/mamba-org/mamba
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

    python -c "import supy; print(f'SuPy version: {supy.__version__}')"
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

Development build
-----------------

.. warning::

The development build can be highly unstable and is not recommended for production use.
However, it is automatically constructed every week for testing purposes and we are happy to receive feedback on the development build.


To install the development build of SUEWS, you need to install ``supy`` in the development mode:

1. git clone the repository::

    git clone https://github.com/UMEP-dev/SUEWS.git

2. navigate to the directory of the cloned repository::

    cd SUEWS

3. install the package in the development mode::

    make dev


