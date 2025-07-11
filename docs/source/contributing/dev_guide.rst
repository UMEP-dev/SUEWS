.. _dev_guide:

Development Guide
=================

.. note:: If you are interested in contributing to the code please open a new discussion in the `UMEP Community`_ to illustrate your proposal: we are happy to collaborate in an open development mode.

Quick Start - Get Coding in 5 Minutes
--------------------------------------

Get a working development environment and start contributing:

.. code-block:: bash

   # Clone and setup
   git clone https://github.com/UMEP-dev/SUEWS.git
   cd SUEWS
   mamba env create -f env.yml
   mamba activate suews-dev
   make dev

   # Verify everything works
   make test

You're ready! GitHub Actions will handle code formatting automatically.

Development Workflow
--------------------

Code → Test → Push → Let CI Handle the Rest
*******************************************

.. code-block:: bash

   # Find what to work on
   gh issue list --assignee @me

   # Create your branch
   git checkout -b feature/your-feature-name

   # Make changes and test locally
   make test

   # Push and create PR
   git push -u origin feature/your-feature-name
   gh pr create

Common Development Tasks
------------------------

Adding a New Output Variable
****************************

1. **Modify Output Definition**: Add variable to ``src/suews/src/suews_ctrl_output.f95``

   .. code-block:: fortran

      varAttr('YourVar', 'units', f104, 'Description', aA, 'Group', 0)

2. **Rebuild**: ``make dev``

3. **Test**: ``pytest test/test_suews_simulation.py -v``

Fixing a Physics Bug
*********************

1. **Find Physics Code**: Look in ``src/suews/src/suews_phys_*.f95``
2. **Use Benchmark Data**: Test against ``test/benchmark1/benchmark1.yml``
3. **Validate**: ``make test``

Adding YAML Configuration Option
*********************************

1. **Update Data Model**: Modify ``src/supy/data_model/``
2. **Test Validation**: ``pytest test/test_data_model.py -v``

Quick Testing
*************

.. code-block:: bash

   # Run all tests
   make test

   # Run specific test file
   pytest test/test_supy.py -v

   # Run specific test method
   pytest test/test_supy.py::TestClass::test_method -v

   # Quick iteration with specific config
   python -c "import supy as sp; sp.run('test/benchmark1/benchmark1.yml')"

Debugging
---------

Python Debugging
*****************

.. code-block:: python

   # Interactive debugging
   import ipdb; ipdb.set_trace()

   # Quick inspection
   print(f"Variable value: {your_variable}")

Fortran Debugging
*****************

For Fortran debugging, see the GDB section in ``README.md``.

Build Issues
************

.. code-block:: bash

   # Common fixes for build problems
   make clean && make dev    # Clean rebuild
   mamba activate suews-dev  # Ensure correct environment

Test Data Resources
-------------------

Use these for validation and testing:

**Benchmark Configuration:**
   ``test/benchmark1/benchmark1.yml``

**Forcing Data:**
   ``test/benchmark1/forcing/Kc1_2011_data_5.txt``

**Multi-grid Tests:**
   ``test/data_test/multi-grid/``

**ERA5 Test Data:**
   ``test/data_test/single-grid/``

Project Structure
-----------------

Key directories for development:

.. code-block:: text

   SUEWS/
   ├── src/
   │   ├── suews/          # Fortran physics engine
   │   │   └── src/        # Core physics modules
   │   ├── supy/           # Python interface
   │   │   ├── data_model/ # YAML configuration models
   │   │   └── util/       # Utility functions
   │   └── supy_driver/    # F2Py wrapper
   ├── test/               # Test suite and data
   ├── docs/               # Documentation source
   └── Makefile           # Build commands

Code Quality Tools (Handled by CI)
-----------------------------------

These tools run automatically in GitHub Actions:

**Python:**
   - **ruff**: Fast linting and formatting
   - **pytest**: Testing framework

**Fortran:**
   - **fprettify**: Auto-formatting
   - **gfortran**: Compilation with warnings

**VS Code Extensions** (Optional):
   - Modern Fortran
   - Python
   - GitLens
   - GitHub Copilot

Performance Analysis (Optional)
*******************************

For performance work:

.. code-block:: bash

   # Python profiling
   python -m cProfile your_script.py

   # Line-by-line profiling
   pip install line_profiler
   @profile  # Add decorator to functions
   kernprof -l -v your_script.py

Build Commands Reference
------------------------

.. code-block:: bash

   make dev          # Fast development build (recommended)
   make              # Full build with tests
   make test         # Run test suite only
   make clean        # Clean build artifacts
   make docs         # Build documentation
   make livehtml     # Live documentation preview

SUEWS-Specific Patterns
-----------------------

Variable Naming
***************

Follow the existing pattern in the codebase:

- Include units in variable names: ``Temp_C``, ``Press_hPa``
- Use descriptive names: ``LatentHeatFlux`` not ``LHF``
- Fortran: ALL_CAPS for parameters, CamelCase for variables

Output Variables
****************

When adding output variables:

1. Define in ``suews_ctrl_output.f95``
2. Calculate in appropriate physics module
3. Add to output group (SUEWS, ESTM, BEERS, etc.)
4. Document in output files documentation

Testing Philosophy
******************

- **Always test against benchmark data** before submitting
- **Add tests for new features** in ``test/test_*.py``
- **Use existing test patterns** - copy similar tests
- **Test edge cases** - what happens with missing data?

Getting Help
------------

- **GitHub Issues**: `Report bugs or request features <https://github.com/UMEP-dev/SUEWS/issues>`_
- **Discussions**: `Ask questions <https://github.com/UMEP-dev/UMEP/discussions>`_
- **Documentation**: This manual and inline code comments

Troubleshooting Common Issues
-----------------------------

**Import Errors**
   ``make clean && make dev``

**Test Failures After Fortran Changes**
   Need full rebuild: ``make clean && make``

**F2PY Compilation Issues**
   Check function signatures match between Fortran and Python wrapper

**Permission Errors (Windows)**
   Right-click project folder → Properties → Security → Edit → Everyone → Allow

.. _UMEP Community: https://github.com/UMEP-dev/UMEP/discussions