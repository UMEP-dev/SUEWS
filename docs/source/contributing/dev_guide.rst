.. _dev_guide:

Development Guide
=================

.. note:: **Code development is handled by the SUEWS core team.** For detailed development documentation, see the `Developer Documentation <https://github.com/UMEP-dev/SUEWS/tree/master/dev-ref>`_ on GitHub.

.. note:: If you are interested in contributing to the project, please open a new discussion in the `UMEP Community`_ to share your ideas.

Community Contributions
-----------------------

We welcome community contributions in the following areas:

- **Bug Reports**: Open an issue on `GitHub Issues <https://github.com/UMEP-dev/SUEWS/issues>`_
- **Documentation Improvements**: Submit PRs for documentation fixes
- **Feature Requests**: Discuss ideas in `GitHub Discussions <https://github.com/UMEP-dev/UMEP/discussions>`_

Building SUEWS Locally
----------------------

If you want to build SUEWS from source for local use:

Prerequisites
*************

- Python 3.9+
- gfortran compiler
- pip or conda

Basic Setup
***********

.. code-block:: bash

   # Clone repository
   git clone https://github.com/UMEP-dev/SUEWS.git
   cd SUEWS
   
   # Install in development mode
   pip install -e .
   
   # Or using conda/mamba
   mamba env create -f env.yml
   mamba activate suews-dev
   make dev

Running Tests
*************

To verify your build:

.. code-block:: bash

   # Run test suite
   make test
   
   # Or using pytest directly
   pytest test/

Project Structure
-----------------

.. code-block:: text

   SUEWS/
   ├── src/
   │   ├── suews/          # Fortran physics engine
   │   ├── supy/           # Python interface
   │   └── supy_driver/    # Python-Fortran bridge
   ├── test/               # Test suite
   ├── docs/               # Documentation
   └── dev-ref/            # Developer documentation

Getting Help
------------

- **Issues**: `GitHub Issues <https://github.com/UMEP-dev/SUEWS/issues>`_
- **Discussions**: `GitHub Discussions <https://github.com/UMEP-dev/UMEP/discussions>`_
- **Documentation**: `ReadTheDocs <https://suews.readthedocs.io>`_

.. _UMEP Community: https://github.com/UMEP-dev/UMEP/discussions