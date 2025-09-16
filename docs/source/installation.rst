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



Development Versions
--------------------

For testing pre-release versions from test.pypi.org, see :doc:`installation-dev`.

.. warning::

   Development versions are unstable and not recommended for production use or research publications.

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


