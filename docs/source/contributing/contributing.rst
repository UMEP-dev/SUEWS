:doc-source: hand-written

.. _Development_Suggestions_Support:

Contributing Guide
====================================


.. note:: **Code development is handled by the SUEWS core team.** For detailed development documentation, see the `Developer Documentation <https://github.com/UMEP-dev/SUEWS/blob/master/dev-ref/README.md>`_ on GitHub.


Community Contributions
-----------------------

We welcome community contributions in the following areas:

- **Bug Reports**: Open an issue on `GitHub Issues <https://github.com/UMEP-dev/SUEWS/issues>`_
- **Documentation Improvements**: Click the "Suggest an edit" button at the top right of the page (the GitHub icon) and make your changes.
- **Feature Requests**: Discuss ideas in the `SUEWS Community <https://community.suews.io>`_

.. tip:: **Cannot open an issue?** Opening issues on GitHub is limited to existing contributors, so a first-time reporter may see a message that they cannot interact with the repository. Post the report in the `SUEWS Community <https://community.suews.io>`_ instead, and a maintainer will turn it into a GitHub issue. If you work on SUEWS regularly, ask there to be added as a contributor.

.. note:: If you are interested in contributing to the project, please start a discussion in the `SUEWS Community <https://community.suews.io>`_ to share your ideas.

Adding documentation pages
--------------------------

If a new page restates fields, defaults, variables or options held in the code,
extend an existing documentation generator or add one instead of maintaining a
second copy by hand. The generator must write these fields at the top of every
RST file it emits, using its own repository-relative path:

.. code-block:: rst

   :doc-source: generated
   :doc-generator: docs/generate_datamodel_rst.py

If the page explains or instructs, write it by hand and put
``:doc-source: hand-written`` at the top. Keep these file-wide fields together
with any ``:orphan:`` field, before the title, targets or other content, and leave
a blank line after the field list. Included RST fragments also need a label.
Generated regions within a hand-written page need no extra label: their
``autodoc``, ``autosummary``, bibliography or table directive identifies the source.

Sphinx-tool output paths are declared once in ``docs/source/conf.py`` through
``doc_source_tool_outputs``. The build checks all RST source files and fails if
a label is missing or invalid, or a named project generator does not exist.
Before building, run the same check without Sphinx from the repository root:

.. code-block:: bash

   python scripts/lint/check_doc_sources.py

Building SUEWS Locally
----------------------

Building SUEWS locally allows you to:

- Test modifications to the source code
- Experiment with new features or algorithms
- Debug issues in your specific environment
- Contribute patches and improvements back to the project

See the `Building SUEWS Locally guide <https://github.com/UMEP-dev/SUEWS/blob/master/dev-ref/building-locally.md>`_ in the developer docs.

Getting Help
------------

- **Issues**: `GitHub Issues <https://github.com/UMEP-dev/SUEWS/issues>`_
- **Discussions**: `SUEWS Community <https://community.suews.io>`_
- **Documentation**: `ReadTheDocs <https://docs.suews.io>`_

