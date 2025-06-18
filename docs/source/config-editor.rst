.. _config-editor:

SUEWS Configuration Editor
=========================

Interactive Editor
----------------

Use the editor below to create and validate your SUEWS configuration file:

.. suews-config-editor::

   This editor is powered by React and uses the official SUEWS configuration schema.

Using the Generated Configuration
-------------------------------

After creating your configuration:

1. Click the "Copy to Clipboard" button to copy the YAML
2. Save it to a file (e.g., ``config-suews.yml``)
3. Use it with SUEWS::

    import supy as sp
    config = sp.SUEWSConfig.from_yaml('config-suews.yml')

Schema Documentation
------------------

The editor uses the official SUEWS configuration schema, which includes:

* Required fields and their types
* Field descriptions and validation rules
* Default values where applicable

For more details about specific configuration options, see :ref:`configuration-reference`.