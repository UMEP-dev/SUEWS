Small reader journey
====================

Install the package.

.. journey: fixture/install

.. code-block:: bash

   pip install supy

The accumulator reaches six and the consumer prints it.

.. journey: fixture/python

.. code-block:: python

   import warnings
   total = 0
   for value in (1, 2, 3):
       total += value
   warnings.warn("fixture warning", UserWarning)
   print(total)
