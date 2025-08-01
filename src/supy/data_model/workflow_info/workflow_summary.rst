.. _workflow_validation:


Model inputs check
===================

Purpose
-------

To ensure required model inputs are all present and physically sensible:
1. within appropriate ranges
1. consistent with other data submitted [Link: to section with order/logic checked] - e.g. assume date/latitude and longitude are correct - to determine initial state based on season etc
1. part of SUEWS_V2025


SUEWS_V2025
~~~~~~~~~~~
1. New input format uses  YAML  [link: to generic description website]. This has the advantage of being easier for the user to see input selected and to navigate between sections of inputs in a more logical manner [Link: manual overview of new format] and can be easily translated to JSON [link: to generic description website] which allows XXX.
1. There are some additional inputs: [link LIST new requirements]
1. Many model improvements [ link  list]
1. Many BUG corrections [link: list]


Workflow Summary
----------------

Prior to using the code you may need to:
1. Convert SUEWS namelist to YAML [link: instructions] - this applies to pre-SUEWS_v2025
2. Update SUEWS version = [link: instructions] - check you are running tha latest verison of SUEWS so the YAML file checks are consistent 

Within the CODE XXX - a series of steps occur  [link: instructions]:
1. A: YAML file consistency to the Standard_yaml version is checked  -. [link: overview]
1. B: Science check that parameters are  present and physcially reasonable [link: details] for science options chosend [Link manual]
1. C: Conditional validation using Pydantic
   
   
Namelist to YAML
================
1. Convert pre- SUEWS_V2025 input format [linK: manual reference of old format] to structured YAML format.


Background to Namelist to YAML conversion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Code used:
Developers:
Required inputs:
Outputs:

Instructions:

Steps 
~~~~~
.. note::

   MP code 




YAML checks
===========

Overview
--------

Within the CODE XXX - a series of steps occur  [link: instructions]:
1. A: YAML file consistency to the Standard_yaml version is checked  -[link: A  overview]
1. B: Science check that parameters are  present and physcially reasonable [link: details] for science options chosend [Link manual]
1. C: Conditional validation using Pydantic

Background
----------
Code Used:
Developers:
Required inputs:
1. files:
2. Code options to be run

Outputs
1. section A - 
 a. just  scrreen - there are no problems
 b. if files need  to be fixed  fo to [Action A]
3. Section B
4. Section C
   


How to run 
~~~~~~~~~~

- Example of run
- Interpreting the Output


Section A Overview
------------------

Standard-vyyyy-mm-dd.YML

e.g standard-v2025-07-16.yaml



What is checked in A how and why
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If code stop at the end of Section A - this is because the YAML file is missing some standard YAML parameters. [link:Actions for A]


Actions to fix A issues
~~~~~~~~~~~~~~~~~~~~~~


Section B: Overview
-------------------

The check are for 
1. Initial states -- exok
2. Grid characteristics
a. Land cover 
b. XXX

What is checked In B how and why
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 - assumptions -etc






Actions for fixing B issues
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Output: an updated YAML  saved as py0_<filename>.yml and a CSV report listing all changes.

.. note::

   The output will be changed to have a single file (the py0 updated yaml) with commented the parameters that have been updated by the precheck.




Section C: Overview
-------------------


Pydantic performs validation of a YAML file according to selected model options.

Output: An annotated YAML with inline error messages

.. note::

   The output will be changed to produce also an updated YAML file (py1_<filename>.yml) with comments at the level of the parameters that have been updated according to conditional validation. On top of that, the annotated YAML will be revised to work correctly.
 

What is checked in C how and why
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Actions to fix C issues
~~~~~~~~~~~~~~~~~~