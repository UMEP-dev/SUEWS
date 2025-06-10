.. _faq:

Troubleshooting
===============


General
-------

How to report an issue of this manual?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. only:: html
Please click the link in the top banner of each page to report page-specific issues.

.. only:: latex
Please submit your issue via `our GitHub page. <https://github.com/UMEP-dev/SUEWS/issues>`_

2. How to join your email-list?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Please join our email-list `here. <https://www.lists.reading.ac.uk/mailman/listinfo/met-suews>`_

3. How to create a directory?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Please search the web using this phrase if you do not know how to create a folder or directory

4. How to unzip a file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Please search the web using this phrase if you do not know how to unzip a file

.. _A_text_editor:

5. A text editor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A program to edit plain text files.
If you search on the web using the phrase ‘text editor’ you will find numerous programs.
These include for example, NotePad, EditPad, Text Pad etc

6. Command prompt
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
From Start select run –type cmd – this will open a window. Change directory to the location of where you stored your files.
The following website may be helpful if you do not know what a command prompt is: http://dosprompt.info/

7. Day of year [DOY]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
January 1st is day 1, February 1st is day 32. If you search on the web using the phrase ‘day of year calendar’ you will find tables that allow rapid conversions. Remember that after February 28th DOY will be different between leap years and non-leap years.

SUEWS related
-------------

ESTM output
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
First time steps of storage output could give NaN values during the initial converging phase.

First things to Check if the program seems to have problems
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-  Check the problems.txt file.
-  Check file options – in RunControl.nml.
-  Look in the output directory for the SS_FileChoices.txt. This allows you to check all options that were used in the run. You may want to compare it with the original version supplied with the model.
-  Note there can not be missing time steps in the data. If you need help with this you may want to checkout `UMEP`_

A pop-up saying “file path not found"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This means the program cannot find the file paths defined in RunControl.nml file. Possible solutions:
-  Check that you have created the folder that you specified in RunControl.nml.
-  Check does the output directory exist?
-  Check that you have a single or double quotes around the FileInputPath, FileOutputPath and FileCode

.. code-block:: language

    ====“%sat_vap_press.f temp=0.0000 pressure dectime”==== Temperature is zero in the calculation of water vapour pressure parameterization.

-  You don’t need to worry if the temperature should be (is) 0°C.
-  If it should not be 0°C this suggests that there is a problem with the data.

``%T`` changed to fit limits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[TL =0.1]/ [TL =39.9] You may want to change the coefficients for surface resistance.
If you have data from these temperatures, we would happily determine them.

%Iteration loop stopped for too stable conditions.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[zL]/[USTAR] This warning indicates that the atmospheric stability gets above 2.
In these conditions `MO theory <http://glossary.ametsoc.org/wiki/Monin-obukhov_similarity_theory>`__ is not necessarily valid.
The iteration loop to calculate the `Obukhov length <http://glossary.ametsoc.org/wiki/Obukhov_length>`__ and `friction velocity <http://glossary.ametsoc.org/wiki/Friction_velocity>`__ is stopped so that stability does not get too high values.
This is something you do not need to worry as it does not mean wrong input data.

“Reference to undefined variable, array element or function result”
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Parameter(s) missing from input files.
See also the error messages provided in problems.txt and warnings.txt

SuPy related
------------

I cannot install SuPy following the docs, what is wrong there?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Please check if your environment meets the following requirements:

1. Operating system (OS):

   1. is it 64 bit? only 64 bit systems are supported.

   2. is your OS up to date? only recent desktop systems are supported:

   - Windows 10 and above
   - macOS 10.13 and above
   - Linux: no restriction; If SuPy cannot run on your specific Linux distribution, please report it to us.

   You can get the OS information with the following code:

   .. code-block:: python

        import platform
        platform.platform()

3. Python interpreter:

   1. is your Python interpreter 64 bit?

      Check running mode with the following code:

      .. code-block:: python

          import struct
          struct.calcsize('P')*8

   2. is your Python version above 3.5?

      Check version info with the following code:

      .. code-block:: python

          import sys
          sys.version

If your environment doesn't meet the requirement by SuPy, please use a proper environment; otherwise, `please report your issue <https://github.com/UMEP-dev/SUEWS/issues/new/choose>`_.



How do I know which version of SuPy I am using?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use the following code:

.. code-block:: python

    import supy
    supy.show_version()

.. note:: `show_version` is only available after v2019.5.28.



A `kernel may have died` exception happened, where did I go wrong?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The issue is highly likely due to invalid input to SuPy and SUEWS kernel.
We are trying to avoid such exceptions,
but unfortunately they might happen in some edge cases.

Please `report such issues to us`__ with your input files for debugging.
Thanks!

__ GitHub Issues_


How can I upgrade SuPy to an up-to-date version?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Run the following code in your terminal:

.. code-block:: python

    python3 -m pip install supy --upgrade


How to deal with ``KeyError`` when trying to load initial model states or running SuPy (e.g. ``KeyError: 'sfr_surf'``)?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is usually due to the incompatibility between the input files and the model version.

Please check the following:
    - if you are using the `init_supy` to generate the initial model states from your input data, please make sure the file format is consistent with `the sample data shipped by SuPy <https://github.com/UMEP-dev/SUEWS/tree/master/src/supy/supy/sample_run>`_.
    - if you are using the `df_state` generated from a previous run, please double-check if your `df_state` has the same format as the sample `df_state` generated by `load_SampleData`.

A general rule of thumb is to use the `load_SampleData` to generate the initial model states from the sample data shipped by SuPy.


Build and Installation Issues
-----------------------------

Windows build fails with ``undefined reference to '_setjmpex'``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

**Problem**: When building SUEWS on Windows with MinGW/GCC, the build fails during the linking stage with errors like:

.. code-block:: text

    undefined reference to '_setjmpex'
    collect2.exe: error: ld returned 1 exit status

**Cause**: This error occurs because f2py-generated C code uses MSVC-specific functions like ``_setjmpex`` that are not available in MinGW. The f2py wrapper assumes a Microsoft Visual C++ environment but the build system uses MinGW/GCC.

**Solution**: This issue requires a two-part solution:

1. **Add Windows-specific compiler flags** in the project-level ``meson.build``:

.. code-block:: meson

    # Windows-specific configuration for f2py with MinGW
    if host_machine.system() == 'windows'
      # Get compilers
      cc = meson.get_compiler('c')
      fc = meson.get_compiler('fortran')
      
      # Windows 64-bit specific flags
      if host_machine.cpu_family() == 'x86_64'
        add_project_arguments(
          '-DMS_WIN64',
          '-D_hypot=hypot',
          language : ['c']
        )
      endif
      
      # MinGW-specific configuration
      if cc.get_id() == 'gcc'
        # Use static libraries to avoid DLL issues
        add_project_arguments(
          '-static-libgfortran',
          '-static-libgcc',
          language : ['fortran']
        )
        
        # Add Windows system libraries
        add_project_link_arguments(
          '-lmsvcrt',
          language : ['c', 'fortran']
        )
      endif
    endif

2. **Patch f2py-generated C files** to add setjmp compatibility on Windows x64. The build system automatically applies ``src/supy_driver/patch_setjmp_windows.py`` which adds compatibility definitions before the setjmp.h include to force use of MinGW's setjmp implementation.

This approach:
- Uses static linking to avoid runtime DLL dependencies
- Ensures proper headers are used for setjmp functionality on Windows x64
- Is the standard approach used by numpy and other scientific Python packages

**Note**: This issue is most commonly seen in CI environments using Windows Server 2025 or similar images with MinGW toolchains.


