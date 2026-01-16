#!/usr/bin/env python

#  f90wrap: F90 to Python interface generator with derived type support
#
#  Copyright James Kermode 2011-2018
#
#  This file is part of f90wrap
#  For the latest version see github.com/jameskermode/f90wrap
#
#  f90wrap is free software: you can redistribute it and/or modify
#  it under the terms of the GNU Lesser General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  f90wrap is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public License
#  along with f90wrap. If not, see <http://www.gnu.org/licenses/>.
#
#  If you would like to license the source code under different terms,
#  please contact James Kermode, james.kermode@gmail.com

"""
This script patches :mod:`numpy.f2py` at runtime, to customise the C code that
is generated. We make several changes to f2py:

  1. Allow the Fortran :c:func:`present` function to work correctly with optional arguments.
     If an argument to an f2py wrapped function is optional and is not given, replace it
     with ``NULL``.

  2. Allow Fortran routines to raise a :exc:`RuntimeError` exception
     with a message by calling an external function
     :c:func:`f90wrap_abort`. This is implemented using a
     :c:func:`setjmp`/ :c:func:`longjmp` trap.

SUEWS VENDORING NOTE (GH-1035)
------------------------------
This file is vendored from f90wrap with the following modification:

- REMOVED: SIGINT handler installation/restoration around Fortran calls.
  The original code called signal(SIGINT, ...) before/after every Fortran
  routine, which conflicts with Qt's signal handlers on Windows, causing
  QGIS to crash. The setjmp/longjmp mechanism for f90wrap_abort() is
  preserved for error handling.

- TRADE-OFF: Users cannot Ctrl+C interrupt long-running Fortran computations,
  but Qt/QGIS applications remain stable.

Original upstream: github.com/jameskermode/f90wrap
Vendored version base: f90wrap 0.2.x (circa 2024)

"""

from __future__ import print_function
import sys
from numpy.f2py.auxfuncs import *

# __all__ = []

def main():

    import numpy
    numpy_version = tuple([int(x) for x in numpy.__version__.split('.')[0:2]])
    if not numpy_version >= (1,13):
       raise ImportError('f2py-f90wrap tested with numpy version 1.13 or later, found version %s' % numpy.__version__)


    import numpy.f2py.auxfuncs

    import numpy.f2py.capi_maps
    numpy.f2py.capi_maps.cformat_map['long_long'] = '%Ld'

    import numpy.f2py.rules, numpy.f2py.cb_rules

    includes_inject = "#includes0#\n"

    includes_inject = includes_inject + """

    /* custom abort handler - James Kermode <james.kermode@gmail.com> */
    """
    if(sys.platform == 'win32'):
       includes_inject = includes_inject + "#include <signal.h> // fix https://github.com/jameskermode/f90wrap/issues/73 \n#include <setjmpex.h> // fix https://github.com/jameskermode/f90wrap/issues/96\n"
    else:
       includes_inject = includes_inject + "#include <signal.h>\n#include <setjmp.h>\n"

    includes_inject = includes_inject + """
    #define ABORT_BUFFER_SIZE 1024
    extern jmp_buf environment_buffer;
    extern char abort_message[ABORT_BUFFER_SIZE];
    void f90wrap_abort_(char *message, int len);
    void f90wrap_abort_int_handler(int signum);
    void suews_stop_string(const char *message, int len);
    void suews_stop_numeric(int code);
    void _gfortran_stop_string(char *message, int len);
    void _gfortran_stop_numeric(int code);
    void for_stop_core(char *message, int len);

    #include <stdlib.h>
    #include <string.h>

    jmp_buf environment_buffer;
    char abort_message[ABORT_BUFFER_SIZE];

    void f90wrap_abort_(char *message, int len_message)
    {
      strncpy(abort_message, message, ABORT_BUFFER_SIZE);
      abort_message[ABORT_BUFFER_SIZE-1] = '\\0';
      longjmp(environment_buffer, 0);
    }

    // copy of f90wrap_abort_ with a second underscore
    // void (*f90wrap_abort__)(char *, int) = &f90wrap_abort_;
    void f90wrap_abort__(char *message, int len_message)
    {
      strncpy(abort_message, message, ABORT_BUFFER_SIZE);
      abort_message[ABORT_BUFFER_SIZE-1] = '\\0';
      longjmp(environment_buffer, 0);
    }

    /* Fortran STOP interception (GH-1035):
       Convert STOP to a Python RuntimeError via longjmp. */
    void suews_stop_string(const char *message, int len_message)
    {
      int n = len_message;
      if (message == NULL || n <= 0) {
        strncpy(abort_message, "Fortran STOP", ABORT_BUFFER_SIZE);
        abort_message[ABORT_BUFFER_SIZE-1] = '\\0';
      } else {
        if (n >= ABORT_BUFFER_SIZE) n = ABORT_BUFFER_SIZE - 1;
        memcpy(abort_message, message, n);
        abort_message[n] = '\\0';
      }
      longjmp(environment_buffer, 1);
    }

    void suews_stop_numeric(int code)
    {
      (void)code; /* suppress unused warning */
      strncpy(abort_message, "Fortran STOP (numeric)", ABORT_BUFFER_SIZE);
      abort_message[ABORT_BUFFER_SIZE-1] = '\\0';
      longjmp(environment_buffer, 1);
    }

    /* GNU Fortran STOP hooks */
    void _gfortran_stop_string(char *message, int len_message)
    {
      suews_stop_string(message, len_message);
    }

    void _gfortran_stop_numeric(int code)
    {
      suews_stop_numeric(code);
    }

    /* Intel Fortran STOP hook */
    void for_stop_core(char *message, int len_message)
    {
      suews_stop_string(message, len_message);
    }


    void f90wrap_abort_int_handler(int signum)
    {
      char message[] = "Interrupt occured";
      f90wrap_abort_(message, strlen(message));
    }

    /* end of custom abort handler  */
    """

    numpy.f2py.rules.module_rules['modulebody'] = numpy.f2py.rules.module_rules['modulebody'].replace("#includes0#\n", includes_inject)

    numpy.f2py.rules.routine_rules['body'] = numpy.f2py.rules.routine_rules['body'].replace("volatile int f2py_success = 1;\n", """volatile int f2py_success = 1;
        int setjmpvalue; /* James Kermode - for setjmp */
    """)

    # Qt-safe version: SIGINT handler removed for QGIS compatibility (GH-1035)
    # Original code modified SIGINT handler before/after every Fortran call,
    # which conflicts with Qt's signal handlers on Windows, causing QGIS crashes.
    # The setjmp/longjmp mechanism for f90wrap_abort() is preserved.
    numpy.f2py.rules.routine_rules['body'] = numpy.f2py.rules.routine_rules['body'].replace('#callfortranroutine#\n', """/* setjmp() exception handling - Qt-safe version (SIGINT handler removed) */
    setjmpvalue = setjmp(environment_buffer);
    if (setjmpvalue != 0) {
      PyErr_SetString(PyExc_RuntimeError, abort_message);
    } else {
     #callfortranroutine#
    }
    /* End addition */
    """)


    numpy.f2py.auxfuncs.options['persistant_callbacks'] = True

    # Disable callback argument cleanup so that callbacks can be called after function returns.
    # This will lead to a small memory leak every time a function with callback arguments is called.
    def persistant_callbacks(var):
       return True

    numpy.f2py.cb_rules.cb_routine_rules['body'] = numpy.f2py.cb_rules.cb_routine_rules['body'].replace('capi_longjmp_ok = 1', 'capi_longjmp_ok = 0')

    numpy.f2py.rules.arg_rules[7]['cleanupfrompyobj'] = {l_not(persistant_callbacks): numpy.f2py.rules.arg_rules[7]['cleanupfrompyobj'],
                                                         persistant_callbacks: '}'}

    numpy.f2py.rules.arg_rules[8]['callfortran'] = {isintent_c:'#varname#,',
                                                    l_and(isoptional,l_not(isintent_c)):'#varname#_capi == Py_None ? NULL : &#varname#,',
                                                    l_and(l_not(isoptional),l_not(isintent_c)):'&#varname#,'}


    numpy.f2py.rules.arg_rules[14]['callfortran'] = {isintent_c:'#varname#,',l_and(isoptional,l_not(isintent_c)):'#varname#_capi == Py_None ? NULL : &#varname#,',
                                                     l_and(l_not(isoptional),l_not(isintent_c)):'&#varname#,'}

    numpy.f2py.rules.arg_rules[21]['callfortran'] = {isintent_out:'#varname#,', l_and(isoptional, l_not(isintent_out)):'#varname#_capi == Py_None ? NULL : #varname#,',
                                                     l_and(l_not(isoptional), l_not(isintent_out)): '#varname#,'}


    numpy.f2py.rules.arg_rules[26]['callfortran'] = {isintent_out:'#varname#,', l_and(isoptional, l_not(isintent_out)):'#varname#_capi == Py_None ? NULL : #varname#,',
                                                     l_and(l_not(isoptional), l_not(isintent_out)): '#varname#,'}

    if numpy_version < (1,24):
        numpy.f2py.rules.arg_rules[33]['frompyobj'].insert(2, {isoptional: 'if (#varname#_capi != Py_None) {'})
        numpy.f2py.rules.arg_rules[33]['frompyobj'].insert(5, {isoptional: '}'})

        del numpy.f2py.rules.arg_rules[33]['frompyobj'][6]
        numpy.f2py.rules.arg_rules[33]['frompyobj'].insert(6, {l_not(isoptional): \
        "\n\t\tif (capi_#varname#_tmp == NULL) {"
        "\n\t\t\tif (!PyErr_Occurred())"
        "\n\t\t\t\tPyErr_SetString(#modulename#_error,\"failed in converting #nth# `#varname#\' of #pyname# to C/Fortran array\" );"
        "\n\t\t} else {"
        "\n\t\t\t#varname# = (#ctype# *)(capi_#varname#_tmp->data);"
        })

        numpy.f2py.rules.arg_rules[33]['frompyobj'].insert(7, {isoptional:\
        "\n\t\tif (#varname#_capi != Py_None && capi_#varname#_tmp == NULL) {"
        "\n\t\t\tif (!PyErr_Occurred())"
        "\n\t\t\t\tPyErr_SetString(#modulename#_error,\"failed in converting #nth# `#varname#\' of #pyname# to C/Fortran array\" );"
        "\n\t\t} else {"
        "\n\t\t\tif (#varname#_capi != Py_None) #varname# = (#ctype# *)(capi_#varname#_tmp->data);"
        })

    else:
        numpy.f2py.rules.arg_rules[33]['frompyobj'].insert(3, {isoptional: '\tif (#varname#_capi != Py_None) {'})
        numpy.f2py.rules.arg_rules[33]['frompyobj'].insert(6, {isoptional: '\t}'})

        del numpy.f2py.rules.arg_rules[33]['frompyobj'][7]
        numpy.f2py.rules.arg_rules[33]['frompyobj'].insert(7, {l_not(isoptional):\
        "\n\t\tif (capi_#varname#_as_array == NULL) {"
        "\n\t\t\tPyObject* capi_err = PyErr_Occurred();"
        "\n\t\t\tif (capi_err == NULL) {"
        "\n\t\t\t\tcapi_err = #modulename#_error;"
        "\n\t\t\t\tPyErr_SetString(capi_err, capi_errmess);"
        "\n\t\t\t}"
        "\n\t\t} else {"
        "\n\t\t\t#varname# = (#ctype# *)(PyArray_DATA(capi_#varname#_as_array));"
        })

        del numpy.f2py.rules.arg_rules[33]['frompyobj'][8]
        numpy.f2py.rules.arg_rules[33]['frompyobj'].insert(8, {isstringarray:\
        "\t\tif (#varname#_capi != Py_None) slen(#varname#) = f2py_itemsize(#varname#);"
        })

        numpy.f2py.rules.arg_rules[33]['frompyobj'].insert(9, {isoptional:\
        "\n\t\tif (#varname#_capi != Py_None && capi_#varname#_as_array == NULL) {"
        "\n\t\t\tPyObject* capi_err = PyErr_Occurred();"
        "\n\t\t\tif (capi_err == NULL) {"
        "\n\t\t\t\tcapi_err = #modulename#_error;"
        "\n\t\t\t\tPyErr_SetString(capi_err, capi_errmess);"
        "\n\t\t\t}"
        "\n\t\t} else {"
        "\n\t\t\tif (#varname#_capi != Py_None) #varname# = (#ctype# *)(PyArray_DATA(capi_#varname#_as_array));"
        })

    # now call the main function
    print('\n!! f90wrap patched version of f2py - James Kermode <james.kermode@gmail.com> !!\n')
    numpy.f2py.main()


if __name__ == "__main__":
    sys.exit(main())
