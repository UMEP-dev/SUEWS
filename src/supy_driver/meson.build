# This is the `meson.build` file for building `supy_driver`, which relies on SUEWS - a Fortran-dominant project with some C code.

# project('supy_driver', 'fortran', 'c')

# building process:
# 1. build the SUEWS library using fortran compiler
# 2. use f90wrap to generate the modified fortran code with support for derived type
# 3. use f2py-f90wrap to generate the C file for the python interface
# 4. build the python extension module

##################################################################
# to fix newer windows (later than 2019) build
if host_machine.system() == 'windows'
  add_project_arguments(
    '-D__MINGW_USE_VC_SETJMP=1',
    language : ['c', 'cpp', 'fortran']
  )
endif

##################################################################
# set the compiler flags
fortran_compiler = meson.get_compiler('fortran')
if fortran_compiler.get_id() == 'gcc'
  f90flags = ['-fPIC']
  # Minimal flags for different build types
  if get_option('buildtype') == 'debug'
    f90flags += ['-g', '-fcheck=all', '-fconvert=big-endian']
  elif get_option('buildtype') == 'plain'
    # Fast build - no extra flags
  else
    f90flags += ['-fconvert=big-endian']
  endif
  fortran_program = find_program('gfortran')
elif fortran_compiler.get_id() == 'intel'
  f90flags = ['-fpscomp logicals', '-fPIC']
  fortran_program = find_program('ifort')
endif
add_global_arguments(f90flags, language: 'fortran')

c_compiler = meson.get_compiler('c')
cflags = ['-fPIC']
# Minimal flags for different build types  
if get_option('buildtype') == 'debug'
  cflags += ['-g', '-D_POSIX_C_SOURCE=200809L']
elif get_option('buildtype') == 'plain'
  # Fast build - minimal flags
else
  cflags += ['-D_POSIX_C_SOURCE=200809L']
endif
if c_compiler.get_id() == 'clang'
  cflags += ['-fbracket-depth=1024']
endif
add_global_arguments(cflags, language: 'c')

# flags for converting f95 to fpp
fpp_flags = ['-E', '-x', 'f95-cpp-input', '-fPIC']

# Use UCRT for everything when on Windows
if host_machine.system() == 'windows'
  # The UCRT64 msys2 environment implicitly handles linking to the UCRT.
  # Explicitly adding '-mcrtdll=ucrtbase' can fail with older GCC versions.
  # Note: We don't use -D__USE_MINGW_SETJMP_NON_SEH with UCRT64 as it supports SEH properly
endif
##################################################################

##################################################################
# include the SUEWS library
dir_suews = meson.current_source_dir() / '../suews'

include_dir_suews = include_directories('../suews/mod')

dir_src_suews = dir_suews / 'src'

src_f95_all = [
  'suews_ctrl_const.f95',
  'suews_ctrl_type.f95',
  'suews_util_stringmod.f95',
  'suews_ctrl_input.f95',
  'suews_ctrl_output.f95',
  'suews_util_time.f95',
  'suews_util_meteo.f95',
  'suews_phys_waterdist.f95',
  'suews_phys_narp.f95',
  'suews_phys_beers.f95',
  'suews_phys_atmmoiststab.f95',
  'suews_phys_resist.f95',
  'suews_phys_evap.f95',
  'suews_phys_snow.f95',
  'suews_phys_dailystate.f95',
  'suews_phys_lumps.f95',
  'suews_phys_anthro.f95',
  'suews_phys_rslprof.f95',
  'suews_phys_biogenco2.f95',
  'suews_phys_ohm.f95',
  # 'suews_phys_estm.f95',
  'suews_phys_spartacus.f95',
  'suews_phys_stebbs.f95',
  'suews_ctrl_driver.f95',
]

src_f95_files = []
foreach f : src_f95_all
  src_f95_files += join_paths(dir_src_suews, f)
endforeach

src_f95_files = files(src_f95_files)

src_f95_sub = [
  # 'suews_ctrl_const.f95',
  'suews_ctrl_type.f95',
  'suews_ctrl_driver.f95',
  'suews_phys_anthro.f95',
  'suews_phys_atmmoiststab.f95',
  'suews_phys_dailystate.f95',
  'suews_phys_evap.f95',
  'suews_phys_narp.f95',
  'suews_phys_resist.f95',
  'suews_phys_rslprof.f95',
  'suews_phys_spartacus.f95',
  #'suews_phys_stebbs.f95',
  'suews_phys_waterdist.f95',
  'suews_util_meteo.f95',
]
src_to_wrap = src_f95_sub

src_wrap_files = []
foreach f : src_to_wrap
  src_wrap_files += join_paths(dir_src_suews, f)
endforeach
##################################################################

##################################################################
# 1. build the SUEWS library using fortran compiler
# invoke make in the suews directory to build the objects
# Pass DEBUG variable only if buildtype is debug
make_env = []
if get_option('buildtype') != 'debug'
  make_env = ['env', 'DEBUG=']
endif
build_suews = custom_target(
  'suews_objects',
  command: make_env + ['make', '-C', meson.current_source_dir() / '../suews/', 'all'],
  build_by_default: true,
  output: 'suews_objects',
)

# build the static library for later use
build_lib=static_library(
  'src',
  [
    build_suews,
    src_f95_files,
  ],
  include_directories: include_dir_suews,
  build_by_default: true,
)

# run_command('bash', '-c', 'sleep 5 && ls -l')

##################################################################
# 2. use f90wrap to generate the modified fortran code with support for derived type
# Use the generator to process each source file
f95_to_fpp = generator(
  fortran_program,
  output: '@BASENAME@.fpp',
  arguments: [fpp_flags, '@INPUT@', '-o', '@OUTPUT@'],
)
gen_fpp_files = f95_to_fpp.process(src_wrap_files)

# construct the list of f95 files to be generated by f90wrap
out_wrap_f90 = []
# foreach f : src_f95
foreach f : src_to_wrap
  out_wrap_f90 += 'f90wrap_' + f.replace('.f95', '.f90')
endforeach
# out_wrap_f90 += 'f90wrap_toplevel.f90'

prog_f90wrap = find_program('f90wrap')
# file_kind_map = files('kind_map')

gen_f90wrap_f90 = custom_target(
  command: [
    py,
    '@CURRENT_SOURCE_DIR@/run_f90wrap.py', # Path to your Python wrapper script
    prog_f90wrap, # Assuming this is defined elsewhere as the f2py executable
    'supy_driver',
    '@CURRENT_SOURCE_DIR@',
    '@OUTDIR@',
    '@INPUT@',
    '@OUTPUT@',
  ],
  input: [gen_fpp_files],
  output: [out_wrap_f90],
  depends: build_suews,
  build_by_default: true,
)

##################################################################
# 3. use f2py-f90wrap to generate the C file for the python interface
# build the driver module c file
prog_f2py_f90wrap = find_program('f2py-f90wrap')
gen_module_c = custom_target(
  command: [
    py,
    '@CURRENT_SOURCE_DIR@/run_f2py.py', # Path to your Python wrapper script
    prog_f2py_f90wrap, # Assuming this is defined elsewhere as the f2py executable
    '_supy_driver',
    '@CURRENT_SOURCE_DIR@',
    '@OUTDIR@',
    '@INPUT@',
    '@OUTPUT@',
  ],
  input: [gen_f90wrap_f90],
  output: [
    '_supy_drivermodule.c',
    '_supy_driver-f2pywrappers.f',
  ],
  depends: [gen_f90wrap_f90],
  build_by_default: true,
)

##################################################################
# 4. build the python extension module

incdir_numpy = run_command(

  py,
  '-c', 'import os, numpy; print(numpy.get_include())',

  check: true,
).stdout().strip()

inc_np = include_directories(incdir_numpy)

# this is the path to the f2py source code - better compatibility with older numpy versions
incdir_f2py = incdir_numpy / '..' / '..' / 'f2py' / 'src'
inc_f2py = include_directories(incdir_f2py)
fortranobject_c = incdir_f2py / 'fortranobject.c'

inc_np = include_directories(incdir_numpy, incdir_f2py)

# gh-25000
dep_quadmath = fortran_compiler.find_library('quadmath', required: false)

# Share this object across multiple modules.
lib_fortranobject = static_library(
  '_fortranobject',
  fortranobject_c,
  dependencies: dep_py,
  include_directories: [inc_np, inc_f2py],
)
dep_fortranobject = declare_dependency(
  link_with: lib_fortranobject,
  include_directories: [inc_np, inc_f2py],
)

dep_lib_src = declare_dependency(
  link_args: [
    '-L' + meson.current_build_dir(), # the path is relative to the build directory when running meson
    '-lsrc',
  ],
)
dep_lib_suews = declare_dependency(
  link_args: [
    '-L' + dir_suews / 'lib', # the path is relative to the build directory when running meson
    '-lspartacus',
    '-lsuewsdriver',
  ],
)
dep_suews = [dep_lib_src, dep_lib_suews]

inc_suews_mod = include_directories('../suews/mod')

link_args_windows = []
c_args_windows = ['-D__MINGW_USE_VC_SETJMP=1']

if host_machine.system() == 'windows'
  if host_machine.cpu_family() == 'x86'
    # 32-bit MSVCRT build – link explicitly for _setjmp
    link_args_windows = ['-lmsvcrt']
  else
    # 64-bit: _setjmp is in the default CRT; add mingwex for safety
    link_args_windows = ['-lmingwex']
  endif
endif

ext_supy_driver = py.extension_module(
  '_supy_driver',
  sources: [
    gen_module_c,
    gen_f90wrap_f90,
    fortranobject_c,
  ],
  include_directories: [
    inc_np,
    inc_suews_mod, # the path is relative to the current directory where the meson.build file is located
  ],
  c_args: c_args_windows,
  dependencies: [
    dep_py,
    dep_fortranobject,
    dep_quadmath,
    dep_suews,
  ],
  link_args: link_args_windows,
  install: true,
  subdir: 'supy',
)

# move the generated `supy_driver.py` to the correct directory
# this is kind of dummy, but it is necessary to make the installation work
gen_supy_driver_py = custom_target(
  command: [
    py,
    '@CURRENT_SOURCE_DIR@/patch_supy_driver.py', # Path to your Python wrapper script
    # meson.current_build_dir() / '@OUTPUT@', # this file is actually generated by gen_f90wrap_f90
    meson.current_build_dir(),
    '@OUTPUT@',
    '@OUTDIR@',
  ],
  input: [gen_f90wrap_f90],
  output: ['supy_driver.py'],
  depends: ext_supy_driver,
  build_by_default: true,
  install: true,
  install_dir: [py.get_install_dir() / 'supy'], # this is the directory where the python module will be installed
)
##################################################################
# 5. add the test for the python module