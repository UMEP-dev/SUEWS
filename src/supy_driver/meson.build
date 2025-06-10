# This is the `meson.build` file for building `supy_driver`, which relies on SUEWS - a Fortran-dominant project with some C code.

# project('supy_driver', 'fortran', 'c')

# building process:
# 1. build the SUEWS library using fortran compiler
# 2. use f90wrap to generate the modified fortran code with support for derived type
# 3. use f2py-f90wrap to generate the C file for the python interface
# 4. build the python extension module

##################################################################
# set the compiler flags
fortran_compiler = meson.get_compiler('fortran')

if fortran_compiler.get_id() == 'gcc'
  f90flags = [
    '-fPIC',
    '-fconvert=big-endian',
    '-g',
    '-O0',
    '-fcheck=all',
  ]
  fortran_program = find_program('gfortran')
elif fortran_compiler.get_id() == 'intel'
  f90flags = ['-fpscomp logicals', '-fPIC']
  fortran_program = find_program('ifort')
endif

# Windows-specific flags for MinGW
if host_machine.system() == 'windows'
  if fortran_compiler.get_id() == 'gcc'
    # Add Windows-specific flags for MinGW
    f90flags += ['-static-libgfortran', '-static-libgcc']
    # Remove -fPIC as it's not needed for Windows DLLs with MinGW
    f90flags = []
    foreach flag : ['-fconvert=big-endian', '-g', '-O0', '-fcheck=all', '-static-libgfortran', '-static-libgcc']
      f90flags += flag
    endforeach
  endif
endif

add_global_arguments(f90flags, language: 'fortran')

c_compiler = meson.get_compiler('c')
cflags = [
  '-fPIC',
  '-D_POSIX_C_SOURCE=200809L',
  '-g',
  '-O0',
]

# Windows-specific C flags
if host_machine.system() == 'windows'
  # Remove POSIX flag and adjust PIC for Windows
  cflags = [
    '-g',
    '-O0',
  ]
  
  # Add Windows-specific defines
  if c_compiler.get_id() == 'gcc'
    # MinGW doesn't need -fPIC for Windows DLLs
    # The setjmp/setjmpex issue is handled by patch_setjmp_windows.py
  elif c_compiler.get_id() == 'msvc'
    # MSVC doesn't use -fPIC
    cflags = ['/Od', '/Z7']  # Debug flags for MSVC
  endif
endif

if c_compiler.get_id() == 'clang'
  cflags += ['-fbracket-depth=1024']
endif
add_global_arguments(cflags, language: 'c')

# flags for converting f95 to fpp
fpp_flags = ['-E', '-x', 'f95-cpp-input']
if host_machine.system() != 'windows'
  fpp_flags += ['-fPIC']
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
build_suews = custom_target(
  'suews_objects',
  command: ['make', '-C', meson.current_source_dir() / '../suews/'],
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

# On Windows x64, patch the generated C file to fix setjmp issues
if host_machine.system() == 'windows' and host_machine.cpu_family() == 'x86_64'
  gen_module_c_patched = custom_target(
    'patch_supy_drivermodule',
    command: [
      py,
      '@CURRENT_SOURCE_DIR@/patch_setjmp_windows.py',
      '@INPUT@',
      '@OUTPUT@',
    ],
    input: gen_module_c[0],  # _supy_drivermodule.c
    output: '_supy_drivermodule_patched.c',
    depends: gen_module_c,
    build_by_default: true,
  )
  # Use the patched C file for building
  module_c_source = gen_module_c_patched
else
  # Use the original C file on other platforms
  module_c_source = gen_module_c[0]
endif

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
# On Windows with MinGW, quadmath might not be available
if host_machine.system() == 'windows'
  dep_quadmath = fortran_compiler.find_library('quadmath', required: false)
else
  dep_quadmath = fortran_compiler.find_library('quadmath', required: true)
endif

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

# Prepare link arguments for Windows
link_args = []
if host_machine.system() == 'windows'
  if c_compiler.get_id() == 'gcc'
    # Add static linking for MinGW runtime libraries to avoid DLL dependencies
    link_args += ['-static-libgcc', '-static-libgfortran']
    # Add Windows system libraries that might be needed for setjmp
    link_args += ['-lmsvcrt']
  endif
endif

# Build dependencies list
ext_dependencies = [
  dep_py,
  dep_fortranobject,
  dep_suews,
]
# Only add quadmath if it was found
if dep_quadmath.found()
  ext_dependencies += dep_quadmath
endif

ext_supy_driver = py.extension_module(
  '_supy_driver',
  sources: [
    module_c_source,  # Use the potentially patched C source
    gen_module_c[1],  # The Fortran wrapper file
    gen_f90wrap_f90,
    fortranobject_c,
  ],
  include_directories: [
    inc_np,
    inc_suews_mod, # the path is relative to the current directory where the meson.build file is located
  ],
  dependencies: ext_dependencies,
  link_args: link_args,
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