# meson.build for supy_driver — Fortran static library for the Rust bridge.
#
# The f90wrap/f2py pipeline has been removed. This file now only:
# 1. Sets compiler flags
# 2. Builds the SUEWS Fortran sources into a static library
# The Rust bridge (src/suews_bridge) links against this library via Cargo.

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

# Check for fast build environment variable
fast_build = get_option('fast_build')
if fast_build
  message('Using fast build configuration')
endif

if fortran_compiler.get_id() == 'gcc'
  if fast_build
    f90flags = [
      '-fPIC',
      '-fconvert=big-endian',
      '-ffree-line-length-none',  # Allow unlimited line length
      '-g',
      '-O1',  # Faster than -O0
      '-Wno-missing-include-dirs',
    ]
  else
    f90flags = [
      '-fPIC',
      '-fconvert=big-endian',
      '-ffree-line-length-none',  # Allow unlimited line length
      '-g',
      '-O0',
      '-fcheck=all',
      '-Wno-missing-include-dirs',
    ]
  endif
  fortran_program = find_program('gfortran')
elif fortran_compiler.get_id() == 'intel'
  f90flags = ['-fpscomp logicals', '-fPIC']
  fortran_program = find_program('ifort')
endif
add_global_arguments(f90flags, language: 'fortran')

# flags for converting f95 to fpp
fpp_flags = ['-E', '-x', 'f95-cpp-input', '-fPIC']

# Use UCRT for everything when on Windows
if host_machine.system() == 'windows'
  # The UCRT64 msys2 environment implicitly handles linking to the UCRT.
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
  'suews_util_profile.f95',
  'suews_util_time.f95',
  'suews_util_meteo.f95',
  'suews_util_datetime.f95',
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
##################################################################

##################################################################
# Build the SUEWS library using fortran compiler
# Invoke make in the suews directory to build the objects
makefile_deps = files(dir_suews / 'Makefile')

build_suews = custom_target(
  'suews_objects',
  command: [
    py,
    '@CURRENT_SOURCE_DIR@/run_make.py',
    meson.current_source_dir() / '../suews/',
    '@OUTPUT@',
  ],
  build_by_default: true,
  depend_files: [src_f95_files, makefile_deps],
  output: 'suews_objects',
)

# Build the static library for the Rust bridge to link against
build_lib = static_library(
  'src',
  [
    build_suews,
    src_f95_files,
  ],
  include_directories: include_dir_suews,
  build_by_default: true,
)
##################################################################
