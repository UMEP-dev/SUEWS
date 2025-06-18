project(
    'supy',
    'fortran',
    'c',
    version: run_command('get_ver_git.py',check: true).stdout().strip(),
    meson_version: '>=0.64.0',
)

if host_machine.system() == 'windows'
  # Link against the modern UCRT
  # Note: We don't use -D__USE_MINGW_SETJMP_NON_SEH with UCRT64 as it supports SEH properly
  # Note: recent MinGW-w64 GCC dropped support for the compile-time
  # "-mcrtdll=..." switch.  We therefore omit it here and rely on
  # LDFLAGS (set in the CI workflow) to pull in the UCRT at link time.
  # add_project_arguments('-mcrtdll=ucrtbase', language : ['c','fortran'])
  add_project_link_arguments('-lucrt', language : ['c','fortran'])

  # Extra safety knobs cribbed from SciPy's build system
  cc = meson.get_compiler('c')
  if cc.get_id() == 'gcc'
    # Force gcc to use 64-bit long double (matches MSVC behaviour)
    add_project_arguments('-mlong-double-64', language : 'c')
    # Make fprintf("%zd") work without CRT mismatch warnings
    add_project_arguments('-D__USE_MINGW_ANSI_STDIO=1', language : 'c')
    # For gfortran + BLAS string-length quirk on Windows
    add_project_arguments('-fno-optimize-sibling-calls', language : 'fortran')
    # Statically link the universal CRT to avoid mixed CRTs at runtime
    add_project_link_arguments('-lucrt', language : ['c','fortran'])
  endif
elif host_machine.system() == 'darwin'
  # macOS-specific configuration for conda gfortran linking issues
  add_project_link_arguments([
    '-L/usr/lib', 
    '-L/System/Library/Frameworks'
  ], language : ['c','fortran'])
endif

# find the python installation
py = import('python').find_installation(pure: false)
dep_py = py.dependency()

subdir('src/supy_driver')

subdir('src/supy')
