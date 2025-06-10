project(
    'supy',
    'fortran',
    'c',
    version: run_command('get_ver_git.py',check: true).stdout().strip(),
    meson_version: '>=0.64.0',
)

# Windows-specific configuration for single CRT usage
if host_machine.system() == 'windows'
  add_project_arguments('/nologo', language : ['c','cpp','fortran'])
  add_project_link_arguments('/nologo', language : ['c','cpp','fortran'])

  # Set CRT linking option for Windows builds
  if get_option('b_vscrt') == 'from_buildtype'
    add_project_arguments('-MD', language : ['c', 'cpp'])
    # Intel Fortran will follow the C runtime choice automatically
  endif
endif

# find the python installation
py = import('python').find_installation(pure: false)
dep_py = py.dependency()

subdir('src/supy_driver')

subdir('src/supy')
