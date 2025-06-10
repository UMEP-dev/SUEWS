project(
    'supy',
    'fortran',
    'c',
    version: run_command('get_ver_git.py',check: true).stdout().strip(),
    meson_version: '>=0.64.0',
)

# Windows-specific configuration to fix f2py _setjmpex issues with MinGW
if host_machine.system() == 'windows'
  add_project_arguments(
    '-DMS_WIN64',
    '-D_hypot=hypot',
    '-D__USE_MINGW_SETJMP_TWO_ARGS',
    language : ['c']
  )
  # Also need to ensure we're using the right runtime
  add_project_link_arguments(
    '-lmsvcrt',
    language : ['c', 'fortran']
  )
endif

# find the python installation
py = import('python').find_installation(pure: false)
dep_py = py.dependency()

subdir('src/supy_driver')

subdir('src/supy')
