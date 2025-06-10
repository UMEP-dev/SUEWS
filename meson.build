project(
    'supy',
    'fortran',
    'c',
    version: run_command('get_ver_git.py',check: true).stdout().strip(),
    meson_version: '>=0.64.0',
)

if host_machine.system() == 'windows'
  add_project_arguments('-D__USE_MINGW_SETJMP', language : 'c')   # keep setjmp, not _setjmpex
  add_project_arguments('-mcrtdll=ucrtbase', language : ['c','fortran'])
  add_project_link_arguments('-mcrtdll=ucrtbase', language : ['c','fortran'])
endif

# find the python installation
py = import('python').find_installation(pure: false)
dep_py = py.dependency()

subdir('src/supy_driver')

subdir('src/supy')
