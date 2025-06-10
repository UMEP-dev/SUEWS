project(
    'supy',
    'fortran',
    'c',
    version: run_command('get_ver_git.py',check: true).stdout().strip(),
    meson_version: '>=0.64.0',
)

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

# find the python installation
py = import('python').find_installation(pure: false)
dep_py = py.dependency()

subdir('src/supy_driver')

subdir('src/supy')
