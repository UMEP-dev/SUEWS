# Fix: Meson NumPy Absolute Paths

## Context
When running `make dev` with Meson 1.8.2, the build fails with "ERROR: Tried to form an absolute path to a dir in the source tree." This occurs because Meson's `include_directories()` doesn't accept absolute paths, but NumPy's `get_include()` returns absolute paths.

## GitHub Issues
- Related to build system robustness
- Affects all platforms but enforcement varies

## Progress Tracking
- [x] Identify the issue in meson.build
- [x] Research standard solutions
- [x] Implement fix using compiler arguments
- [x] Test the fix locally
- [x] Create documentation
- [ ] Create PR from fix/meson-numpy-absolute-paths branch
- [ ] Test on different platforms (Windows, Linux)
- [ ] Verify with different NumPy versions

## Key Decisions
- Use compiler arguments (`-I` flags) instead of `include_directories()` for absolute paths
- Follow the pattern used by SciPy and other scientific Python projects
- Keep changes minimal and focused on the specific issue

## Implementation Notes
- Replace `include_directories()` calls with `numpy_inc_args = ['-I' + incdir_numpy, '-I' + incdir_f2py]`
- Use `c_args` in static_library and extension_module
- Use `compile_args` in declare_dependency
- This is a long-standing Meson restriction, not a new breaking change

## Files to Modify
- `src/supy_driver/meson.build` - Update NumPy include handling

## Current Status
- Fix implemented and tested in branch `fix/meson-numpy-absolute-paths`
- All tests pass with the fix
- Ready for PR creation