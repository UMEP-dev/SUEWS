#!/usr/bin/env python
"""
Bundle Windows runtime DLLs with the SUEWS extension module.

This script ensures that the required MinGW-w64 runtime DLLs are copied
to the same directory as the built _supy_driver extension module.
This is necessary for the extension to load properly on Windows.
"""
import os
import sys
import shutil
import platform
import sysconfig
from pathlib import Path


def find_extension_module():
    """Find the location of the built _supy_driver extension module."""
    # For editable installs, the extension is built in the build directory
    # Check common locations
    possible_locations = [
        # Build directory patterns (for meson/meson-python)
        Path.cwd() / "build",
        Path.cwd() / "_build",
        Path.cwd() / "build-install", 
        Path.cwd() / "builddir",
        # Meson-python specific paths
        Path.cwd() / "build" / "cp*",  # This will be expanded below
        Path.cwd() / "_skbuild",
        # Direct in source
        Path.cwd(),
        Path.cwd() / "src" / "supy",
    ]
    
    # Expand any paths with wildcards
    expanded_locations = []
    for loc in possible_locations:
        if '*' in str(loc):
            # Handle pattern matching
            parent = loc.parent
            pattern = loc.name
            if parent.exists():
                expanded_locations.extend(parent.glob(pattern))
        else:
            expanded_locations.append(loc)
    
    for base_path in expanded_locations:
        if not base_path.exists():
            continue
            
        # Search for the .pyd file
        for pyd_file in base_path.rglob("_supy_driver*.pyd"):
            return pyd_file
    
    # If not found in build dirs, check if it's in the package directory
    try:
        import supy
        supy_dir = Path(supy.__file__).parent
        for pyd_file in supy_dir.glob("_supy_driver*.pyd"):
            return pyd_file
    except ImportError:
        pass
    
    # Also check site-packages directly
    try:
        import site
        for site_dir in site.getsitepackages():
            site_path = Path(site_dir)
            if site_path.exists():
                # Check in supy subdirectory
                supy_path = site_path / "supy"
                if supy_path.exists():
                    for pyd_file in supy_path.glob("_supy_driver*.pyd"):
                        return pyd_file
    except:
        pass
    
    return None


def find_mingw_dlls():
    """Find the MinGW-w64 runtime DLLs."""
    required_dlls = [
        "libgcc_s_seh-1.dll",
        "libgfortran-5.dll", 
        "libquadmath-0.dll",
        "libwinpthread-1.dll"
    ]
    
    # Common MinGW-w64 locations
    mingw_paths = [
        Path("C:/msys64/ucrt64/bin"),
        Path("C:/msys64/mingw64/bin"),
        Path("C:/mingw64/bin"),
        Path("C:/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/bin"),
    ]
    
    # Also check PATH
    if "PATH" in os.environ:
        for path in os.environ["PATH"].split(os.pathsep):
            if "mingw" in path.lower() or "msys" in path.lower():
                mingw_paths.append(Path(path))
    
    found_dlls = {}
    for dll_name in required_dlls:
        for mingw_path in mingw_paths:
            dll_path = mingw_path / dll_name
            if dll_path.exists():
                found_dlls[dll_name] = dll_path
                break
    
    return found_dlls


def bundle_dlls():
    """Bundle the DLLs with the extension module."""
    if platform.system() != "Windows":
        print("Not on Windows, skipping DLL bundling")
        # Create stamp file for meson
        stamp_file = Path.cwd() / "bundle_dlls.stamp"
        stamp_file.write_text("Skipped on non-Windows platform\n")
        return 0
    
    # Find the extension module
    ext_path = find_extension_module()
    if not ext_path:
        print("ERROR: Could not find _supy_driver extension module")
        return 1
    
    print(f"Found extension module at: {ext_path}")
    ext_dir = ext_path.parent
    
    # Find the MinGW DLLs
    dlls = find_mingw_dlls()
    if len(dlls) < 4:
        print("ERROR: Could not find all required MinGW DLLs")
        print(f"Found: {list(dlls.keys())}")
        missing = set(["libgcc_s_seh-1.dll", "libgfortran-5.dll", 
                      "libquadmath-0.dll", "libwinpthread-1.dll"]) - set(dlls.keys())
        print(f"Missing: {missing}")
        return 1
    
    # Copy DLLs to extension directory
    print(f"Copying DLLs to: {ext_dir}")
    for dll_name, dll_path in dlls.items():
        dest_path = ext_dir / dll_name
        if not dest_path.exists():
            print(f"  Copying {dll_name}")
            shutil.copy2(dll_path, dest_path)
        else:
            print(f"  {dll_name} already exists")
    
    # Create stamp file for meson
    stamp_file = Path.cwd() / "bundle_dlls.stamp"
    stamp_file.write_text(f"DLLs bundled to {ext_dir}\n")
    
    print("DLL bundling complete")
    return 0


if __name__ == "__main__":
    sys.exit(bundle_dlls())