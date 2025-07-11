from pathlib import Path
import platform


sysname = platform.system()
lib_basename = "supy_driver"
if sysname == "Windows":
    lib_name = lib_basename + ".pyd"
    Path("setup.cfg").write_text(
        "[build_ext]\ncompiler=mingw32\n[build]\ncompiler=mingw32\n"
    )
    print("setup.cfg created")
    print(list(Path.cwd().glob("*")))
elif sysname == "Darwin":
    lib_name = lib_basename + ".so"
    print("setup.cfg is not necessary for Mac")
elif sysname == "Linux":
    lib_name = lib_basename + ".so"
    print("setup.cfg is not necessary for Linux")
