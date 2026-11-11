"""Check Make expansion safety, not platform compilation or static linkage."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


class MakefileTests(unittest.TestCase):
    def test_intel_mac_expansion_preserves_compiler_library(self):
        fragment = Path(__file__).resolve().parents[2] / "src/suews/Makefile.gfortran"
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            dylib = root / "libquadmath.dylib"
            dylib.write_text("compiler-owned sentinel", encoding="utf-8")
            archive = root / "libquadmath.a"
            archive.touch()
            compiler = root / "mock-gfortran"
            compiler.write_text(
                '#!/bin/sh\ncase "$1" in\n'
                f'  -print-file-name=libquadmath.dylib) echo "{dylib}" ;;\n'
                f'  -print-file-name=libquadmath.a) echo "{archive}" ;;\n'
                '  *) exit 1 ;;\nesac\n', encoding="utf-8")
            compiler.chmod(0o755)
            makefile = root / "Makefile"
            makefile.write_text(
                f'include {fragment}\nexport LIBS = $(STATICLIB)\n'
                'probe:\n\t@printf "%s\\n" "$$LIBS"\n', encoding="utf-8")
            result = subprocess.run(
                ["make", "--no-print-directory", "-f", str(makefile), "probe",
                 "OS=", "UNAME_S=Darwin", "UNAME_m=x86_64", f"FC={compiler}"],
                env=os.environ, capture_output=True, text=True)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout.strip(), str(archive))
            self.assertEqual(dylib.read_text(encoding="utf-8"), "compiler-owned sentinel")
