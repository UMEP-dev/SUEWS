"""Check Make expansion safety, not platform compilation or static linkage."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest

import pytest

pytestmark = [
    pytest.mark.tooling,
    pytest.mark.skipif(
        os.name == "nt",
        reason="These Linux workflow regressions require POSIX shell executable fixtures",
    ),
    pytest.mark.contract("C-TOOLING"),
]


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
                "  *) exit 1 ;;\nesac\n",
                encoding="utf-8",
            )
            compiler.chmod(0o755)
            makefile = root / "Makefile"
            makefile.write_text(
                f"include {fragment}\nexport LIBS = $(STATICLIB)\n"
                'probe:\n\t@printf "%s\\n" "$$LIBS"\n',
                encoding="utf-8",
            )
            result = subprocess.run(
                [
                    "make",
                    "--no-print-directory",
                    "-f",
                    str(makefile),
                    "probe",
                    "OS=",
                    "UNAME_S=Darwin",
                    "UNAME_m=x86_64",
                    f"FC={compiler}",
                ],
                env=os.environ,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout.strip(), f"{archive} -Wl,-dead_strip_dylibs")
            self.assertEqual(
                dylib.read_text(encoding="utf-8"), "compiler-owned sentinel"
            )

    def test_init_real_defaults_to_zero_and_allows_snan_override(self):
        fragment = Path(__file__).resolve().parents[2] / "src/suews/Makefile.gfortran"
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            makefile = root / "Makefile"
            makefile.write_text(
                f'include {fragment}\nprobe:\n\t@printf "%s\\n" "$(DEBUGFLAGS)"\n',
                encoding="utf-8",
            )
            base_cmd = [
                "make",
                "--no-print-directory",
                "-f",
                str(makefile),
                "probe",
                "OS=",
                "UNAME_S=Linux",
            ]
            clean_env = {k: v for k, v in os.environ.items() if k != "SUEWS_INIT_REAL"}

            default_res = subprocess.run(
                base_cmd, env=clean_env, capture_output=True, text=True, check=False
            )
            self.assertEqual(default_res.returncode, 0, default_res.stderr)
            self.assertIn("-finit-real=zero", default_res.stdout)
            self.assertNotIn("-finit-real=snan", default_res.stdout)
            self.assertNotIn("-ffpe-trap", default_res.stdout)

            snan_res = subprocess.run(
                [*base_cmd, "SUEWS_INIT_REAL=snan"],
                env=clean_env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(snan_res.returncode, 0, snan_res.stderr)
            self.assertIn("-finit-real=snan", snan_res.stdout)
            self.assertNotIn("-finit-real=zero", snan_res.stdout)
            self.assertNotIn("-ffpe-trap", snan_res.stdout)

            for bad_value in ("bogus", "zero snan", "snan zero", ""):
                bad_res = subprocess.run(
                    [*base_cmd, f"SUEWS_INIT_REAL={bad_value}"],
                    env=clean_env,
                    capture_output=True,
                    text=True,
                    check=False,
                )
                self.assertNotEqual(
                    bad_res.returncode, 0, msg=f"accepted {bad_value!r}"
                )
                self.assertIn(
                    "SUEWS_INIT_REAL must be 'zero' or 'snan'", bad_res.stderr
                )
