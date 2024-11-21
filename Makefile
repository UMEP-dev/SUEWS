# SUEWS Makefile - read the README file before editing

.PHONY: main clean test pip supy docs dev

# OS-specific configurations
ifeq ($(OS),Windows_NT)
	PYTHON_exe = python.exe
	# F2PY_PY= /c/Users/sunt05/Anaconda2/Scripts/f2py.py
	# F2PY_EXE = $(PYTHON) $(F2PY_PY)
	TARGET=$(MODULE).pyd
else
	UNAME_S := $(shell uname -s)
	TARGET=$(MODULE).so

	ifeq ($(UNAME_S),Linux) # Linux
		PYTHON_exe=python
		# F2PY_EXE = f2py
	endif

	ifeq ($(UNAME_S),Darwin) # macOS
		PYTHON_exe=python
		# F2PY_EXE = f2py
	endif

endif

MODULE=SUEWS_driver

suews_dir = src/suews

docs_dir = docs

test_dir= test

release_dir = Release

makefile = Makefile.gfortran

supy_dir = src/supy

PYTHON := $(if $(PYTHON_exe),$(PYTHON_exe),python)

all: test

# set up the development environment
# use `-p [x.y]` to specify python version: e.g. `-p 3.12`
env:
	uv venv .venv -p 3.11

# install dependencies
reqs:
	uv pip compile requirements.in -o requirements.txt
	uv pip install -r requirements.txt

# make suews driver library
suews:
	$(MAKE) -C $(suews_dir) libdriver; # make SUEWS library
	# -rm -rf *.o *.mod *.f95 *.a *.dSYM

# make supy and install locally
dev:
	$(PYTHON) -m pip install --no-build-isolation --editable .

# make supy dist and test
test:
	$(PYTHON) -m pytest test

# make supy wheels using cibuild
wheel:
	$(PYTHON) -m pip wheel --no-deps . -w wheelhouse

# documentation
docs:
	$(MAKE) -B -C $(docs_dir) html

# live html documentation
livehtml:
	$(MAKE) -B -C $(docs_dir) livehtml

# If wanted, clean all *.o files after build
clean:
	$(MAKE) -C $(suews_dir) clean || true
	$(MAKE) -C $(supy_dir) clean || true
	$(MAKE) -C $(docs_dir) clean || true
	rm -rf build dist *.egg-info
	rm -rf ./ext_lib/spartacus-surface//*/*.mod

# this is to test cibuildwheel locally
cibw:
	CIBW_BUILD=cp312-macosx* \
	CIBW_ARCH=arm64 \
	CIBW_TEST_REQUIRES=pytest \
	CIBW_TEST_COMMAND="python -m pytest '{project}/test'" \
	pipx run cibuildwheel==2.16.5 --platform macos