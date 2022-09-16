# -*- makefile -*-
.PHONY: main clean test pip supy

FC = gfortran $(FFLAGS)

# OS-specific configurations
OSFLAG 	:= ""
ifeq ($(OS),Windows_NT)
	OSFLAG += -D WIN32
	# pyver := 3.6
	# PYTHON:= py -${pyver}
	PYTHON:=python
	FC = gfortran $(FFLAGS)
	ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
		OSFLAG += -D AMD64
	endif
	ifeq ($(PROCESSOR_ARCHITECTURE),x86)
		OSFLAG += -D IA32
	endif
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Linux)
		OSFLAG += -D LINUX
		# pyver := 37
		# PYTHON:=/opt/python/cp${pyver}-cp${pyver}m/bin/python
		PYTHON:=python
	endif
	ifeq ($(UNAME_S),Darwin)
		OSFLAG += -D OSX
		PYTHON:=python
	endif
		UNAME_P := $(shell uname -p)
	ifeq ($(UNAME_P),x86_64)
		OSFLAG += -D AMD64
	endif
		ifneq ($(filter %86,$(UNAME_P)),)
	OSFLAG += -D IA32
		endif
	ifneq ($(filter arm%,$(UNAME_P)),)
		OSFLAG += -D ARM
	endif
endif

PYTHON_ver:= $(shell $(PYTHON) --version)


MODULE = supy_driver

SUEWS_dir = ../SUEWS-SourceCode

# SuPy_dir = supy-driver

# PYTHON := $(if $(PYTHON_exe),$(PYTHON_exe),python)

# $(info $$UNAME_S is ${UNAME_S})
# make suews_driver
main:
	# $(MAKE) show
	$(PYTHON) -m pip install -r requirements.txt
	$(PYTHON) -m build --wheel


# make fortran exe
suews:
	$(MAKE) -C $(SUEWS_dir) suews; # make SUEWS with the `suews` recipe

show:
	@echo $(info $$OSFLAG is [${OSFLAG}])
	@echo $(info $$PYTHON is ${PYTHON_ver})


# If wanted, clean all *.o files after build
clean:
	$(MAKE) show
	rm -rf ${MODULE}/*.so ${MODULE}/*.pyd ${MODULE}/*.pyc ${MODULE}/__pycache__ ${MODULE}/*.dSYM
	 -$(PYTHON) setup.py clean --all
	rm -rf build *.mod *.o __pycache__
	rm -rf *.f95

# clean all existing builds, rebuild f2py libs, build wheels and test
test: main
	$(PYTHON) -m pip install -e .
	$(PYTHON) -m pytest -s ${MODULE}/test

# clean all existing builds, rebuild f2py libs, build wheels and install in dev mode
dev:
	$(MAKE) show
	$(MAKE) clean
	$(MAKE) suews
	pip install -e .

# upload wheels to pypi using twine
upload:
	$(MAKE) show
	pip install twine
	twine upload --skip-existing wheelhouse/*whl
	# $(shell twinup wheelhouse/*whl)

# only local testing on mac
cibuid:
	pipx run cibuildwheel==2.9.0 --platform macos
	# pipx run cibuildwheel==2.9.0 --platform macos --config-file pyproject-localtest.toml
