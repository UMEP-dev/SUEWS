# SPARTACUS-Surface Makefile - read the README file before editing

#############################
### --- CONFIGURATION --- ###
#############################

# Use the nf-config utility, if available, to set the NETCDF_INCLUDE
# and NETCDF_LIB flags
HAVE_NFCONFIG := $(shell nf-config --version 2> /dev/null)
ifdef HAVE_NFCONFIG
$(info *** Using nf-config to obtain NetCDF flags)
NETCDF_INCLUDE = $(shell nf-config --fflags)
NETCDF_LIB     = $(shell nf-config --flibs)
ifeq ($(shell nf-config --has-nc4),yes)
NETCDF4        = 1
endif
else
$(info *** nf-config not found)
endif

# make can be invoked using "make PROFILE=<prof>" in which case your
# local configuration parameters will be obtained from
# Makefile_include.<prof>
ifndef PROFILE
$(info *** No "PROFILE" variable provided, assuming "gfortran")
PROFILE = gfortran
endif

# Include a platform-specific makefile that defines FC, FCFLAGS and
# LIBS
include	Makefile_include.$(PROFILE)

# Check for presence of the NETCDF_INCLUDE and NETCDF_LIB flags
ifndef NETCDF_INCLUDE
$(info *** You may need to set NETCDF_INCLUDE manually)
endif
ifndef NETCDF_LIB
$(info *** You may need to set NETCDF_LIB manually)
endif

# Add single-precision flag if SINGLE_PRECISION=1 was given on the
# "make" command line
ifdef SINGLE_PRECISION
CPPFLAGS += -DSINGLE_PRECISION
endif

# Allow the capability to write NetCDF4/HDF5 files, provided the code
# is compiled against the NetCDF4 library
ifdef NETCDF4
$(info *** Building with NetCDF4/HDF5 support)
CPPFLAGS += -DNC_NETCDF4
endif

# Consolidate flags
export FC
export CONST_FFLAGS
export FCFLAGS = $(WARNFLAGS) $(BASICFLAGS) $(CPPFLAGS) \
	$(OPTFLAGS) $(DEBUGFLAGS) $(NETCDF_INCLUDE) $(OMPFLAG)
export LIBS    = $(DEBUGFLAGS) $(LDFLAGS) -L../lib -lradsurf -lradtool -lutilities \
	$(FCLIBS) $(NETCDF_LIB) $(OMPFLAG)

#############################
### --- BUILD TARGETS --- ###
#############################

all: build

help:
	@echo "Usage:"
	@echo "  make PROFILE=<prof>"
	@echo "where <prof> is one of gfortran, pgi etc."

build: libutilities libradtool libradsurf driver

libutilities:
	cd utilities && $(MAKE)

libradtool: libutilities
	cd radtool && $(MAKE)

libradsurf: libradtool
	cd radsurf && $(MAKE)

driver: libradsurf
	cd driver && $(MAKE)

test: test_simple test_rami4pilps test_urban

test_simple:
	cd test/simple && $(MAKE) test

test_rami4pilps:
	cd test/rami4pilps && $(MAKE) test

test_urban:
	cd test/urban && $(MAKE) test

test_urban_single:
	cd test/urban && $(MAKE) test_single

test_code:
	cd driver && $(MAKE) test_code

clean: clean-tests clean-toplevel clean-utilities clean-mods clean-doc

dist-clean: clean-tests clean-toplevel clean-utilities clean-mods dist-clean-doc

clean-tests:
	cd test/simple && $(MAKE) clean
	cd test/rami4pilps && $(MAKE) clean
	cd test/urban && $(MAKE) clean

clean-toplevel:
	cd radsurf && $(MAKE) clean
	cd driver && $(MAKE) clean

clean-utilities:
	cd radtool && $(MAKE) clean
	cd utilities && $(MAKE) clean

clean-mods:
	rm -f mod/*.mod

clean-doc:
	cd doc && $(MAKE) clean

dist-clean-doc:
	cd doc && $(MAKE) dist-clean

clean-autosaves:
	rm -f *~ */*~ */*/*~

.PHONY: libradsurf libradtool libutilities driver clean clean-toplevel test
