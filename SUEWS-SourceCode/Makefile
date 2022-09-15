# SUEWS Makefile - read the README file before editing
# part of this is inspired by SPARTACUS-Surface: https://github.com/ecmwf/spartacus-surface
##################################
###     -- USEFUL PATHS --     ###
##################################

# where SUEWS source code is stored
DIR_SRC_SUEWS = ./src/

# where compiled libraries are stored
DIR_LIB_SUEWS = ./lib/

# where intermediate `.mod` are stored
DIR_MOD_SUEWS = ./mod/

# where to test built version
DIR_BIN_SUEWS = ./bin/

# where to store built binaries for release
DIR_RELEASE_BASE_SUEWS = ../Release/bin/

# system specific settings
ifeq ($(OS),Windows_NT)
	DIR_RELEASE_SUEWS = $(addprefix $(DIR_RELEASE_BASE_SUEWS), win64)
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Linux) # Linux
		DIR_RELEASE_SUEWS = $(addprefix $(DIR_RELEASE_BASE_SUEWS), Linux)
	endif
	ifeq ($(UNAME_S),Darwin) # macOS
		UNAME_m := $(shell uname -m)
		ifeq ($(UNAME_m),arm64) # arm64
			DIR_RELEASE_SUEWS = $(addprefix $(DIR_RELEASE_BASE_SUEWS), macOS_arm64)
		else # x64
			DIR_RELEASE_SUEWS = $(addprefix $(DIR_RELEASE_BASE_SUEWS), macOS_x64)
		endif
	endif
endif

# enable this if we want to use the debug version of SUEWS
DEBUG=1

LIBUTILS = $(addprefix $(DIR_LIB_SUEWS),libsuewsutil.a)
LIBPHYS = $(addprefix $(DIR_LIB_SUEWS),libsuewsphys.a)
LIBDRIVER = $(addprefix $(DIR_LIB_SUEWS),libsuewsdriver.a)



# version specific target name:
# determined from source code
path       := $(DIR_SRC_SUEWS)suews_ctrl_const.f95 # get program name form source code
file :=  $(if $(filter $(OS),Windows_NT),$(strip $(subst /,\,${path})),${path})
# $(info file is $(file))
grep := $(if $(filter $(OS),Windows_NT),findstr,grep)
variable   := $(shell $(grep) progname ${file})
TARGET_str := $(lastword $(subst =, ,${variable})) # program name with quotes
TARGET     := $(strip $(subst $\', ,${TARGET_str})) # program name
VERSION    := $(lastword $(subst _V, ,${TARGET}))
GIT_COMMIT := $(shell git describe --always --dirty=-dirty || echo unknown:non-git)

timestamp := $(shell date +"%Y%m%dT%H%M")
EXECUTABLE := $(strip $(TARGET))_$(strip $(timestamp))

# where to test built version
SUEWS_DIR_TEST := ../Test/BaseRun/$(VERSION)


#############################
### --- CONFIGURATION --- ###
#############################

# Use the nf-config utility, if available, to set the NETCDF_INCLUDE
# and NETCDF_LIB flags
HAVE_NFCONFIG := $(shell nf-config --version)
ifdef HAVE_NFCONFIG
$(info *** Using nf-config to obtain NetCDF flags)
NETCDF_INCLUDE=-I$(shell nf-config --fflags)
NETCDF_LIB=$(shell nf-config --flibs) -lnetcdff
ifeq ($(shell nf-config --has-nc4),yes)
NETCDF4        = 1
endif
else
$(info *** nf-config not found)
endif

ifdef DEBUG
$(info *** DEBUG mode is ON)
else
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
include	Makefile.$(PROFILE)

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
export SUEWS_VER = $(GIT_COMMIT)
export FC
export FC_VER := `$(FC) --version | head -n 1`
export FCFLAGS = $(WARNFLAGS) $(BASICFLAGS) $(CPPFLAGS) \
	$(STATIC)	\
	$(OPTFLAGS) $(DEBUGFLAGS)
	# $(NETCDF_INCLUDE) $(OMPFLAG)
export LIBS    = $(DEBUGFLAGS) $(LDFLAGS) \
	-L$(DIR_LIB_SUEWS) -lsuewsdriver -lspartacus \
	$(STATICLIB)
	# $(FCLIBS) $(NETCDF_LIB) $(OMPFLAG)





####################################
###  -- OTHER CONFIGURATIONS --  ###
####################################

# options for `clean`
clean: suffix :=
clean: TARGET := $(strip $(TARGET))$(strip $(suffix))

# WRF coupling (SuMin) related options
sumin: FFLAGS += -Dwrf=1


###################################
###  -- SOURCE CODE GROUPS  --  ###
###################################


# All the files which include modules used by other modules (these therefore
# needs to be compiled first)
# NB: DO KEEP ALL FILE NAMES in lowercase for compatability across platforms
# utilities
UTILS = suews_ctrl_const.o \
	suews_util_stringmod.o \
	suews_util_qsort.o\
	suews_util_time.o \
	suews_util_meteo.o \
	suews_util_datetime.o \
	suews_util_minpack.o

# SUEWS physics modules
PHYS =	suews_phys_narp.o \
	suews_phys_spartacus.o \
	suews_phys_atmmoiststab.o \
	suews_phys_resist.o \
	suews_ctrl_input.o  \
	suews_phys_bluews.o \
	suews_phys_waterdist.o \
	suews_phys_evap.o \
	suews_phys_snow.o \
	suews_phys_dailystate.o \
	suews_phys_estm.o \
	suews_ctrl_ver.o \
	suews_ctrl_output.o \
	suews_phys_anohm.o \
	suews_phys_lumps.o \
	suews_phys_anemsn.o \
    suews_phys_rslprof.o \
	suews_phys_biogenco2.o \
	suews_phys_ohm.o \
	suews_phys_solweig.o \
	suews_phys_beers.o

# SUEWS driver module (wrapper of all main calculations)
DRIVER = suews_ctrl_driver.o

# Rest of the files including modules and functions which are independent
OTHERS =  suews_ctrl_translate.o \
	suews_ctrl_error.o \
	suews_ctrl_init.o \
	suews_ctrl_calculations.o

# modules under rapid development
TEST =

# modules for WRF coupling
WRF =  suews_ctrl_sumin.o

#############################
### --- EXTERNAL LIBS --- ###
#############################
# SPARTACUS
DIR_SPTCS = ./ext_lib/spartacus-surface/


# DIR_SRC_UTIL_SPTCS := $(foreach dir, utilities radtool, $(addprefix $(DIR_SPTCS), $(dir)/))
DIR_SRC_UTIL_SPTCS := $(addprefix $(DIR_SPTCS),utilities/)
FILE_SRC_UTIL_SPTCS := $(patsubst %.F90,%.o,\
		$(addprefix $(DIR_SRC_UTIL_SPTCS),\
		parkind1.F90 \
		radiation_io.F90 \
		print_matrix.F90 \
		yomhook.F90 \
		)\
	)
# $(info $(FILE_SRC_UTIL_SPTCS))

DIR_SRC_RADTOOL_SPTCS := $(addprefix $(DIR_SPTCS),radtool/)
FILE_SRC_RADTOOL_SPTCS := $(patsubst %.F90,%.o,\
		$(addprefix $(DIR_SRC_RADTOOL_SPTCS),\
		radiation_constants.F90 \
		radtool_legendre_gauss.F90 \
		radtool_matrix.F90 \
		radtool_schur.F90 \
		radtool_eigen_decomposition.F90 \
		radtool_calc_matrices_lw_eig.F90 \
		radtool_calc_matrices_sw_eig.F90 \
		)\
	)
# $(info $(FILE_SRC_RADTOOL_SPTCS))

DIR_SRC_RAD_SPTCS := $(addprefix $(DIR_SPTCS),radsurf/)
FILE_SRC_RAD_SPTCS := $(patsubst %.F90,%.o,\
		$(addprefix $(DIR_SRC_RAD_SPTCS),\
		radsurf_config.F90 \
		radsurf_canopy_properties.F90 \
		radsurf_boundary_conds_out.F90 \
		radsurf_canopy_flux.F90 \
		radsurf_sw_spectral_properties.F90 \
		radsurf_lw_spectral_properties.F90\
		radsurf_overlap.F90 \
		radsurf_forest_sw.F90 \
		radsurf_forest_lw.F90 \
		radsurf_urban_sw.F90 \
		radsurf_urban_lw.F90 \
		radsurf_simple_spectrum.F90 \
		radsurf_interface.F90 \
		)\
	)
# $(info $(FILE_SRC_RAD_SPTCS))

LIBSPTCS = libspartacus.a


#############################
### --- BUILD TARGETS --- ###
#############################

all: libutil libphys suews

help:
	@echo ${HAVE_NFCONFIG} ${NETCDF_INCLUDE} ${NETCDF_LIB}
	@echo "Usage:"
	@echo "  make PROFILE=<prof>"
	@echo "where <prof> can be either 'gfortran' or 'ifort'."
	@echo $(C_MODULE)
	@echo $(SUEWS_VER)
	@echo $(FC_VER)

libspartacus: $(FILE_SRC_UTIL_SPTCS) $(FILE_SRC_RADTOOL_SPTCS) $(FILE_SRC_RAD_SPTCS)
	mkdir -p $(DIR_LIB_SUEWS)
	ar r lib/$(LIBSPTCS) $^

# rules for F90-->f95: SPARTACUS suffix .F90
%.f95: %.F90
	-rm -rf $*.f95
	-ln -sf "$(abspath $*.F90)" "$(abspath $*.f95)"

libutil: $(addprefix $(DIR_SRC_SUEWS), $(C_MODULE) $(UTILS))
	@echo "Compiling SUEWS utilities..."
	@echo $(C_MODULE)
	mkdir -p $(DIR_LIB_SUEWS)
	ar r $(LIBUTILS) $^

libphys: $(addprefix $(DIR_SRC_SUEWS), $(C_MODULE) $(UTILS) $(PHYS))
	@echo "Compiling SUEWS physics modules..."
	mkdir -p $(DIR_LIB_SUEWS)
	ar r $(LIBPHYS) $^

libdriver: $(addprefix $(DIR_SRC_SUEWS), $(C_MODULE) $(UTILS) $(PHYS) $(DRIVER))
	@echo "Compiling SUEWS driver module..."
	mkdir -p $(DIR_LIB_SUEWS)
	ar r $(LIBDRIVER) $^


suews: libspartacus libdriver exe

# special rule for generating version info file
src/suews_ctrl_ver.f95: force
	@echo "generating version info file..."
	@echo "MODULE version" > $@
	@echo "IMPLICIT NONE" >> $@
	@echo "CHARACTER(len=90) :: git_commit = '$(GIT_COMMIT)' " >> $@
	@echo "CHARACTER(len=90) :: compiler_ver = '$(FC_VER)' " >> $@
	@echo "END MODULE version" >> $@


exe: $(addprefix $(DIR_SRC_SUEWS), $(OTHERS) suews_program$(suffix_code))
	@echo  $(LIBUTILS) $(LIBPHYS) $(LIBDRIVER)
	@echo $(TARGET) $(EXECUTABLE) $(OTHERS)
	@echo Building SUEWS ...
	$(FC) $(FCFLAGS) $^ -o $(DIR_BIN_SUEWS)/$(EXECUTABLE) $(LIBS)
	@echo SUEWS built.
	@echo $(TARGET) has been saved in $(DIR_BIN_SUEWS)
	cd $(DIR_BIN_SUEWS) && ln -sf $(EXECUTABLE) $(TARGET)
	rm -rf $(DIR_RELEASE_BASE_SUEWS)
	mkdir -p $(DIR_RELEASE_SUEWS) && cp -f $(DIR_BIN_SUEWS)/$(EXECUTABLE) $(DIR_RELEASE_SUEWS)/$(TARGET)

# special rule for the spartacus wrapper
src/suews_phys_spartacus.o: src/suews_phys_spartacus.f95 libspartacus
	$(FC) $(FCFLAGS) -c $< -o $@

# rules for f95-->f90
%.o: %$(suffix_code)
	@echo $^
	$(FC) $(FCFLAGS) -c $^ -o $@

clean: clean-obj clean-mods clean-lib clean-bin clean-ext

# dist-clean: clean-tests clean-toplevel clean-utilities clean-mods dist-clean-doc

clean-obj:
	find . -name "*.o" -type f -delete


clean-lib:
	find . -name "lib*.a" -type f -delete

clean-mods:
	find . -name "*.mod" -type f -delete

clean-bin:
	rm -rf $(DIR_BIN_SUEWS)/SUEWS_*

clean-ext:
	$(MAKE) -C $(DIR_SPTCS) clean
	rm -rf $(DIR_SPTCS)/*/*.mod



$(C_MODULE): $(subst .o,.c, $(C_MODULE))
	$(info $^)
	$(CC) -c $^ -o $@



# If OTHERS have changed, compile them again
$(OTHERS): $(UTILS) $(PHYS) $(subst .o,$(suffix_code), $(OTHERS))

# If TEST have changed, compile them again
$(TEST): $(subst .o,$(suffix_code), $(TEST))

# WRF coupling required files
$(WRF): $(subst .o,$(suffix_code), $(WRF))



.PHONY: libutil libphys libdriver exe clean-toplevel test force