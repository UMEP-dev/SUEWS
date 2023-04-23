#=======================================================================
#                   define the compiler names
#=======================================================================

CC       = gcc
F90      = gfortran
#F90      = ifort
#F90      =  /opt/intel/composer_xe_2015.3.187/bin/intel64/ifort
PYTHON   = python


#=======================================================================
#                     additional flags
#=======================================================================

ifeq ($(F90),gfortran)
	FPP      = gfortran -E
	FPP_F90FLAGS = -x f95-cpp-input -fPIC
	F90FLAGS = -fPIC -J../suews/mod -fconvert=big-endian
    FCOMP    = gfortran
    LIBS     =
endif

ifeq ($(F90),ifort)

	FPP      = gfortran -E # gfortran f90wrap temp files only. not compilation
	FPP_F90FLAGS = -x f95-cpp-input -fPIC
	F90FLAGS = -fpscomp logicals -fPIC # use 1 and 0 for True and False
    FCOMP    = intelem # for f2py
    LIBS =
endif

CFLAGS = -fPIC #     ==> universal for ifort, gfortran, pgi

#=======================================================================
#=======================================================================

UNAME = $(shell uname)
ifeq (${UNAME}, Darwin)
  LIBTOOL = libtool -static -o
  CFLAGS += -fbracket-depth=1024 -D_POSIX_C_SOURCE=200809L
  CFLAGS += $(ARCH_FLAGS)
else
  LIBTOOL = ar src
endif

# ARCH := $(shell uname -m)
# ifeq ($(ARCH), arm64)
#     ARCH_FLAGS = -arch arm64
# 	CFLAGS += $(ARCH_FLAGS)
# 	F90FLAGS += $(ARCH_FLAGS)
# else ifeq ($(ARCH), x86_64)
#     ARCH_FLAGS = -arch x86_64
# endif


# ======================================================================
# PROJECT CONFIG, do not put spaced behind the variables
# ======================================================================
# Python module name
PYTHON_MODN = supy_driver
# mapping between Fortran and C types
KIND_MAP = kind_map

suffix_code = .f95
#=======================================================================
#
#=======================================================================

#VPATH	=

#=======================================================================
#       List all source files required for the project
#=======================================================================

# names (without suffix), f90 sources
LIBSRC_SOURCES = suews_ctrl_const \
	suews_util_stringmod \
	suews_ctrl_input \
	suews_ctrl_output \
	suews_util_time \
	suews_util_meteo \
	suews_phys_waterdist \
	suews_phys_narp \
	suews_phys_beers \
	suews_phys_atmmoiststab \
	suews_phys_resist \
	suews_phys_evap \
	suews_phys_snow \
	suews_phys_dailystate \
	suews_phys_lumps \
	suews_phys_anemsn \
	suews_phys_rslprof \
	suews_phys_biogenco2 \
	suews_phys_ohm \
	suews_phys_estm \
	suews_phys_spartacus \
	suews_ctrl_driver
	# suews_util_datetime \

# suews source directory
SRC_DIR = ../suews/src

# file names
LIBSRC_FILES = $(addsuffix .f95,${LIBSRC_SOURCES})

# object files
LIBSRC_OBJECTS = $(addsuffix .o,${LIBSRC_SOURCES})

# only used when cleaning up
LIBSRC_FPP_FILES = $(addsuffix .fpp,${LIBSRC_SOURCES})

#=======================================================================
#       List all source files that require a Python interface
#=======================================================================

# names (without suffix), f90 sources
LIBSRC_WRAP_SOURCES = suews_ctrl_driver \
	suews_phys_narp \
	suews_phys_atmmoiststab \
	suews_phys_waterdist \
	suews_phys_resist \
	suews_phys_evap \
	suews_phys_dailystate \
	suews_phys_anemsn \
	suews_phys_rslprof \
	suews_util_meteo \
	suews_phys_spartacus \

# file names
LIBSRC_WRAP_FILES = $(addsuffix .f95,${LIBSRC_WRAP_SOURCES})

# object files
LIBSRC_WRAP_OBJECTS = $(addsuffix .o,${LIBSRC_WRAP_SOURCES})

# fpp files
LIBSRC_WRAP_FPP_FILES = $(addsuffix .fpp,${LIBSRC_WRAP_SOURCES})

#=======================================================================
#                 Relevant suffixes
#=======================================================================

.SUFFIXES: .f95 .fpp

#=======================================================================
#
#=======================================================================

.PHONY: all clean cfg


all: _${PYTHON_MODN} cfg
# all: _${PYTHON_MODN}.so _${PYTHON_MODN}_pkg.so

cfg: gen-setup_cfg.py
	${PYTHON} gen-setup_cfg.py


clean:
	-rm -f ${LIBSRC_OBJECTS} ${LIBSRC_FPP_FILES} libsrc.a _${PYTHON_MODN}*.* \
	*.mod *.fpp f90wrap*.f95 f90wrap*.o *.o ${PYTHON_MODN}.py
	-rm -rf _${PYTHON_MODN}*.*
	-rm -rf src.*/ .f2py_f2cmap .libs/ __pycache__/
	-rm  *f95 f90wrap*.f90
	-rm *.log

%.o: %$(suffix_code)
	@echo $^
	$(F90) $(F90FLAGS) -c $^ -o $@


# Generate a list of corresponding soft link names
LINKS := $(notdir $(LIBSRC_FILES))

# Rule to create the soft links
links: $(LINKS)

$(LINKS):
	ln -s $(SRC_DIR)/$@ $@

# suews_phys_spartacus.o: suews_phys_spartacus.f95 libspartacus
# 	${F90} ${F90FLAGS} -c $< -o $@

%.o: %.c cfg
	${CC} ${CFLAGS} -c $< -o $@
	rm $<


%.fpp: %.f95
	${FPP} ${FPP_F90FLAGS} $<  -o $@
	rm $<


libsrc.a: ${LIBSRC_OBJECTS}
	${LIBTOOL} $@ $?


_${PYTHON_MODN}: libsrc.a ${LIBSRC_FPP_FILES}
	${PYTHON} gen-setup_cfg.py
	f90wrap -m ${PYTHON_MODN} ${LIBSRC_WRAP_FPP_FILES} -k ${KIND_MAP} -v \
	--skip error_hint
	$(CC) --version
	CC="$(CC) $(CFLAGS)" \
	${PYTHON} f2py-f90wrap-patched.py \
	--fcompiler=$(FCOMP) \
	--f90flags="-cpp -I../suews/mod" \
	--build-dir . -c -m _${PYTHON_MODN} -L. -lsrc f90wrap*.f90 \
	-L../suews/lib -lspartacus -lsuewsdriver


test: all
	@echo ""
	@echo "=============================="
	@echo "testing ${PYTHON_MODN}..."
	@${PYTHON} -c "import ${PYTHON_MODN}"
	@echo "done."
	@echo "${PYTHON_MODN} is ready to use."
	@echo "=============================="
