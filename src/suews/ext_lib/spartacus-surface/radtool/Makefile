SOURCES = \
	radiation_constants.F90 \
	radtool_legendre_gauss.F90 \
	radtool_matrix.F90 \
	radtool_eigen_decomposition.F90 \
	radtool_calc_matrices_lw_eig.F90 \
	radtool_calc_matrices_sw_eig.F90 \
	radtool_schur.F90

OBJECTS := suews_ctrl_const.o $(SOURCES:.F90=.o)
LIBRADTOOL = ../lib/libradtool.a

all: $(LIBRADTOOL)

$(LIBRADTOOL): $(OBJECTS)
	ar r $(LIBRADTOOL) $(OBJECTS)

%.o: %.F90
	$(FC) $(FCFLAGS) -c $<

suews_ctrl_const.o: ../../../src/suews_ctrl_const.f95
	$(FC) $(FCFLAGS) $(CONST_FFLAGS) -c $< -o $@

clean:
	rm -f *.o $(LIBRADTOOL)

radiation_constants.o: suews_ctrl_const.o
radtool_legendre_gauss.o: radiation_constants.o
radtool_calc_matrices_sw_eig.o radtool_calc_matrices_lw_eig.o: \
	radtool_matrix.o radtool_eigen_decomposition.o radtool_schur.o
radtool_schur.o: radtool_matrix.o
