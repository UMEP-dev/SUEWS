SOURCES = parkind1.F90 easy_netcdf.F90 radiation_io.F90 print_matrix.F90 yomhook.F90

OBJECTS := $(SOURCES:.F90=.o) suews_ctrl_error_tls.o
LIBUTILITIES = ../lib/libutilities.a

all: $(LIBUTILITIES)

$(LIBUTILITIES): $(OBJECTS)
	ar r $(LIBUTILITIES) $(OBJECTS)

%.o: %.F90
	$(FC) $(FCFLAGS) -c $<

clean:
	rm -f *.o $(LIBUTILITIES)

easy_netcdf.o: radiation_io.o parkind1.o
radiation_io.o: parkind1.o
print_matrix.o: parkind1.o
yomhook.o: parkind1.o

suews_ctrl_error_tls.o: ../../../src/suews_ctrl_error_tls.c
	$(CC) -O2 -fPIC -c $< -o $@
