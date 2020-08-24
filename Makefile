# makefile for interpolation compilation

default: interp

include BATSRUS/Makefile.def
include BATSRUS/Makefile.conf

LIBDIR    = ${GMDIR}/src
BINDIR    = ${GMDIR}/src
SHAREDIR  = ${GMDIR}/share/Library/src

DOUBLEPREC = -frecord-marker=4 -fdefault-real-8 -fdefault-double-8
COMPILER = gfortran ${DOUBLEPREC}
LIBSHARE = ${LIBDIR}/libSHARE.a
LINK.f90 = ${CUSTOMPATH_MPI}mpif90

INTERP_OUTPUT.exe: ${LIBSHARE} interpolate_output.o
	${LINK.f90} -o INTERP_OUTPUT.exe interpolate_output.o \
	-L${LIBDIR} -lSHARE ${Lflag}

interp:
	make INTERP_OUTPUT.exe

.PHONY: clean
clean:
	rm -f *~ *.o *.mod INTERP_OUTPUT.exe
