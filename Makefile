# REIN Makefile
#  INTEL

include ./Makefile.machine
#include ./Makefile.lib

BIN	= get_m

OBJ	=                    \
	spawn.o

$(BIN):	$(OBJ)
				$(F90) $(COPTFLAG) $(OBJ) $(LIBS_MARBLE) $(LIBS_MARBLE) $(LIBS_MARBLE) $(L_MARBLE) $(L_NAMD) $(L_MINI) $(L_REIN) -o $(BIN) $(LIBMBFLAG)

namd:	$(OBJ)
	$(F90) $(COPTFLAG) $(OBJ) $(L_NAMD) $(L_REIN) -o $(BIN) $(LIBMBFLAG)

marble:	$(OBJ)
				$(F90) $(COPTFLAG) $(OBJ) $(LIBS_MARBLE) $(LIBS_MARBLE) $(LIBS_MARBLE) $(L_MARBLE) $(L_REIN) -o $(BIN) $(LIBMBFLAG)

mini:	$(OBJ)
	$(F90) $(COPTFLAG) $(OBJ) $(L_MINI) $(L_REIN) -o $(BIN) $(LIBMBFLAG)

include ./Makefile.comp

clean:
	rm -rf *.f90 *.mod *.o *.a *~ *.bak $(BIN)

all_clean:
	rm -rf *.f90 *.mod *.o *.a *~ *.bak $(BIN)

.PRECIOUS: .f90
.SUFFIXES: .fpp .o
