FTN	  = gfortran-11
FLAGS = -m64 -Ofast -fdefault-real-8
GRIBPATH   =$(shell pkg-config --cflags eccodes)
GRIBLD     =$(shell pkg-config --libs eccodes)
INC   = $(GRIBPATH)
LIB   = $(GRIBPATH) $(GRIBLD)  -leccodes_f90

EXE	= traj_blpb

SUFFIXES = .o .f90

MOD = \
blpb_param.o	blpb_read.o \
blpb_gribf.o	blpb_time.o \
blpb_check.o	blpb_spline.o \
blpb_field.o	blpb_grid.o

OBJ = \
blpb_trajectory.o

all:	$(MOD) $(OBJ)
	$(FTN) *.o -o $(EXE) $(FLAGS) $(LIB)
	
$(OBJ):	$(MOD)

%.o: %.f90
	$(FTN) -c  $(FLAGS) $(INC) $<

veryclean:
	rm -f $(EXE) *.o *.mod

clean:
	rm  -f *.o *.mod
	
