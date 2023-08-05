FC = gfortran 
CFLAGS = `sdl2-config --cflags`
LIBS = lib/libsdl2.a `sdl2-config --libs`
INCLUDE = -I$(CURDIR)/fortran-sdl2/

FILES = src/**.f90
OUTPUT =  -o bin/main.out

all:
	$(FC) $(CFLAGS) $(OUTPUT) $(FILES) $(LIBS) $(INCLUDE)

run:
	./bin/main.out