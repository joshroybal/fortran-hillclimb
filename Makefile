FC = gfortran
FCFLAGS = -pedantic-errors -std=f95 -Wall -Wextra -Werror -O2
FLFLAGS = -static -s
SRC = $(wildcard *.f90)
OBJ = ${SRC:.f90=.o}
BIN = driver

$(BIN): $(OBJ)
	$(FC) -o $@ $^ $(FLFLAGS)

%.o: %.f90
	$(FC) -c $< $(FCFLAGS)

subcipher.mod := subcipher.o
hillclimb.mod := hillclimb.o subcipher.o
driver.o: $(subcipher.mod) $(hillclimb.mod)
hillclimb.o: $(subcipher.mod)

.PHONY: clean
clean:
	$(RM) $(BIN) *.o *.mod *~
