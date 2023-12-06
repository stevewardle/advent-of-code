all: seed seed2
.PHONY: all

seed: seed.f90
	gfortran $< -o $@

seed2: seed2.f90
	gfortran -O3 $< -o $@

