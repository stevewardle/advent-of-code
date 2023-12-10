NAME=cube

all: part1 part2
.PHONY: all part1 part2 clean

part1: ${NAME} 
	time ./$<

part2: ${NAME}2
	time ./$<

${NAME}: ${NAME}.f90
	gfortran -Wall -Wpedantic -O3 $< -o $@

${NAME}2: ${NAME}2.f90
	gfortran -Wall -Wpedantic -O3 $< -o $@

clean:
	rm ${NAME} ${NAME}2 2> /dev/null
