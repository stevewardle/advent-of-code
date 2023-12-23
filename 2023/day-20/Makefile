NAME=pulse

# URL for retrieving my input
YEAR=$(shell basename $(dir ${CURDIR}))
DAY=$(subst day-,,$(shell basename ${CURDIR}))
INPUT_URL=https://adventofcode.com/${YEAR}/day/${DAY}/input

all: part1 part2
.PHONY: all part1 part2 clean

part1: ${NAME} 
	time ./$<

part2: ${NAME}2
	time ./$<

linked_list.o: linked_list.f90
	gfortran -c -Wall -Wpedantic -O3 $< -o $@

pulse_module.o: pulse_module.f90
	gfortran -c -Wall -Wpedantic -O3 $< -o $@

${NAME}: ${NAME}.f90 linked_list.o pulse_module.o
	gfortran -Wall -Wpedantic -O3 $^ -o $@

${NAME}2: ${NAME}2.f90
	gfortran -Wall -Wpedantic -O3 $< -o $@

input.txt: ../../session-token
	@curl '${INPUT_URL}' -X GET -H 'Cookie: $(shell cat ../../session-token)' > $@
	chmod -w $@

clean:
	rm ${NAME} ${NAME}2 2> /dev/null
