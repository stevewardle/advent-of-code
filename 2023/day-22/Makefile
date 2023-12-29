NAME=slabs

# URL for retrieving my input
YEAR=$(shell basename $(dir ${CURDIR}))
DAY=$(subst day-,,$(shell basename ${CURDIR}))
INPUT_URL=https://adventofcode.com/${YEAR}/day/${DAY}/input
# Next day
DAYN=$(shell printf "%02d" $$((${DAY}+1)))

all: run
.PHONY: all run clean

run: ${NAME} 
	time ./$<

${NAME}: ${NAME}.f90
	gfortran -Wall -Wpedantic -O3 $^ -o $@

input.txt: ../../session-token
	@curl '${INPUT_URL}' -X GET -H 'Cookie: $(shell cat ../../session-token)' > $@
	chmod -w $@

nextday:
	mkdir ../day-${DAYN}
	cp Makefile ../day-${DAYN}
	echo "input.txt" >> ../day-${DAYN}/.gitignore
	echo "example.txt" >> ../day-${DAYN}/.gitignore

clean:
	rm ${NAME} ${NAME}2 2> /dev/null
