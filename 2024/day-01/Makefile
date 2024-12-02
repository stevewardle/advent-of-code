# URL for retrieving my input
YEAR=$(shell basename $(dir ${CURDIR}))
DAY=$(shell printf "%d" $(subst day-,,$(shell basename ${CURDIR})))
INPUT_URL=https://adventofcode.com/${YEAR}/day/${DAY}/input
# Next day
DAYN=$(shell printf "%02d" $$((${DAY}+1)))

all: run
.PHONY: all run clean

run: main
	time ./$<

main: main.rs 
	rustc $^ -o $@

input.txt: ../../session-token
	@curl '${INPUT_URL}' -X GET -H 'Cookie: $(shell cat ../../session-token)' > $@
	chmod -w $@

nextday:
	mkdir ../day-${DAYN}
	cp Makefile ../day-${DAYN}
	echo "input.txt" >> ../day-${DAYN}/.gitignore
	echo "example.txt" >> ../day-${DAYN}/.gitignore

clean:
	rm main 2> /dev/null
