# URL for retrieving my input
YEAR=$(shell basename $(dir ${CURDIR}))
DAY=$(shell printf "%d" $(shell echo $(subst day-,,$(shell basename ${CURDIR})) | sed 's/^0*//'))
INPUT_URL=https://adventofcode.com/${YEAR}/day/${DAY}/input
# Next day
DAYN=$(shell printf "%02d" $$((${DAY}+1)))

all: run
.PHONY: run

run-%:
	time python main.py $*

input.txt: ../../session-token
	@curl '${INPUT_URL}' -X GET -H 'Cookie: $(shell cat ../../session-token)' > $@
	chmod -w $@

nextday:
	mkdir ../day-${DAYN}
	cp Makefile ../day-${DAYN}
	echo "input.txt" >> ../day-${DAYN}/.gitignore
	echo "example.txt" >> ../day-${DAYN}/.gitignore
