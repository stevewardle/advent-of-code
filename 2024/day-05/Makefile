# URL for retrieving my input
YEAR=$(shell basename $(dir ${CURDIR}))
DAY=$(shell printf "%d" $(subst day-,,$(shell basename ${CURDIR})))
INPUT_URL=https://adventofcode.com/${YEAR}/day/${DAY}/input
# Next day
DAYN=$(shell printf "%02d" $$((${DAY}+1)))

all: release
.PHONY: all release debug clean

release: target/release/aoc_2024 
	time $<

target/release/aoc_2024: src/main.rs
	cargo build --release

debug: src/main.rs
	cargo run 

input.txt: ../../session-token
	@curl '${INPUT_URL}' -X GET -H 'Cookie: $(shell cat ../../session-token)' > $@
	chmod -w $@

nextday:
	mkdir ../day-${DAYN}
	cp Makefile ../day-${DAYN}
	echo "input.txt" >> ../day-${DAYN}/.gitignore
	echo "output.txt" >> ../day-${DAYN}/.gitignore
	echo "example.txt" >> ../day-${DAYN}/.gitignore
	echo "target" >> ../day-${DAYN}/.gitignore
	echo "Cargo.lock" >> ../day-${DAYN}/.gitignore
	cp Cargo.toml ../day-${DAYN}

clean:
	rm -rf target Cargo.lock 2> /dev/null
