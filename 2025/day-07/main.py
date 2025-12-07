#!/usr/bin/env python3

import sys
import copy
from functools import lru_cache


def read_input(file):
    manifold_rows = []
    with open(file, "r") as f:
        for line in f:
            manifold_rows.append(list(line.strip()))
    return manifold_rows


def process_manifold_rows(rows):
    splits = 0
    for i, row in enumerate(rows):
        if i == 0:
            continue
        for j in range(len(row)):
            if rows[i-1][j] in ("S", "|"):
                if row[j] == ".":
                    row[j] = "|"
                elif row[j] == "^":
                    splits += 1
                    row[j-1] = "|"
                    row[j+1] = "|"
    return splits


@lru_cache(maxsize=1024)
def process_manifold_timelines(rows_tuple):
    timelines = 0
    rows = [list(row) for row in rows_tuple]
    for i, row in enumerate(rows):
        if i == 0:
            continue
        for j in range(len(row)):
            if rows[i-1][j] in ("S", "|"):
                if row[j] == ".":
                    row[j] = "|"
                elif row[j] == "^":
                    rows_copy = copy.deepcopy(rows[i:])
                    rows_copy[0] = ["."] * len(row)
                    rows_copy[0][j-1] = "S"
                    timelines += process_manifold_timelines(
                        tuple(tuple(row) for row in rows_copy)
                    )
                    rows_copy = copy.deepcopy(rows[i:])
                    rows_copy[0] = ["."] * len(row)
                    rows_copy[0][j+1] = "S"
                    timelines += process_manifold_timelines(
                        tuple(tuple(row) for row in rows_copy)
                    )
                    return timelines
    timelines += 1
    return timelines


def main(input_file):
    manifold_rows = read_input(input_file)
    splits = process_manifold_rows(copy.deepcopy(manifold_rows))
    print(f"Number of beam splits: {splits}")
    timelines = process_manifold_timelines(
        tuple(tuple(row) for row in manifold_rows))
    print(f"Number of timelines: {timelines}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
