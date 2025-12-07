#!/usr/bin/env python3

import sys
import re


def read_input(file):
    with open(file, "r") as f:
        lines = f.readlines()
        problems = []
        operations = re.findall(r"([+*]\s*)\s", lines[-1])
        problems = []
        for line in lines[:-1]:
            idx = 0
            row = []
            for op in operations:
                width = len(op)
                row.append(line[idx:idx+width])
                idx += width + 1
            problems.append(row)
    return problems, operations


def calc_solutions(problems, operations):
    sums = ["" for _ in operations]
    for i, operation in enumerate(operations):
        for j, problem in enumerate(problems):
            if j > 0:
                sums[i] += operation
            sums[i] += problem[i]
    solutions = [eval(s) for s in sums]
    return solutions


def calc_solutions_correctly(problems, operations):
    sums = ["" for _ in operations]
    for i, operation in enumerate(operations):
        for j in range(len(problems[0][i])):
            for problem in problems:
                sums[i] += problem[i][j]
            if j < len(problems[0][i]) - 1:
                sums[i] += operation
    solutions = [eval(s) for s in sums]
    return solutions


def main(input_file):
    problems, operations = read_input(input_file)
    solutions = calc_solutions(problems, operations)
    print("Grand sum of solutions (Part 1):", sum(solutions))
    solutions = calc_solutions_correctly(problems, operations)
    print("Grand sum of solutions (Part 2):", sum(solutions))


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
