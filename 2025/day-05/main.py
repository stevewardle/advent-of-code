#!/usr/bin/env python3

import sys


def read_input(file):
    with open(file, 'r') as f:
        sections = f.read().strip().split('\n\n')
        ingredient_id_ranges = []
        for line in sections[0].splitlines():
            start, end = map(int, line.split('-'))
            ingredient_id_ranges.append((start, end))
        availaible_ids = [int(line) for line in sections[1].splitlines()]
    return ingredient_id_ranges, availaible_ids


def sort_id_ranges(ranges):
    return sorted(ranges, key=lambda x: x[0])


def check_fresh(id_range, ids, freshness):
    start, end = id_range
    for i, id in enumerate(ids):
        if start <= id <= end:
            freshness[i] += 1


def get_total_possible_fresh_ids(ingredient_id_ranges):
    merged_ranges = []
    for start, end in ingredient_id_ranges:
        if not merged_ranges or merged_ranges[-1][1] < start - 1:
            merged_ranges.append([start, end])
        else:
            merged_ranges[-1][1] = max(merged_ranges[-1][1], end)
    return sum(end - start + 1 for start, end in merged_ranges)


def main(input_file):
    ingredient_id_ranges, available_ids = read_input(input_file)
    sorted_ranges = sort_id_ranges(ingredient_id_ranges)

    freshness = [0] * len(available_ids)
    for id_range in sorted_ranges:
        check_fresh(id_range, available_ids, freshness)

    fresh_count = sum(1 for f in freshness if f > 0)
    print(f"Fresh ingredients: {fresh_count}")

    total_possible_fresh_ids = get_total_possible_fresh_ids(
        sorted_ranges)
    print(f"Total possible fresh ingredient IDs: {total_possible_fresh_ids}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
