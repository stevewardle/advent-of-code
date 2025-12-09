#!/usr/bin/env python3

import sys


def read_input(file):
    coords = []
    with open(file, "r") as f:
        for line in f:
            x, y = map(int, line.strip().split(","))
            coords.append((x, y))
    return coords


def calc_largest_rectangle(red_tile_coords):
    coords_sorted = sorted(red_tile_coords)
    largest = 0
    for i, (x, y) in enumerate(coords_sorted):
        for x1, y1 in coords_sorted[i+1:]:
            area = abs((x1 - x + 1) * (y1 - y + 1))
            if area > largest:
                largest = area
    return largest


def main(input_file):
    red_tile_coords = read_input(input_file)
    largest_rectangle_area = calc_largest_rectangle(red_tile_coords)
    print(f"Largest rectangle area: {largest_rectangle_area}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
