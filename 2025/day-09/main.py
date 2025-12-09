#!/usr/bin/env python3

import sys
from shapely.geometry import Point, Polygon
import itertools


def read_input(file):
    coords = []
    with open(file, "r") as f:
        for line in f:
            x, y = map(int, line.strip().split(","))
            coords.append((x, y))
    return coords


def calc_largest_rectangle(red_tile_coords, red_and_green=False):
    if red_and_green:
        polygon = Polygon(red_tile_coords)
    largest = 0
    for i, ((x, y), (x1, y1)) in enumerate(itertools.combinations(red_tile_coords, 2)):
        area = (abs(x1 - x)+1) * (abs(y1 - y)+1)
        if area > largest:
            if red_and_green:
                rectangle = Polygon([(x, y), (x1, y), (x1, y1), (x, y1)])
                if polygon.covers(rectangle):
                    largest = area
            else:
                largest = area
    return largest


def main(input_file):
    red_tile_coords = read_input(input_file)
    largest_rectangle_area = calc_largest_rectangle(red_tile_coords)
    print(f"Largest rectangle area: {largest_rectangle_area}")
    largest_rectangle_area = calc_largest_rectangle(
        red_tile_coords, red_and_green=True)
    print(f"Largest rectangle area (green tiles): {largest_rectangle_area}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
