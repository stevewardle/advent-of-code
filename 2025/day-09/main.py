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
        print(f"Checking rectangle {
              i}: ({x},{y}) to ({x1},{y1}) with area {area}")
        if area > largest:
            if red_and_green:
                all_points_inside = True
                if not polygon.covers(Point(x, y1)) or not polygon.covers(Point(x1, y)) or area > polygon.area:
                    continue
                for xi in range(min(x, x1), max(x, x1) + 1):
                    if not polygon.covers(Point(xi, min(y, y1))) or not polygon.covers(Point(xi, max(y, y1))):
                        all_points_inside = False
                        break
                if not all_points_inside:
                    continue
                for yi in range(min(y, y1), max(y, y1) + 1):
                    if not polygon.covers(Point(min(x, x1), yi)) or not polygon.covers(Point(max(x, x1), yi)):
                        all_points_inside = False
                        break
                if not all_points_inside:
                    continue
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
