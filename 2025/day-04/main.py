#!/usr/bin/env python3

import sys
import numpy as np
from numpy.lib.stride_tricks import sliding_window_view


def read_input(file):
    with open(file, 'r') as f:
        lines = f.readlines()
        rows, cols = len(lines), len(lines[0].strip())
        roll_grid = np.zeros((rows + 2, cols + 2), dtype=int)
        for i in range(rows):
            line = lines[i].strip()
            for j in range(cols):
                if line[j] == '@':
                    roll_grid[i+1, j+1] = 1
                else:
                    roll_grid[i+1, j+1] = 0
    return roll_grid


def get_accessible_rolls(roll_grid):
    rows, cols = roll_grid.shape
    accessible_rolls = np.zeros((rows-2, cols-2), dtype=int)
    sliding_view = sliding_window_view(roll_grid, (3, 3))
    for i in range(rows-2):
        for j in range(cols-2):
            window = sliding_view[i, j]
            if window[1, 1] == 0:
                continue
            neighbor_roll_count = np.sum(window) - window[1, 1]
            if neighbor_roll_count < 4:
                accessible_rolls[i, j] = 1
    return accessible_rolls


def main(input_file):
    roll_grid = read_input(input_file)
    accessable_rolls = get_accessible_rolls(roll_grid)
    print(f"Total Accessible Rolls: {np.sum(accessable_rolls)}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
