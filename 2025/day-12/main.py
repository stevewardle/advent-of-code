#!/usr/bin/env python3

import sys
import numpy as np


def read_input(file):
    presents = []
    grids = []
    with open(file, "r") as f:
        lines = f.read().strip().split("\n")
        i = 0
        while i < len(lines):
            line = lines[i]
            if line == "":
                i += 1
                continue
            if 'x' in line and ':' in line:
                break
            if line.endswith(":"):
                present_index = int(line[:-1])
                i += 1
                grid_lines = []
                while i < len(lines) and lines[i] != "":
                    grid_lines.append(lines[i])
                    i += 1
                grid_array = np.array(
                    [[1 if char == '#' else 0 for char in row] for row in grid_lines])
                presents.append((present_index, grid_array))
            else:
                i += 1

        while i < len(lines):
            line = lines[i]
            if line == "":
                i += 1
                continue
            if 'x' in line and ':' in line:
                dim_part, presents_part = line.split(':')
                rows, cols = map(int, dim_part.split('x'))
                counts = list(map(int, presents_part.strip().split()))
                present_indices = []
                for pid, count in enumerate(counts):
                    present_indices.extend([pid] * count)
                grid_array = np.zeros((rows, cols), dtype=int)
                grids.append((grid_array, present_indices))
            i += 1

    return presents, grids


def generate_unique_variations(present):
    variations = []
    seen = set()

    candidates = [present, np.flip(present, axis=0)]

    for shape in candidates:
        for k in range(4):
            rotated = np.rot90(shape, k=k)
            shape_bytes = rotated.tobytes()
            if shape_bytes not in seen:
                seen.add(shape_bytes)
                variations.append(rotated)

    return variations


def can_place(grid, present, row, col):
    p_height, p_width = present.shape
    g_height, g_width = grid.shape

    if (row < 0
            or col < 0
            or row + p_height > g_height
            or col + p_width > g_width):
        return False

    region = grid[row:row + p_height, col:col + p_width]
    return not np.any((region != 0) & (present == 1))


def place_present(grid, present, row, col, val):
    p_height, p_width = present.shape
    mask = (present == 1)
    grid[row:row + p_height, col:col + p_width][mask] = val


def solve(grid, present_ids, present_map):

    if all(pid == -1 for pid in present_ids):
        return True

    empty_cells = np.where(grid == 0)
    if len(empty_cells[0]) == 0:
        return True

    next_row, next_col = empty_cells[0][0], empty_cells[1][0]

    candidates = sorted(list(set(present_ids)), key=lambda x: x == -1)

    for pid in candidates:
        variations = present_map[pid]
        for present in variations:
            p_rows, p_cols = np.where(present == 1)
            p_row, p_col = p_rows[0], p_cols[0]

            row, col = next_row - p_row, next_col - p_col
            if can_place(grid, present, row, col):
                val = 2 if pid == -1 else 1
                place_present(grid, present, row, col, val)

                remaining_ids = list(present_ids)
                remaining_ids.remove(pid)

                if solve(grid, remaining_ids, present_map):
                    return True

                place_present(grid, present, row, col, 0)
    return False


def main(input_file):
    presents, grids = read_input(input_file)

    present_map = {}
    present_areas = {}
    for pid, p_grid in presents:
        present_map[pid] = generate_unique_variations(p_grid)
        present_areas[pid] = np.sum(p_grid)
    present_map[-1] = [np.array([[1]])]
    present_areas[-1] = 1

    will_fit = 0
    for i, (grid, p_ids) in enumerate(grids):

        covered_area = sum(present_areas[pid] for pid in p_ids)
        grid_area = grid.size
        leftover = grid_area - covered_area

        if leftover < 0:
            continue
        p_ids.extend([-1] * leftover)

        if solve(grid, p_ids, present_map):
            will_fit += 1
    print(f"Number of grids that can fit all presents: {will_fit}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
