#!/usr/bin/env python3

import sys


class ProductIdRange:
    def __init__(self, id_range):
        self._start = id_range.split('-')[0]
        self._end = id_range.split('-')[1]

    @property
    def start(self):
        return int(self._start)

    @property
    def end(self):
        return int(self._end)

    def find_invalid_ids(self):
        invalid_ids = []
        for id in range(self.start, self.end + 1):
            id_str = str(id)
            # print(f"Checking id: {id_str}")
            length = len(id_str)
            for chunk_size in range(1, length // 2 + 1):
                if length % chunk_size != 0:
                    continue
                chunks = [id_str[i:i + chunk_size]
                          for i in range(0, length, chunk_size)]
                # print(f"Chunks (size {chunk_size}): {chunks}")
                if all(chunk == chunks[0] for chunk in chunks):
                    # print(f"Invalid id found - {id_str}")
                    if id_str not in invalid_ids:
                        invalid_ids.append(id_str)

        return invalid_ids


def read_input(file):
    with open(file, 'r') as f:
        return [ProductIdRange(id_range) for id_range in f.read().strip().split(',')]


def main(input_file):
    id_ranges = read_input(input_file)
    invalid_ids_p1_total = 0
    invalid_ids_p2_total = 0
    for id_range in id_ranges:
        # print(f"Product ID Range: {id_range.start} to {id_range.end}")
        if id_range.start != 0 and id_range.end != 0:
            for invalid_id in id_range.find_invalid_ids():
                if len(invalid_id) % 2 == 0:
                    half = len(invalid_id) // 2
                    if invalid_id[:half] == invalid_id[half:]:
                        invalid_ids_p1_total += int(invalid_id)
                invalid_ids_p2_total += int(invalid_id)
    print(f"Total of invalid product IDs (part 1): {invalid_ids_p1_total}")
    print(f"Total of invalid product IDs (part 2): {invalid_ids_p2_total}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
