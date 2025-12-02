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
    def start_p1(self):
        return self._start[:len(self._start)//2]

    @property
    def start_p2(self):
        return self._start[len(self._start)//2:]

    @property
    def end_p1(self):
        return self._end[:len(self._end)//2]

    @property
    def end_p2(self):
        return self._end[len(self._end)//2:]

    @property
    def end(self):
        return int(self._end)

    def cut_odd_length_ids(self):
        start_length = len(self._start)
        if start_length % 2 != 0:
            new_start = 10 ** start_length
            if new_start <= self.end:
                self._start = str(new_start)
            else:
                self._start = "0"
                self._end = "0"
                return
        end_length = len(self._end)
        if end_length % 2 != 0:
            self._end = str((10 ** (end_length - 1)) - 1)

    def find_invalid_ids(self):
        invalid_ids = []
        for p1 in range(int(self.start_p1), int(self.end_p1) + 1):
            print(f"Checking p1: {p1}")
            if int(f"{p1}{p1}") <= self.end and int(f"{p1}{p1}") >= self.start:
                print(f"Invalid id found - {p1}{p1}")
                invalid_ids.append(f"{p1}{p1}")
        return invalid_ids


def read_input(file):
    with open(file, 'r') as f:
        return [ProductIdRange(id_range) for id_range in f.read().strip().split(',')]


def main(input_file):
    id_ranges = read_input(input_file)
    invalid_ids_total = 0
    for id_range in id_ranges:
        print(f"Product ID Range: {id_range.start} to {id_range.end}")
        id_range.cut_odd_length_ids()
        print(f"Product ID Range with odd IDs removed: {
              id_range.start} to {id_range.end}")
        if id_range.start != 0 and id_range.end != 0:
            print(f"{id_range.start_p1} - {id_range.start_p2}")
            print(f"{id_range.end_p1} - {id_range.end_p2}")
            for invalid_id in id_range.find_invalid_ids():
                invalid_ids_total += int(invalid_id)
    print(f"Total of invalid product IDs: {invalid_ids_total}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
