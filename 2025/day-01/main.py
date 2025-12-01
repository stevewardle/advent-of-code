#!/usr/bin/env python3

import sys


class Lock:
    def __init__(self, start_position, lock_size):
        self._position = start_position
        self._lock_size = lock_size + 1
        self._zero_hit = 0
        self._zero_pass = 0

    def turn(self, direction, clicks):
        # Do not double count leaving zero to the left
        if self._position == 0 and direction == 'L':
            self._zero_pass -= 1

        if direction == 'L':
            abs_position = self._position - clicks
        elif direction == 'R':
            abs_position = self._position + clicks
        else:
            raise ValueError("Direction must be 'L' or 'R'")

        self._position = abs_position % self._lock_size
        passes = abs(abs_position // self._lock_size)
        # Times we land on zero
        if self._position == 0:
            self._zero_hit += 1

            # Do not double count landing on zero from the right
            if direction == 'R':
                self._zero_pass -= 1

        # Times we pass zero
        self._zero_pass += passes

    @property
    def position(self):
        return self._position

    @property
    def zero_hit(self):
        return self._zero_hit

    @property
    def zero_pass(self):
        return self._zero_pass


def read_input(file):
    with open(file, 'r') as f:
        for line in f:
            direction = line[0]
            click = int(line[1:].strip())
            yield direction, click


def main(input_file, lock):
    for direction, click in read_input(input_file):
        lock.turn(direction, click)
        print(f"Turned {direction}{click}: position {lock.position}, passes zero {
              lock.zero_pass}, hits zero {lock.zero_hit}")
    print(f"Lock hit position zero {lock.zero_hit} times")
    print(f"Lock passed position zero {lock.zero_pass} times")
    print(f"Total times lock saw zero {lock.zero_hit + lock.zero_pass}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    lock = Lock(start_position=50, lock_size=99)
    main(input_file, lock)

    # Guesses part 2
    # 6676; incorrect!
