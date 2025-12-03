#!/usr/bin/env python3

import sys


def read_input(file):
    with open(file, 'r') as f:
        banks = [list(map(int, line.strip())) for line in f.readlines()]
    return banks


def calc_joltage(bank, battery_count=2):
    batteries = []
    bank_search = bank
    while len(batteries) < battery_count:
        # Find the largest battery that still leaves enough available
        # further in the bank
        upper_bound = len(bank_search) - (battery_count - len(batteries)) + 1
        max_battery = max(bank_search[:upper_bound])
        max_battery_index = bank_search.index(max_battery)
        # Cut the search space to only after the found battery
        bank_search = bank_search[max_battery_index + 1:]
        batteries.append(max_battery)
    joltage = int("".join(map(str, batteries)))
    return joltage


def main(input_file):
    banks = read_input(input_file)
    total_output_joltage_p1 = 0
    total_output_joltage_p2 = 0
    for bank in banks:
        joltage_p1 = calc_joltage(bank)
        joltage_p2 = calc_joltage(bank, battery_count=12)
        total_output_joltage_p1 += joltage_p1
        total_output_joltage_p2 += joltage_p2
    print(f"Total Output Joltage (Part 1): {total_output_joltage_p1}")
    print(f"Total Output Joltage (Part 2): {total_output_joltage_p2}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
