#!/usr/bin/env python3

import sys


def read_input(file):
    with open(file, 'r') as f:
        banks = [list(map(int, line.strip())) for line in f.readlines()]
    return banks


def calc_joltage_p1(bank):
    print(''.join(map(str, bank)))
    max_battery = max(bank)
    max_battery_index = bank.index(max_battery)
    bank_right = bank[max_battery_index + 1:]
    if bank_right:
        max_battery_right = max(bank_right)
        joltage = int(f"{max_battery}{max_battery_right}")
    else:
        bank_left = bank[:max_battery_index]
        max_battery_left = max(bank_left)
        joltage = int(f"{max_battery_left}{max_battery}")
    return joltage


def main(input_file):
    banks = read_input(input_file)
    total_output_joltage = 0
    for bank in banks:
        joltage = calc_joltage_p1(bank)
        print(f"Joltage: {joltage}")
        total_output_joltage += joltage
    print(f"Total Output Joltage: {total_output_joltage}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
