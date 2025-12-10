#!/usr/bin/env python3

import sys
import itertools
import numpy as np


class Machine:
    def __init__(self, activation_state, buttons, joltage):
        self.activation_state = activation_state
        self.buttons = buttons
        self.joltage = joltage
        self.lights = np.zeros(len(activation_state), dtype=bool)

    def push_button(self, button_index):
        if button_index < 0 or button_index >= len(self.buttons):
            raise IndexError("Button index out of range")
        self.lights ^= self.buttons[button_index]


def read_input(file):
    machines = []
    with open(file, "r") as f:
        for line in f:
            line = line.strip()
            blocks = line.split(" ")

            state_block = blocks[0][1:-1]
            state = np.array([c == "#" for c in state_block], dtype=bool)

            button_blocks = blocks[1:-1]
            buttons = np.zeros((len(button_blocks), len(state)), dtype=bool)
            for i, block in enumerate(button_blocks):
                indices = block[1:-1]
                if indices:
                    for index in map(int, indices.split(",")):
                        buttons[i, index] = True
            joltage_block = blocks[-1][1:-1]
            joltage = np.array([int(x)
                               for x in joltage_block.split(",")], dtype=int)
            machines.append(
                Machine(state, buttons, joltage))
    return machines


def calc_min_presses(machine):
    n_buttons = len(machine.buttons)
    for r in range(n_buttons + 1):
        for combo in itertools.combinations(range(n_buttons), r):
            machine.lights = np.zeros(
                len(machine.activation_state), dtype=bool)
            for button_index in combo:
                machine.push_button(button_index)
            if np.array_equal(machine.lights, machine.activation_state):
                return r
    raise ValueError(
        "No combination of button presses can achieve the activation state")


def main(input_file):
    machines = read_input(input_file)
    total_presses = 0
    for machine in machines:
        total_presses += calc_min_presses(machine)
    print(f"Total minimum button presses: {total_presses}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
