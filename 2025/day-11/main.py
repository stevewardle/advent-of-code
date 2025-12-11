#!/usr/bin/env python3

import sys
import itertools
import numpy as np


class Device:
    def __init__(self, id, outputs):
        self.id = id
        self.outputs = outputs


def calc_paths_from_you_to_out(devices):
    device_map = {device.id: device for device in devices}
    paths = []

    def dfs(current_id, path):
        if current_id == "out":
            paths.append(path.copy())
            return
        current_device = device_map.get(current_id)
        if not current_device:
            return
        for output in current_device.outputs:
            path.append(output)
            dfs(output, path)
            path.pop()

    dfs("you", ["you"])
    return paths


def read_input(file):
    devices = []
    with open(file, "r") as f:
        for line in f:
            parts = line.strip().split(":")
            id = parts[0]
            outputs = parts[1].strip().split()
            devices.append(Device(id, outputs))
    return devices


def main(input_file):
    devices = read_input(input_file)
    paths = calc_paths_from_you_to_out(devices)
    print(f"Number of paths from 'you' to 'out': {len(paths)}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
