#!/usr/bin/env python3

import sys
from collections import Counter


class JunctionBox:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z
        self.connected_boxes = []
        self.circuit_id = 0

    def distance(self, other):
        return ((self.x - other.x) ** 2 +
                (self.y - other.y) ** 2 +
                (self.z - other.z) ** 2) ** 0.5

    def connect(self, other):
        if other not in self.connected_boxes:
            self.connected_boxes.append(other)
            other.connected_boxes.append(self)
        if self.circuit_id == 0 and other.circuit_id != 0:
            self.circuit_id = other.circuit_id
        elif other.circuit_id == 0 and self.circuit_id != 0:
            other.circuit_id = self.circuit_id
        elif self.circuit_id != other.circuit_id:
            old_id = other.circuit_id
            new_id = self.circuit_id
            to_update = [other]
            while to_update:
                box = to_update.pop()
                box.circuit_id = new_id
                for connected in box.connected_boxes:
                    if connected.circuit_id == old_id:
                        to_update.append(connected)


def read_input(file):
    junction_boxes = []
    with open(file, "r") as f:
        for line in f:
            junction_boxes.append(JunctionBox(
                *map(int, line.strip().split(","))))
    return junction_boxes


def calc_distances(junction_boxes):
    distances = {}
    for i, box1 in enumerate(junction_boxes):
        for box2 in junction_boxes[i+1:]:
            dist = box1.distance(box2)
            distances[(box1, box2)] = dist
    return distances


def connect_boxes(junction_boxes, distances, count):
    sorted_distances = sorted(distances.items(), key=lambda item: item[1])
    connections_made = 0
    circuit_id = 1
    for (box1, box2), dist in sorted_distances:
        if connections_made >= count:
            break
        if box1.circuit_id == 0 and box2.circuit_id == 0:
            box1.circuit_id = box2.circuit_id = circuit_id
            circuit_id += 1
        box1.connect(box2)
        connections_made += 1


def main(input_file):
    junction_boxes = read_input(input_file)
    distances = calc_distances(junction_boxes)
    if input_file == "example.txt":
        count = 10
    else:
        count = 1000
    connect_boxes(junction_boxes, distances, count)
    circuit_ids = [
        box.circuit_id for box in junction_boxes if box.circuit_id != 0]
    circuit_count = Counter(circuit_ids)
    largest_circuits = circuit_count.most_common(3)
    result = 1
    for _, size in largest_circuits:
        result *= size
    print(f"Multiple of 3 largest circuits: {result}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
