#!/usr/bin/env python3

import sys
import functools
import networkx as nx


class Device:
    def __init__(self, id, outputs):
        self.id = id
        self.outputs = outputs


def count_paths(devices, start_id, end_id, must_pass_through=[]):
    sys.setrecursionlimit(20000)

    all_ids = set()
    device_lookup = {}
    for d in devices:
        device_lookup[d.id] = d
        all_ids.add(d.id)
        for out in d.outputs:
            all_ids.add(out)

    # Change ids to numbers in a consistent order
    sorted_ids = sorted(list(all_ids))
    id_map = {name: i for i, name in enumerate(sorted_ids)}
    N = len(sorted_ids)

    start, end = id_map[start_id], id_map[end_id]

    # Build a graph
    adj = [[] for _ in range(N)]
    G = nx.DiGraph()

    for name in sorted_ids:
        if name in device_lookup:
            u = id_map[name]
            for out in device_lookup[name].outputs:
                v = id_map[out]
                adj[u].append(v)
                G.add_edge(u, v)

    # Create bitmask of required nodes
    final_req_mask = 0
    for name in must_pass_through:
        if name in id_map:
            final_req_mask |= (1 << id_map[name])

    @functools.lru_cache(maxsize=None)
    def solve_dag(u, req_mask):
        if (1 << u) & final_req_mask:
            req_mask |= (1 << u)

        if u == end:
            return 1 if req_mask == final_req_mask else 0

        return sum(solve_dag(v, req_mask) for v in adj[u])

    return solve_dag(start, 0)


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
    paths = count_paths(devices, "you", "out")
    print(f"Number of paths from 'you' to 'out': {paths}")
    paths = count_paths(
        devices, "svr", "out", must_pass_through=["dac", "fft"])
    print(f"Number of paths from 'svr' to 'out' (passing 'dac' and 'fft'): {
          paths}")


if __name__ == "__main__":
    input_file = sys.argv[1]
    main(input_file)
