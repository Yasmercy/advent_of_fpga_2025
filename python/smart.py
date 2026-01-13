"""
Thank you to the following reddit user which I found this solution from.
Takes advantage of the metric property of Euclidean space, which indicates that
all edges in an MST must be the nearest neighbor of some vertex.

https://www.reddit.com/r/adventofcode/comments/1ph3tfc/comment/nt1bxu7/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
"""

import functools as ft
from time import perf_counter

import numpy as np


def timing(f):
    @ft.wraps(f)
    def wrap(*args, **kw):
        ts = perf_counter()
        reps = 100
        for _ in range(reps):
            result = f(*args, **kw)
        te = perf_counter()
        print(f"{f.__name__} took {(te - ts) / reps:2.4f} sec")
        return result

    return wrap


@timing
def solve(nodes):
    dists = np.sum((nodes[:, None, :] - nodes[None, :, :]) ** 2, axis=2)
    np.fill_diagonal(dists, np.iinfo(np.int64).max)

    nearest = np.argmin(dists, axis=1)
    longest = np.argmax(np.sum((nodes - nodes[nearest]) ** 2, axis=1))

    return nodes[longest, 0] * nodes[nearest[longest], 0]


@timing
def solve2(nodes):
    infty = np.iinfo(np.int64).max
    nearest = []
    for v, pv in enumerate(nodes):
        dist = np.sum((nodes - pv) ** 2, axis=1)
        dist[v] = infty
        nearest.append(np.argmin(dist))

    longest = np.argmax(np.sum((nodes - nodes[nearest]) ** 2, axis=1))
    return nodes[longest, 0] * nodes[nearest[longest], 0]


def main():
    input = "../data/input.in"
    nodes = np.genfromtxt(input, dtype=np.int64, comments=None, delimiter=",")
    print(solve(nodes))
    print(solve2(nodes))


if __name__ == "__main__":
    main()
