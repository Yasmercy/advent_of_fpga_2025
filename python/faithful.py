import functools as ft
import itertools as it
from time import time

import networkx as nx
import numpy as np


def timing(f):
    @ft.wraps(f)
    def wrap(*args, **kw):
        ts = time()
        result = f(*args, **kw)
        te = time()
        print(f"{f.__name__} took {te - ts:2.4f} sec")
        return result

    return wrap


@timing
def library_solution(nodes):
    edges = [
        (u, v, sum((x1 - x2) * (x1 - x2) for x1, x2 in zip(u, v)))
        for u, v in list(it.combinations(nodes, 2))
    ]

    graph = nx.Graph()
    graph.add_nodes_from(nodes)
    graph.add_weighted_edges_from(edges)
    mst2 = nx.minimum_spanning_tree(graph)

    u, v, w = max((e for e in mst2.edges(data=True)), key=lambda e: e[2]["weight"])
    return u[0] * v[0]


@timing
def numpy_solution(nodes):
    n = len(nodes)
    infty = np.iinfo(np.int64).max
    labels = np.arange(n)
    nodes = np.array(nodes, dtype=np.int64)

    diff = nodes[:, None, :] - nodes[None, :, :]
    dist2 = np.sum(diff * diff, axis=2)
    np.fill_diagonal(dist2, infty)

    best_u, best_v, best_dist = -1, -1, -1

    k = 0
    while np.unique(labels).size > 1:
        k += 1

        # Step 1: nearest boundary edge for each vertex
        mask = labels[:, None] != labels[None, :]
        nearest = np.argmin(np.where(mask, dist2, infty), axis=1)
        distances = dist2[np.arange(n), nearest]

        # Step 2: nearest boundary edge for each component
        reduced = np.array(
            [np.argmin(np.where(labels == lu, distances, infty)) for lu in labels]
        )

        # Step 3: accumulate the MST edges
        _, reps = np.unique(labels, return_index=True)

        mst_u = reduced[reps]
        mst_v, mst_d = nearest[mst_u], distances[mst_u]

        i = np.argmax(mst_d)
        if mst_d[i] > best_dist:
            best_u, best_v, best_dist = mst_u[i], mst_v[i], mst_d[i]

        # Step 4: star contraction
        coin = np.random.randint(2, size=n)[labels]
        head = nearest[reduced]
        labels = np.where((coin == 0) & (coin[head] == 1), labels[head], labels)

    # return best_dist
    return nodes[best_u, 0] * nodes[best_v, 0]


def main():
    with open("../data/input.in") as f:
        lines = f.readlines()

    nodes = [tuple(map(int, line.strip().split(","))) for line in lines]
    # sol1 = library_solution(nodes)
    sol2 = numpy_solution(nodes)

    # assert sol1 == sol2
    # print(sol1)


if __name__ == "__main__":
    main()
