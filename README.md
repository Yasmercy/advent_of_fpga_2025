# Advent of FPGA (2025 Day 8)

Here, I am implementing Day 8 on an FPGA.
The chip that I am targeting is the KV260 (arbitrarily).

# Algorithmic Details

## Background

The main algorithmic complexity for this problem is a minimum spanning tree (MST) algorithm.
Both parts can be easily solved once you find the MST of the vertices in Euclidean space.
In particular, part2 is simply to find the longest edge in the MST and return the product of x coordinates.

Note that I am assuming that all edge weights are distinct (so that the MST is unique).

First, I designed an implementation inspired by the P-RAM model.
The standard P-RAM model allows for concurrent reads and exclusive writes (CREW).

Suppose we have infinite processors in the CREW model.
Then, for example, taking the max of n elements requires O(log n) time.
The Boruvka's algorithm fits well here, as inherently there are O(log n) sequential *phases* of contracting the graph.

The Boruvka algorithm is straightforward using the fact of *safe* edges in a MST.
That is, supposing all edge weights are unique, an edge xy must be in the MST if and only if there is a partition (X, Y) of V where xy is the shortest edge spanning the partition.
Boruvka's algorithm builds an MST incrementally by adding safe edges (the edges that are the shortest spanning a partition (C, V - C) for C a connected component in the current graph).
The efficiency of this algorithm follows by the number of components being halved each phase (as there are at least n/2 unique edges that are added).
Therefore, there are O(log n) phases.

The key steps for a Boruvka phase are:

1. Identify the shortest edge exiting the component (for each component in parallel)
2. Update the component identifiers for each vertex according to the edges

## Software Implementation

For n = 1000, I modeled this problem as having n processors (one for each vertex).
The main data structure is just an array of the component ids for each vertex.

Finding the shortest edge exiting a component is embarassingly parallel, and is simply taking the min of O(n^2) edges.
With only n processors, this takes O(n + logn) = O(n) time to do the reduction.
Merging the component identifiers is much more interesting.

The more "faithful" implementation is in faithful.py, while a faster optimized version is in unfaithful.py

### Naive method

One straightforward way in a sequential implementation is to remap the endpoints of each edge according to the merge.
For example, edge (u, v) gets remaped to have endpoints (ComponentId(u), ComponentId(v)).
On a merge of ComponentIds x and y, you can just set them to have new id of min(x, y).

However, this method could have long dependency chains.
Consider for example we want to merge 1 into 2, 2 into 3, 3 into 4, .., n-1 into n.
Clearly, a single step of merging in parallel cannot set all the ids properly.
In this case, the optimal strategy is to match the edges and merge in log2(n) steps.

However, finding a good matching of vertices is not sufficient either!
For example, consider a star (merge 2 into 1, merge 3 into 1, ..., merge n into 1).
Then, this would need n iterations if we only do pairwise merges.

### Star Contractions

Notice that the merging the above example is easy -- if 1 is the head of a star and 2, 3, .., n are the tails, then we can just set all the ids for the tail as the id of the head.
As such, the star contraction merging strategy proceeds as follows:

1. From the forest of merge edges, find a set of disjoint stars
2. Merge each of the stars

A randomized implementation is sufficient here.
For each component, we roll a coin.
On heads, we mark the vertex as a "head" of a star, and on tails, we mark it as a "tail".
Then, we simply merge each of edges that go from tail to star (and discard the rest).

### Analysis

Let's now analyze the runtime of this algorithm (assuming n processors).
For each phase, we have O(n) time to identify the nearest edges for each component.
Then, it takes O(1) time to do the merging.
Moreover, the expected number of edges that we merge is at least half that of without the star contraction (because each edge has a 1/4th chance of having different endpoints), so we expect to decrease the number of nodes by at least 1/4, leading to an upper bound of expected log_{4/3}(n) iterations.
Therefore, this runs in O(n log n) time.

Note that with n^2 processors, this instead is only O(log^2(n)) time.

## Hardware Implementation

### Input Format

The input is 1000 points in 3-D space (with integer 18-bit coordinates).
Therefore, there are ~54 kilobit of input data, which I can store across 4 dual-port BRAM18s before spending the first few (1000/(4*2) = 125) cycles loading them into the registers.

### Finding the Shortest Outgoing Edge

I will do this in two stages:

1. Each vertex finds its shortest outgoing edge
2. Each component finds its shortest outgoing edge

Part 1 requires at least n iterations.
One way of implementing this is to have each vertex store the shortest outgoing edge in the first k vertices, and updating that upon each new vertex.

Part 2 only log2(n) iterations, as I can do a reduction across each component.
In particular, each vertex needs to take the minimum distance among all other vertices in its component.
To do this, I iterate through k=0, 1, ..., log_2(n), where for each iteration I reduce among vertex pair (v, v+2^k) (cyclical indexing)

1. if v and v+2^k are in different components, do nothing
2. otherwise, update the minimum outgoing edge

where by step k, then vertex v must have compared against v + 1, v + 2, .., v + 2^k - 1, so after step k (inductively) I will have compared against v + 1, ..., v + 2^{k+1} - 1.

The comparison targets are static and can be hardwired during synthesis.

### Updating the Output

I need to store the heaviest edge in the MST.
Therefore, all I need to do is do a reduction between each of the edges I found in the above step and store the maximum.

To determine when to stop, I simply need to check whether all nodes are in the same component.
To do this, again it is a log_2(n) step equality reduction among the component ids.

### Applying the Merge

Recall that we are proceeding with the star contraction strategy.
First, I need to hash the component ids to determine the heads and tails.
Then, for each vertex that goes from a tail->head, I update the component id to that of the tail's.

### Pseudorandom generation

TODO

### Analysis

Reading the input takes O(n) time (~125 cycles).
As in the software implementation, we expect to have at most log_{4/3}(n) phases (~24 phases), and with high probability it won't be much more than this (TODO: bound this probability using Chernoff).

Let us now analyze the time per each phase.

1. n iterations (~1000 cycles) finding the vertex shortest outgoing edge.
2. log_2(n) iterations (~10 cycles) reducing to the component shortest edge.
3. 1-2 cycles to do the merging
4. log_2(n) iterations (~10 cycles) reducing to update the longest edge (for the output).

This leads to about 25,000 cycles in total.

# Results

## Software Parallelism

With vectorization via numpy, the code ran about 8 times faster compared to a standard library implementation of the MST algorithm.
Of course, there are many ways to improve the software (as I am doing a lot of redundant work as I am assuming n processors), but the results are promising.

## Hardware Parallelism

TODO; make sure this synthesizes.
TODO; evaluate the number of cycles it takes on my input.

# Further Improvements

## Contractions (once again)

The star contractions was partly chosen for simplicity (avoiding a sequential dependency chain).
Instead, sequentially merging ALL of the edges would more than halve the number of iterations!

There are several problems here.

First is that we don't know how long the dependency chain is.
As described above, for a chain (merging 2 into 1, 3 into 2, 4 into 3, ...) a naive implementation will take O(n) iterations!
However, there is significant advantage to doing so (as the time for a phase is completely dominated by the distance finding).

Testing this in software, even the above naive implementation (of repeating the contraction along the edge until it converges) only takes at most 5 iterations per phase.
Moreover, it cut the software runtime by about a half.

Now, let us explore the results in the hardware simulation.
TODO

## Shortest Outgoing Edges

The bottleneck is in the reduction to find the minimum outgoing edge for each vertex.
The conceptually simplest way to improve this is to do this minimum outgoing edge *per component*, so each further step becomes faster and faster (with less and less components).

I did not have time to explore this in more detail.

## The Geometry of Euclidean Space

Here, the only place I used the Euclidean MST was in avoiding the O(n^2) cost of storing an adjacency list.
There are many more optimizations to do in Euclidean space, many of which can be seen in [Prokopenko et al](https://dl.acm.org/doi/fullHtml/10.1145/3545008.3546185).
