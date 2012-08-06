# Dijkstra in Clojure

This was implemented twice.  The implementations differ in how they determine which node to explore next (that is, which node has the shortest distance, so far).

## with_array_walk.clj

Uses an array walk to find the next node to explore.

Time complexity:

O(|V|^2)
|V| = number of vertices in graph

## with_fibonacci_heap.clj

Uses a Fibonacci heap to find the next node to explore.

Fibonacci heaps were developed for use with Dijkstra in 1984 by Fredman and Tarjan (http://www.computer.org/portal/web/csdl/doi/10.1109/SFCS.1984.715934).

Time complexity:

O(|E| + |V|log|V|)
|E| = number of edges in graph
|V| = number of vertices in graph

## Outcome

In fact, the Fibonacci heap implementation takes about twice as long to run as the array walk implementation.  It is suspected that this is because the Fibonacci heap implementation does a heap walk to find each node for which a shorter distance has just been found.

If the implementation had used a language with mutable data, it would have been possible to store references that linked nodes in the graph data structure to their corresponding nodes in the heap.  However, with Clojure's immutable data, this seemed impossible.  Zippers are fine to traverse individual data structures (and this is what is used to traverse the graph and the heap), but they cannot traverse across data structures.
