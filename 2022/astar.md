---
date: 2022-12-01
tags: algorithm haskell programming
---

# A* Algorithm in Haskell

The start of the [Advent of code](https://adventofcode.com/) today reminded me of the
[A* algorithm](https://en.wikipedia.org/wiki/A*_search_algorithm), which I often find myself using
for graph [pathfinding](https://en.wikipedia.org/wiki/Pathfinding) related problems.

If there is a heuristic function known that estimates the cost of the cheapest path from a node in
the graph to the goal node, then A* can perform better than other search algorithms like
[Breadth-first Search](https://en.wikipedia.org/wiki/Breadth-first_search) and
[Best-first Search](https://en.wikipedia.org/wiki/Best-first_search)
by cutting down on the number of nodes visited.

So without further ado, here is my well-commented implementation of the A* algorithm in Haskell[^fn1][^fn2]:

[^fn1]: I use the Set and Map container data structures from the [containers](https://hackage.haskell.org/package/containers) library, and the minimum priority queue data structure from the the [pqueue](https://hackage.haskell.org/package/pqueue) library.

[^fn2]: `ScopedTypeVariables` extension is needed here to write the type signature of the `astar'` function. We can omit the signature without any loss of functionality, and then we can remove the extension as well.

```haskell
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

module AStar (astar) where

import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as Set

astar ::
  forall node cost. (Ord node, Ord cost, Num cost) =>
  -- | The start node.
  node ->
  -- | The goal node.
  node ->
  -- | The function to get the next nodes and their costs from a given node.
  (node -> [(node, cost)]) ->
  -- | The heuristic function to estimate the cost of going from a given node to
  --   the goal node.
  (node -> node -> cost) ->
  -- | Returns Nothing if no path found.
  --   Else returns Just (path cost, path as a list of nodes).
  Maybe (cost, [node])
astar startNode goalNode nextNodes heuristic =
  astar'
    (PQ.singleton (heuristic startNode goalNode) (startNode, 0))
    Set.empty
    (Map.singleton startNode 0)
    Map.empty
  where
    astar' ::
      -- | The set of discovered nodes that need to be visited, stored
      --   in a min-priority queue prioritized by sum of costs of reaching to
      --   the nodes from the start node, and heuristic costs of reaching
      --   from the nodes to the goal node.
      PQ.MinPQueue cost (node, cost) ->
      -- | The set of already visited nodes.
      Set.Set node ->
      -- | The map of visited or discovered nodes to the currently known minimum
      --   costs from the start node to the nodes.
      Map.Map node cost ->
      -- | The map of visited nodes to the previous nodes in the currently known
      --   best path from the start node.
      Map.Map node node ->
      -- | Returns Nothing if no path found.
      --   Else returns Just (path cost, path as a list of nodes).
      Maybe (cost, [node])
    astar' !discovered !visited !minCosts tracks
      -- If the discovered set is empty then the search has failed. Return Nothing.
      | PQ.null discovered = Nothing
      -- If the current node is the goal node then return the current node cost and
      -- path to the current node constructed from the tracks.
      | node == goalNode = Just (cost, findPath tracks node)
      -- If the current node has already been visited then discard it and continue.
      | node `Set.member` visited =
          astar' discoveredSansCurrent visited minCosts tracks
      -- Else visit the current node and continue.
      | otherwise =
          let
            -- Add the current node to the visited set.
            visited' = Set.insert node visited
            -- Find the successor nodes of the current node that have not been
            -- visited yet, along with their costs and heuristic costs.
            successors =
              [ (node', cost', heuristic node' goalNode)
                | (node', nodeCost) <- nextNodes node, -- Get next nodes.
                  node' `Set.notMember` visited', -- Keep only unvisited ones.
                  let cost' = cost + nodeCost, -- Cost of the next node.
                  -- Keep only unvisited nodes, or previously visited nodes now
                  -- discovered via less costly paths.
                  node' `Map.notMember` minCosts || cost' < minCosts Map.! node'
              ]

            -- Insert the successors in the discovered set.
            discovered' = foldl' (\q (n, c, h) -> PQ.insert (c + h) (n, c) q)
                discoveredSansCurrent successors
            -- Insert the successor costs in the minimum cost map.
            minCosts' = foldl' (\m (n, c, _) -> Map.insert n c m) minCosts successors
            -- Insert the tracks of the successors.
            tracks' = foldl' (\m (n, _, _) -> Map.insert n node m) tracks successors

            -- Continue via recursion.
          in astar' discovered' visited' minCosts' tracks'
      where
        -- Get (and delete) the node with minimum cost and its cost from the
        -- discovered set.
        ((_, (node, cost)), discoveredSansCurrent) = PQ.deleteFindMin discovered

    -- Construct the path of the given node from the start node using the
    -- recorded tracks.
    findPath tracks node =
      if Map.member node tracks
        then findPath tracks (tracks Map.! node) ++ [node]
        else [node]
```

That's it for this short post. Happy pathfinding!

Like, repost, or reply to this note on [Fediverse](https://fantastic.earth/@abnv/109438819806774018){:class="mastodon-link"}.
