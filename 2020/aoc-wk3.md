---
date: 2020-12-13
tags: programming aoc haskell
---

# Advent of Code 2020 — Week 3

I'm solving the [Advent of Code 2020](https://adventofcode.com/2020/) in the Haskell REPL (GHCi). You can copy the code and paste it in GHCi to play with it. Here are my solutions for week 3 (Dec 13–19):

- [Day 13](2020/aoc-wk3#day-13)

## Day 13

Problem: <https://adventofcode.com/2020/day/13>

Solution:

```haskell
import Data.List.Split (splitOn)
input <- lines <$> readFile "/tmp/input13"
startT = read (input !! 0) :: Int
busIds =  map read . filter (/= "x") . splitOn "," $ input !! 1 :: [Int]
:{
(t, busId) =
  head [ (x, busId) | x <- [startT..], busId <- busIds, x `mod` busId == 0 ]
:}
(t - startT) * busId -- part 1
import Data.Maybe (catMaybes)
:{
busIds@((_,jump):_) = catMaybes
  . zipWith (\i x -> if x == "x" then Nothing else Just $ (i, read x)) [0..]
  . splitOn ","
  $ input !! 1 :: [(Int, Int)]
:}
:{
step (jump, t) (inc, bid) =
  ( jump * bid
  , head [ t' | i <- [1..], let t' = t + jump * i, (t' + inc) `mod` bid == 0 ] )
:}
snd $ foldl step (jump, 0) $ tail busIds -- part 2
```
