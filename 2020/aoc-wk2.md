---
date: 2020-12-06
tags: programming aoc haskell
---

# Advent of Code 2020 — Week 2

## Day 6

Problem: <https://adventofcode.com/2020/day/6>

Solution:

```haskell
> import Data.List.Split (splitOn)
> answers <- map words . splitOn "\n\n" <$> readFile "/tmp/input6"
> import Data.List (nub)
> sum $ map (length . nub . concat) answers -- part 1
> import Data.List (intersect)
> sum $ map (length . foldl1 intersect) answers -- part 2
```
