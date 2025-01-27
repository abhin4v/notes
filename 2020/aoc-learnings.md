---
date: 2020-12-26
tags: programming advent-of-code haskell
---

# Learnings from Solving Advent of Code 2020 in Haskell

After many years of trying unsuccessfully, I finally completed all 25 days of the [Advent of Code 2020](https://adventofcode.com/2020/) in Haskell. Here is a summary of my learnings and solutions.

## Learnings

- GHCi is a powerful REPL. We can do almost anything in it which we can do in a file. It is also fast and great to play with code.
- [Zippers](http://learnyouahaskell.com/zippers) are an awesome technique to move around in a data structure. We can also think of them as focus points in spaces like lines, plains or 3D volumes. Many AoC problems are about moving around in space, doing things at the focus points. Zippers are quite suitable for such problems.
- [Data.List.Split](https://hackage.haskell.org/package/split/docs/Data-List-Split.html) module is good enough for basic input parsing.
- It is trivially easy to write a simple but feature-rich parser framework in Haskell. [Here](/2020/aoc-wk2#day-7) is one in its entirety, with some example parsers, in just 24 lines.
- [Data.Graph.Wrapper](https://hackage.haskell.org/package/graph-wrapper/docs/Data-Graph-Wrapper.html) is a useful wrapper over [Data.Graph](https://hackage.haskell.org/package/containers/docs/Data-Graph.html).
- Haskell is good for writing interpreters.
- Graph traversal + Memoization = Dynamic programming.
- Use [Data.Memotrie](https://hackage.haskell.org/package/MemoTrie/docs/Data-MemoTrie.html) for side-effect-free memoization in Haskell.
- Sometimes it's faster to recompute than to memoize because of the lazy nature of Haskell and the extra memory usage caused by memoization.
- [Comonads](https://hackage.haskell.org/package/comonad) are great to simulate [Cellular automata](https://en.wikipedia.org/wiki/Cellular_automaton). Zippers are comonads.
- Comonad based cellular automata do not mutate the state of the automata universe, neither do they compute and materialize the whole universe at every step of the automata. Rather, they just stack functions over functions to create new lazy views over the original universe. This means that we can have lazy infinite universes. This also means that simulating cellular automata using comonads tends to get slower with increasing number of neighbours/dimensions.
- Sometimes mutability is the only option if we want to implement a fast algorithm. Mutable vectors from the [vector](https://hackage.haskell.org/package/vector) library are great for this.
- Writing the [four-dimensional zipper comonad](/2020/aoc-wk3#day-17) from scratch is complex and takes a really long time.
- [There are no words](https://english.stackexchange.com/questions/56472/x-y-z-horizontal-vertical-and) similar to _horizontal_ and _vertical_ for three dimensions or more.
- [ReadP](https://hackage.haskell.org/package/base/docs/Text-ParserCombinators-ReadP.html) is a good, minimal and easy to use parser framework which is included in the Haskell standard library.
- Try to use [Bit arrays](https://en.wikipedia.org/wiki/Bit_array) when they fit, for performant solutions.
- Some problems, when scaled up, cannot be solved with lazy lists in a reasonable time.
- We can simulate a linked list of integers over a vector.
- If a program generates a lot of garbage, turning on multithreading (`-threaded`) and parallel garbage collection (`-qg0 -N`) may make it run faster.
- Tweaking the heap size (`-H`) and the allocation area size (`-A`) may make a program run faster.
- Use the [`Strict`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-Strict) extension cautiously. Sometimes it may unexpectedly make a program run slower.
- [Hexagons are the bestagons](https://www.youtube.com/watch?v=thOifuHs6eY).

## Solutions
Here's the index of all the solutions I wrote for AoC 2020:

Problem                                    | Solution                 | Salient points                   | Libraries/modules used
-------------------------------------------|--------------------------|----------------------------------|------------------------------------------
[1](https://adventofcode.com/2020/day/1)   | [↗](/2020/aoc-wk1#day-1)  | List comprehensions              | _None_
[2](https://adventofcode.com/2020/day/2)   | [↗](/2020/aoc-wk1#day-2)  | Validation                       | _None_
[3](https://adventofcode.com/2020/day/3)   | [↗](/2020/aoc-wk1#day-3)  | Zippers                          | _None_
[4](https://adventofcode.com/2020/day/4)   | [↗](/2020/aoc-wk1#day-4)  | Validation                       | [split](https://hackage.haskell.org/package/split)
[5](https://adventofcode.com/2020/day/5)   | [↗](/2020/aoc-wk1#day-5)  | Decoding                         | _None_
[6](https://adventofcode.com/2020/day/6)   | [↗](/2020/aoc-wk2#day-6)  | _None_                           | _None_
[7](https://adventofcode.com/2020/day/7)   | [↗](/2020/aoc-wk2#day-7)  | Parsing, graphs                  | [mtl](https://hackage.haskell.org/package/mtl), [graph-wrapper](https://hackage.haskell.org/package/graph-wrapper)
[8](https://adventofcode.com/2020/day/8)   | [↗](/2020/aoc-wk2#day-8)  | Parsing, interpreter             | [mtl](https://hackage.haskell.org/package/mtl)
[9](https://adventofcode.com/2020/day/9)   | [↗](/2020/aoc-wk2#day-9)  | _None_                           | _None_
[10](https://adventofcode.com/2020/day/10) | [↗](/2020/aoc-wk2#day-10) | Graphs, memoization              | _None_
[11](https://adventofcode.com/2020/day/11) | [↗](/2020/aoc-wk2#day-11) | Cellular automata, zippers       | [comonad](https://hackage.haskell.org/package/comonad), [Data.Sequence](https://hackage.haskell.org/package/containers/docs/Data-Sequence.html)
[12](https://adventofcode.com/2020/day/12) | [↗](/2020/aoc-wk2#day-12) | Geometry                         | _None_
[13](https://adventofcode.com/2020/day/13) | [↗](/2020/aoc-wk3#day-13) | Number theory                    | _None_
[14](https://adventofcode.com/2020/day/14) | [↗](/2020/aoc-wk3#day-14) | Parsing, interpreter             | [mtl](https://hackage.haskell.org/package/mtl)
[15](https://adventofcode.com/2020/day/15) | [↗](/2020/aoc-wk3#day-15) | Number sequence                  | [Data.Vector.Unboxed.Mutable](https://hackage.haskell.org/package/vector/docs/Data-Vector-Unboxed-Mutable.html)
[16](https://adventofcode.com/2020/day/16) | [↗](/2020/aoc-wk3#day-16) | Parsing, constraint satisfaction | [mtl](https://hackage.haskell.org/package/mtl)
[17](https://adventofcode.com/2020/day/17) | [↗](/2020/aoc-wk3#day-17) | Cellular automata, zippers       | [comonad](https://hackage.haskell.org/package/comonad), [Data.List](https://hackage.haskell.org/package/base/docs/Data-List.html)
[18](https://adventofcode.com/2020/day/18) | [↗](/2020/aoc-wk3#day-18) | Parsing, interpreter             | [mtl](https://hackage.haskell.org/package/mtl)
[19](https://adventofcode.com/2020/day/19) | [↗](/2020/aoc-wk3#day-19) | Parsing                          | [ReadP](https://hackage.haskell.org/package/base/docs/src/Text.ParserCombinators.ReadP.html)
[20](https://adventofcode.com/2020/day/20) | [↗](/2020/aoc-wk4#day-20) | Image manipulation               | [Data.Array.BitArray](https://hackage.haskell.org/package/bitwise/docs/Data-Array-BitArray.html)
[21](https://adventofcode.com/2020/day/21) | [↗](/2020/aoc-wk4#day-21) | Parsing, constraint satisfaction | [ReadP](https://hackage.haskell.org/package/base/docs/src/Text.ParserCombinators.ReadP.html)
[22](https://adventofcode.com/2020/day/22) | [↗](/2020/aoc-wk4#day-22) | Recursion, game                  | _None_
[23](https://adventofcode.com/2020/day/23) | [↗](/2020/aoc-wk4#day-23) | Linked list, game                | [Data.Vector.Primitive.Mutable](https://hackage.haskell.org/package/vector/docs/Data-Vector-Primitive-Mutable.html)
[24](https://adventofcode.com/2020/day/24) | [↗](/2020/aoc-wk4#day-24) | Parsing, cellular automata       | [ReadP](https://hackage.haskell.org/package/base/docs/src/Text.ParserCombinators.ReadP.html), [Map](https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html)
[25](https://adventofcode.com/2020/day/25) | [↗](/2020/aoc-wk4#day-25) | Cryptography                     | _None_
