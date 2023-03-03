---
date: 2022-12-02
tags: haskell aoc programming
---

# Solving Rock-Paper-Scissors in Type-level Haskell

Let's solve part 1 of today's Advent of Code [challenge](https://adventofcode.com/2022/day/2) "Rock Paper Scissors" in type-level Haskell.

Instead of using term-level programming as we usually do, we make Haskell's type system do the work of
calculating the solution. So the solution is known right after we compile the program, and we do not even need to run the compiled program.

Here goes the code:

```haskell
{-# LANGUAGE DataKinds, TypeFamilies, TypeApplications #-}
{-# LANGUAGE TypeOperators, UndecidableInstances #-}

module Main where

import Data.Proxy
import GHC.TypeLits

data Move = Rock | Paper | Scissors
data Result = Lose | Draw | Win

type family Parse (move :: Symbol) :: Move where
   Parse "A"  = Rock
   Parse "B"  = Paper
   Parse "C"  = Scissors
   Parse "X"  = Rock
   Parse "Y"  = Paper
   Parse "Z"  = Scissors
   Parse move = TypeError (Text "Invalid move" :<>: ShowType move)

type family Play (opMove :: Move) (myMove :: Move) :: Result where
  Play Rock     Scissors = Lose
  Play Scissors Paper    = Lose
  Play Paper    Rock     = Lose
  Play Rock     Rock     = Draw
  Play Scissors Scissors = Draw
  Play Paper    Paper    = Draw
  Play _        _        = Win

type family ScoreMove (move :: Move) :: Nat where
  ScoreMove Rock     = 1
  ScoreMove Paper    = 2
  ScoreMove Scissors = 3

type family ScoreResult (result :: Result) :: Nat where
  ScoreResult Lose = 0
  ScoreResult Draw = 3
  ScoreResult Win  = 6

type family Score (moves :: (Move, Move)) :: Nat where
  Score '(opMove, myMove) = ScoreMove myMove + ScoreResult (Play opMove myMove)

type family Solve (input :: [(Symbol, Symbol)]) :: Nat where
  Solve '[] = 0
  Solve ('(opMove, myMove) : rest) = Score '(Parse opMove, Parse myMove) + Solve rest

type Input = [
    '("A", "Y"),
    '("B", "X"),
    '("C", "Z")
  ]

type Solution = Solve Input

solution = natVal $ Proxy @Solution

main = print solution
```
<center><em>tl-rps.hs</em></center>

The above code solves the challenge for the sample input, but it works for the real input as well (which is thousands of lines long). We can compile the program like this:

```
$ ghc -O2 --make -freduction-depth=0 tl-rps.hs
```

Running the output binary gives us the right result:
```
$ ./tl-rps
15
```

To verify that the solution is known at compile time, we can compile the program with `-ddump-simpl` flag to dump the simplifier output, and inspect it manually.

```
$ ghc -O2 -c -freduction-depth=0 -ddump-simpl tl-rps.hs
```

Right at the top of the simplifier output, we find this:

```haskell
solution :: Integer
[GblId,
 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 100 0}]
solution = 15
```

Alternatively, we can load the code in GHCi, and inspect the kind of the `Solution` type:

```
$ ghci -freduction-depth=0 tl-rps.hs
GHCi, version 9.0.2: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( tl-rps.hs, interpreted )
Ok, one module loaded.
λ> :kind! Solution
Solution :: Nat
= 15
```

This proves that the solution has already been calculated at compile time.

To understand what the code does, read my [blog post](https://abhinavsarkar.net/posts/type-level-haskell-aoc7/) about type-level Haskell solution of one of last year's Advent of Code challenges. Or better yet, learn some type-level programming in Haskell by reading the book [Thinking with Types](https://thinkingwithtypes.com/) by [Sandy Maguire](https://sandymaguire.me/).

Like, repost, or reply to this note on [Fediverse](https://fantastic.earth/@abnv/109445431721468848).
