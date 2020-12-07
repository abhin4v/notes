---
date: 2020-12-07
tags: programming aoc haskell
---

# Advent of Code 2020 — Week 2

I'm solving the [Advent of Code 2020](https://adventofcode.com/2020/) in the Haskell REPL (GHCi). You can copy the code and paste it in GHCi to play with it. Here are my solutions for week 2 (Dec 6–12):

## Day 6

Problem: <https://adventofcode.com/2020/day/6>

Solution:

```haskell
import Data.List.Split (splitOn)
answers <- map words . splitOn "\n\n" <$> readFile "/tmp/input6"
import Data.List (nub)
sum $ map (length . nub . concat) answers -- part 1
import Data.List (intersect)
sum $ map (length . foldl1 intersect) answers -- part 2
```

## Day 7

Problem: <https://adventofcode.com/2020/day/7>

Solution:

```haskell
-- First, a parser framework from (almost) scratch:
:set -XGeneralizedNewtypeDeriving
:set -XLambdaCase
import Control.Monad.State.Strict (MonadState(..), StateT, runStateT)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Identity (Identity (..), runIdentity)
import Control.Applicative (Alternative(..), optional)
:{
newtype Parser i o = Parser { runParser_ :: StateT i (MaybeT Identity) o }
  deriving (Functor, Applicative, Alternative, Monad , MonadState i)
runParser input =
  fmap fst . runIdentity . runMaybeT . flip runStateT input . runParser_
satisfy :: (a -> Bool) -> Parser [a] a
satisfy pr = get >>= \case { (c:cs) | pr c -> put cs >> return c; _ -> empty }
:}
char c = satisfy (== c)
string = \case { "" -> pure ""; (c:cs) -> (:) <$> char c <*> string cs }
import Data.Char (digitToInt, isDigit, isAlpha)
digit = digitToInt <$> satisfy isDigit
num = foldl1 (\num d -> num * 10 + d) <$> some digit
space = char ' '
word = some $ satisfy isAlpha
separatedBy v s = (:) <$> v <*> many (s *> v) <|> pure []

-- Next, use the parser framework to parse the input:
color = (++) <$> (word <* space) <*> word
bags = string " bag" <* optional (char 's')
container = color <* bags <* string " contain "
:{
contained = string "no other bags" *> pure []
  <|> ((,) <$> num <*> (space *> color <* bags)) `separatedBy` string ", "
:}
rule = (,) <$> container <*> contained <* char '.'
rules = rule `separatedBy` char '\n'
Just bagRules <- flip runParser rules <$> readFile "/tmp/input7"

-- Finally, solve the problem:
import Data.Graph.Wrapper (fromListSimple, transpose, reachableVertices)
colorDeps = transpose . fromListSimple $ map (fmap $ map snd) bagRules
length (reachableVertices colorDeps "shinygold") - 1 -- part 1
import Data.Maybe (fromMaybe)
:{
bagCount color =
  let colors = fromMaybe [] $ lookup color bagRules
  in sum (map fst colors) + sum (map (\(c, col) -> c * bagCount col) colors)
:}
bagCount "shinygold" -- part 2
```
