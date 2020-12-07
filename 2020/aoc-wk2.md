---
date: 2020-12-07
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

## Day 7

Problem: <https://adventofcode.com/2020/day/7>

Solution:

First, a parser framework from (almost) scratch:

```haskell
> :set -XGeneralizedNewtypeDeriving
> :set -XLambdaCase
> import Control.Monad.State.Strict (MonadState(..), StateT, runStateT)
> import Control.Monad.Trans.Maybe (MaybeT(..))
> import Control.Monad.Identity (Identity (..), runIdentity)
> import Control.Applicative (Alternative(..), optional)
> :{
| newtype Parser i o = Parser { runParser_ :: StateT i (MaybeT Identity) o }
|   deriving (Functor, Applicative, Alternative, Monad , MonadState i)
| :}
> :{
| runParser input =
|   fmap fst . runIdentity . runMaybeT . flip runStateT input . runParser_
| :}
> :{
| satisfy :: (a -> Bool) -> Parser [a] a
| satisfy pr = get >>= \case { (c:cs) | pr c -> put cs >> return c; _ -> empty }
| :}
> char c = satisfy (== c)
> string = \case { "" -> pure ""; (c:cs) -> (:) <$> char c <*> string cs }
> import Data.Char (digitToInt, isDigit, isAlpha)
> digit = digitToInt <$> satisfy isDigit
> num = foldl1 (\num d -> num * 10 + d) <$> some digit
> space = char ' '
> word = some $ satisfy isAlpha
> separatedBy v s = (:) <$> v <*> many (s *> v) <|> pure []
```

Now, use the parser framework to solve the problem:

```haskell
> color = (++) <$> (word <* space) <*> word
> bags = string " bag" <* optional (char 's')
> :{
| rule = (,)
|  <$> (color <* bags <* string " contain ")
|  <*> (string "no other bags." *> pure []
|      <|> ((((,) <$> num <*> (space *> color <* bags)) `separatedBy` string ", ")
|          <* char '.'))
| :}
> content <- lines <$> readFile "/tmp/input7"
> import Data.Maybe (fromJust, fromMaybe)
> import qualified Data.Map.Strict as Map
> import Data.List (nub)
> rules = map (fromJust . flip runParser rule) content
> colorDeps = Map.fromList $ map (fmap $ map snd) rules
> :{
| revColorDeps = Map.foldlWithKey'
|   (\m k -> foldl (\m' v -> Map.insertWith (++) v [k] m') m)
|   Map.empty colorDeps
| :}
> :{
| canContainColor color =
|   let colors = fromMaybe [] $ Map.lookup color revColorDeps
|   in nub $ colors ++ concatMap canContainColor colors
| :}
> length $ canContainColor "shinygold" -- part1
> :{
| bagCount color =
|   let colors = fromMaybe [] $ lookup color rules
|   in sum (map fst colors) + sum (map (\(c, col) -> c * bagCount col) colors)
| :}
> bagCount "shinygold" -- part 2
```
