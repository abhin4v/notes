---
date: 2020-12-16
tags: programming aoc haskell
---

# Advent of Code 2020 — Week 3

I'm solving the [Advent of Code 2020](https://adventofcode.com/2020/) in the Haskell REPL (GHCi). You can copy the code and paste it in GHCi to play with it. Here are my solutions for week 3 (Dec 13–19):

- [Day 13](2020/aoc-wk3#day-13)
- [Day 14](2020/aoc-wk3#day-14)
- [Day 15](2020/aoc-wk3#day-15)
- [Day 16](2020/aoc-wk3#day-16)

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

## Day 14

Problem: <https://adventofcode.com/2020/day/14>

Solution:

```haskell
-- First, copy-paste the parser framework from day 7
-- Next, parse the input:
data Ins = SetMask String | SetMem Int Int deriving (Show)
mask = SetMask <$> (string "mask = " *> some (char 'X' <|> char '1' <|> char '0'))
mem = SetMem <$> (string "mem[" *> num) <*> (string "] = " *> num)
program = (mask <|> mem) `separatedBy` char '\n'
Just prog <- flip runParser program <$> readFile "/tmp/input14"

-- Next, solve the problem:
import qualified Data.IntMap.Strict as M
import Data.Bits (setBit, clearBit, testBit)
SetMask startMask = head prog
:{
run1 (mask, mem) = \case
  SetMask mask' -> (mask', mem)
  SetMem index value -> (mask, M.insert index (applyMask value) mem)
  where
    applyMask value = foldl overrideBits value bitsToSet
    bitsToSet = filter ((/= 'X') . snd) . zip [0..] $ reverse mask
    overrideBits x (i, b) = if b == '0' then clearBit x i else setBit x i
:}
runProg run = sum . M.elems . snd . foldl run (startMask, M.empty) $ tail prog
runProg run1 -- part 1

:{
float = \case { 'X' -> ['1', '0']; x -> [x] }
expand [x] = [[x'] | x' <- float x]
expand (x:xs) = [(x':xs') | xs' <- expand xs, x' <- float x]

run2 (mask, mem) = \case
  SetMask mask' -> (mask', mem)
  SetMem index value ->
    (mask, foldl (\m i -> M.insert i value m) mem $ applyMask index)
  where
    applyMask = map fromBitString
      . expand
      . zipWith (\m b -> if m == '0' then b else m) mask
      . toBitString
    toBitString x = map (\i -> if testBit x i then '1' else '0') [35,34..0]
    fromBitString = foldl (\x (i, b) -> if b == '1' then setBit x i else x) 0
      . zip [0..]
      . reverse
:}
runProg run2 -- part 2
```

## Day 15

Problem: <https://adventofcode.com/2020/day/15>

Solution:

```haskell
:set -XLambdaCase
:set -XStrict
import qualified Data.Vector.Unboxed.Mutable as V
import Control.Monad (forM_)
:{
play lastSaid (now:rest) n lastSaidTime =
  if n == now
  then return lastSaid
  else do
    lt <- V.read lastSaidTime lastSaid
    V.write lastSaidTime lastSaid (now - 1)
    if lt == -1
      then play 0 rest n lastSaidTime
      else play (now - 1 - lt) rest n lastSaidTime
:}
input = [8,13,1,0,18,9]
start = length input + 1
lastSaidTime <- V.replicate 2020 (-1) :: IO (V.IOVector Int)
forM_ (zip input [1..]) $ \(x, i) -> V.write lastSaidTime x i
play (last input) [start..] 2021 lastSaidTime >>= print -- part1

lastSaidTime <- V.replicate 30000000 (-1) :: IO (V.IOVector Int)
forM_ (zip input [1..]) $ \(x, i) -> V.write lastSaidTime x i
play (last input) [start..] 30000001 lastSaidTime >>= print -- part2
```

## Day 16

Problem: <https://adventofcode.com/2020/day/16>

Solution:

```haskell
-- First, copy-paste the parser framework from day 7
-- Next, parse the input:
import Data.List.Split (splitOn)
input <- splitOn "\n\n" <$> readFile "/tmp/input16"
fieldValP = (,) <$> (num <* char '-') <*> num
fieldValsP = fieldValP `separatedBy` string " or "
fieldNameP = word `separatedBy` space
:{
fieldSpecP = (\name val -> (concat name, val))
  <$> (fieldNameP <* string ": ") <*> fieldValsP
:}
fieldSpecsP = fieldSpecP `separatedBy` char '\n'
Just fieldSpecs = runParser (input !! 0) fieldSpecsP
myTicket = map read . splitOn "," . (!! 1). lines $ input !! 1 :: [Int]
nearbyTickets = map (map read . splitOn ",") . tail . lines $ (input !! 2) :: [[Int]]

-- Next, solve the problem:
:{
invalidFields = filter $ \val ->
  not $ any (\(_, ranges) -> any (\(x, y) -> val >= x && val <= y) ranges) fieldSpecs
:}
sum $ concatMap invalidFields nearbyTickets -- part 1

validNearbyTickets = filter (null . invalidFields) nearbyTickets
import Data.List (transpose, nub, isPrefixOf)
:{
fieldPossibilities = zip [0..]
  . map (\vals -> nub . map fst $ filter (allValsInRange vals) fieldSpecs)
  $ transpose validNearbyTickets
  where
    allValsInRange vals (name, ranges) =
      all (\val -> any (\(x, y) -> val >= x && val <= y) ranges) vals
prune possibilities =
  if sum (map (length . snd) possibilities) == length possibilities
  then map (fmap head) possibilities
  else let fixedPossibilities = filter ((== 1) . length . snd) possibilities
           prunedPossibilities = flip map possibilities $ \(id, fns) ->
             if id `elem` map fst fixedPossibilities
             then (id, fns)
             else (id, filter (`notElem` concatMap snd fixedPossibilities) fns)
       in prune prunedPossibilities
part2 = product
  . map ((myTicket !!) . fst)
  . filter (("departure" `isPrefixOf`) . snd)
  $ prune fieldPossibilities
:}
```
