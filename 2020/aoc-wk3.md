---
date: 2020-12-17
tags: programming aoc haskell
---

# Advent of Code 2020 — Week 3

I'm solving the [Advent of Code 2020](https://adventofcode.com/2020/) in the Haskell REPL (GHCi). You can copy the code and paste it in GHCi to play with it. Here are my solutions for week 3 (Dec 13–19):

- [Day 13](2020/aoc-wk3#day-13)
- [Day 14](2020/aoc-wk3#day-14)
- [Day 15](2020/aoc-wk3#day-15)
- [Day 16](2020/aoc-wk3#day-16)
- [Day 17](2020/aoc-wk3#day-17)

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

## Day 17

Problem: <https://adventofcode.com/2020/day/17>

This problem is too big to be solved in GHCi so I wrote the solution in a file and compiled with GHC with optimizations.

Solution:

```haskell
-- run with  +RTS -H8g -A64m -n4m -s -qg0 -N
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Comonad (Comonad(..))
import Data.List (foldl')

data Z a = Z [a] a [a]

zLeft, zRight :: Z a -> Z a
zLeft ~(Z (x:xs) f r) = Z xs x (f:r)
zRight ~(Z l f (x:xs)) = Z (f:l) x xs

iterate1 :: (a -> a) -> a -> [a]
iterate1 f = tail . iterate f

instance Functor Z where
  fmap f (Z l a r) = Z (fmap f l) (f a) (fmap f r)

instance Comonad Z where
  extract (Z _ f _) = f
  duplicate z = Z (iterate1 zLeft z) z (iterate1 zRight z)

newtype Z2 a = Z2 (Z (Z a))

z2Up, z2Down, z2Left, z2Right :: Z2 a -> Z2 a
z2Up (Z2 z) = Z2 (zLeft z)
z2Down (Z2 z) = Z2 (zRight z)
z2Left (Z2 z) = Z2 (fmap zLeft z)
z2Right (Z2 z) = Z2 (fmap zRight z)

instance Functor Z2 where
  fmap f (Z2 z) = Z2 (fmap (fmap f) z)

instance Comonad Z2 where
  extract (Z2 z) = extract . extract $ z
  duplicate = Z2 . fmap horizontal . vertical
    where
      horizontal z = Z (iterate1 z2Left z) z (iterate1 z2Right z)
      vertical z = Z (iterate1 z2Up z) z (iterate1 z2Down z)

newtype Z3 a = Z3 (Z (Z2 a))

z3Backward, z3Forward, z3Up, z3Down, z3Left, z3Right :: Z3 a -> Z3 a
z3Backward (Z3 z) = Z3 (zLeft z)
z3Forward (Z3 z) = Z3 (zRight z)
z3Up (Z3 z) = Z3 (fmap z2Up z)
z3Down (Z3 z) = Z3 (fmap z2Down z)
z3Left (Z3 z) = Z3 (fmap z2Left z)
z3Right (Z3 z) = Z3 (fmap z2Right z)

instance Functor Z3 where
  fmap f (Z3 z) = Z3 (fmap (fmap f) z)

instance Comonad Z3 where
  extract (Z3 z) = extract . extract $ z
  duplicate = Z3 . fmap (Z2 . fmap horizontal . vertical) . depthical
    where
    horizontal z = Z (iterate1 z3Left z) z (iterate1 z3Right z)
    vertical z = Z (iterate1 z3Up z) z (iterate1 z3Down z)
    depthical z = Z (iterate1 z3Backward z) z (iterate1 z3Forward z)

data State = Active | Inactive deriving (Show, Eq)

readInput :: [String] -> [[State]]
readInput = map (map (\case {'.'  -> Inactive; ~'#' -> Active}))

inactiveLine :: Z State
inactiveLine = Z (repeat Inactive) Inactive (repeat Inactive)

inactivePlain :: Z2 State
inactivePlain = Z2 $ Z (repeat inactiveLine) inactiveLine (repeat inactiveLine)

inputTo3dGrid :: [[State]] -> Z3 State
inputTo3dGrid input = let
    zs = map (\(x:xs) -> Z (repeat Inactive) x (xs ++ repeat Inactive)) input
    z2 = Z2 $ Z (repeat inactiveLine) (head zs) (tail zs ++ repeat inactiveLine)
  in Z3 $ Z (repeat inactivePlain) z2 (repeat inactivePlain)

mkMove :: (a -> a) -> (a -> a) -> Int -> a -> a
mkMove awayMove closeMove n
  | n == 0 = id
  | n < 0 = applyNTimes (abs n) awayMove
  | otherwise = applyNTimes n closeMove
  where
    applyNTimes n' f = foldr (.) id (replicate n' f)

to3dMove :: (Int, Int, Int) -> (Z3 a -> Z3 a)
to3dMove (x, y, z) =
  mkMove z3Left z3Right x
  . mkMove z3Up z3Down y
  . mkMove z3Backward z3Forward z

scanPoint :: Comonad w => w State -> Int
scanPoint grid = case extract grid of { Active -> 1; Inactive -> 0 }

scan :: Int -> (w a -> Int) -> (w a -> w a) -> w a -> Int
scan n lowerDimScanner move grid = fst $
  foldl' (\(activeCount, g) _ -> (lowerDimScanner g + activeCount, move g))
    (0, grid) [0..n-1]

scanVolume :: Int -> Z3 State -> Int
scanVolume n = scan n (scan n (scan n scanPoint z3Right) z3Down) z3Forward

neighbours3d :: (Int, Int) -> Z3 State -> Int
neighbours3d (start, end) =
  scanVolume (end - start + 1) . to3dMove (start, start, start)

type ActiveCount w = w State -> Int
type Rule w = w State -> State

mkRule :: Comonad w => ActiveCount w -> Rule w
mkRule activeCount grid =
  let focus = extract grid
      activeNs = activeCount grid - if focus == Active then 1 else 0
  in case focus of
    Active -> if activeNs `elem` [2, 3] then Active else Inactive
    Inactive -> if activeNs == 3 then Active else Inactive

simulate :: Comonad w => Int -> (w a -> a) -> w a -> w a
simulate cycles rule = (!! cycles) . iterate (extend rule)

finalActiveCount :: Comonad w => Int -> Rule w -> ActiveCount w -> w State -> Int
finalActiveCount cycles rule activeCount = activeCount . simulate cycles rule

newtype Z4 a = Z4 (Z (Z3 a))

z4Outward, z4Inward, z4Backward, z4Forward, z4Up, z4Down, z4Left, z4Right ::
  Z4 a -> Z4 a
z4Outward (Z4 z) = Z4 (zLeft z)
z4Inward (Z4 z) = Z4 (zRight z)
z4Backward (Z4 z) = Z4 (fmap z3Backward z)
z4Forward (Z4 z) = Z4 (fmap z3Forward z)
z4Up (Z4 z) = Z4 (fmap z3Up z)
z4Down (Z4 z) = Z4 (fmap z3Down z)
z4Left (Z4 z) = Z4 (fmap z3Left z)
z4Right (Z4 z) = Z4 (fmap z3Right z)

instance Functor Z4 where
  fmap f (Z4 z) = Z4 (fmap (fmap f) z)

instance Comonad Z4 where
  extract (Z4 z) = extract . extract $ z
  duplicate =
    Z4 . fmap (Z3 . fmap (Z2 . fmap horizontal . vertical) . depthical) . enclosical
    where
      horizontal z = Z (iterate1 z4Left z) z (iterate1 z4Right z)
      vertical z = Z (iterate1 z4Up z) z (iterate1 z4Down z)
      depthical z = Z (iterate1 z4Backward z) z (iterate1 z4Forward z)
      enclosical z = Z (iterate1 z4Outward z) z (iterate1 z4Inward z)

inputTo4dGrid :: [[State]] -> Z4 State
inputTo4dGrid input = let
    z3 = inputTo3dGrid input
    inactiveVolume =
      Z3 $ Z (repeat inactivePlain) inactivePlain (repeat inactivePlain)
  in Z4 $ Z (repeat inactiveVolume) z3 (repeat inactiveVolume)

to4dMove :: (Int, Int, Int, Int) -> (Z4 a -> Z4 a)
to4dMove (x, y, z, w) = mkMove z4Left z4Right x
  . mkMove z4Up z4Down y
  . mkMove z4Backward z4Forward z
  . mkMove z4Outward z4Inward w

scanHyperVolume :: Int -> Z4 State -> Int
scanHyperVolume n =
  scan n(scan n (scan n (scan n scanPoint z4Right) z4Down) z4Forward) z4Inward

neighbours4d :: (Int, Int) -> Z4 State -> Int
neighbours4d (start, end) =
  scanHyperVolume (end - start + 1) . to4dMove (start, start, start, start)

main :: IO ()
main = do
  let input = [ "..##.#.#"
              , ".#####.."
              , "#.....##"
              , "##.##.#."
              , "..#...#."
              , ".#..##.."
              , ".#...#.#"
              , "#..##.##"
              ]
      initialGridSize = length $ head input
      states = readInput input
      cycles = 6
      neighbours3d1 = neighbours3d (-1, 1)
      neighbours3dFinal = neighbours3d (-cycles, initialGridSize + cycles)
      neighbours4d1 = neighbours4d (-1, 1)
      neighbours4dFinal = neighbours4d (-cycles, initialGridSize + cycles)

  print
    $ finalActiveCount cycles (mkRule neighbours3d1) neighbours3dFinal
    $ inputTo3dGrid states
  print
    $ finalActiveCount cycles (mkRule neighbours4d1) neighbours4dFinal
    $ inputTo4dGrid states
```
