---
date: 2020-12-21
tags: programming aoc haskell
---

# Advent of Code 2020 — Week 4

I'm solving the [Advent of Code 2020](https://adventofcode.com/2020/) in the Haskell REPL (GHCi). You can copy the code and paste it in GHCi to play with it. Here are my solutions for week 4 (Dec 20–25):

- [Day 20](2020/aoc-wk4#day-20)
- [Day 21](2020/aoc-wk4#day-21)

## Day 20

Problem: <https://adventofcode.com/2020/day/20>

Solution:

```haskell
:set -XLambdaCase
import qualified Data.Array.BitArray as BA
c2b = \case { '#' -> True; _ -> False }
b2c = \case { True -> '•'; False -> ' ' }
data Tile = Tile { tId :: Int, tSize :: Int, tArr :: BA.BitArray (Int, Int) }
import Data.List (intercalate, intersperse, delete)
import Data.Function (on)
:{
instance Show Tile where
  show (Tile _ size tile) = intercalate "\n" [
    intersperse ' ' [b2c $ tile BA.! (i, j) | j <- [0..size-1]] | i <- [0..size-1]]
instance Eq Tile where
  (==) = (==) `on` tId
readTile id ti = let size = length ti
  in Tile id size . BA.listArray ((0,0), (size-1, size-1)) . map c2b . concat $ ti
:}
:{
transform f (Tile id size tile) =
  Tile id size $ BA.ixmap (BA.bounds tile) (f size) tile
transforms = [ id
  , transform $ \s (x, y) -> (s-1-x, y)
  , transform $ \s (x, y) -> (x, s-1-y)
  , transform $ \s (x, y) -> (s-1-x, s-1-y)
  , transform $ \_ (x, y) -> (y, x)
  , transform $ \s (x, y) -> (s-1-y, x)
  , transform $ \s (x, y) -> (y, s-1-x)
  , transform $ \s (x, y) -> (s-1-y, s-1-x)
  ]
:}
import qualified Data.Map.Strict as Map
import Data.List.Split (splitOn)
:{
tiles <- map ((\(x:xs) -> readTile (read . take 4 . drop 5 $ x) xs) . lines)
  . init . splitOn "\n\n" <$> readFile "/tmp/input20" :: IO [Tile]
:}
gridSize = floor . sqrt . fromIntegral . length $ tiles
tileSize = tSize $ head tiles

topBorder (Tile _ size tile) = [tile BA.! (0,j) | j <- [0..size-1]]
bottomBorder (Tile _ size tile) = [tile BA.! (size-1,j) | j <- [0..size-1]]
leftBorder (Tile _ size tile) = [tile BA.! (i,0) | i <- [0..size-1]]
rightBorder (Tile _ size tile) = [tile BA.! (i,size-1) | i <- [0..size-1]]
matchLeftBorder leftTile tile = rightBorder leftTile == leftBorder tile
matchTopBorder topTile tile = bottomBorder topTile == topBorder tile
import Control.Monad (guard)
:{
solve :: [Tile] -> Map.Map (Int, Int) Tile
solve tiles = head $ solve' 0 Map.empty tiles
  where
    solve' _ grid [] = return grid
    solve' count grid tiles = do
      let (row, col) = count `divMod` gridSize
          leftTile = grid Map.! (row, col-1)
          topTile = grid Map.! (row-1, col)
      tile <- tiles
      tTile <- map ($ tile) transforms
      guard $ case (row, col) of
        (0, 0) -> True
        (0, _) -> matchLeftBorder leftTile tTile
        (_, 0) -> matchTopBorder topTile tTile
        _  ->  matchLeftBorder leftTile tTile && matchTopBorder topTile tTile
      solve' (count + 1) (Map.insert (row, col) tTile grid) (delete tile tiles)
:}
grid = solve tiles
:{
part1 = product . map (tId . (grid Map.!)) $
  [(0, 0), (0, gridSize-1), (gridSize-1, 0), (gridSize-1, gridSize-1)]
:}

:{
removeBorder (Tile id size tile) =
  [((x-1,y-1), b) | ((x,y), b) <- BA.assocs tile
                  , x /= 0, y /= 0, x /= size - 1, y /= size - 1]
:}
newTileSize = tileSize - 2
imageSize = gridSize * newTileSize
:{
removeBorderAndReindex ((x,y), tile) =
  map (\((x',y'), b) -> ((x * newTileSize + x', y * newTileSize + y'), b))
  $ removeBorder tile
image = Tile 0 imageSize . BA.array ((0,0),(imageSize - 1, imageSize - 1))
  $ concatMap removeBorderAndReindex
  $ Map.assocs grid
:}
:{
monster = [ "                  # "
          , "#    ##    ##    ###"
          , " #  #  #  #  #  #   " ]
:}
monWidth = length monster
monLength = length $ head monster
:{
cutouts (Tile _ size tile) = [
    [tile BA.! (i + si, j + sj) | i <- [0..monWidth-1], j <- [0..monLength-1]]
    | si <- [0 .. size - monWidth - 2], sj <- [0 .. size - monLength - 2]]
:}
monsterSig = map c2b $ concat monster
matchMonsterSig = (== monsterSig) . zipWith (&&) monsterSig
:{
(imageWithMonster, monsterCount) = head . filter ((> 1) . snd)
  . map (\transform -> let tImage = transform image
      in (tImage, length . filter matchMonsterSig . cutouts $ tImage))
  $ transforms
:}
monsterBodySize = length $ filter id monsterSig
part2 = BA.popCount (tArr imageWithMonster) - monsterBodySize * monsterCount
```

## Day 21

Problem: <https://adventofcode.com/2020/day/21>

Solution:

```haskell
import qualified Text.ParserCombinators.ReadP as P
import Data.Char (isAlpha)
import qualified Data.Map.Strict as Map
import Data.List (intersect, nub)
data Food = Food { fIngs :: [String], fAlrs :: [String] } deriving (Show)
word = P.many1 (P.satisfy isAlpha)
:{
foodP = Food
  <$> (word `P.sepBy` P.char ' ')
  <*> (P.string " (contains "
       *> (word `P.sepBy` P.string ", ")
       <* P.char ')' <* P.eof)
:}
parse = fst . head . P.readP_to_S foodP
foods <- map parse . lines <$> readFile "/tmp/input21"
:{
alrToIngsMap = foldl
  (\m (Food ings alrs) ->
      foldl (\m' alr -> Map.insertWith intersect alr ings m') m alrs)
  Map.empty foods
:}
ingsWithAlrs = nub $ concat $ Map.elems alrToIngsMap
length $ filter (`notElem` ingsWithAlrs) $ concat $ map fIngs foods -- part 1

import Data.List (sortBy, intercalate, (\\))
import Data.Ord (comparing)
:{
prune possibilities =
  if all (== 1) . map length $ Map.elems possibilities
  then Map.map head possibilities
  else let fixedPossibilities =
             concat $ Map.elems $ Map.filter ((== 1) . length) possibilities
           prunedPossibilities = flip Map.map possibilities $ \ings ->
             if length ings == 1 then ings else ings \\ fixedPossibilities
       in prune prunedPossibilities
:}
-- part 2
intercalate "," . map snd . sortBy (comparing fst) . Map.assocs $ prune alrToIngsMap
```
