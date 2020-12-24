---
date: 2020-12-24
tags: programming aoc haskell
---

# Advent of Code 2020 — Week 4

I'm solving the [Advent of Code 2020](https://adventofcode.com/2020/) in the Haskell REPL (GHCi). You can copy the code and paste it in GHCi to play with it. Here are my solutions for week 4 (Dec 20–25):

- [Day 20](2020/aoc-wk4#day-20)
- [Day 21](2020/aoc-wk4#day-21)
- [Day 22](2020/aoc-wk4#day-22)
- [Day 23](2020/aoc-wk4#day-23)
- [Day 24](2020/aoc-wk4#day-24)

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

## Day 22

Problem: <https://adventofcode.com/2020/day/22>

Solution:

```haskell
:{
playCombat [] p2 = p2
playCombat p1 [] = p1
playCombat (x:xs) (y:ys) = if x > y
  then playCombat (xs ++ [x,y]) ys
  else playCombat xs (ys ++ [y,x])
:}
import Data.List.Split (splitOn)
:{
[player1, player2] <- map (map read . tail . lines) . splitOn "\n\n"
  <$> readFile "/tmp/input22" :: IO [[Int]]
:}
cards = playCombat player1 player2
sum $ zipWith (*) (reverse cards) [1..] -- part 1

import qualified Data.Set as Set
:{
playRecCombat _ [] p2 = Right p2
playRecCombat _ p1 [] = Left p1
playRecCombat seen p1@(x:xs) p2@(y:ys)
  | Set.member (p1, p2) seen = Left p1
  | x <= length xs && y <= length ys =
      case playRecCombat Set.empty (take x xs) (take y ys) of
        Left _ -> playRecCombat seen' (xs ++ [x,y]) ys
        Right _ -> playRecCombat seen' xs (ys ++ [y,x])
  | x > y = playRecCombat seen' (xs ++ [x,y]) ys
  | otherwise = playRecCombat seen' xs (ys ++ [y,x])
  where
    seen' = Set.insert (p1, p2) seen
:}
cards = either id id $ playRecCombat Set.empty player1 player2
sum $ zipWith (*) (reverse cards) [1..] -- part 2
```

## Day 23

Problem: <https://adventofcode.com/2020/day/23>

Solution:

```haskell
import Data.List (sortOn)
:{
dest picks left@(current:rest) = head
  . filter (`notElem` picks)
  . tail
  . dropWhile (> current)
  . cycle
  $ sortOn negate left
play (x:xs) = let
    (picks, left) = (take 3 xs, x:drop 3 xs)
    d = dest picks left
    (a,b) = span (/= d) left
  in tail a ++ [d] ++ picks ++ tail b ++ if not (null a) then [head a] else []
:}
input = [3,2,6,5,1,9,4,7,8]
:{
part1 = take (length input - 1)
  . drop 1
  . dropWhile (/= 1)
  . cycle
  $ iterate play input !! 100
:}

input = [3,2,6,5,1,9,4,7,8] ++ [10..1000000] :: [Int]
import qualified Data.Vector.Primitive.Mutable as MV
import Control.Monad (forM_)
cups <- MV.new 1000001 :: IO (MV.IOVector Int)
forM_ (zip input $ tail input ++ [head input]) $ uncurry (MV.unsafeWrite cups)

import Data.List ((\\))
:{
dest :: Int -> [Int] -> Int
dest current picks
  | d < min' = max'
  | d `notElem` picks = d
  | otherwise = dest d picks
  where
    d = current - 1
    max' = maximum $ [999997,999998,999999,1000000] \\ picks
    min' = minimum $ [1,2,3,4] \\ picks

play :: Int -> Int -> IO Int
play 0 _ = do
  a <- MV.unsafeRead cups 1
  b <- MV.unsafeRead cups a
  return $ a * b
play n current = do
  a <- MV.unsafeRead cups current
  b <- MV.unsafeRead cups a
  c <- MV.unsafeRead cups b
  let d = dest current [a,b,c]
  dnext <- MV.unsafeRead cups d
  cnext <- MV.unsafeRead cups c
  MV.unsafeWrite cups d a
  MV.unsafeWrite cups c dnext
  MV.unsafeWrite cups current cnext
  play (n - 1) cnext
:}
play 10000000 3 -- part 2
```

## Day 24

Problem: <https://adventofcode.com/2020/day/24>

Solution:

```haskell
:set -XLambdaCase
import qualified Text.ParserCombinators.ReadP as P
data Dir = E | W | NW | NE | SW | SE deriving Show
import Data.Functor (($>))
import Control.Applicative ((<|>))
:{
parser =  P.string "nw" $> NW
      <|> P.string "sw" $> SW
      <|> P.string "ne" $> NE
      <|> P.string "se" $> SE
      <|> P.char 'e' $> E
      <|> P.char 'w' $> W
:}
parse = fst . head . P.readP_to_S (P.many1 parser <* P.eof)
tileDirs <- map parse . lines <$> readFile "/tmp/input24"
:{
interpret :: (Int, Int, Int) -> Dir -> (Int, Int, Int)
interpret (x, y, z) = \case
  E -> (x+1,y+1,z)
  W -> (x-1,y-1,z)
  NE -> (x,y+1,z+1)
  SW -> (x,y-1,z-1)
  NW -> (x-1,y,z+1)
  SE -> (x+1,y,z-1)
:}
tileCos = map (foldl interpret (0,0,0)) tileDirs
import qualified Data.Map.Strict as M
flipCounts = foldl (\m c -> M.insertWith (+) c 1 m) M.empty tileCos
length $ M.filter odd $ flipCounts -- part 1

data Color = White | Black deriving (Show, Eq, Ord)
start = M.map (\x -> if odd x then Black else White) flipCounts
import Data.MemoTrie (memo)
neighbours = memo $ \point -> map (interpret point) [E, W, NW, NE, SW, SE]
import Data.Maybe (fromMaybe)
tileColor tile = fromMaybe White . M.lookup tile
import qualified Data.Set as S
:{
day tiles = M.fromSet step
  $ foldl (\s -> S.union s . S.fromList . neighbours) (M.keysSet tiles)
  $ M.keys tiles
  where
    step tile = let
        neighbourColors = map (flip tileColor tiles) $ neighbours tile
        blackNeighbourCount = length $ filter (== Black) neighbourColors
      in case tileColor tile tiles of
           White | blackNeighbourCount == 2 -> Black
           Black | blackNeighbourCount == 0 || blackNeighbourCount > 2 -> White
           color -> color
:}
length $ M.filter (== Black) $ iterate day start !! 100 -- part 2
```
