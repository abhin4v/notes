---
date: 2020-12-12
tags: programming aoc haskell
---

# Advent of Code 2020 — Week 2

I'm solving the [Advent of Code 2020](https://adventofcode.com/2020/) in the Haskell REPL (GHCi). You can copy the code and paste it in GHCi to play with it. Here are my solutions for week 2 (Dec 6–12):

- [Day 6](#day-6)
- [Day 7](#day-7)
- [Day 8](#day-8)
- [Day 9](#day-9)
- [Day 10](#day-10)
- [Day 11](#day-11)
- [Day 12](#day-12)

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
import Control.Monad.State.Strict (MonadState(..), StateT, runStateT, modify)
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
lookahead :: Parser [a] (Maybe a)
lookahead = get >>= \case { [] -> return Nothing; (c:_) -> return (Just c) }
consume :: Parser [a] ()
consume = modify tail
:}
char c = satisfy (== c)
string = \case { "" -> pure ""; (c:cs) -> (:) <$> char c <*> string cs }
import Data.Char (digitToInt, isDigit, isAlpha)
digit = digitToInt <$> satisfy isDigit
num = foldl1 (\n d -> n * 10 + d) <$> some digit
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

## Day 8

Problem: <https://adventofcode.com/2020/day/8>

Solution:

```haskell
-- First, copy-paste the parser framework from day 7
-- Next, parse the input:
:{
argument = (\s n -> case s of { '-' -> negate n; '+' -> n } )
  <$> (char '+' <|> char '-') <*> num
:}
data Instruction = Acc | Jmp | Nop deriving (Show)
import Data.Functor (($>))
instruction = string "acc" $> Acc <|> string "nop" $> Nop <|> string "jmp" $> Jmp
program = ((,) <$> (instruction <* space) <*> argument) `separatedBy` char '\n'

-- Finally, solve it using an interpreter:
Just input <- flip runParser program <$> readFile "/tmp/input8"
import qualified Data.Set as Set
:{
interpret seen acc pc = if Set.member pc seen then acc else
  let (ins, arg) = input !! pc
      seen' = Set.insert pc seen
  in case ins of
    Nop -> interpret seen' acc (pc + 1)
    Acc -> interpret seen' (acc + arg) (pc + 1)
    Jmp -> interpret seen' acc (pc + arg)
:}
interpret Set.empty 0 0 -- part 1
:{
interpret' seen acc pc moded
  | length input == pc = Right acc
  | Set.member pc seen = Left acc
  | otherwise = let
      (ins, arg) = input !! pc
      seen' = Set.insert pc seen
      interpretNop = interpret' seen' acc (pc + 1)
      interpretJmp = interpret' seen' acc (pc + arg)
    in case ins of
      Acc -> interpret' seen' (acc + arg) (pc + 1) moded
      Nop | moded -> interpretNop True
      Nop -> either (const $ interpretJmp True) Right $ interpretNop False
      Jmp | moded -> interpretJmp True
      Jmp -> either (const $ interpretNop True) Right $ interpretJmp False
:}
interpret' Set.empty 0 0 False -- part 2
```

## Day 9

Problem: <https://adventofcode.com/2020/day/9>

Solution:

```haskell
isValid xs x = x `elem` [ a + b | a <- xs, b <- xs, a /= b ]
:{
findInvalid xs
  | null xs = Nothing
  | length xs <= 25 = Nothing
  | isValid as b = findInvalid (tail xs)
  | otherwise = Just b
      where (as, (b:_)) = splitAt 25 xs
:}
input <- map read . lines <$> readFile "/tmp/input9" :: IO [Int]
Just part1 = findInvalid input
:{
sliding [] _ = []
sliding xs size
  | length xs >= size = take size xs : sliding (drop 1 xs) size
  | otherwise = []
:}
ranges xs = concatMap (sliding xs) [2..(length xs)]
:{
part2 = head
  . map (\range -> maximum range + minimum range)
  . filter ((== part1) . sum)
  . ranges
  $ input
:}
```

## Day 10

Problem: <https://adventofcode.com/2020/day/10>

Solution:

```haskell
import Data.List (sort)
input <- (0:) . sort . map read . lines <$> readFile "/tmp/input10" :: IO [Int]
diffs = 3 : zipWith (-) (tail input) input
count n = length . filter (== n)
count 1 diffs * count 3 diffs -- part 1
sliding xs = if null xs then [] else take 4 xs : sliding (drop 1 xs)
edges = map (\(x:xs) -> (x, [y | y <- xs, y <= x + 3])) $ sliding input
import Data.Maybe (fromMaybe, fromJust)
import Data.Function (fix)
memoize f = flip lookup (map f input)
:{
arrCount = fix (memoize . go)
  where
    go f x = if x == goal then (goal, 1)
      else (x, sum . map (fromJust . f) . fromMaybe [] $ lookup x edges)
    goal = last input
:}
arrCount 0 -- part 2
```

## Day 11

Problem: <https://adventofcode.com/2020/day/10>

Solution:

```haskell
:set -XDeriveFunctor
:set -XLambdaCase
import qualified Data.Sequence as S
import Data.Sequence (Seq(..), (|>), (<|), (><))

-- First, data types for list zipper and celluar automata universe
data Z a = Z (S.Seq a) a (S.Seq a) deriving (Show, Eq, Functor)
newtype Univ a = Univ (Z (Z a)) deriving (Show, Eq, Functor)

-- next, function to move around the universe
zLeft z@(Z l f r) = case l of { S.Empty -> z; xs :|> x -> Z xs x (f <| r) }
zRight z@(Z l f r) = case r of { S.Empty -> z; x :<| xs -> Z (l |> f) x xs }
up (Univ z) = Univ $ zLeft z
down (Univ z) = Univ $ zRight z
left (Univ z) = Univ $ fmap zLeft z
right (Univ z) = Univ $ fmap zRight z

-- next, functions to measure the universe
zPos (Z l _ _) = S.length l
zLength (Z a _ b) = S.length a + S.length b + 1
uPos (Univ (Z a f _)) = (S.length a, zPos f)
uRowCount (Univ z) = zLength z
uColCount (Univ (Z _ z _)) = zLength z

-- next, functions to convert to and from the universe
:{
toUniv rows =
  let (first :<| rest) = fmap (\(cell :<| cells) -> Z S.empty cell cells) rows
  in Univ $ Z S.empty first rest
fromUniv (Univ (Z l f r)) = (fmap toSeq l |> toSeq f) >< fmap toSeq r
  where
    toSeq (Z l f r) = (l |> f) >< r
:}

-- next, Comonad instances of the list zipper and the universe
import Control.Comonad
:{
instance Comonad Z where
  extract (Z _ f _) = f
  duplicate z = Z l z r
    where
      l = S.reverse $ S.iterateN (zPos z) zLeft $ zLeft z
      r = S.iterateN (zLength z - zPos z - 1) zRight $ zRight z
instance Comonad Univ where
  extract (Univ u) = extract $ extract u
  duplicate u@(Univ univ) = Univ $ Z l f r
    where
      uf = extract univ
      f = Z fl u fr
      fl = S.reverse $ S.iterateN (zPos uf) left (left u)
      fr = S.iterateN (zLength uf - zPos uf - 1) right (right u)
      l = S.reverse $ S.iterateN (zPos univ) (fmap up) (fmap up f)
      r = S.iterateN (zLength univ - zPos univ - 1) (fmap down) (fmap down f)
:}

-- next, model the seats
data Seat = Empty | Occupied | Floor deriving (Eq)
instance Show Seat where show = \case { Empty -> "L"; Occupied -> "#"; Floor -> "." }
toSeat = \case { 'L' -> Empty; '.' -> Floor; '#' -> Occupied }

-- next, read and measure the input
:{
input <- S.fromList
  . map (S.fromList . map toSeat)
  . lines
  <$> readFile "/tmp/input11"
:}
inputRowCount = S.length input
inputColCount = let (row :<| _) = input in S.length row
validIndex (x, y) = and [x >= 0, y >= 0, x < inputRowCount, y < inputColCount]

-- next, rules for finding neighbours and occupying seats for part 1
:{
neighbours1 grid (x, y) = [
    grid `S.index` (x + i) `S.index` (y + j)
  | i <- [-1, 0, 1], j <- [-1, 0, 1]
  , validIndex (x + i, y + j)
  , (x + i, y + j) /= (x, y)
  ]
rule1 univ = let
    ns = neighbours1 (fromUniv univ) $ uPos univ
    f = extract univ
    occupied = [() | n <- ns, n == Occupied]
  in case f of
       Empty | null occupied -> Occupied
       Occupied | length occupied >= 4 -> Empty
       _ -> f
:}

-- next, run the automata till it settles and count the occupied seats
fix f x = let x' = f x in if x == x' then x else fix f x'
:{
finallyOccupiedSeatCount rule = sum
  . fmap (S.length . S.filter (== Occupied))
  . fromUniv
  . fix (extend rule)
  $ toUniv input
:}

-- finally, count of occupied seats for part 1.
finallyOccupiedSeatCount rule1

-- Next, rules for finding neighbours and occupying seats for part 2
inputDiaCount = ceiling $ sqrt 2 * fromIntegral (min inputRowCount inputColCount)
:{
neighbours2 grid (x, y) =
  map (map (\(x', y') -> grid `S.index` x' `S.index` y')) indices
  where
    indices = map (filter validIndex) [
        [(i, y) | i <- [x-1, x-2 .. 0]]
      , [(i, y) | i <- [x+1 .. inputRowCount-1]]
      , [(x, j) | j <- [y-1, y-2 .. 0]]
      , [(x, j) | j <- [y+1 .. inputColCount-1]]
      , [(x+i, y+i) | i <- [1.. inputDiaCount]]
      , [(x+i, y-i) | i <- [1.. inputDiaCount]]
      , [(x-i, y-i) | i <- [1.. inputDiaCount]]
      , [(x-i, y+i) | i <- [1.. inputDiaCount]]
      ]
rule2 univ = let
    nss = neighbours2 (fromUniv univ) $ uPos univ
    f = extract univ
    occupied = [() | ns <- nss, take 1 (dropWhile (== Floor) ns) == [Occupied]]
  in case f of
       Empty | null occupied -> Occupied
       Occupied | length occupied >= 5 -> Empty
       _ -> f
:}

-- and, count of occupied seats for part 2
finallyOccupiedSeatCount rule2
```

## Day 12

Problem: <https://adventofcode.com/2020/day/12>

Solution:

```haskell
:set -XLambdaCase
:{
input <- map (\(x:xs) -> (x, read xs))
  . lines <$> readFile "/tmp/input12" :: IO [(Char, Double)]
:}
radian deg = deg / 180 * pi
:{
run1 ((x, y), deg) = \case
  ('N', del) -> ((x, y + del), deg)
  ('S', del) -> ((x, y - del), deg)
  ('E', del) -> ((x + del, y), deg)
  ('W', del) -> ((x - del, y), deg)
  ('L', del) -> ((x, y), (deg + del))
  ('R', del) -> ((x, y), (deg - del))
  ('F', del) -> ((x + del * cos (radian deg), y + del * sin (radian deg)), deg)
:}
let ((x, y), _) = foldl run1 ((0, 0), 0) input in abs x + abs y -- part 1

:{
run2 (pos@(x, y), wp@(wx, wy)) = \case
  ('N', del) -> (pos, (wx, wy + del))
  ('S', del) -> (pos, (wx, wy - del))
  ('E', del) -> (pos, (wx + del, wy))
  ('W', del) -> (pos, (wx - del, wy))
  ('L', del) -> (pos, rotate (radian del))
  ('R', del) -> (pos, rotate (- (radian del)))
  ('F', del) -> ((x + wx * del, y + wy * del), wp)
  where
    rotate del = (wx * cos del - wy * sin del, wy * cos del + wx * sin del)
:}
let ((x, y), _) = foldl run2 ((0, 0), (10, 1)) input in abs x + abs y -- part 2
```
