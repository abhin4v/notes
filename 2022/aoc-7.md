---
date: 2022-12-09
tags: aoc haskell programming
---
# Solving Advent of Code Day 7 with Parsers, Zippers and Interpreters

In this post, we solve the [Advent of Code 2022, Day 7](https://adventofcode.com/2022/day/7) challenge in Haskell using parsers, zippers and interpreters.

## Table of contents

- [The Challenge](#the-challenge)
- [Setup](#setup)
- [Modeling the filesystem](#modeling-the-filesystem)
- [Parsing the browsing session](#parsing-the-browsing-session)
- [Navigating and modifying the file system](#navigating-and-modifying-the-file-system)
- [Interpreting the browsing session](#interpreting-the-browsing-session)
- [Solving the challenge](#solving-the-challenge)
- [Running the program](#running-the-program)

## The challenge

Here's a quick recap of the challenge:

> You browse around the filesystem to assess the situation and save the resulting terminal output (your puzzle input). For example:

```
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
```

We need to write a program that understands the browsing session, builds a model of the filesystem, and then solves the challenge.

Let's get started!

## Setup

First, some imports:

```haskell
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char (isDigit)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Text.ParserCombinators.ReadP ((<++))
import qualified Text.ParserCombinators.ReadP as P
```

We are using the [`ReadP`](https://hackage.haskell.org/package/base/docs/Text-ParserCombinators-ReadP.html) parser combinator library, which is part of the `base` package. We also use the `LambdaCase` extension, which allows us to write `case` expressions in a more concise way.

## Modeling the filesystem

To start with, we need to model the filesystem. We do this by defining the `File` and `Dir` data types:

```haskell
data File = File {fName :: String, fSize :: Int}

instance Show File where
  show (File name size) = name <> "(file, size=" <> show size <> ")"

data Dir = Dir
  { dName :: String
  , dSize :: Int
  , dFiles :: Map.Map String File
  , dDirs :: Map.Map String Dir
  }

instance Show Dir where
  showsPrec d (Dir name _ files dirs) =
    showString (concat $ replicate d "  ")
      . showString "- "
      . showString name
      . showString " (dir)\n"
      . showDirs (sortOn dName $ Map.elems dirs)
      . showFiles (sortOn fName $ Map.elems files)
    where
      showDirs :: [Dir] -> ShowS
      showDirs = foldr (.) id . fmap (showsPrec $ d + 1)

      showFiles :: [File] -> ShowS
      showFiles = foldr ((.) . showFile) id

      showFile :: File -> ShowS
      showFile (File name' size) =
        showString (concat $ replicate (d + 1) "  ")
          . showString "- "
          . showString name'
          . showString " (file, size="
          . shows size
          . showString ")\n"

emptyDir :: String -> Dir
emptyDir name = Dir name 0 Map.empty Map.empty
```

The `File` type is pretty straightforward. The `Dir` type is a bit more complex. It contains a name, a size, and two maps of files and directories for the files and directories contained within respectively. The size is the sum of the sizes of all the files and directories contained in the directory.

The `Show` instance for `Dir` is used to pretty-print the directory structure. It uses a recursive function to print the files and directories contained in the directory.

We check it out in GHCi:

```haskell
> :{
> fs = Dir "/" 0
    ( Map.fromList
        [ ("b.txt", File "b.txt" 14848514)
        , ("c.dat", File "c.dat" 8504156)
        ]
    )
    ( Map.fromList
        [ ("a", Dir "a" 0 (Map.fromList [("f", File "f" 29116)]) Map.empty)
        , ("d", Dir "d" 0 (Map.fromList [("d.log", File "d.log" 8033020), ("k", File "k" 7214296)]) Map.empty)
        ]
    )
> :}
> print fs
- / (dir)
  - a (dir)
    - f (file, size=29116)
  - d (dir)
    - d.log (file, size=8033020)
    - k (file, size=7214296)
  - b.txt (file, size=14848514)
  - c.dat (file, size=8504156)
```

That looks good!

## Parsing the browsing session

Let's first define some data types to model the browsing session:

```haskell
data CdArg = CdDir String | CdUp | CdRoot deriving (Show)

data Command = Cd CdArg | Ls deriving (Show)

data Output = OutputFile File | OutputDir Dir deriving (Show)

data Line = LCommand Command | LOutput Output deriving (Show)
```

Now, we define the parsers:

```haskell
commandParser :: P.ReadP Command
commandParser =
  P.char '$' *> P.skipSpaces *> P.choice [Cd <$> cdParser, Ls <$ lsParser]
  where
    lsParser = P.string "ls"
    cdParser =
      P.string "cd"
        *> P.skipSpaces
        *> ((CdUp <$ P.string "..")
             <++ (CdRoot <$ P.string "/")
             <++ (CdDir <$> P.munch1 (/= ' ')))

outputParser :: P.ReadP Output
outputParser = P.choice [OutputFile <$> fileParser, OutputDir <$> dirParser]
  where
    fileParser =
      flip File
        <$> (read <$> P.munch1 isDigit)
        <*> (P.skipSpaces *> P.munch1 (/= ' '))
    dirParser = emptyDir <$> (P.string "dir " *> P.munch1 (/= ' '))

lineParser :: P.ReadP Line
lineParser = P.choice [LOutput <$> outputParser, LCommand <$> commandParser]

parseLine :: String -> Line
parseLine s = case P.readP_to_S lineParser s of
  [(l, "")] -> l
  _ -> error $ "Failed to parse line: " <> s
```

The `commandParser` parses a command. A command starts with a `$` character, followed by a space, followed by the command name. The `cd` command is followed by a space and a directory name, or `..` to go up, or `/` to go the root directory. The `ls` command takes no arguments.

The `outputParser` parses a line of command output. It can be either a file or a directory. A file is a size followed by a space and a name. A directory is the string `dir ` followed by a name.

The `lineParser` parses a line of the browsing session. It can be either a command or a line of command output.

Finally, the `parseLine` function parses a line of the browsing session, and returns a `Line` value.

We try it out in GHCi:

```haskell
> parseLine "$ cd .."
LCommand (Cd CdUp)
> parseLine "$ cd /"
LCommand (Cd CdRoot)
> parseLine "$ cd a"
LCommand (Cd (CdDir "a"))
> parseLine "$ ls"
LCommand Ls
> parseLine "29116 f"
LOutput (OutputFile f(file, size=29116))
```

Works as expected.

## Navigating and modifying the file system

Now, we define functions to navigate and modify the file system. We are going to use a Zipper to navigate the file system.

A [Zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)) is a data structure that allows us to navigate a data structure, focus on a part of it, and to modify the data structure at the focus.

```haskell
data FsZipper = FsZipper {zPath :: [Dir], zCurrent :: Dir} deriving (Show)

moveUp :: FsZipper -> FsZipper
moveUp = \case
  FsZipper [] _ -> error "Can't move up from root"
  FsZipper (d : ds) cur ->
    FsZipper ds $ d {dDirs = Map.insert (dName cur) cur $ dDirs d}

moveDown :: String -> FsZipper -> FsZipper
moveDown name (FsZipper ds d) = FsZipper (d : ds) $ findDir d
  where
    findDir Dir {dDirs = ds'} = case Map.lookup name ds' of
      Nothing -> error $ "Can't find directory " <> name
      Just d' -> d'

moveToRoot :: FsZipper -> FsZipper
moveToRoot zipper = case zipper of
  FsZipper [] _ -> zipper
  _ -> moveToRoot $ moveUp zipper

addFile :: File -> FsZipper -> FsZipper
addFile f (FsZipper ds d) =
  FsZipper ds d {dFiles = Map.insert (fName f) f $ dFiles d}

addDir :: Dir -> FsZipper -> FsZipper
addDir d (FsZipper ds d') =
  FsZipper ds d' {dDirs = Map.insert (dName d) d $ dDirs d'}

toZipper :: Dir -> FsZipper
toZipper = FsZipper []

fromZipper :: FsZipper -> Dir
fromZipper = zCurrent . moveToRoot
```

The `FsZipper` type is a zipper for our file system model. It contains the path to the current directory, and the current directory.

The `moveUp` function moves up a directory in the file system. It takes the current directory, and replaces it in the parent directory. It then returns a zipper with the parent directory as the current directory.

The `moveDown` function moves down a directory in the file system. It takes the name of the directory to move to, and returns a zipper with the new directory as the current directory.

The `moveToRoot` function moves to the root directory in the file system. It uses the `moveUp` function to move up the file system until it reaches the root directory.

The `addFile` and `addDir` functions add a file or a directory to the current directory, respectively.

The `toZipper` function converts the root directory of the file system to a zipper, and The `fromZipper` function does the opposite.

We test these functions in GHCi:

```haskell
> :{
> fs = Dir "/" 0
    ( Map.fromList
        [ ("b.txt", File "b.txt" 14848514)
        , ("c.dat", File "c.dat" 8504156)
        ]
    )
    ( Map.fromList
        [ ("a", Dir "a" 0 (Map.fromList [("f", File "f" 29116)]) Map.empty)
        , ("d", Dir "d" 0 (Map.fromList [("d.log", File "d.log" 8033020), ("k", File "k" 7214296)]) Map.empty)
        ]
    )
> :}
> print fs
- / (dir)
  - a (dir)
    - f (file, size=29116)
  - d (dir)
    - d.log (file, size=8033020)
    - k (file, size=7214296)
  - b.txt (file, size=14848514)
  - c.dat (file, size=8504156)
> import Data.Function ((&))
> :{
> fs
  & toZipper
  & moveDown "a"
  & addFile (File "g" 12345)
  & moveUp
  & moveDown "d"
  & addDir (Dir "e" 0 Map.empty Map.empty)
  & fromZipper
> :}
- / (dir)
  - a (dir)
    - f (file, size=29116)
    - g (file, size=12345)
  - d (dir)
    - e (dir)
    - d.log (file, size=8033020)
    - k (file, size=7214296)
  - b.txt (file, size=14848514)
  - c.dat (file, size=8504156)
```

Perfect!

## Interpreting the browsing session

Now that we have a way to navigate and modify the file system, we define a function to interpret the browsing session.

```haskell
interpretLine :: FsZipper -> Line -> FsZipper
interpretLine zipper = \case
  LCommand (Cd CdUp) -> moveUp zipper
  LCommand (Cd CdRoot) -> moveToRoot zipper
  LCommand (Cd (CdDir name)) -> moveDown name zipper
  LCommand Ls -> zipper
  LOutput (OutputFile f) -> addFile f zipper
  LOutput (OutputDir d) -> addDir d zipper

interpret :: [Line] -> Dir
interpret = fromZipper . foldl interpretLine (toZipper $ emptyDir "/")
```

The `interpretLine` function interprets a parsed line of the browsing session, and returns the updated file system zipper. It uses pattern matching to handle each command and output.

The `interpret` function interprets the entire parsed browsing session, and returns the root directory of the final file system.

We test the interpreter in GHCi on the test browsing session:

```haskell
> :{
> testInput = [
    "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k"
    ]
> :}
> interpret $ map parseLine testInput
- / (dir)
  - a (dir)
    - e (dir)
      - i (file, size=584)
    - f (file, size=29116)
    - g (file, size=2557)
    - h.lst (file, size=62596)
  - d (dir)
    - d.ext (file, size=5626152)
    - d.log (file, size=8033020)
    - j (file, size=4060174)
    - k (file, size=7214296)
  - b.txt (file, size=14848514)
  - c.dat (file, size=8504156)
```

This matches the file system shown in the problem statement.

## Solving the challenge

Next, we solve the challenge.

### Part 1

For part 1, we need to return the sum of the sizes of directories smaller than 100000.

```haskell
calcAndSetDirSize :: Dir -> Dir
calcAndSetDirSize d@Dir {dFiles = fs, dDirs = ds} =
  let ds' = fmap calcAndSetDirSize ds
   in d {dSize = sum $ fmap fSize fs <> fmap dSize ds', dDirs = ds'}

findDirsSmallerThan :: Int -> Dir -> [Dir]
findDirsSmallerThan size d@(Dir {dSize = dSize', dDirs = ds}) =
  [d | dSize' <= size] <> concatMap (findDirsSmallerThan size) ds
```

First, we write the `calcAndSetDirSize` function that calculates and sets the size of each directory. It recursively calculates the size of the subdirectories, and sets their sizes. It then sets the size of the current directory to the sum of the sizes of the files and subdirectories.

Then, we write the `findDirsSmallerThan` function that finds the directories smaller than the given size. It recursively finds the directories smaller than the given size in the subdirectories, and appends the current directory to the returned list if it is smaller than the given size.

Finally, we write the `part1` function that solves the first part of the challenge.

```haskell
part1 :: Dir -> Int
part1 = sum . map dSize . findDirsSmallerThan 100000
```

`part1` returns the sum of the sizes of the directories smaller than 100000.

We'll see how `part1` in invoked in the final section.

### Part 2

For part 2, we need to return the size of the smallest directory larger than space required for the update.

```haskell
findDirsLargerThan :: Int -> Dir -> [Dir]
findDirsLargerThan size d@(Dir {dSize = dSize', dDirs = ds}) =
  [d | dSize' >= size] <> concatMap (findDirsLargerThan size) ds

part2 :: Dir -> Int
part2 fs =
  let freeSpace = totalSpace - dSize fs
      spaceRequired = updateSpace - freeSpace
      dirs = findDirsLargerThan spaceRequired fs
   in minimum $ map dSize dirs
  where
    totalSpace = 70000000
    updateSpace = 30000000
```

We write the `findDirsLargerThan` function, which is similar to the `findDirsSmallerThan` function, except that it finds the directories larger than the given size.

Then, we write the `part2` function that solves the second part of the challenge. It calculates the amount of free space in the file system, and the amount of space required for the update. It then finds the directories larger than the amount of space required for the update, and returns the size of the smallest directory.

## Running the program

Finally, we run the program.

```haskell
main :: IO ()
main = do
  input <- lines <$> getContents
  let fs = calcAndSetDirSize $ interpret $ map parseLine input
  print fs
  print $ part1 fs
  print $ part2 fs
```

The `main` function reads the browsing session from the standard input. It then parses the browsing session, interprets it, and calculates and sets the size of each directory.

Then, it prints the final file system, and the solutions to the two parts of the challenge.

That's it for this post! I hope you enjoyed it. If you have any questions or comments, please [leave a comment](https://fantastic.earth/@abnv/109485091901491189){:class="mastodon-link"}. If you liked this post, please share it with your friends. Thanks for reading!
