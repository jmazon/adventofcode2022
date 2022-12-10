---
title: "AoC Day 10: Cathode-Ray Tube"
author: Jean-Baptiste Mazon
date: 2022-12-10T09:36:03-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: OCR on a matrix scan
image: aoc-haskell.jpeg
---

Day 10 of [Advent of Code][aoc] is the first day of the year of
pseudo-assembly.

[aoc]: https://adventofcode.com/2022/day/10

As a [literate Haskell][gh] post, we'll start with a couple of
imports.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day10.lhs

> import Control.Monad.Writer (execWriter,tell,foldM)
> import Data.List.Split      (chunksOf)

So ok, this is assembly language, but it's barely interesting.  It's
certainly not Turing-complete as usual.  It merely maintains an
accumulator.  Its only distinctive feature is that its two operations
have distinct cycle lengths.  We're not going to track them directly.
Instead, we'll generate the list of values the accumulator takes, per
cycle.

> toSignal :: String -> [Int]
> toSignal (lines -> ls) = undefined : execWriter (foldM go 1 ls)
>   where
>     go x "noop" = tell [x] *> pure x
>     go x (words -> ["addx",read -> addx]) =
>       tell [x,x] *> pure (x + addx)

Part 1 of the puzzle asks us to sum the signal's value at certain
times.

> part1 :: [Int] -> Int
> part1 signal = sum [ i * signal !! i | i <- [20,60,100,140,180,220] ]

Part 2 asks us when the signal coincides with an electron beam scan
with rollover.  The definition of “coincides” is “at a distance of 0
or 1”.  We present the result in monospace textual form for better
readability.

> part2 :: [Int] -> [String]
> part2 = chunksOf 40 . map ((".#" !!) . fromEnum) .
>         zipWith close (cycle [0..39]) . tail
>   where close a b = abs (b-a) <= 1

A little wrapper puts it all together.

> main :: IO ()
> main = do
>   signal <- toSignal <$> getContents
>   print (part1 signal)
>   putStr (unlines (part2 signal))

This concludes today's puzzle.  See you tomorrow!
