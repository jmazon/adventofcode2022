---
title: "AoC Day 6: Tuning Trouble"
author: Jean-Baptiste Mazon
date: 2022-12-06T10:03:57-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: There's no need to be fine when the input is small
image: aoc-haskell.jpeg
---

Today's [Advent of Code puzzle][aoc] is the simplest code of the year
to date.  And I'm making it longer for posting than what I solved
with.

[aoc]: https://adventofcode.com/2022/day/6

It's as [literate Haskell][gh] as usual, but it really won't take
long.   First two imports.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day06.lhs

> import Control.Arrow ((&&&))
> import Data.List (findIndex,nub,tails)

We're looking for a start-of-message marker, defined as `w` distinct
characters.  Here's a function to identify whether we're looking at
that.  It works by extracting the `w` first characters of its input,
and checking whether removing duplicates is a no-op.

> isMessageStart :: Int -> String -> Bool
> isMessageStart w (take w -> pfx) = nub pfx == pfx

Finding the start-of-message end is now a simple matter of iterating,
and adjusting the resulting index for prefix length.

> locate :: Int -> String -> Maybe Int
> locate w = fmap (+ w) . findIndex (isMessageStart w) . tails

A simple wrapper to perform both parts in a single sitting, and we're
done!

> main :: IO ()
> main = interact $ show . (locate 4 &&& locate 14)

This concludes today's puzzle.  See you tomorrow!
