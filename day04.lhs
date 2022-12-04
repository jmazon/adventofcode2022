---
title: "AoC Day 4: Camp Cleanup"
author: Jean-Baptiste Mazon
date: 2022-12-04T09:25:19-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: Just doing what it says to
image: aoc-haskell.jpeg
---

Today in [Advent of Code][aoc], we're counting various interval
relationships.

[aoc]: https://adventofcode.com/2022/day/4

This post is [literate Haskell][gh], so here's the traditional imports
header:

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day04.lhs

> import Control.Applicative (liftA2)
> import Control.Category ((>>>))
> import Data.List.Split (wordsBy)
> import Numeric.Interval (Interval,(...),contains,intersection,isSubsetOf,null)
> import Prelude hiding (null)

Since there's really not much thinking involved in doing the interval
relationship part, let's spend some quality time parsing.

> main :: IO ()
> main = interact $

First, split by lines.

>   lines >>>

On each line, extract the intervals.

>   map (wordsBy (== ',')) >>>

For each interval, split the lower and upper bound.

>   (map . map) (wordsBy (== '-')) >>>

Parse the bounds as integers.  (No need to actually specify “integer”
anywhere, the types are inferred.  What's not is that we're actually
*converting*.)

>   (map . map . map) read >>>

Convert the bound pairs to actual `Interval`s.

>   (map . map) (uncurry (...) . listToPair) >>>

Perform the same kind of functional hackery we did on
[day 1](day01.html) to compute both parts using the same pipeline.

>   flip countIf >>>
>   flip map [part1,part2] >>>
>   show

Both parts have the same core: count the number of interval pairs who
are in a certain relationship, so that much was factored.  Now the
actual specificities.

> part1,part2 :: (Interval Int,Interval Int) -> Bool

In part 1, we're looking for intervals pairs where one contains the
other.  There are multiple ways to do this; here's one.

> part1 = liftA2 (||) (uncurry contains) (uncurry isSubsetOf)

In part 2, we're looking for intersecting intervals.  I couldn't find
a function to do directly this in [the library I'm using][intervals],
but it's simple enough to reproduce.

[intervals]: https://hackage.haskell.org/package/intervals

> part2 = not . null . uncurry intersection

For completeness, a couple of helper functions.

> listToPair :: [a] -> (a,a)
> listToPair [a,b] = (a,b)
> 
> countIf :: ((a,a) -> Bool) -> [[a]] -> Int
> countIf p = filter (listToPair >>> p) >>> length

This concludes today's puzzle.  I'm sorry I had to spice it up with
pointless twiddling, I'm sure the following days will be more
interesting.  See you then!
