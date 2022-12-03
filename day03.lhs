---
title: "AoC Day 3: Rucksack Reorganization"
author: Jean-Baptiste Mazon
date: 2022-12-03T11:53:21-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: The uninteresting puzzle of the year
image: aoc-haskell.jpeg
---

On Advent of Code's [3<sup>rd</sup> day][aoc], we are to perform
simple duplicate detection among various parts of the input.  First
between two halves of a line, then between three adjacent lines.

[aoc]: https://adventofcode.com/2022/day/3

As usual, this post is [literate Haskell][gh] post, with a few imports
to get out of the way.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day03.lhs

> import Control.Category ((>>>))
> import Data.Char        (isUpper,ord,toLower)
> import Data.List        (foldl1',intersect,nub)
> import Data.List.Split  (chunksOf)

There's really nothing complicated at all in today's puzzle.  How do
we score a resulting common letter?  By applying the formula given in
the problem statement, that's how:

> prioritize :: String -> Int
> prioritize [c] =
>   ord (toLower c) - ord 'a' + 1
>   + 26 * fromEnum (isUpper c)

With that out of the way, we can just write a solving pipeline:

> main :: IO ()
> main = interact $
>   lines >>>
>   flip ($) >>>
>   flip map [part1,part2] >>>

Ok, so that `[part1,part2]` is where we're going to do the magic.
That magic consists in doing what's different between the two parts,
namely: converting the list of input lines into the list of sets to
compare for common letters.

Once we have those, we can actually identify the dupe:

>   map (
>     map (nub . foldl1' intersect) >>>

I'm using `nub` because, of course, the input is evil enough to ensure
there's a single letter in common between all sets, but when it's
there it's often replicated.

Next we can score and display:

>     map prioritize >>>
>     sum
>   ) >>>
>   show

Part 2 actually has the easier case: we're simply comparing lines by
sets of 3.

> part1,part2 :: [String] -> [[String]]
> part2 = chunksOf 3

In part 1, we're comparing left versus right, which has us write out
trivial string manipulation before we're there:

> part1 = map halve where
>   halve s = [take h s,drop h s]
>     where h = (length s `div` 2)

And… that's it.

I hope you enjoyed it, even though there's really not much here.
Let's meet tomorrow, for what'll definitely be a more interesting
problem!
