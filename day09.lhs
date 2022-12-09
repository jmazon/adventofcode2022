---
title: "AoC Day 9: Rope Bridge"
author: Jean-Baptiste Mazon
date: 2022-12-09T09:56:25-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: Back to functional and infinite lists
image: aoc-haskell.jpeg
---

[Advent of Code][aoc] today has us reimplements the 80s classic game
“Snakes”, disguising the slithering reptile as a rope.

[aoc]: https://adventofcode.com/2022/day/9

Keeping in the [literate Haskell][gh] spirit, this post starts with two lines of imports.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day09.lhs

> import Data.Containers.ListUtils (nubOrd)
> import Linear.V2 (V2(V2))

That wasn't so bad.  Now we've imported `V2`, let's define our useful
type out of it.

> type V = V2 Int

So we have a snake.  We're given its head's movements in some kind of
textual form.  We convert that to a nice and clean move vector.

> parseMove :: String -> [V]
> parseMove (words -> [dir,n]) = replicate (read n) $ case dir of
>   "U" -> V2   1   0
>   "D" -> V2 (-1)  0
>   "L" -> V2   0 (-1)
>   "R" -> V2   0   1

Keeping track of the position's head would then be a simple matter of
folding on that move list from an arbitrary starting position.
Arbitrary because we'll only be counting distinct places.

> headPath :: [V] -> [V]
> headPath = scanl (+) 0

The next primary operation is the act of following.  Each snake ring
follows the previous according to a simple algorithm: move as close as
possible according to Manhattan distance, limiting displacement to 1
per axis and per turn.  But don't move so close as to meet; and don't
move either if already in contact.

Those two last conditions seem like a complication, but they can
actually be summarized in a single condition.

> followPos :: V -> V -> V
> followPos src dst
>   | maximum (abs <$> delta) <= 1 = src
>   | otherwise = src + (signum <$> delta)
>   where delta = dst - src

Keeping track of a follower's path is then the same simple matter of
folding on its predecessor's position.  The starting position is the
same.

> followPath :: [V] -> [V]
> followPath = scanl followPos 0

Wrapping it up.  Collect moves from input stream.

> main :: IO ()
> main = do
>   moves <- concatMap parseMove . lines <$> getContents

Expand the head's path out of it.

>   let headPlaces = headPath moves

Expand an infinite list of successor knots' paths out of it.

>   let knotPlacess = iterate followPath headPlaces

Part 1 asks for the number of distinct positions the first follower
knot goes through.

>   print (length (nubOrd (knotPlacess !! 1)))

Part 2 asks for the number of distinct positions the ninth follower
knot goes through.

>   print (length (nubOrd (knotPlacess !! 9)))

And that's it!  See you tomorrow!
