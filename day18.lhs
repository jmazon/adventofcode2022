---
title: "AoC Day 18: Boiling Boulders"
author: Jean-Baptiste Mazon
date: 2022-12-18T09:00:50-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: Discrete cristallography
image: aoc-haskell.jpeg
---

The [Advent of Code][aoc] puzzle of the day, “Boiling Boulders”, has
us measure a chunk of lava in different ways.  In [literate
Haskell][gh] as usual.

[aoc]: https://adventofcode.com/2022/day/18
[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day18.lhs

> import           Control.Arrow   ((&&&))
> import           Control.Lens    ((+~),view)
> import           Data.Ix         (inRange)
> import           Data.List       (partition)
> import           Data.List.Split (wordsBy)
> import           Linear          (V3(V3),_x,_y,_z,basis)
> import           Data.Set        (Set)
> import qualified Data.Set as Set

We're doing 3D.  Whee!

> type V = V3 Int

Parsing is easy today.

> parse :: String -> Set V
> parse = Set.fromList . map v . lines where
>   v (wordsBy (== ',') -> map read -> [i,j,k]) = V3 i j k

Part 1 has us measure the chunk's outside area.  Outside being locally
defined as “has no cube of lava glued on there”.

A raw cube has an area of 6; we could remove 1 per cube that's in
contact, but that's kind of wasteful when we could remove 2 per cube
that's in contact, but only on a single direction per axis.  I'll do
it with set operations for the fun of it.

> part1 :: Set V -> Int
> part1 lava = 2 * (3 * Set.size lava - disp _x - disp _y - disp _z) where
>   disp l = Set.size ((Set.intersection =<< (Set.mapMonotonic (l +~ 1))) lava)

For part 2, we can't locally observe whether a non-covered square is
to the inside or outside, so we'll need a more holistic approach.

I'll simply flood-fill a bounding box around our chunk and count
blocks as external surface.  Using a simple DFS as a driver.

> part2 :: Set V -> Int
> part2 lava =

First compute the bounding box, and paranoidly extend it by one in
every direction just in case the puzzle input attempted to block us
along some plane.

>   let extrema l = (pred . minimum &&& succ . maximum) (Set.map (view l) lava)
>       [(xmin,xmax),(ymin,ymax),(zmin,zmax)] = extrema <$> [_x,_y,_z]
>       boundingBox = (V3 xmin ymin zmin,V3 xmax ymax zmax)

Then DFS from a point known to be outside.  Such as a corner of our
extended bounding box.

>       neighbors = basis ++ map negate basis
>       dfs _ [] !acc = acc
>       dfs cl (p:ps) acc
>         | p `Set.member` cl = dfs cl ps acc
>         | otherwise = dfs cl' ps' acc' where
>             cl' = Set.insert p cl
>             ns = filter (inRange boundingBox) ((p +) <$> neighbors)
>             (surface,void) = partition (`Set.member` lava) ns
>             ps' = filter (`Set.notMember` cl) void ++ ps
>             acc' = acc + length surface
>   in dfs Set.empty [V3 xmin ymin zmin] 0

And that's it.  Just a little wrapper to have it stand alone.

> main :: IO ()
> main = interact $ show . (part1 &&& part2) . parse

This puzzle was orders of magnitude easier than what's around.  Nice
breather.  See you tomorrow!
