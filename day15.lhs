---
title: "AoC Day 15: Beacon Exclusion Zone"
author: Jean-Baptiste Mazon
date: 2022-12-15T13:32:10-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: Interval scanning
image: aoc-haskell.jpeg
---

In today's [Advent of Code][aoc] puzzle, “Beacon Exclusion Zone”,
we'll be performing some interval arithmetic to achieve reasonable
times rasterizing a fairly large bitmap.

[aoc]: https://adventofcode.com/2022/day/15

This post is [literate Haskell][gh], with a few imports to begin with.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day15.lhs

> import Control.Applicative (liftA2)
> import Control.Lens        ((^.),view)
> import Data.Char           (isDigit)
> import Data.List           (nub,sort,uncons)
> import Data.Maybe          (mapMaybe)
> import Data.Monoid         (Alt(Alt))
> import Linear.V2           (V2(V2),_x,_y)
> import Numeric.Interval hiding (empty)
> import Prelude          hiding (null)

I'm barely doing anything with coordinates, but using `Linear` makes
it so seamless anyway it's not worth avoiding.

> type V = V2 Int

I'll also throw in the most point-free abusive implementation of
Manhattan distance.

> dist :: V -> V -> Int
> dist = ((sum . fmap abs) .) . (-)

Our input is provided as a list of reports, each one revealing the
position of a distinct sensor, and the position of the nearest beacon
it detects.

> data Report = Report { _sensor :: V, beacon :: V }

I'll keep with the month's style of terse optimistic parsing.

> parse :: String -> [Report]
> parse = map report . lines where
>   report (words -> ["Sensor","at",sx,sy,"closest","beacon","is","at",bx,by]) =
>     Report (V2 (pos sx) (pos sy)) (V2 (pos bx) (pos by))
>   pos = read . takeWhile isNumber . dropWhile (not . isNumber)
>   isNumber = liftA2 (||) (== '-') isDigit

For part 1, we're counting the number of positions on a given
horizontal line where we know for sure there isn't a beacon.

Let's split the problem and first identify which positions on a
horizontal line are covered by a single sensor's sweep.  I'll first
compute its Manhattan radius, then reduce it for eccentricity, then
make an interval out of it, centered on the beacon's X coordinate.

> scanSensorOnLine :: Int -> Report -> Maybe (Interval Int)
> scanSensorOnLine y (Report sensor beacon) =
>   let radius = dist sensor beacon
>       radius' = radius - abs (sensor^._y - y)
>   in if radius' < 0 then Nothing else Just (sensor^._x +/- radius')

The `+/-` operator would gracefully yields an empty interval when
called on a negative argument, but I'll go further and eliminate those
right here before they're even produced so I can more easily merge the
ones that overlap when aggregating coverage intervals from all
sensors.

> scanLine :: Int -> [Report] -> [Interval Int]
> scanLine y = mergeOverlaps . mapMaybe (scanSensorOnLine y)

I'll use a short helper to ensure all intervals we're left with are
separate.

> mergeOverlaps :: Ord a => [Interval a] -> [Interval a]
> mergeOverlaps is0 = maybe [] (uncurry go) (uncons (sort is0)) where
>   go i (i':is) | not (null (intersection i i')) = go (hull i i') is
>                | otherwise = i : go i' is
>   go i [] = [i]

The count of positions where there can't be a beacon is then easily
computed by summing the intervals' size, and removing any beacons
known to be on the scanned line.

> part1 :: [Report] -> Int -> Int
> part1 reports scanY =
>   let beacons = nub (beacon <$> reports)
>   in sum (map (succ . width) (scanLine scanY reports))
>      - length (filter ((== scanY) . view _y) beacons)

For part 2, we're asked for the hypothetical single position in a
four-million-positions-wide square that isn't in reach of our sensor
array.

There are lots of smart and efficient ways to do this, but to ssolve
the puzzle just once they'd be a waste of engineering time: my overlap
merging is efficient enough that I can just scan the four million
lines and detect the first hole.

> part2 :: [Report] -> Int -> Alt Maybe Int
> part2 reports size = flip foldMap [0..size] $ \y -> Alt $
>   case intersection (0...size) <$> scanLine y reports of
>     [i1,i2] | distance i1 i2 == 2 -> Just (tuningFrequency (sup i1 + 1) y)
>     [i] | i == (0...size) -> Nothing

The above cases are enough to solve, since, for various reasons, the
hole is necessarily off the square's side borders.  But here's a bit
of paranoia coverage for the case it *is* on one of the side borders:

>         | inf i >  0 -> Just (tuningFrequency    0 y)
>         | sup i < 20 -> Just (tuningFrequency size y)

And a bit of additional paranoia in case I failed at my interval
arithmetic:

>     [] -> error ("No sensor at all covers line " ++ show y)
>     _ -> error ("Multiple coverage holes in line " ++ show y)

A small checksum is all that's needed to present the result in the
format the puzzle expects.

> tuningFrequency :: Int -> Int -> Int
> tuningFrequency x y = 4000000*x + y

Let's wrap it up in a `main`, with an unpleasant “if” because the
statement example input and the puzzle inputs are not subject to the
same rules:

> main :: IO ()
> main = do
>   reports <- parse <$> getContents
>   let (scanY,size)
>         | length reports <= 15 = (10,20)
>         | otherwise = (2000000,4000000)
>   print $ part1 reports scanY
>   print $ part2 reports size

And that's enough to reap the stars!

It runs in a few seconds, so it's obviously pretty far from optimal.
From here, the typical route to speed would read something like this:

1. Optimize the raster aspect out by using space partitioning
techniques or another form of constructive geometry.  The puzzle
duality between Manhattan (sensors) and Chebyshev (scan) distances is
likely to make that more painful than it's worth, however.

2. A more “competitive programming” route would exploit the fact the
number of reports is very small (I have 33), so the cardinality of
interesting sectors in the grid is also quite small ($O(N^2)$).

3. And, of course the real pros out there exploited the structure
inferred from the what the puzzle requests as an *answer*.  Check out
the [reddit thread][reddit] for spoilers.

[reddit]: https://www.reddit.com/r/adventofcode/comments/zmcn64/2022_day_15_solutions/

All of this does make this puzzle a prime target for revisiting.  But
that's a task for another day.  See you tomorrow!
