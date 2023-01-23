---
title: "AoC Day 23: Unstable Diffusion"
author: Jean-Baptiste Mazon
date: 2022-12-23T12:39:32-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: Just do it already
image: aoc-haskell.jpeg
---

Today's [Advent of Code][aoc] puzzle, “Unstable Diffusion”, is the
cellular automaton of the year.  Cells on a grid are either elven or
dead, and evolve according to a simple pattern.

[aoc]: https://adventofcode.com/2022/day/23

This is [literate Haskell][gh] because I wouldn't want to upset
anyone by now, and it starts with a few imports.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day23.lhs

> import           Control.Arrow   ((&&&))
> import           Control.Lens    (view)
> import           Linear          (V2(V2),_x,_y)
> import           Data.List       (elemIndex,find)
> import           Data.Maybe      (catMaybes)
> import           Data.Map.Strict (Map)
> import qualified Data.Map.Strict as Map
> import           Data.Set        (Set)
> import qualified Data.Set as Set

So we have a set of elves again.  I'll represent them with their 2D
positions.

> type V = V2 Int

Parsing is just labeling each hash mark in the input with its
coordinates.

> parse :: String -> Set.Set V
> parse = Set.fromList . concat . zipWith row [0..] . lines where
>   row i = catMaybes . zipWith (col i) [0..]
>   col i j '#' = Just (V2 i j)
>   col _ _  _  = Nothing

The diffusion we're managing today has a few simple rules.  I'm mostly
following them, I'll only allow myself a little twist to make my life
easier: whereas the statement says if an elf is isolated, they don't
move, I'm instead expressing it as if isolated elves proposed a “stay
in place” move.

Why doesn't it change anything?  When moves are resolved in phase 2,
only moves where the target doesn't collide get performed.  Moving to
the current place is the same as not moving, but what about the
influence on elves that attempt to move to the same place?  It turns
out there can't be any, as move proposals only happen when there's
nobody in that general direction.

So let's implement that.  We're computing the set of elf positions
after turn $i$, considering the former set of elf positions.

> step :: Set V -> Int -> Set V
> step cur i = next where

Each elf proposes its next position.  As mentioned earlier, an
isolated elf, *i.e.* one with no neighbors, proposes to remain in
place.  The others go in the first cardinal direction from their
preference list with no one nearby, be it directly in that direction
or diagonally.

>   proposal from
>     | all ((`Set.notMember` cur) . (from +)) neighbors = from
>     | otherwise = maybe from head $
>                   find (all (`Set.notMember` cur)) $
>                   (map . map) (from +) attempts

Preferred direction cycles per turn.

>   attempts0 =
>     [ [ V2 (-1)  0,  V2 (-1)   1,  V2 (-1) (-1) ]
>     , [ V2   1   0,  V2   1    1,  V2   1  (-1) ]
>     , [ V2   0 (-1), V2 (-1) (-1), V2   1  (-1) ]
>     , [ V2   0   1,  V2 (-1)   1,  V2   1    1  ]
>     ]
>   (al,ar) = splitAt (i `mod` 4) attempts0
>   attempts = ar ++ al

Let's compute proposed position for each elf and store that in a map.

>   proposals = Map.fromSet proposal cur

Between phases 1 and 2, we check for conflicts.  A conflict is a cell
two^[Theoretically, or more, but it can't happen here.] elves want to
move to during the same turn.

>   conflicts = Map.filter (> 1) $ Map.fromListWith (+) $
>                 zip (Map.elems proposals) (repeat 1)

The actual movement thus only happens in absence of conflict.

>   move from prop = if prop `Map.member` conflicts then from else prop

And we can compute the global next set.

>   next = Set.fromList $ Map.elems $ Map.mapWithKey move proposals

The set of neighbors is a constant.

> neighbors :: [V]
> neighbors =
>   [ V2 (-1) (-1), V2 (-1) 0, V2 (-1) 1
>   , V2   0  (-1),            V2   0  1
>   , V2   1  (-1), V2   1  0, V2   1  1
>   ]
 
Running the entire simulation is then a simple matter of repeatedly
invoking the `step` function on an infinite list of turn indices.

> run :: Set V -> [Set V]
> run = flip (scanl step) [0..]

For part 1, we're asked for an arbitrary checksum of the final
position's span.

> checksum :: Set V -> Int
> checksum s = (imax - imin + 1) * (jmax - jmin + 1) - Set.size s
>   where
>     imin = minimum (view _x <$> Set.elems s)
>     imax = maximum (view _x <$> Set.elems s)
>     jmin = minimum (view _y <$> Set.elems s)
>     jmax = maximum (view _y <$> Set.elems s)
> 
> part1 :: Set V -> Int
> part1 = checksum . (!! 10) . run

For part 2, we merely time stability.

> part2 :: Set V -> Maybe Int
> part2 = fmap succ . elemIndex True . (zipWith (==) =<< tail) . run

And that's it!

> main :: IO ()
> main = interact $ show . (part1 &&& part2) . parse

This concludes today's solution.  See you tomorrow!
