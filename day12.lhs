---
title: "AoC Day 12: Hill Climbing Algorithm"
author: Jean-Baptiste Mazon
date: 2022-12-12T17:34:59-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: A simple grid problem
image: aoc-haskell.jpeg
---

For day 12 “Hill Climbing Algorithm”[^irony] of [Advent of Code][aoc],
we're looking for limited-descent-rate paths from a grid's top spot to
the lowest ones.  The ones labelled `S` for part 1, the ones labelled
`a` for part 2.

[^irony]: I do appreciate the irony of titling the puzzle with an
  algorithm class that's not really the one used to solve it.

[aoc]: https://adventofcode.com/2022/day/12

Continuing in the [literate Haskell][gh] groove, here are a few
imports.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day12.lhs

> import           Control.Applicative (asum,liftA2)
> import           Control.Arrow       ((&&&),(***))
> import           Control.Monad       (guard)
> import           Data.Array
> import           Data.Char           (ord)
> import           Data.List           (find,foldl')
> import           Data.Monoid         (Alt(Alt,getAlt))
> import           Data.Sequence       (ViewL((:<)),(><))
> import qualified Data.Sequence as Q
> import qualified Data.Set as Set

I'll be using the input grid's characters raw, so let's define a
function to extract an altitude out of them without having to traverse
the grid too many times to isolate its singular points.

> altitude :: Char -> Int
> altitude 'E' = ord 'z'
> altitude 'S' = ord 'a'
> altitude  x  = ord  x

No need to normalize or unbias as we're only ever comparing them to
each other, never using the absolute value.

Now let's read the grid from standard input:

> main :: IO ()
> main = do
>   rawGrid <- lines <$> getContents
>   let h = length rawGrid
>       w = length (head rawGrid)
>       grid = listArray ((1,1),(h,w)) (concat rawGrid)

We can now define point expansion routines.  Movement is standard axes
only, no diagonals:

>   let neighbors (i,j) =
>         filter (inRange (bounds grid))
>           [ (i+1,j), (i-1,j), (i,j+1), (i,j-1) ]

We can walk from a point to the other if the backwards ascent (`p'` to
`p`) doesn't go higher than 1:

>       steppable p p' = altitude (grid ! p) - altitude (grid ! p') <= 1

Node expansion is now a simple combination of the both:

>       expand = filter <$> steppable <*> neighbors

We'll look for shortest paths using a simple BFS.  We'll need to
traverse the grid once to find the search starting point.  Which
happens to be the Elves' target.

>   let Just target = find ((== 'E') . (grid !)) (indices grid)

Let's now generate the list of all distances from that point:

>       bfo = bfs target expand

We can now look for `S` or `a`, going through the list at most once.

>       lookFor c (p,i) = guard (grid!p == c) *> Alt (Just i)
>       lookForEither = scanl1 (<>) . map (lookFor 'S' &&& lookFor 'a')
>       needBoth = uncurry (liftA2 (,)) . (getAlt *** getAlt)
>   print $ asum $ map needBoth $ lookForEither bfo

That's a lot of plumbing for little actual computation.  `lookFor`
returns `Just i` when it finds the altitude it was looking for,
`Nothing` in other spots.  We call it once per puzzle part and wrap
its results in `Alt` so that we can merge them by pairs, making use of
the standard `Monoid` instances for pairs in `lookForEither`.  We then
unwrap using `getAlt` and reinterpret in a single `Maybe` functor per
pair, this time with straight `Monoid` semantics, returning `Just
(a,b)` only when both components of the input pair were `Just`s.
Finally `asum` short-circuits to the first such item, yielding both
parts' answers at once.

As support code, a fairly generic BFS:

> bfs :: Ord a => a -> (a -> [a]) -> [(a,Int)]
> bfs start expand = go (Set.singleton start) (Q.singleton (start,0)) where
>   go cl q = case Q.viewl q of
>     Q.EmptyL    ->               []
>     (p,d) :< q' -> d' `seq` (p,d) : go cl' (q' >< q'')
>       where
>         d' = d + 1
>         q'' = Q.fromList (map (, d') ns)
>         ns = filter (`Set.notMember` cl) (expand p)
>         cl' = foldl' (flip Set.insert) cl ns

This concludes today's solution.  Much more straightforward than
yesterday's, as promised.  See you tomorrow!
