---
title: "AoC Day 8: Treetop Tree House"
author: Jean-Baptiste Mazon
date: 2022-12-08T10:18:27-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: A bit of imperative array computation
image: aoc-haskell.jpeg
---

Let's do some 2D array manipulation for today's [Advent of Code][aoc]!

[aoc]: https://adventofcode.com/2022/day/8

This post is [literate Haskell][gh].  I've tried to keep the import
list short, but I'll still need a few things.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day08.lhs

> import Data.Array    (Array,(!),bounds,inRange,indices,listArray)
> import Data.Array.ST (newArray,runSTArray,writeArray)
> import Data.Char     (digitToInt)
> import Data.Function (fix)
> import Control.Monad (forM_,when)
> import Linear.V2     (V2(V2))

We'll be manipulating 2D coordinates in arrays all the time, so let's
try and use a sensible type for that.

> type V = V2 Int

Let's assume we've already parsed the input to a 2D array of tree
heights represented as `Int`s.  In part 1, we'd like to know which
trees in the grid are visible from the outside, which we'll represent
as a `Bool`.

> part1 :: Array V Int -> Array V Bool

A tree is visible from a side if it is the tallest in the line of
trees from itself to that side.  This can be nicely computed in linear
time by taking it the other way: start from a side and dive in; for
each tree encountered while going across, mark it as visible if it's
taller than the tallest tree to that point, then update the height of
the tallest tree yet seen in this line.

We'll start with a tallest seen tree of $-1$ to make sure the tree at
the edge is visible no matter what.

We accept a tree as visible if it is visible from any side, so we'll
start with an initial visibility map of `False` and unconditionally
mark as `True` when we discover new visible trees.

It's still unnatural imperative style code, but sometimes it has to be
done.  Start by allocating the result array.

> part1 g = runSTArray $ do
>   result <- newArray (bounds g) False

Define a function to perform all visibility checks from a given side.
Represent said side as a list of starting points, the `edge`, and a
direction.

>   let beam edge dir =

Iterate on the starting points.

>         forM_ edge $ \p0 ->

Remember the tallest height seen to date, starting at $-1$, and
iterate.

>           flip fix (p0,(-1)) $ \loop (p,h) ->

Test we're still in range.

>             when (inRange (bounds g) p) $ do

Determine visibility by comparing to current tallest known height.

>               when (g!p > h) $ writeArray result p True

Recurse, updating next considered tree position and new tallest known
height.

>               loop (p + dir,max h (g!p))

With that handy function, all we have to do is call it four times,
once per side, with the appropriate fringe and direction.

>   let (V2 t l,V2 b r) = bounds g
>   beam [V2 t j | j <- [l..r]] (V2 1 0)
>   beam [V2 b j | j <- [l..r]] (V2 (-1) 0)
>   beam [V2 i l | i <- [t..b]] (V2 0 1)
>   beam [V2 i r | i <- [t..b]] (V2 0 (-1))
>   pure result

Counting `True`s would then basically yield the AoC checksum.

For part 2, we consider visibility *from within* the grid.  There's a
fancy way to compute it in linear time, but it requires getting out
the DP, and the AoC input is so small it's really not worth it.  Maybe
later this year!

Instead we'll simply do exactly what it says and shoot rays out from
every single tree in the grid, counting how far until sight is
obstructed.  Quadratic but simple.

And no imperative bullshit this time: we can return the array values
directly:

> part2 :: Array V Int -> Array V Int
> part2 g = listArray (bounds g) (scenicScore <$> indices g)

The scenic score is defined as the product of the viewing distances in
the 4 directions.

>   where
>     scenicScore p =
>       product $ viewingDistance p <$> [V2 1 0,V2 (-1) 0,V2 0 1,V2 0 (-1)]

The viewing distance is the distance to the first tree as tall or
taller as the starting point.

>     viewingDistance p dir =
>       length $ takeWhile' (< g!p) $ map (g!) $ rayFromDir' p dir

A first helper function yields valid tree indices in a line from a
given starting point.

>     rayFromDir' p d = tail $ takeWhile (inRange (bounds g)) $ iterate (+d) p

A second helper will behave like standard Prelude function
`takeWhile`, but additionally returning the first list element that
fails the test.

>     takeWhile' :: (a -> Bool) -> [a] -> [a]
>     takeWhile' f = go where
>       go [] = []
>       go (x:xs) = x : if f x then go xs else []

And we're done!

Well, almost.  Just a little wrapper to parse the input and compute
the output:

> main :: IO ()
> main = do
>   raw <- lines <$> getContents
>   let w = length (head raw)
>       h = length raw
>       grid = listArray (0,V2 (h-1) (w-1)) (digitToInt <$> concat raw)
>   print $ sum $ fmap fromEnum $ part1 grid
>   print $ maximum $ part2 grid

This concludes today's puzzle.  See you tomorrow!
