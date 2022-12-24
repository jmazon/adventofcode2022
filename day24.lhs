---
title: "AoC Day 24: Blizzard Basin"
author: Jean-Baptiste Mazon
date: 2022-12-24T19:35:21-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: The final BFS before the finish line
image: aoc-haskell.jpeg
---

Let's tackle “Blizzard Basin”, today's penultimate [Advent of
Code][aoc] 2022 puzzle.  We are to walk across a 2D grid, avoiding
moving obstacles.

[aoc]: https://adventofcode.com/2022/day/24

Not going to change a winning horse: this post is as [literate
Haskell][gh] as ever, leading the way with some imports.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day24.lhs

> import           Data.Array ((!),accumArray,assocs,bounds,indices,inRange,listArray)
> import           Data.List  (find,foldl')
> import           Data.Maybe (mapMaybe)
> import           Linear     (V2(V2))
> import qualified Data.Set as Set

If there's one year change I'm going to stay with, it's using `linear`
to handle my vectors.

> type V = V2 Int

All I need to solve those pathfinding problems is a simple function to
tell me where I'm allowed to step.  In this puzzle it depends on the
turn count, so I'll take that as an input argument.

> type Walkable = Int -> V -> Bool

Parsing ought to provide me with the following:

> data Env = Env
>   { walkable :: !Walkable
>   , entry    :: !V
>   , exit     :: !V
>   }

How so?  Let's start by just reading the grid.

> parse :: String -> Env
> parse (lines -> ls) = Env{..} where
>   h = length ls
>   w = length (head ls)
>   grid = listArray (V2 1 1,V2 h w) (concat ls)

Entry and exit point happen to be the first/last spot in the raw
input.

>   Just entry = find (walkable 0) (indices grid)
>   Just exit = find (walkable 0) (reverse (indices grid))

We still need that `walkable` function.  Let's divide and conquer:
delegate to checking whether there are any horizontal or vertical
blizzards going across that given position.

>   walkable t v@(V2 i j) =
>     inRange (bounds grid) v &&
>     grid!v /= '#' &&
>     not (any (\b -> b t v) (hBlizzards ! i)) &&
>     not (any (\b -> b t v) (vBlizzards ! j))

Horizontal blizzards are detected by their representation in the
puzzle input: greater to and less than signs.  I'll convert those to a
function of time, modulo map width.

>   hBlizzards = accumArray (flip (:)) [] (1,h) $ mapMaybe mkHB (assocs grid)
>   mkHB (V2 i0 j0,c) = case c of
>     '>' -> Just (i0,\t (V2 _ j) -> (j-j0-t) `mod` (w-2) == 0)
>     '<' -> Just (i0,\t (V2 _ j) -> (j-j0+t) `mod` (w-2) == 0)
>     _   -> Nothing

Conversely, vertical blizzards use other input characters and work
along grid height.

>   vBlizzards = accumArray (flip (:)) [] (1,w) $ mapMaybe mkVB (assocs grid)
>   mkVB (V2 i0 j0,c) = case c of
>     'v' -> Just (j0,\t (V2 i _) -> (i-i0-t) `mod` (h-2) == 0)
>     '^' -> Just (j0,\t (V2 i _) -> (i-i0+t) `mod` (h-2) == 0)
>     _   -> Nothing

Solving part 1 is as simple as a BFS.

> solve :: Walkable -> V -> V -> Int -> Int
> solve walkable src dst = bfs (Set.singleton src) where
>   bfs open t
>     | dst `Set.member` open = t
>     | otherwise = go (Set.elems open) Set.empty where
>     go [] next = bfs next $! t + 1
>     go (p:ps) next
>       | not (walkable t p) = go ps next
>       | otherwise = go ps $ foldl' (\n dp -> Set.insert (p+dp) n) next
>                             [ V2 0 0, V2 (-1) 0, V2 1 0, V2 0 (-1), V2 0 1 ]

Really, the only tricky part in that was remembering that waiting in
place is a very viable option.

For part 2, we just tack on a return-and-back trip to the end of
part 1.

> main :: IO ()
> main = do
>   Env{..} <- parse <$> getContents
>   let t1 = solve walkable entry exit 0
>       t2 = solve walkable exit entry t1
>       t3 = solve walkable entry exit t2
>   print t1
>   print t3

This concludes today's solution.  Can't wait for the final puzzle!
