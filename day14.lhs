---
title: "AoC Day 14: Regolith Reservoir"
author: Jean-Baptiste Mazon
date: 2022-12-14T07:37:03-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: The sand must flow.
image: aoc-haskell.jpeg
flags: [ mathjax ]
---

<script src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" type="text/javascript" async></script>

In “Regolith Reservoir”, day 14 of [Advent of Code][aoc], we'll drop
sand and watch it settle among rocky structures.  This post is
[literate Haskell][gh], starting with a few imports.

[aoc]: https://adventofcode.com/2022/day/14
[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day14.lhs

> import Control.Arrow      ((&&&))
> import Control.Lens       ((^.),view,_1)
> import Control.Monad.Cont (callCC,lift,runContT,when)
> import Control.Monad.ST   (ST,runST)
> import Data.Array
> import Data.Array.ST      (STUArray,thaw,readArray,writeArray)
> import Data.Function      (fix)
> import Data.List          (tails)
> import Data.List.Split    (wordsBy)
> import Data.STRef         (newSTRef,readSTRef,modifySTRef')
> import Linear.V2          (V2(V2),_y)

2D vectors in a grid.  The cells' contents don't really matter to us,
all that counts is whether *something*'s in there, be it rock or sand.

> type V = V2 Int
> type Grid = Array V Bool

Half of the problem is parsing the input.  It's not the interesting
part, so I won't comment much more apart from:

* allocate a fixed size grid
* parse the coordinates
* convert to line segments
* fill in the 2D array where appropriate

> parse :: String -> Grid
> parse = accumArray (||) False (V2 0 0,V2 1000 500) . map (,True) .
>         concatMap toPoints . concatMap (tails . parseLine) . lines
>   where
>     parseLine :: String -> [V]
>     parseLine = map parseV . filter (/= "->") . words
> 
>     parseV :: String -> V
>     parseV (wordsBy (== ',') -> [i,j]) = V2 (read i) (read j)
> 
>     toPoints :: [V] -> [V]
>     toPoints (V2 a b:V2 c d:_)
>       | a == c = [ V2 a j | j <- [min b d..max b d] ]
>       | b == d = [ V2 i b | i <- [min a c..max a c] ]
>     toPoints _ = []

And now the real meat: filling the playing grid up with sand.

As you may have guessed from the parsing code, I'm only considering
positions where $0 \leq x \leq 1000$ and $0 \leq y \leq 500$, as sand
can't really get outside of that pyramid-shaped mound.  Whether we're
in part 1 and it overflows to the side, or in part 2 and it gobbles
back up to the source, but it's all contained therewithin^[And
*that*'s definitely not a word I use everyday.].

There's a common trap in that kind of “follow a very specific path
many times” puzzle, when the path is mostly the same from a time to
the next.  That is: simulating every grain's path individually will
tend to make things quadratic.  Consider this: my part 2 answer is in
the order of 30k.  Grains don't all follow each other, of course, but
within the pyramid, the longest path can be expected to be as long as
a pyramid diagonal.  Area being 30k, that makes for a diagonal of
about 173.  (easiest to draw it to realize what's going on)

So to trace those 30k paths, we're looking at an algorithm that's
easily $O(30~000×173)$, so in the order of… five millions.

Wait, is that it?

Oh well, enjoy your sneak peak into higher-level grain simulation,
with a solution that's merely linear instead of $O(N^{3\over
2})$.^[You're very allowed to remind me what the adjective for
**that** is.]  At the expense of $O(\sqrt N)$ (stack) space.

Part 1 asks for the number of grains of sand to settle before the
first overflow.  Part 2 asks for the number of grains of sand to
settle before the starting position is taken.  Which is another kind
of overflow, though less pictorial.  So, really, we're counting grain,
until some kind of event decides we're not anymore.

I'll implement that mostly in the `ST s` monad, filling in a mutable
grid as we pour sand in, updating an `STRef` with the count of grain.

To allow for interruption, I'll transform the monad with `ContT`.  The
exit gateway will be a simple “just return what's in the sand
counter”.

The core of the algorithm will be to consider the flow not as a
repetition of some kind of “let a new grain of sand fall” operation.
But instead see it as “infinitely fill up the grid with sand falling
from this location”.

Let's see how this goes.

> simulate :: Bool -> Grid -> Int

We're taking a flag and a grid in, and return the number of grains of
sand we successfully settled before something bad happened.  The flag
is obviously “is this part 1 or 2?” but its definition can be refined
as “do we simulate a floor?”  In which case we'll immediately wonder:
“where is it?”

> simulate haveFloor g0 = runST $ do
>   let floorLevel = maximum (view (_1 . _y) <$> filter snd (assocs g0)) + 2

We're in `ST s`.  Let's thaw our grid and initialize our sand counter.

>   g <- thaw g0 :: ST s (STUArray s V Bool)
>   count <- newSTRef 0

We want easy interruptibility.  We don't really care about any value
the deeper stack could bring up, we're only couting sand.  Through the
`STRef` counter we just defined.  So that's all our `ContT` handler
will yield.

>   flip runContT (const (readSTRef count)) $ callCC $ \exit ->

For every new position we want the sand to pour from, we recursively
call `go`.  Starting from the initial pour position $(500,0)$.

>     flip fix (V2 500 0) $ \go p -> do

Adding sand lower than the floor level means we overflowed.  So exit.

>       when (p ^. _y > floorLevel) (exit ())

Ok, so we're above ground.  Still, we'd better check if we're trying
to flow someplace that's already taken.

>       blocked <- lift (readArray g p)

This is the specific place we operate differently between part 1 and
part 2.  Obviously we only try to add sand if the spot is free, but
more importantly we also limit that to when either there is no floor
(part 1) or we're above it.

>       when (not blocked && (not haveFloor || p ^. _y < floorLevel)) $ do

Our spot is free.  First choice is to spill downwards.

>         go (p + V2 0 1)

Once spilling downwards is complete, as literally as to return control
flow here, we can assume the spot one cell lower to be full, so we
spill leftwards.

>         go (p + V2 (-1) 1)

And then rightwards.

>         go (p + V2 1 1)

We've filled the lower layer, at least downwards and diagonally
downwards.  It's time to fill up the current spot then let the
recursion take care of whatever upper or upper-derived spots there may
be.

So fill it up and count one.

>         lift (writeArray g p True)
>         lift (modifySTRef' count succ)

It's worth taking a second to understand what's happening in both
parts' cases.

For part 1, we're waiting for overflow.  We do not have a floor, so
the algorihtm will escape as soon as a grain reaches a Y coordinate
below the virtual floor level.

For part 2, there's no interruption.  We're done once our
grain-feeding algorithm completes.  Since each level has a maximum
branching factor of 3 and necessarily completes since there's a lower
bound on Y coordinates, it has to complete.  There's obviously a large
overlap between sublevels' positions, so the complexity is not as bad
as $O(N^{3\over 3})$.^[That's $O(\sqrt N)$ (the average path length)
to the third (branching factor) power.]  If you've been following the
geometrical picture, you already know it's $O(N)$.^[This seems like
the most appropriate place to remind $N$ here is the output order of
magnitude, so a square root of any of the input's 2D coordinates.
It's a constant factor away from the number of sand grains poured in.
It's notably not proportional to the input *size*, since it is so
efficiently encoded.

It's worth noticing the input could very add a trailing $0$ to every
coordinate (ok, recentering on $X=500$) and make our life miserable at
very little cost to global evil.

There are definitely ways around that: just make more visual use of
the pyramid shapes.  It's just more complicated than it's worth for my
*actual* input dimensionality.]

To wrap both parts, a simple `main` will do.

> main :: IO ()
> main = interact $ show . (simulate False &&& simulate True) . parse

There's a fair amount of runtime overlap between both parts.  Namely,
part 1 is a strict subset: the first grain of sand that overflowss off
part 1 to complete it would be the first grain to settle on the floor
for part 2.

I'm not grouping the flows just now as my times are not only well
beneath a second, but also part 1's time is negligible with respect to
part 2's.

And… this concludes today's problem!  See you tomorrow!
