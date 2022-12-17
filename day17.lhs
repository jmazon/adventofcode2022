---
title: "AoC Day 17: Pyroclastic Flow"
author: Jean-Baptiste Mazon
date: 2022-12-17T18:19:08-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: I am the coder who arranges the rocks
image: aoc-haskell.jpeg
---

In today's [Advent of Code][aoc] puzzle, “Pyroclastic Flow”, we're
playing a bastardized, depressing “game” of Tetris.  Bastardized
because there's no rotation; depressing because there's no winning.
The saving grace is there's no playing either, all we get to do is
watch.

[aoc]: https://adventofcode.com/2022/day/17

Let's write some imports to maintain our strict [literate Haskell][gh]
tradition going.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day17.lhs

> import           Control.Applicative        (liftA2)
> import           Control.Arrow              ((&&&),(>>>))
> import           Control.Lens               (makeLenses,makeWrapped,Lens',(^.),(%=),(.=),(+=),(+~),(.~),use,uses,view,zoom,_Wrapped)
> import           Control.Monad              (replicateM_)
> import           Control.Monad.Reader       (Reader,runReader,fix)
> import           Control.Monad.State.Strict (MonadState,StateT,evalStateT,execStateT,get,modify)
> import           Data.Array                 (Array,(!),bounds,listArray)
> import           Data.Bits                  (setBit)
> import           Data.Function              ((&),on)
> import           Data.List                  (foldl',groupBy)
> import qualified Data.Map.Strict as Map
> import qualified Data.Set as Set
> import qualified Data.Vector.Unboxed as V
> import           Data.Word                  (Word8)
> import qualified Linear

This puzzle wasn't really hard to solve or get stars for.  What's
trickier now is solving it while keeping some minimum viable
mathematical rigor.  And it'll be slower too, both in coding time and
runtime.  Oh well.

What makes our puzzle unique is its input: the bitstream of left/right
values the cave's jet pattern loops on.  Part 1 could be nicely solved
with it stored in an infinite list, but I'll save it to an array to
ease part 2 later on.  We'll remember where we are by maintaining a
clock for it.

> newtype Pattern = Pattern (Array Int Char)
> makeWrapped ''Pattern
>
> parse :: String -> Pattern
> parse pats = Pattern (listArray (0,length pats - 1) pats)

As is common this year, I'll use the `linear` package to implement
coordinates.  As is less common this year, I'll use positive X
coordinates to go up and positive Y coordinates to go right.  Y going
right doesn't really matter except for strict statement
interpretation.  On the other hand, X going up strongly risks being
confusing for everybody.  So I'll rename them $i$ for up and $j$ for
horizontal.

> type V = Linear.V2 Int
> v :: Int -> Int -> V
> v = Linear.V2
> _i,_j :: Linear.R2 t => Lens' (t a) a
> _i = Linear._x; _j = Linear._y

To reduce risk of error while consuming the clocked jet pattern, I'll
package it up in a small utility.

> nextDir :: StateT Int (Reader Pattern) V
> nextDir = do
>   (l,h) <- bounds <$> view _Wrapped
>   let np = h - l + 1
>   p <- liftA2 (!) (view _Wrapped) get
>   modify $ (`mod` np) . succ
>   pure $ case p of
>     '<' -> v 0 (-1)
>     '>' -> v 0   1

The falling rocks are just a list of coordinates.  I'll index them so
the lowest point is on $i=0$ and the leftmost on $j=0$.  Shapes and
their sequence are known in advance.

> type Shape = [V]
> 
> dash,plus,ell,eye,square :: Shape
> dash = [v 0 0,v 0 1,v 0 2,v 0 3]
> plus = [v 0 1,v 1 0,v 2 1,v 1 2,v 1 1]
> ell = [v 0 0,v 0 1,v 0 2,v 1 2,v 2 2]
> eye = [v 0 0,v 1 0,v 2 0,v 3 0]
> square = [v 0 0,v 0 1,v 1 0,v 1 1]
> 
> nShapes :: Int
> nShapes = 5
> 
> shapes :: Array Int Shape
> shapes = listArray (0,nShapes-1) [dash,plus,ell,eye,square]

The cave is a tall narrow rectangle.  It's vertically unbounded, so
I'll represent it as a set.  For the same reason, it'll start with a
floor, but for lateral walls we'll just remember to check for
collision by coordinates instead of by set lookup.

> type Cave = Set.Set V
> 
> cave0 :: Cave
> cave0 = Set.fromList [ v 0 i | i <- [0..6] ]

The main operation we'll perform on a cave is query its current
height.  Since we put the floor on $i=0$, the height is merely the $i$
coordinate in the set.  Which necessarily exists since the set doesn't
start empty.

(This is why we want the vector's first coordinate to be vertical:
fast access to the highest element.)

> height :: Cave -> Int
> height = view _i . maximum

Now let's combine all of this to make a rock fall.  Still operating on
a state monad over the jet pattern index.

> fallRock :: Cave -> Shape -> StateT Int (Reader Pattern) Cave

Falling works in an alternating sequence of atomic moves: pushes due
to the jets and falls due to gravity.  We start with a push.

> fallRock cave shape = push startPos where

Starting position is a function of the cave's current highest element:
4 positions higher to provide the requested spacing, and 2 units off
the left wall.

>   startPos :: V
>   startPos = maximum cave & _i +~ 4 & _j .~ 2

Moves apply some geometric translation to the shape, check for
collision, then decide on an outcome.  We can factor that.

>   attemptMove f success failure pos = do
>     let pos' = f pos
>         shape' = (pos' +) <$> shape
>     if all (liftA2 (&&) (>= 0) (< 7) . view _j) shape'
>        && all (`Set.notMember` cave) shape'
>       then success pos'
>       else failure pos

So then pushes consume the next direction from the jet pattern, and
attempt such a horizontal move.  Whether the move succeeds or not, the
flow proceeds to fall.

>   push pos = do
>     dir <- nextDir
>     attemptMove (+ dir) fall fall pos

Falls just attempt moving 1 unit down.  Successes trigger a new cycle;
failure assimilates the rock into the cave for the next round.

>   fall = attemptMove (+ v (-1) 0) push stop
>   stop pos = pure $ foldl' (flip Set.insert) cave ((pos +) <$> shape)

Let's expand our state a bit.  We've only been using the jet pattern
clock up to now.  Let's add a shape clock, the current cave
representation, and another statistic we'll use later in part 2.

> data St = St
>   { _shapeClock :: !Int
>   , _patternClock :: !Int
>   , _curCave :: !Cave
>   , _maxFall :: !Int
>   }
> st0 :: St
> st0 = St { _shapeClock = 0, _patternClock = 0, _curCave = cave0, _maxFall = 0 }
> makeLenses ''St

Same as we did for the jet pattern, we can wrap the next shape's
consumption in a small function.

> nextShape :: MonadState St m => m Shape
> nextShape = do
>   clock <- use shapeClock
>   shapeClock += 1
>   pure $ shapes ! (clock `mod` nShapes)

And combine all of the above into a single step function.^[If anyone
knows how to massage the code into a single `%=` on `curCave` instead
of the current `get`, operate, `put` sequence, I'd like to know too.
Swapping the order of arguments to `fallRock` is a likely start, but
definitely not enough.]

> fallNextRock :: StateT St (Reader Pattern) ()
> fallNextRock = do
>   cave <- use curCave
>   shape <- nextShape
>   cave' <- zoom patternClock (fallRock cave shape)
>   curCave .= cave'

Nothing more's needed for part 1.  A bit of point-free abuse because
it's Advent, and the result drops in.

> part1 :: Int -> Pattern -> Int
> part1 =
>   runReader .
>   fmap (height . view curCave) .
>   flip execStateT st0 .
>   flip replicateM_ fallNextRock

For part 2, simulating all of those steps is going to be too long.
But our useful state is small enough that at some point, we're bound
to be doing the same thing as we did before, so we can detect a loop
and factor it out.

On what parts of state does the future depend?  The two clocks, shape
and jet pattern, are a start, but strictly speaking are not enough:
the number of jet pushes a single rock will consume depends on what it
will encounter on its way down.

Now the cave is narrow.  Intuitively, though its starting state
“empty” is very different from its cruise speed state, at some point,
there just aren't so many different ways to prevent a rock from
falling down, and it's very likely the loop will form.

While initially solving it for the stars, I simply waited for the
first time a clock combination was revisited for the first time, and
assumed the next clock loop found after that would be a full state
loop.

Doing it cleanly now, we'll actually check a rock in the same jet
pattern state as previously also falls upon the same save.  Or at
least, the part of the cave that matters.

Which part is that?  It's the part the falling rock can “interact
with”, so its falling trajectory.  Tracking that strictly is a bit of
a pain, but we can simplify by expanding to the entire section of the
cave, left to right, over the entire height of its fall.

But shapes won't all have the same fall height!  So we'll consider the
top section of the cave over the maximum fall height we've observed up
to now.  Let's start tracking that:

> fallNextRock' :: StateT St (Reader Pattern) ()
> fallNextRock' = do
>   prev <- use patternClock
>   fallNextRock
>   fall <- subtract (prev + 1) <$> use patternClock
>   maxFall %= max fall

A rock's fall height is the difference in pattern clocks around its
fall, minus one since in a sequence there's always one more jet push
than successful gravity falls.

We'll serialize the top section of the cave over however many levels
we're going to end up using.^[In my case, 38.  Which is a lot more
than my intuition would have guessed.]  The cave is 7 positions wide,
so it packs nicely into a `Word8`.

> serializeTop :: Int -> Cave -> V.Vector Word8
> serializeTop h =
>   Set.toDescList                                  >>>
>   groupBy ((==) `on` view _i)                     >>>
>   map (foldl' (\bs p -> bs `setBit` (p ^. _j)) 0) >>>
>   V.fromListN h

And we can start implementing part 2 proper.  We start with the same
monad stack:

> part2 :: Int -> Pattern -> Int
> part2 target =
>   runReader $ flip evalStateT st0 $

To track loops, we'll maintain a map of key to current height.  The
key being the two clocks and the serialized top of the cave.  It may
seem scary that the tops of caves in the map won't all have the same
size.  It's not a problem in practice: we'll merely catch the cycle a
bit later, after the max observed fall height stabilizes.^[It's
tempting to assume a change in max observed height implies a different
state.  That isn't the case.  There could very well be a loop between
clock states whose individual rocks have a very short fall, with
high-fall rocks in-between.  It's the price of having simplified the
observed state to the max fall height!]

>   flip fix Map.empty $ \loop cl -> do
>     clock <- use shapeClock
>     let k1 = clock `mod` nShapes
>     k2 <- use patternClock
>     k3 <- serializeTop <$> uses maxFall (subtract 2) <*> use curCave
>     let key = (k1,k2,k3)

I'm subtracting two from the max fall height.  This is the three top
empty positions the rock falls across, minus the bottom landing
position the rock doesn't fall across, but has to count as being
there for the state.

>     h <- uses curCave height
>     case Map.lookup key cl of
>       Nothing -> do
>         fallNextRock'
>         loop (Map.insert key (clock,h) cl)
>       Just (clock0,h0) -> do
>         let (q,r) = (target - clock) `divMod` (clock - clock0)
>         replicateM_ r fallNextRock
>         h' <- uses curCave height
>         pure $ h' + q * (h - h0)

A simple wrapper to call it all.  (`head . words` is a hack to not be
too sensitive to whether or not my input has a newline at the end.)

> main :: IO ()
> main = interact $
>   show . (part1 2022 &&& part2 1000000000000) . parse . head . words

This concludes today's solution.  See you tomorrow!
