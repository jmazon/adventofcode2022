---
title: "AoC Day 19: Not Enough Minerals"
author: Jean-Baptiste Mazon
date: 2022-12-18T15:20:37-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: Graph search, round 2
image: aoc-haskell.jpeg
---

In today's [Advent of Code][aoc] puzzle “Not Enough Minerals”, we're
optimizing robot construction so as to crack as many geodes as
possible in the alloted time, Whatever that means.

[aoc]: https://adventofcode.com/2022/day/19

This is a prime candidate for my least favorite puzzle of the year, as
I really suck at this kind of impure heuristic-tuning OR task.  Let it
be known that the code I'm presenting here is pretty far removed from
the dog slow horror that got me my stars.

Oh well.  Here a few imports, to get the [literate Haskell][gh] ball
rolling.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day19.lhs

> import           Control.Applicative (liftA2)
> import           Control.Lens        (makeLenses,Getter,Lens',(<&>),(&)
>                                      ,(^.),(%~),(.~),(+~),(-~),view)
> import           Data.Foldable       (foldl',traverse_)
> import           Control.Monad       (guard)
> import           Data.Bifunctor      (first)
> import           Data.Maybe          (catMaybes,mapMaybe,maybeToList)
> import           Linear              ((*^))

The set of minerals is four-dimensional.  We'll only be storing
`Int`s, but I'm making it generic so I can have an easy `Applicative`
instance and perform bulk operations on them.

> data Minerals a = Minerals
>   { _ore      :: !a
>   , _clay     :: !a
>   , _obsidian :: !a
>   , _geode    :: !a
>   }
>   deriving (Eq,Ord,Functor)
> type Mineral = Lens' (Minerals Int) Int
> makeLenses ''Minerals

The blueprint is the factory settings, that determines our graph's
structure by assigning different costs to the various robots.  The
last field, *caps*, is the visible part of a first
search-space-reducing heuristic: there is no point in building more
robots for a certain type of mineral than we can consume in a single
turn.  Since its figures are constant within a blueprint, I'm storing
it along.

> data BluePrint = BluePrint
>   { _bpId                   :: !Int
>   , _oreRobotOreCost        :: !Int
>   , _clayRobotOreCost       :: !Int
>   , _obsidianRobotOreCost   :: !Int
>   , _obsidianRobotClayCost  :: !Int
>   , _geodeRobotOreCost      :: !Int
>   , _geodeRobotObsidianCost :: !Int
>   , _caps                   :: !(Minerals Int)
>   }
> type Cost = Getter BluePrint Int
> makeLenses ''BluePrint

Parsing a blueprint will use our same old AoC-style optimistic view.
That has the benefit of closely verifying that indeed, though the
robot costs themselves may change, the cost structure—which specific
minerals are needed—is very static.

> parseBP :: String -> BluePrint
> parseBP (words ->
>          [ "Blueprint",init -> read -> _bpId
>          , "Each","ore","robot","costs", read -> _oreRobotOreCost,"ore."
>          , "Each","clay","robot","costs", read -> _clayRobotOreCost,"ore."
>          , "Each","obsidian","robot","costs",read -> _obsidianRobotOreCost,"ore"
>          , "and",read -> _obsidianRobotClayCost,"clay."
>          , "Each","geode","robot","costs",read -> _geodeRobotOreCost,"ore"
>          , "and",read -> _geodeRobotObsidianCost,"obsidian."
>          ])
>   = let _caps = Minerals oreCap clayCap obsidianCap maxBound
>         oreCap = maximum
>           [ _oreRobotOreCost
>           , _clayRobotOreCost
>           , _obsidianRobotOreCost
>           , _geodeRobotOreCost
>           ]
>         clayCap = _obsidianRobotClayCost
>         obsidianCap = _geodeRobotObsidianCost
>     in BluePrint{..}

A state will be a vector of minerals in stock, and a vector of robots
we've already built.

> data St = St
>   { _stock  :: !(Minerals Int)
>   , _robots :: !(Minerals Int)
>   }
>   deriving (Eq,Ord)
> makeLenses ''St

We start with nothing but an ore robot.

> st0 :: St
> st0 = St 0 0 & robots . ore .~ 1

I'll implement some form of search.  It's mostly a DFS.  But without
going as far as remembering and excluding already-visited states.  And
with a branch-and-bound margin improvement cutoff.

I've tried a lot of variations, and this is the one that ends up
fastest with my codebase.  It's all pure intellectual masturbation at
this point anyway: when debating the best algorithm for the subject
requires having a terminating one to compare against, the puzzle is
solved before we even begin.  Such are the joys of OR…

> maxGeodes :: Int -> BluePrint -> Int
> maxGeodes deadline bp = go [(deadline,st0)] 0 where
>   go [] !best = best
>   go ((remaining,st):q') !best =

My improvement cutoff function verifies that a state could beat the
current known score—the number of geodes at timeout.  It does this by
adding the count of geodes already cracked open with the number of
geodes that can't fail to produce—the number of geode robots times the
remaining time—, and an optimistic bound of how many geodes we'd crack
open if we reached the theoretical optimum of building a
geode-cracking robot per turn.

>         let mayImprove (remaining',st') = do
>               let score = st' ^. stock.geode +
>                        remaining' * st' ^. robots.geode
>               guard (score + remaining' * (remaining'-1) `div` 2 > best)
>               pure (score,(remaining',st'))

Expand the nodes.  First check, for each robot type, when if ever it
can first be build.

>             (best',q'') =
>               catMaybes
>                 [ futureRobot bp st geode geodeRobotOreCost
>                     (Just (geodeRobotObsidianCost,obsidian))
>                 , futureRobot bp st obsidian obsidianRobotOreCost
>                     (Just (obsidianRobotClayCost,clay))
>                 , futureRobot bp st clay clayRobotOreCost Nothing
>                 , futureRobot bp st ore oreRobotOreCost Nothing
>                 ]                       &

Convert duration to remaining time.

>               map (first (remaining -)) &

Cull insufficient remaining times.  We can afford to require $1$
instead of $0$ here, as completing a geode exactly at the end of the
alloted time still won't produce its first one in time to make a dent
in the total score.

>               filter ((>= 1) . fst)     &

Reduce search space by clamping state values to sensible maxima.

>               map (clamp bp)            &

Ensure expanded states have margin for improvement.

>               mapMaybe mayImprove       &

Aggregate best observed score for further cutoff.

>               unzip & first (maximum . (best :))

And recurse!

>         in go (q'' ++ q') best'

The state clamping function will limit states to stockpiling enough
minerals to build the most expensive robot once per turn per consumed
mineral.

> clamp :: BluePrint -> (Int,St) -> (Int,St)
> clamp bp (t,st) =
>   (t, st
>       & stock.ore %~ min (t*maximum[ bp^.oreRobotOreCost
>                                    , bp^.clayRobotOreCost
>                                    , bp^.obsidianRobotOreCost
>                                    , bp^.geodeRobotOreCost ])
>       & stock.clay %~ min (t * bp^.obsidianRobotClayCost)
>       & stock.obsidian %~ min (t * bp^.geodeRobotObsidianCost)
>   )

State expansion computes the smallest duration before a robot can be
built.  It first cuts off any attempt to build past the robot caps
computed earlier.

> {-# INLINE futureRobot #-}
> futureRobot :: BluePrint -> St -> Mineral -> Cost -> Maybe (Cost,Mineral)
>             -> Maybe (Int,St)
> futureRobot bp st mineral oreCost costMineral =
>   guard (st^.robots.mineral < bp^.caps.mineral) *>

It then verifies we actually have robots producing the category of
minerals required.  Note this isn't needed for ore, as we start with
one of those.

>   traverse_ (\(_,r) -> guard (st^.robots.r > 0)) costMineral *>

The needed time is then a rounded-up quotient of requirements by
production.  There's always a computation for ore, and optionally one
for a second mineral.

>   let t = maybe id
>           (\(c,r) -> max ((bp^.c - st^.stock.r) `divC` (st^.robots.r)))
>           costMineral $
>           (bp^.oreCost - st^.stock.ore) `divC` (st^.robots.ore)

The successor state is then computed, by increasing resource count by
the production over that time, paying for the robot, and registering
its creation.

>   in pure ( t + 1
>           , foldl' (&) st
>             ( (stock +~ (t+1) *^ (st^.robots))
>               : (robots.mineral +~ 1)
>               : (stock.ore -~ bp^.oreCost)
>               : maybeToList (costMineral <&>
>                              (\(c,r) -> (stock.r) -~ (bp^.c)))
>             )
>           )

A small helper for rounded-up (“ceiling”) division.

> divC :: Int -> Int -> Int
> divC dividend divisor = max 0 ((dividend + divisor - 1) `div` divisor)

Parts 1 and 2 are mostly the same thing.  Just a different checksum
function, and a bit of input filtering for part 2.

> part1,part2 :: [BluePrint] -> Int
> part1 = sum . map ((*) <$> view bpId <*> maxGeodes 24)
> part2 = product . map (maxGeodes 32) . take 3

Here's a `main` wrapper for a standalone executable.

> main :: IO ()
> main = interact $
>   unlines . sequence [show . part1,show . part2] . map parseBP . lines

The rest of the code is the boilerplate needed[^need] to be able to
express resource creation in a single line as we did above.  It's
obviously not worth it, right?

[^need]: Of course, not all it is actually used.  Does that make it
  unneeded?  The Haskell numeric typeclasses, though not perfect,
  still are intended to come as atomic blocks, and I don't want to
  have to track individual methods' use.

> instance Applicative Minerals where
>   pure a = Minerals a a a a
>   {-# INLINE pure #-}
>   Minerals a b c d <*> Minerals e f g h = Minerals (a e) (b f) (c g) (d h)
>   {-# INLINE (<*>) #-}
>
> instance Num a => Num (Minerals a) where
>   (+) = liftA2 (+)
>   {-# INLINE (+) #-}
>   (-) = liftA2 (-)
>   {-# INLINE (-) #-}
>   (*) = liftA2 (*)
>   {-# INLINE (*) #-}
>   negate = fmap negate
>   {-# INLINE negate #-}
>   abs = fmap abs
>   {-# INLINE abs #-}
>   signum = fmap signum
>   {-# INLINE signum #-}
>   fromInteger = pure . fromInteger
>   {-# INLINE fromInteger #-}

Anyhow, that's it for today.  I hate those OR-minded AoC puzzles with
a passion, but I do feel I'm getting slightly better at them every
time.

See you tomorrow!
