---
title: "AoC Day 2: Rock Paper Scissors"
author: Jean-Baptiste Mazon
date: 2022-12-02T09:01:10-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: May contain traces of modular arithmetic.
image: aoc-haskell.jpeg
---

[Day 2][aoc] of Advent of Code brings back the perma-classic
rock-paper-scissors.  The gist of the day is to fill in a round's data
from partial information.  We are always given the opponent move.  In
part 1 we determine outcome from our move.  In part 2 we do the
opposite: we determine our move from the outcome.

[aoc]: https://adventofcode.com/2022/day/2

This post is [literate Haskell][gh] post.  Let's clear the floor with
a few imports.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day02.lhs

> import Control.Category ((>>>))
> import Control.Applicative (liftA2)

The model here is a classic rock-paper-scissors game.  At each round,
a player plays a move.

> data Move = Rock | Paper | Scissors deriving Enum

A round has three possible outcomes, relative to a specific player.

> data Outcome = Defeat | Draw | Victory deriving Enum

So the data for a round can be summarized as:

> data Round = Round
>   { opponentMove :: Move
>   , selfMove     :: Move
>   , outcome      :: Outcome
>   }

Now, that data is redundant.  That's the whole point of today's
challenge!

In part 1, we're given both players' moves.  So the round result has
to be computed using the standard rule table.

> readRoundPart1 :: String -> Round
> readRoundPart1 [a,_,b] = Round{..} where
>   opponentMove = readLeft a
>   selfMove = case b of
>     'X' -> Rock
>     'Y' -> Paper
>     'Z' -> Scissors
>   outcome = toEnum ((fromEnum selfMove - fromEnum opponentMove + 1) `mod` 3)

I'm using a bit of modular arithmetic to compute the outcome, because,
on such small set cardinalities, it's a reasonable implementation of
permutation group theory.

Just think of it this way: the result is derived from a comparison
(`-`) of both players' moves.  Equality (zero difference) should
result in a draw, so we increase (`+ 1`) to reach `Draw`'s index in
the `Outcome` enum.  We then verify that either victory or defeat
works properly.  For example, I play paper (1), the opponent plays
rock (0), so the difference is 1, 2 after the draw adjustment.  This
nicely falls on the “Victory” outcome, so we do nothing more.^[Of
course, this isn't a coincidence: I specifically picked the `Outcome`
enum's ordering to get that.  But if it was a given, you could just
negate.]

We're going to need the actual parsing for the left column.  It's a
separate function because we're going to be reusing it for part 2.

> readLeft :: Char -> Move
> readLeft 'A' = Rock
> readLeft 'B' = Paper
> readLeft 'C' = Scissors

Now we know all about our rounds, we can compute the individual
rounds' scores.

> roundScore :: Round -> Int
> roundScore = liftA2 (+) shapeScore outcomeScore where
>   shapeScore Round {selfMove} = case selfMove of
>     Rock     -> 1
>     Paper    -> 2
>     Scissors -> 3
>   outcomeScore Round {outcome} = case outcome of
>     Defeat  -> 0
>     Draw    -> 3
>     Victory -> 6

And we can implement the puzzle as a much more simple pipeline as
yesterday's.

> main :: IO ()
> main = interact $
>   lines >>>
>   flip map >>>
>   flip map [readRoundPart1,readRoundPart2] >>>
>   map (
>     map roundScore >>>
>     sum
>   ) >>>
>   show

Part 2 has us compute move from outcome.

This is made trivial by the modular arithmetic we used above: all we
have to do is solve an equation.

$$
outcome \equiv self\_move - opponent\_move + 1 \pmod 3
$$

$$
self\_move \equiv outcome + opponent\_move - 1 \pmod 3
$$

> readRoundPart2 :: String -> Round
> readRoundPart2 [a,_,b] = Round{..} where
>   opponentMove = readLeft a
>   outcome = case b of
>     'X' -> Defeat
>     'Y' -> Draw
>     'Z' -> Victory
>   selfMove = toEnum ((fromEnum outcome + fromEnum opponentMove - 1) `mod` 3)

And that's all there was to it!

This concludes today's problem.  See you tomorrow!
