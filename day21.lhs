---
title: "AoC Day 21: Monkey Math"
author: Jean-Baptiste Mazon
date: 2022-12-21T19:32:41-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: Firing a BFG9000 at a monkey problem
image: aoc-haskell.jpeg
---

We'll perform some “Monkey Math” for today's [Advent of Code][aoc]
puzzle.  Using a totally overpowered solver, because I didn't want to
follow along and type all of the code for the part 2 twist.^[A total
lie, I really got my stars by typing all the code for the part 2
twist.  But it didn't feel satisfying, and I'd wanted to get familiar
with Z3 some time ago already, so here we are with a rewritten
solution.]

[aoc]: https://adventofcode.com/2022/day/21

No surprise, this is still [literate Haskell][gh] and a header of
imports.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day21.lhs

> import           Data.Bifunctor     (first)
> import           Data.Map.Lazy      (Map,(!))
> import qualified Data.Map.Lazy as Map
> import           Data.SBV
> import           Data.SBV.Internals (SMTModel(SMTModel),modelAssocs)

The traditional mid-puzzle twist is a great idea.  Its realization, on
the other hand, is a bit disappointing, as it not only reinterprets
the input, which is always interesting, but actually goes as far as
making part of the puzzle input useless, which is a bit lame.

So I can't easily have a single parsing function to solve it all.  Or
rather, I can't parse to a really usable form, I must keep along
pseudo-redundant information to be hammered back into shape for
part 2.

Too bad.

So my intermediate representation will be a map of monkey name to a
pair of an operation and a pair of monkey names to perform it on.
Yes, this does allow for the puzzle input's literals, implemented as
an operation that simply ignores its inputs and returns what it wants.

> type Parse = Map String (SInteger -> SInteger -> SInteger,(String,String))

Parsing is then uninteresting, you may just want to notice I'm using
`SInteger`s instead of the standard Haskell types.  More on that
later.

> parse :: String -> Parse
> parse = Map.fromList . map monkey . lines where
>   monkey (words -> (init -> name) : ws) = (name,) $ case ws of
>     [read -> n] -> (const (const (fromInteger n)),undefined)
>     [m1,op,m2] -> case op of
>       "+" -> ( (+),(m1,m2))
>       "-" -> ( (-),(m1,m2))
>       "*" -> ( (*),(m1,m2))
>       "/" -> (sDiv,(m1,m2))

The data structure I'd really want to be handling is the map of monkey
to numbers.  Here's how to obtain it from the parse result.

> apply :: Parse -> Map String SInteger
> apply m = m' where m' = Map.map (\(op,~(a,b)) -> op (m'!a) (m'!b)) m

The lazy pattern match `~(a,b)` is there so I can afford not to write
out an entire pair of `undefined`s when parsing literals.^[The benefit
of that being later on negated by me having to explain it.]

In part 1, we're merely computing the value assigned to the monkey
named root.  Let's go overboard and solve it with Z3.  I'll first
define a symbolic variable named root.

> part1,part2 :: Parse -> Symbolic SBool
> part1 m = do
>   root <- sInteger "root"

And then equate it with the one in the monkey map.

>   solve [apply m!"root" .== root]

A small utility then compiles that to Z3 and extracts the resulting
binding.

> resolve :: Provable p => p -> IO ()
> resolve p = do
>   SatResult (Satisfiable _ SMTModel{modelAssocs = [res]}) <- sat p
>   print res

This is obviously not needed, I could really just read the value out
of `SBV` without calling a solver on it, or even just compute it at
parse time.^[As I got my first star, obviously.]

But for part 2, direct parsing isn't enough: we need to reverse the
operations of the entire tree chain from root to the monkey named
hunm.  Which is too much typing.

So I'll replace the monkey named humn's literal with a named symbolic
variable, replace root's operation with a subtraction, and equate its
value to $0$.

> part2 m = do
>   humn <- sInteger "humn"
>   let m' = Map.insert "humn" (const (const humn),undefined) $
>            Map.adjust (first (const (-))) "root" m
>   solve [apply m'!"root" .== 0]

A little wrapper then concludes the code.

> main :: IO ()
> main = do
>   m <- parse <$> getContents
>   resolve $ part1 m
>   resolve $ part2 m

So I think it's the second time this year I'm mildly disappointed at a
puzzle that was pretty close to being very nice, but where some minor
aspect of the part 2 twist ruins the fun.  Really, just making the
root monkey's operation a subtraction would have gone such a long way…

Oh well.

See you tomorrow!
