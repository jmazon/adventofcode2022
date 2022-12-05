---
title: "AoC Day 5: Supply Stacks"
author: Jean-Baptiste Mazon
date: 2022-12-05T09:56:19-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: Between Hanoi and FreeCell
image: aoc-haskell.jpeg
---

In today's [Advent of Code puzzle][aoc], we're moving stacks of
~~plates~~ crates around.

[aoc]: https://adventofcode.com/2022/day/5

In the usual clash of literate programming with Haskell's strict code
ordering, here's a bunch of imports to start this [literate
Haskell][gh] post.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day05.lhs

> import Data.Array (Array,(!),(//),listArray,elems)
> import Data.Char  (isSpace)
> import Data.List  (foldl',transpose)

We're handling stacke of crates.  Crates display a letter, so let's
store that in a `Char`.  Instructions refer to stacks by index, so
let's store those in an `Array` for ~~performance~~ convenience.

> type Stacks = Array Int [Char]

The parsing could be the most complex here.  But in a twist of bad
luck, today's problem is more interesting than the two previous ones,
so I won't indulge too much detail here.

In a nutshell: I'm splitting by paragraphs to separate the initial
stacks from the instructions.  Stacks are then parsed in a very crude
“only read very specific indices” way.  Instructions are parsed in a
no less crude “expect very specific words in very specific positions,
and read decimal elsewhere”.

> parse :: String -> (Stacks,[(Int,Int,Int)])
> parse (lines -> break null -> (init -> stacks,tail -> steps)) =
>     ( listArray (1,9) $
>       dropWhile isSpace <$>
>       transpose (extractCrates <$> stacks)
>     , parseStep <$> steps
>     )
>   where
>     extractCrates l = [ l !! i | i <- [1,5,9,13,17,21,25,29,33] ]
>     parseStep (words -> ["move",n,"from",from,"to",to]) =
>       (read n,read from,read to)

Yes, `[1,5,9,13,17,21,25,29,33]` is shorter than `[1+4*i|i<-[0..8]]`
if you actually tolerated inserting a reasonable amount of whitespace.

Now most of the puzzle is simply implementing our state transition
function.

> step1,step2 :: Stacks -> (Int,Int,Int) -> Stacks

In part 1, we move crates one by one.  So we implement the state
transition recursively, moving one crate at each iteration.

> step1 stacks (0,_,_) = stacks
> step1 stacks (n,from,to) = 
>   let h:t = stacks ! from
>   in step1 (stacks // [(from,t),(to,h:stacks!to)]) (n-1,from,to)

In part 2, we move crates all at once.  No more recursion needed!

> step2 stacks (n,from,to) =
>   let (top,bot) = splitAt n (stacks!from)
>   in stacks // [(from,bot),(to,top ++ stacks!to)]

All that's left is wrap it up.

> main :: IO ()
> main = do
>   (stacks,procedure) <- parse <$> getContents
>   let result1 = foldl' step1 stacks procedure
>       result2 = foldl' step2 stacks procedure
>   putStrLn (head <$> elems result1)
>   putStrLn (head <$> elems result2)

You undoubtedly noticed I didn't demonstrate any point-free abuse this
time.  The problem was *that* much more interesting.

Anyway, that's it for today.  See you tomorrow!
