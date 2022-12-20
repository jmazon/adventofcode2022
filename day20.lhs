---
title: "AoC Day 20: Grove Positioning System"
author: Jean-Baptiste Mazon
date: 2022-12-20T10:54:31-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: "*Yawn*"
image: aoc-haskell.jpeg
---

Today's [Advent of Code][aoc] puzzle “Grove Positioning System”, is
mostly a disappointment.  It's not really hard compared to its
position in the month, part 2 doesn't really bring anything
interesting to the story, and my nifty coding to solve the first part
optimally was rendered totally useless by the secondone.  Which
doesn't help appreciate the statement's genius.

[aoc]: https://adventofcode.com/2022/day/20

Anyway, it's not that bad, but there really is nothing more to it than
mere implementation.  This makes for fine stars, but a boring
write-up.  Which starts right here, [literate Haskell][gh]-style, with
precious few imports.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day20.lhs

> import           Control.Arrow ((&&&))
> import           Data.Sequence (Seq,ViewL((:<)),(<|),(><))
> import qualified Data.Sequence as Q

The input is a simple list of numbers.  We parse it to a sequence.

> parse :: String -> Seq Int
> parse = Q.fromList . map read . lines

If I had been using vectors, I'd have this function backwards, but for
free.

> indexed :: Seq a -> Seq (a,Int)
> indexed = Q.mapWithIndex (flip (,))

Mixing can be thought of as 4 operations, performed as many times as
we have elements:

1. let $x$ be the $i$<sup>th</sup> element; find its offset
2. split the sequence around it to $left_0 x right_0$
3. split sequence $right_0 left_0$ in the $x$<sup>th</sup> position
(modulo sequence length)
4. insert $x$ there

> mix :: Seq (Int,Int) -> Seq (Int,Int)
> mix = go 0 where
>   go n s | n == Q.length s = s
>          | otherwise =
>     let Just offset0 = Q.findIndexL ((== n) . snd) s
>         (left0,Q.viewl -> (x,_) :< right0) = Q.splitAt offset0 s
>         offset = x `mod` (Q.length s - 1)
>         (left,right) = Q.splitAt offset (right0 >< left0)
>     in go (n+1) (left >< (x,n) <| right)

To prove we performed the task, we're to sum the numbers in positions
1000, 2000, 3000.  Wow.

> checksum :: Seq (Int,a) -> Int
> checksum s =
>   let Just z = Q.findIndexL ((== 0) . fst) s
>       i = fst . Q.index s . (`mod` Q.length s) . (+ z)
>   in i 1000 + i 2000 + i 3000

For part 1, that's it.

> part1,part2 :: Seq Int -> Int
> part1 = checksum . mix . indexed

For part 2, we encounter the totally shocking surprise that we have to
perform mixing repeatedly, for a total count of a mind-boggling… 10.

> part2 = checksum . (!! 10) . iterate mix . indexed . fmap (811589153 *)

Really, the only change for part 2 is to retract the
complexity-nerfing optimization you may have had in there before,
multiply the input list for some reason, and… there's no and.

> main :: IO ()
> main = interact $ show . (part1 &&& part2) . parse

This completes today's see you tomorrow.
