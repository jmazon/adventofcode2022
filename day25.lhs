---
title: "AoC Day 25: Full of Hot Air"
author: Jean-Baptiste Mazon
date: 2022-12-25T11:22:03-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: So it ends
image: aoc-haskell.jpeg
---

Interesting numerals!  We hadn't got a lot of that this year, so “Full
of Hot Air”, today's final [Advent of Code][aoc] 2022 puzzle, stands
up to the challenge.

[aoc]: https://adventofcode.com/2022/day/25

[Literate Haskell][gh] because it's the name of the game, though
today's import list might not stimulate your imagination as much as
usual.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day25.lhs

> import Data.List (foldl1')

Ok, so we're implementing addition on a balanced quinary numbering
system.  Let's parse.

> fromB :: Char -> Int
> fromB '=' = -2
> fromB '-' = -1
> fromB '0' =  0
> fromB '1' =  1
> fromB '2' =  2
>
> parse :: String -> [[Int]]
> parse = map (reverse . map fromB) . lines

There are more or less two easy ways to play this.  We can either
convert balanced quinary to native, add on native, and convert back to
balanced quinary.

Or we can add directly on balanced quinary.

My favorite.

> add :: [Int] -> [Int] -> [Int]
> add = go 0 where
>   go c (a:as) (b:bs) =
>     let s = c + a + b
>         (c',d) = if | s < (-2) -> (-1,s + 5)
>                     | s >   2  -> ( 1,s - 5)
>                     | otherwise -> (0,s)
>     in d : go c' as bs
>   go c [] [] = [c]
>   go c [] bs = go c [0] bs
>   go c as [] = go c as [0]

Granted, we still need to reorder terms for proper presentation.

> toB :: Int -> Char
> toB (-2) = '='
> toB (-1) = '-'
> toB   0  = '0'
> toB   1  = '1'
> toB   2  = '2'
>
> format :: [Int] -> String
> format = reverse . map toB

Whew.  Let's wrap it all in a `main` function.

> main :: IO ()
> main = interact $ format . foldl1' add . parse

Yay!  Done at last!

Hot take: Advent of Code remains one of the best-quality problemset
out there.  I'm merely starting to notice a decrease in quality when
comparing it to its former years.

That's probably be worthy of a postmortem of its own.

In the meantime, let's appreciate the fact I appear to have ranked
4631<sup>th</sup> this year.  And have not solved a single puzzle in
more than 24 hours.  Not bad for a transition year.

This will definitely not be remembered as my best year of writing
about it, but I'm glad I could *mostly* maintain the pace.  For the
longest time I thought I was only writing this for myself, but at some
point I started getting actual feedback, Mastodon DMs, GitHub pull
requests, oh my!

So wow.

Like, I'm really going to have to implement comments here now.

This concludes the regularly scheduled posts about this.  I hope I'll
be able to follow up.  At any rate, it's been a pleasure writing to
you.  See you soon!
