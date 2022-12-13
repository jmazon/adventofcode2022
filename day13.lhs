---
title: "AoC Day 13: Distress Signal"
author: Jean-Baptiste Mazon
date: 2022-12-13T12:05:08-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: Making good use of the infrastructure
image: aoc-haskell.jpeg
---

Today's [Advent of Code][aoc] puzzle, “Distress Signal”, is one of
those classic define-a-bespoke-ordering-on-a-data-structure problems.

[aoc]: https://adventofcode.com/2022/day/13

We'll tackle it by writing as little code as possble, which translates
to a largeish import list.  Reproduced right from the start, as this
is [literate Haskell][gh].

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day13.lhs

> import           Control.Applicative ((<|>),liftA2)
> import           Data.Aeson          (FromJSON,eitherDecodeStrict',parseJSON,withArray,withScientific)
> import           Data.List           (findIndices,sort)
> import           Data.Foldable       (toList)
> import           Data.Functor        ((<&>))
> import           Data.Scientific     (floatingOrInteger)
> import qualified Data.ByteString.Char8 as BS

A packet is either an integer or a list of recursive packets.^[If
you've read the problem, you'll know this is factually wrong: a packet
there is necessarily a list.  My extension doesn't result in further
inconsistency down the road, so I'll go with it as it gives the
datastructure better regularity, with no need to single out a separate
“value” type.]

> data Packet = I Int | L [Packet]

A packet as given in the puzzle input happens to be valid JSON, so
let's parse it from there and spare futile head-scratching.

> instance FromJSON Packet where
>   parseJSON = liftA2 (<|>) parseInt parseList
>     where
>       parseInt = withScientific "integer" $
>         floatingOrInteger                           >>>
>         either (const (fail "Not an integer")) pure >>>
>         fmap I
>       parseList = withArray "list" $
>         toList             >>>
>         traverse parseJSON >>>
>         fmap L

To parse globally, I'll assume the input is well-formed and ignore the
explicit pairings by skipping blank lines and deferring the re-pairing
to part 1 which is the only one to make use of it.

> main :: IO ()
> main = do
>   Right packets <-
>     BS.getContents <&>
>     BS.lines               >>>
>     filter (not . BS.null) >>>
>     traverse eitherDecodeStrict'
>   print (part1 packets)
>   print (part2 packets)

Now the puzzle revolves entirely around being able to sort packets.

Comparing two integer values is straightforward, I'll delegate it to
the underlying `Int`s:

> comparePackets :: Packet -> Packet -> Ordering
> comparePackets (I a)  (I b)  = compare a  b

Comparing two list values follows the same lexicographical ordering
rules as standard Haskell lists, provided their elements have a proper
ordering.  Which is not the case for us here yet, as there's no `Ord`
instance on `Packet`s.  But assuming we could define one, we could
delegate all the same:

> comparePackets (L as) (L bs) = compare as bs

Last, comparing an integer to a list works as if promoting the integer
to a single-element list.

> comparePackets a@I{} l = comparePackets (L [a]) l
> comparePackets l b@I{} = comparePackets l (L [b])

So we need to define an `Ord` instance on `Packet`s.  How would we do
that?  Well, all we need to do is call our `comparePackets` function!

> instance Eq Packet where (==) = ((== EQ) .) . comparePackets
> instance Ord Packet where compare = comparePackets

It's worth pondering why we needed both an `Ord` instance and an
external comparison function.  The reason is that sneaky `Eq` instance
above.  It's needed to declare an `Ord` instance.  And though what
I'll be doing probably doesn't need to call it, I don't want to be
liable for breakage when that changes later on.^[And while we
explicitly only call `(<)` in part 1, the `sort` function we use in
part 2 is much more opaque and liable to freedom.]

In part 1, we're only looking to identify the correctly ordered pairs.

> part1,part2 :: [Packet] -> Int
> part1 = sum . map succ . findIndices (uncurry (<)) . pairs

In part 2, we're looking for the sorted positions of two special
markers called “dividers”.

> part2 = let dividers = [L[L[I 2]],L[L[I 6]]] in
>   product . map succ . findIndices (`elem` dividers) . sort . (dividers ++)

And that's all there is to it!

Well, except for a little bit of boring support code.^[`(>>>)` is
already present in the standard library, but with a fixity that's not
perfect for our use here.  And it's shorter to redefine it than to
import it altering fixity, so…]

> pairs :: [a] -> [(a,a)]
> pairs (a:b:xs) = (a,b) : pairs xs
> pairs [] = []
> (>>>) :: (a -> b) -> (b -> c) -> a -> c
> (>>>) = flip (.)
> infixr 2 >>>

This concludes today's solution.  See you tomorrow!
