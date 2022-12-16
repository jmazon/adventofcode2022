---
title: "AoC Day 16: Proboscidea Volcanium"
author: Jean-Baptiste Mazon
date: 2022-12-16T14:02:06-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: Graph search, round 1
image: aoc-haskell.jpeg
---

[Advent of Code][aoc] today, “Proboscidea Volcanium”, has us explore a
maze of twisty little passages, all alike.  It's one of those graph
exploration puzzles of the year I enjoyed the most, as it appears to
have been globally difficult, but I felt in tune with the author's
vision and had no trouble solving cleanly.  The polar opposite of
day 19.

[aoc]: https://adventofcode.com/2022/day/16

Keeping with usual practice, this post is [literate Haskell][gh],
starting with a few imports.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day16.lhs

> import Control.Arrow               ((***))
> import Control.Applicative         (liftA2)
> import Control.Monad               (guard,forM_,when)
> import Data.Array.ST               (runSTUArray,newArray,writeArray,readArray)
> import Data.Array.Unboxed          (Ix,UArray,(!),assocs,bounds,elems,indices,listArray)
> import Data.Bits                   ((.&.),bit,testBit,setBit)
> import Data.Char                   (isDigit)
> import Data.Graph                  (Graph,graphFromEdges,vertices)
> import Data.List                   (sort)

In the end, this will be entirely solved using integers and integer
arrays.  So while my live code just went with them, let's start
cleaner and give them type names.

> type Rate = Int
> type GraphNode = (Rate,String,[String])

I'll use our usual AoC-grade `ViewPattern` parsing, and delegate the
graph's adjacency lists' construction to the standard library.  I
preprocess input by sorting the lines as a little hack to ensure our
starting point, `AA`, ends up in known index 0.

> parse :: String -> (Graph,Int -> GraphNode)
> parse (lines -> sort -> ls) = (g,v2n) where
>   valve
>     (words -> ( "Valve" : name : "has" : "flow" : (readRate -> rate)
>               : _tunnels : _leads : "to" : _valves
>               : (map (filter (/= ',')) -> vs))) =
>       (rate,name,vs)
>   readRate = read . takeWhile isDigit . dropWhile (not . isDigit)
>   (g,v2n,_) = graphFromEdges (valve <$> ls)

That adjacency list representation is a good start, but it's not the
core problem structure.  Most of the valves (nodes) are utterly
uninteresting from an exploration point of view: they release no
pressure at all.  Their sole role is to make it take longer to go from
useful valve A to useful valve B.  My puzzle input is 59 valves long,
of which only 15 are useful, so the nuance is significant.

So I'll first convert the graph to an adjacency matrix representation,
a 2D array of valve-to-valve shortest path distances, coupled with a
lookup table of individual valves' release rates.  Using the classic
Floyd-Warshall algorithm for efficient all-pairs shortest paths, which
I won't expand on too much here.

> newtype Valve = Valve Int deriving (Eq,Ord,Ix)
> type ValveRate = UArray Valve Rate
> type Dist = Int
> type DistMap = UArray (Valve,Valve) Dist
> 
> compress :: Graph -> (Int -> GraphNode) -> (ValveRate,DistMap)
> compress g v2n = (valveRate,floydWarshall) where
>   vs = vertices g
>   nodeValue (v,_,_) = v
>   valveRate = listArray ((Valve *** Valve) (bounds g))
>                 (nodeValue . v2n <$> vs)
>   floydWarshall = runSTUArray $ do
>     let (l,h) = bounds g
>     fw <- newArray ((Valve l,Valve l),(Valve h,Valve h)) (maxBound `div` 2)
>     forM_ vs $ \i -> do
>       forM_ (g ! i) $ \j -> writeArray fw (Valve i,Valve j) 1
>       writeArray fw (Valve i,Valve i) 0
>     forM_ vs $ \k -> forM_ vs $ \i -> forM_ vs $ \j -> do
>       prev <- readArray fw (Valve i,Valve j)
>       candidate <- liftA2 (+)
>                      (readArray fw (Valve i,Valve k))
>                      (readArray fw (Valve k,Valve j))
>       when (candidate < prev) $ writeArray fw (Valve i,Valve j) candidate
>     pure fw

That adjacency matrix was the point when I verified the input graph
was indeed connected enough that every valve is reachable from every
other.  Which may feel safer from a “getting lost” point of view, but
will tend to make the search combinatorics big.  So something to be
aware of.

I'll then simplify the graph representation to eliminate all
irrelevant valves.  So we're keeping only our starting point and the
valves with a non-null release rate.  I'm shifting the starting
valve's index down to $-1$ as a tweak to have the useful valves start
from $0$.  This will make for cleaner code when we'll start using them
as bit indices.

> simplify :: ValveRate -> DistMap -> (ValveRate,DistMap)
> simplify valveRate dist = (valveRate',dist') where
>   keep = Valve 0 : filter ((> 0) . (valveRate !)) (indices valveRate)
>   v'min = Valve (-1)
>   v'max = Valve (length keep - 2)
>   valveRate' = listArray (v'min,v'max) ((valveRate !) <$> keep)
>   dist' = listArray ((v'min,v'min),(v'max,v'max))
>             (liftA2 (curry (dist !)) keep keep)

(This is something I'm only doing now to provide a clean view.  When I
initially got my stars, I just wedged a reindexing table between
bitsets and adjacency matrices, which was cumbersome and somewhat
error-prone.)

With all of that set up, now's the time to actually search through.

A bit of groundwork is in order.  What class of search algorithm is
appropriate here?

We're looking for the maximum total released pressure in a fixed time
frame.  The total released pressure can be expressed as the sum of the
opened valves' release rates weighted by how long they'll have been
open.  So the order in which we open them is crucial.

And that's scary: the number of orderings is $O(N!)$.  Even for a
small figure such as my input's $15$, that's still a borderline order
of magnitude in the billions.

What saves us here is the graph structure.  Remember all those useless
valves?  They serve to space out the useful ones.  We have a limited
time to walk around, 30 minutes in part 1 and 26 in part 2, and the
average distance between two useful valves is more than 1, so we're
likely never really going to have paths that long.

The uncomfortable truth is that it's still tricky to estimate whether
it'll be enough to work.  But at least we have hope.  Let's explore!

> type Time = Dist
> newtype ValveSet = ValveSet Int deriving (Eq,Ord,Ix)
> type Pressure = Rate
> type ReleaseMap = UArray ValveSet Pressure

I'll use a DFS for simplicity, the agenda queue being reified directly
in the call stack.  To avoid looping, I'll maintain a closed set of
valves already opened.^[This is valve *opening* looping we're talking
about.  Visiting a valve on the way to another one is still possible,
it just happens to be hidden behind our Floyd-Warshall adjacency
matrix.]  That closed set can double to remember the best total
released pressure observed with such a set of opened valves, which we
can then construct puzzle answers from.  (A single set of opened
valves tends to have multiple possible total released pressure counts
because we may have opened them in different orders.)  Since the
starting valve is never to be opened, I'll perform the cutoffs and
decisions to go or not before recursing, which may be somewhat unusual
for a typical DFS.  But I'll expand inline.

> dfs :: ValveRate -> DistMap -> Time -> ReleaseMap

Take as arguments our valve release rate lookup table, our distance
matrix, and the allocated time, which varies between parts 1 and 2.
Return the aforementioned map of best releases by opened set.

> dfs valveRate dist time = runSTUArray $ do

Partition our valves between the starting point and the destination
ones we'll explore.  They really are separate: we never intend to
return to the starting point since there is no pressure to be released
there.

>   let valveStart : valveTargets = indices valveRate

Initialize our result map with an initial value of 0 total released
pressure.

>   res <- newArray (vsEmpty,vsFull (length valveTargets)) 0

Now program the recursive search.  It takes as parameters the closed
set^[Closed in the graph search meaning of “not open for exploration
anymore”.  Of course, in the present case, it means “of valves we
already opened”.  I'd be curious to know if Eric did *that* on
purpose.], the current valve position, the remaining time and the
current total released pressure.

What does “current total released pressure” even mean?  Well, since
we're exploring from the start position forward, we always know how
much remaining time we have, so we always know how much any valve we
open will contribute to the total released pressure in the end, even
if our exploration hasn't reached the end yet.  This saves us from
having to follow each individual path to the end: any valve we reach
immediately yields a candidate best result.

>   let go cl v t p = do

From our current position, what other yet-unopened valves are
reachable?^[Disregarding whether they're opened yet, they happend to
all be reachable in the puzzle inputs, but the code would work in the
other case too: the big distance we used in Floyd-Warshall as an
initial “unreachable” value would simply be detected as bringing us
over time in the next step.]

>         forM_ (filter (not . (`vsMember` cl)) valveTargets) $ \v' -> do

Let's walk there and open it.  And deduct the time that takes from the
remaining time we have.

>           let t' = t - dist!(v,v') - 1

If there's no time left after that, there's no observable contribution
to the bottom line, so we can cut off immediately.

>           when (t' > 0) $ do

Aggregate the valve's contribution to total pressure release, and save
it if it improves over what we previously had for this opened valve
set.

>             let p' = p + t' * valveRate!v'
>                 cl' = vsInsert v' cl
>             writeArray res cl' . max p' =<< readArray res cl'

And recurse from there!

>             go cl' v' t' p'

Initialize the search, run it and return the results.

>   go vsEmpty valveStart time 0
>   pure res

In part 1, things are simple: we're looking for the most pressure we
can release during those 30 minutes, so we can directly compute it
from the map our search returns.

> part1,part2 :: ReleaseMap -> Pressure
> part1 = maximum . elems

This is also when we can verify that our search was indeed feasible.
And it was!  My deepest reachable path opened 9 valves, which is much
better than the feared worst case of the entire 15.

In part 2, we have fewer allocated minutes, but we're allowed two
concurrent agents.

This is trickier.  The first half of it is easier: running the same
search with a time limit of 26 reduces the deepest reachable path to
8 valves, which is likely a 15-fold speed boost.  On the other hand,
that would only return the smaller total pressure release we're bound
to get with less time, while the puzzle asks us for the bigger total
pressure release we're striving to achieve by parallelizing work.

So how do we parallelize?

The core performance issue doing things properly is that while
duplicating the number of positions explored at once will obviously
have a better rate of opening valves, it's also going to result in a
combinatorial explosion of search states.  Consider this: the part 1
space empirically consisted of an up-to-9-bits-out-of-15 closed set,
one position out of 15, and a fuzzily discrete remaining time.  For
part 2 we have a full 15-bit closed set, two positions, and a
higher-density fuzzily discrete remaining time.  Abstracting over the
fuzzy remaining time, that's still a 20% increase for the closed set
and a 15-fold increase in positions.

Wait.

That's absolutely not as intractable as I thought it was when I got my
stars.  The only real issue is managing position/time state, since
with the current representation most of the time an agent will be
between two valves.

Anyhow.  There's a better way.

We were already able to explore the entire search space with a single
agent, and that space is even smaller with the reduced minutes.  The
process to release the maximum total pressure is going to have both
agents open non-overlapping subsets of the valves.  (If both attempt
to open the same valve, the 1 minute it takes to open it is *wasted*
for one of the agents, who could have used it to try to go and open
another one.  Seen differently: if we count the agents' individual
contribution to total pressure released, this double opening could
only be taken into account for one of them.)

But before we computed the maximum in part 1, we happened to know the
total pressure released for any subset of valves reachable by a single
agent!  With that information available, it's trivial to run a nested
loops search in there for subsets that have no overlap!

Sadly, it's also quadratic.  But the figures are small enough that it
works (I have a few thousand pre-squaring).

> part2 m = maximum $ do
>   let cls = filter ((> 0) . snd) (assocs m)
>   (ValveSet cl1,p1) <- cls
>   (ValveSet cl2,p2) <- cls
>   guard (cl1 .&. cl2 == 0)
>   pure (p1 + p2)

And that's all there is to it.  A small wrapper to bind it all:

> main :: IO ()
> main = do
>   (vr,d) <- uncurry simplify . uncurry compress . parse <$> getContents
>   print $ part1 $ dfs vr d 30
>   print $ part2 $ dfs vr d 26

And a little type-supporting code for completeness.

> vsEmpty :: ValveSet
> vsEmpty = ValveSet 0
>
> vsFull :: Int -> ValveSet
> vsFull size = ValveSet (bit size - 1)
> 
> vsMember :: Valve -> ValveSet -> Bool
> vsMember (Valve b) (ValveSet bs) = bs `testBit` b
> 
> vsInsert :: Valve -> ValveSet -> ValveSet
> vsInsert (Valve b) (ValveSet bs) = ValveSet (bs `setBit` b)

This concludes today's solution, which is a strong contender for being
my favorite puzzle of the year.  See you tomorrow!
