---
title: "AoC Day 7: No Space Left On Device"
author: Jean-Baptiste Mazon
date: 2022-12-07T13:24:09-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: Hybrid fun
image: aoc-haskell.jpeg
---

Today we have a drastic shift for [Advent of Code][aoc], in both
difficulty of implementation and resulting program length.

[aoc]: https://adventofcode.com/2022/day/7

I'm still going [literate Haskell][gh], so you're not getting away
with the rest of the post without me first dropping a few imports.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day07.lhs

> import           Control.Applicative      (Alternative,empty)
> import           Control.Arrow            ((&&&))
> import           Control.Monad.RWS.Strict (join,evalRWS,get,put,modify,tell)
> import           Data.List                (find,foldl',sort,unfoldr)
> import           System.FilePath.Posix    ((</>),takeDirectory)
> import qualified Data.Map.Strict as Map

The name of the game today is accounting for aggregated file sizes in
a tree structure.

Today is the first day in a long time where I can see a plethora of
different ways to implement it.

* we could do it somewhat optimally and reproduce a clean tree
  structure of size occupied the files at the local directory level,
  summing it up a single time for the aggregate volumes.
* we could do it somewhat brutally and convert the input into an
  actual filesystem representation of what the Elves have, and then
  analyze it with regular filesystem tools.  A cursory glance at the
  input file verifies it's safe enough.  That's not the way I picked,
  but it's definitely one of the fun ones, and if nobody on the
  subreddit did I will have to.
* I picked a more hybrid way.

I am going to merely generate a list of path-size pairs, where path
represents the complete directory trace from root to file.  I will
then, file by file, aggregate those sizes upwards in the directory
structure.  This is time-suboptimal because I am summing each file
individually on each level it can contribute to, as opposed to summing
only once per first-degree directory entry (file or subdir).  It gains
a lot on the code simplicity front: I skip on building a tree
structure, and I can use standard third-party functions to parse
paths.

It's not so obvious to deduce a time complexity from that.  For a
given tree structure, it's obvious my way is $O(n_{files} ×
max\_file\_depth)$, when optimal would be $O(n_{files}
\log(max\_file\_depth))$.  But actually constructing a worst case out
of that formula from a constant-size input such as mine gets tricky.
And on my input, total time remains small no matter what.

So I'll start with a stateful parsing phase.  The state will be the
current path.  The output will be the list of $(file,size)$ pairs.

> type FileEvent = (FilePath,Int)

I'll tokenize by words.  Starting state is root directory `/`.

> parse :: String -> [FileEvent]
> parse s = snd $ evalRWS (readCommand (words s)) () "/"
>   where
>     readCommand [] = pure ()

Directory-changing commands will update the state accordingly and
yield nothing.

Changing to root is a pure state assignment:

>     readCommand ("$":"cd":"/":ts) = put "/" *> readCommand ts

…whereas relative changes make use of standard directory-handling
functions and operators.

>     readCommand ("$":"cd":"..":ts) = modify takeDirectory *> readCommand ts
>     readCommand ("$":"cd":p:ts) = modify (</> p) *> readCommand ts

The command that lists entries switches parsing from command mode to
file listing parsing mode.

>     readCommand ("$":"ls":ts) = readLsOutput ts

We remain in list parsing mode until we encounter something that looks
like a command again.  Or reach end of input.

>     readLsOutput ts@("$":_) = readCommand ts
>     readLsOutput [] = pure ()

Encountering a subdirectory in a listing would be very useful if we
were actually generating those commands.  But we're merely analyzing
their result, so we don't need to do anything specific when
discovering a new directory.

>     readLsOutput ("dir":_:ts) = readLsOutput ts

Encountering a file is the real meat: we get to generate one of those
coveted filesystem entry pairs.

>     readLsOutput (fs:fn:ts) = do
>       p <- get
>       tell [(p </> fn,read fs)]
>       readLsOutput ts

Once we have those entries, we can aggregate sizes on all levels.

The outer loop handles files one by one:

> type FileSystem = Map.Map FilePath Int
> 
> buildFileSystem :: String -> FileSystem
> buildFileSystem = foldl' registerFile Map.empty . parse

And the inner loop adds their sizes to each of their parent
directories' tally.

> registerFile :: FileSystem -> FileEvent -> FileSystem
> registerFile t0 (path,size) = foldl' account t0 (trail path)
>   where account t fp' = Map.insertWith (+) fp' size t

The `trail` helper enumerates a path's parent directories.

> trail :: FilePath -> [FilePath]
> trail = ("/" :) . unfoldr (fmap dup . guarded (/= "/") . takeDirectory)

`guarded` and `dup` are semi-standard auxiliaries I could have
imported from most anywhere else but didn't bother to this time.

>   where
>     guarded :: Alternative f => (a -> Bool) -> a -> f a
>     guarded p a = if p a then pure a else empty
>     dup :: a -> (a,a)
>     dup = join (,)

Once we have this directory to aggregate volume mapping, solving
part 1 is a simple matter of performing the requested computation:
summing all directory sizes among directories of size smaller than
100 000.

You'll notice there's really no need for directory names or paths
anymore, so we'll operate directly on the sizes.

> part1 :: [Int] -> Int
> part1 = sum . filter (<= 100000)

A small wrapper gets us in business:

> main :: IO ()
> main = interact $ show . (part1 &&& part2) . Map.elems . buildFileSystem

Ah, yes, part 2.  There we are to perform a more convoluted
computation.  We could probably set the math down and simplify it, but
it's really a constant-time operation much better left to the compiler
to optimize.  As if it could have any significant runtime impact.

> part2 :: [Int] -> Maybe Int
> part2 ss = find (>= target) (sort ss)
>   where
>     size = 70000000
>     need = 30000000
>     used = maximum ss
>     unused = size - used
>     target = need - unused

This concludes today's solution.  But this one is a good candidate to
come back to.
