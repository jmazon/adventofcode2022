---
title: "AoC Day 11: Monkey in the Middle"
author: Jean-Baptiste Mazon
date: 2022-12-11T14:42:21-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: The perpetual flux of “correct” code
image: aoc-haskell.jpeg
---

I solved [Advent of Code][aoc] day 11, “Monkey in the Middle”, at the
appropriate time on December 11th.  I solved it by passing the first
part, reading the second, and copy-pasting my part 1 code so as to
edit it into shape for part 2.

[aoc]: https://adventofcode.com/2022/day/11

This does the job and reaps the stars.  There's only one problem.

It's not satisfying.

It duplicates.  There's redundant code.  It duplicates.  There are
bits of functionality that appear common to both parts, yet aren't
shared.

And bringing the code to a level where nothing is superfluous,
paradoxically, required me to more than double the code size.

Let's see where this takes us.  This post is [literate Haskell][gh] as
usual, starting with a few imports.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day11.lhs

> import           Control.Lens
> import           Control.Monad              (forM_,replicateM_)
> import           Control.Monad.Primitive    (PrimMonad,PrimState)
> import           Control.Monad.Reader       (MonadReader,asks,runReaderT)
> import           Control.Monad.State.Strict (MonadState,evalStateT)
> import           Control.Monad.ST           (runST)
> import           Data.List                  (sortOn)
> import           Data.List.Split            (wordsBy)
> import           Data.Ord                   (Down(Down))
> import qualified Data.Vector as V
> import           Data.Vector.Unboxed        ((!))
> import qualified Data.Vector.Unboxed as UV
> import qualified Data.Vector.Unboxed.Mutable as UMV

The puzzle input is a list of monkeys, along with their most
distinctive properties.  I'll split those between what will change,
the “state”, and what won't, the “environment”.

The state here is the list of items.  They're passed along from a
monkey to the next, so we'll have one container per monkey.  When
passing along, we address monkeys by index, so a `Vector` will be
fine.  The actual operations per monkey are just emptying their
collection and appending to another one's, element by element.
Appending performance is mostly irrelevant considering the small item
count, so any collection type will do; I'm using native lists for
simplicity.

> type St wl = V.Vector [wl]

Mmm, what is that `wl` type parameter?

It's the beginning of our complications.  We'll alter item
representation between parts 1 and 2, so it's a placeholder for said
representation.  `wl` means worry level, as that's all we know of
them.

Next up, the environment.  That's the monkeys' algorithm to shove
items around.

When I coded this for part 1, this was a simple vector of
`wl→(Int,wl)` functions that yielded the item's new worry level and
position.  But which kind of worry level should the parsing return?
If I want to parse only once, it will have to return something generic
enough for both parts to be able to consume.

So I'll parse to an abstract environment, that both parts will later
reify to a specific type of their choosing.

> data Operation = Square | Times !Int | Plus !Int
> data Monkey = Monkey
>   { _monkeyId   :: !Int
>   , _monkeyOp   :: !Operation
>   , _divisor    :: !Int
>   , _monkeyThen :: !Int
>   , _monkeyElse :: !Int
>   }
> makeLenses ''Monkey

Parsing itself is rather uninteresting.  It's a prime example of that
view pattern abuse I find so well-suited to AoC and never really use
anywhere else.  The important aspect of it is that it returns a
perfectly generic, unparameterized type.

> parse :: String -> (V.Vector Monkey,St Int)
> parse input = (V.fromList itemss,V.fromList algs) where
>   ms = wordsBy null (lines input)
>   monkey
>     [ words -> ["Monkey",init -> read -> _monkeyId]
>     , filter (/= ',') -> words -> "Starting":"items:":(map read -> items)
>     , words -> splitAt 3 -> (["Operation:","new","="],parseOp -> _monkeyOp)
>     , words -> ["Test:","divisible","by",read -> _divisor]
>     , words -> ["If","true:","throw","to","monkey",read -> _monkeyThen]
>     , words -> ["If","false:","throw","to","monkey",read -> _monkeyElse]
>     ]
>     = (Monkey{..},items)
>   (itemss,algs) = unzip (monkey <$> ms)
>   parseOp ["old","*","old"] = Square
>   parseOp ["old","*",  n  ] = Times (read n)
>   parseOp ["old","+",  n  ] = Plus  (read n)

So let's now expand on what we'll really consider the environment when
operating the game.  We want the monkeys' testing divisors.  Mostly
for part 2, but having them in a dedicated location is actually
helpful for part 1 as well.  We obviously also want the actual
function, parameterized over a generic monad-like argument for the
result.

> type Algorithm wl m = wl -> m (wl,Int)
> type Divisors = UV.Vector Int
> data Env wl m = Env
>   { _divisors :: Divisors
>   , _algorithms :: V.Vector (Algorithm wl m)
>   }
> makeLenses ''Env

The monkey algorithms are constructed the same way for both parts,
it's only the resulting types that differ.  We'll make use of
typeclass methods, prefixed `wl`, to differentiate.

The first half constructs the “operation” function the monkey will
perform before testing:

> reifyAlgorithm :: (WorryLevel wl,MonadReader Divisors m)
>                => Monkey -> Algorithm wl m
> reifyAlgorithm monkey =
>   let o = case monkey ^. monkeyOp of
>             Square  -> \x -> wlIntOp (*) x x
>             Times n -> \x -> wlIntOp (*) x =<< wlFromInt n
>             Plus n  -> \x -> wlIntOp (+) x =<< wlFromInt n

The second half integrates the operation with the relieving, testing,
and assignment to a new monkey's queue, and packages that into our
awaited algorithm function for that monkey.

>   in \wl -> do
>     wl' <- wlRelieve <$> o wl
>     isDivisible <- wl' `wlDivisibleTest` (monkey ^. monkeyId)
>     pure (wl',monkey ^. if isDivisible then monkeyThen else monkeyElse)

It's important to note that the `reifyAlgorithm` function doesn't need
the divisors: only the returned function does.

This all relies on the typeclass methods.

> class WorryLevel wl where
>   turnCount       :: Int
>   wlRelieve       :: wl -> wl
>   wlFromInt       :: MonadReader Divisors m => Int -> m wl
>   wlDivisibleTest :: MonadReader Divisors m => wl -> Int -> m Bool
>   wlIntOp         :: MonadReader Divisors m
>                   => (Int -> Int -> Int) -> wl -> wl -> m wl

That first one is a bit special.  It doesn't depend on the typeclass
parameter at all!  It's a hack to keep all of the differentiated code
together.

The rest is mostly self-explaining.  The last three functions have
access to the divisors part of the environment.  For part 1, it's not
really needed at all, though we'll use it for `wlDisivibleTest` for
easy access to said divisor.  If there wasn't part 2 we'd have tested
directly, but this is a reasonable tradeoff.

Now the implementation, using very basic type `Int`.

> instance WorryLevel Int where
>   turnCount = 20
>   wlRelieve = (`div` 3)
>   wlFromInt = pure
>   wlDivisibleTest wl i = asks ((== 0) . (wl `mod`) . (! i))
>   wlIntOp = ((pure .) .)

They're mostly self-explaining too, if only because the requirements
are very simple and you can guess through my point-free abuse.

Let's implement a game turn.  I managed to keep this extremely
generic; this comes at a price: the very long and explicit list of
constraints on the resulting function.

> turn :: forall wl m m'.
>       ( WorryLevel wl
>       , MonadReader (Env wl m') m
>       , Magnify m' m Divisors (Env wl m')
>       , MonadState (St wl) m
>       , PrimMonad m )
>      => UV.MVector (PrimState m) Int -> m ()

Let's go through them one by one.

The `WorryLevel` constraint is the core one, that chooses between
part 1 and part 2.

The `MonadReader` expresses the fact we operate in the environment we
defined earlier.

The `Magnify` expresses the fact that reader is not convoluted enough
to prevent us from presenting an inner function call with a restricted
view of the environment to the divisors only.

The `MonadState` defines our mutable state: the items' worry levels
and positions.

The `PrimMonad` is our means to count how many times each monkey
inspected an item using a mutable vector, provided to the function as
its single argument.

The function body is a fairly straightforward transcription of the
puzzle statement, with the caveat that a lot of it is compressed in
that `alg` function we spent so much code constructing.

> turn tallies = do
>   algs <- view algorithms
>   forM_ (V.indexed algs) $ \(i,alg) -> do
>     agenda <- use (ix i)
>     ix i .= []
>     forM_ agenda $ \wl -> do
>       UMV.modify tallies succ i
>       (wl',i') <- magnify divisors (alg wl)
>       ix i' %= (|> wl')

The entire game now consists of building up the environment and
initial state, constructing the appropriate monad stack for all we
want to be able to do, and repeatedly running the game turn we just
defined.  At the end, we extract the information checksum the puzzle
requires from the monkey inspection tally.

> monkeyBusiness :: forall wl. WorryLevel wl
>                => V.Vector Monkey -> St Int -> Int
> monkeyBusiness ms st0i =
>   product $ take 2 $ sortOn Down $ UV.toList $ runST $ do
>     let ds = UV.convert (view divisor <$> ms)
>         st0 = (traverse . traverse) wlFromInt st0i ds
>         env = Env ds (reifyAlgorithm <$> ms)
>     ts <- UV.thaw (UV.map (const 0) ds)
>     flip evalStateT st0 $
>       flip runReaderT env $
>       replicateM_ (turnCount @wl) (turn @wl ts)
>     UV.freeze ts

Solving for part 1 is now complete:

> main :: IO ()
> main = do
>   (monkeys,st0) <- parse <$> getContents
>   print $ monkeyBusiness @Int  monkeys st0

So, what's left for part 2?

The problem with part 2 is that the worry levels get higher.  We're
not dividing by 3 anymore, but that's really the least of our worries.
We still have 10000 rounds to go, and the operations the monkey
perform range from a few insignificant (adding a small number, for 5
out of my 8 monkeys) to some scarier (multiplying by 11 or 17 for 2
out of my 8 monkeys) to a downright evil one: squaring (for the
remaining monkey).

Of course the chain of monkeys an item will go through won't be fully
the one we fear, but assuming a mostly-uniform spread of handling
(which seems reasonable: the additions will tend to break any prime
affinity an item could have), we're still multiplying by 11 one time
out of 8, by 17 another, and squaring another.

My least worrying item is more than 50 for a start, a homogeneous
sequence of those operations alone could bring it to a final value of
$(50×11×17)^2^2500$.  Which is so large our computers don't even have
enough bits to express its size, let alone its value.  Infinitely fast
BigNums wouldn't even save us here.

There are a few ways out.

What do we need the worry level for anyway?  We need it to determine
which monkey the item is thrown to.  This has a direct influence on
the inspection tallies, there's no approximating it.  How is it used
to determine the next target?  It's used by a modulus test.

So we can achieve the same results by replacing that crazy-ass number
by one that gives the same results modulo what the monkey is testing
for.

Unfortunately, they're all testing for a different one.

There's two obvious ways out of this.

* We can maintain the worry level modulo the LCM of all of the
  monkeys' divisors.  Which are all distinct and prime, so that's the
  same thing as their product.  Mine is about ten millions.  It's
  large; we'd overflow 32 bits when squaring, but not 64.
* We can maintain a vector of the worry level modulo *all* the monkey
  divisors.  There are only 8 of them, we could store them in 8 times
  8 bits (but operate on 16 so as not to overflow the squaring) if we
  really wanted to optimize the useless aspects of the problem.

I'll pick the second option, for fun.  Let's call our datatype `Mods`
and complete our `main` function.

>   print $ monkeyBusiness @Mods monkeys st0
> 
> newtype Mods = Mods (UV.Vector Int)

And now define the specialized operations:

> instance WorryLevel Mods where
>   turnCount = 10000
>   wlRelieve = id
>   wlFromInt i = asks (Mods . UV.map (mod i))
>   wlDivisibleTest (Mods wl) i = pure (wl!i == 0)
>   wlIntOp o (Mods a) (Mods b) =
>     asks (Mods . UV.zipWith mod (UV.zipWith o a b))

And that's all there is to it!

Writing this up, I did have to wonder why the current state is good
enough for me, and why it's still not perfect yet.

A lot of the imperfection is very directly related to the extremely
procedurally imperative instructions we have.  That and the constant
tradeoff between solving one's personal input vs solving for generic
inputs.

The remainder is my choice to retain both parts' worry level
implementations, when the second one would actually work just fine for
part 1.  Of course, we'd need to implement turn count and relief
differently, but that's hardly an issue.

And the combination is messy: we have a fairly involved monad stack
with a `StateT` inside to account for the imperative state management,
but we also need to thread type parameters all around, and they tend
to clash.

There's also the fact for the longest time, I wanted to use
statically-typed fixed vectors for anything monkey-indexed, and my
puzzle input being a different size than the puzzle example made that
unpolished.

So all in all…  it was fun solving.  It was not fun cleaning up.  See
you tomorrow for a puzzle that will undoubtedly get written up
quicker!
