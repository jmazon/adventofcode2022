---
title: "AoC Day 22: Monkey Map"
author: Jean-Baptiste Mazon
date: 2022-12-22T19:15:23-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: Done almost right
image: aoc-haskell.jpeg
---

In today's [Advent of Code][aoc] puzzle, “Monkey Map”, we're going
with the old classic of following a map using different topological
rules.

[aoc]: https://adventofcode.com/2022/day/22

This post is [literate Haskell][gh], starting with a few imports.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day22.lhs

> import           Data.Array
> import           Data.Char       (isDigit)
> import           Data.List       (elemIndex,find,foldl')
> import           Data.List.Split (wordsBy)
> import           Data.Map.Strict (Map)
> import qualified Data.Map.Strict as Map
> import           Data.Maybe      (fromMaybe)
> import           Linear

We're given a map and a line of instructions.  Let's parse them
crudely, keeping the map in a 2D array and the instructions in a list.

> data Instr =
>     Advance Int
>   | TurnLeft
>   | TurnRight
> 
> type Board = Array (V2 Int) Char
> type Path = [Instr]

I'm once again not going to detail my parsing so much, it's pretty
much in line with the usual.

> parse :: String -> (Board,Path)
> parse (lines -> wordsBy null -> [gls,[p]]) = (g,go p)
>   where
>     g = listArray (V2 1 1,V2 h w) (concatMap widen gls)
>     h = length gls
>     w = maximum (length <$> gls)
>     widen l = l ++ replicate (w - length l) ' '
> 
>     go [] = []
>     go s | isDigit (head s) =
>              let (l,r) = span isDigit s
>              in Advance (read l) : go r
>     go ('L':s) = TurnLeft : go s
>     go ('R':s) = TurnRight : go s

Following the map is a simple matter of interpreting the instructions
properly.  An advance order moves a step in the current direction and
recurses; its terminating condition, an advance order of $0$ or
bumping in a hash mark on the map, shifts to the next order.  Turn
orders update the current direction.  At end of orders, compute the
puzzle checksum, that's nothing more than a numbering of board
(position,direction) pairs.

> solve :: Monkey m => m -> Path -> Int
> solve m = go (startPos m) (startDir m) where
>   free p = readTile m p /= '#'
> 
>   go p d [] = let V2 i j = readPos m p in 1000*i + 4*j + readDir m p d
>   go p d (Advance 0:is) = go p d is
>   go p d (Advance n:is)
>     | (p',d') <- advance m p d, free p' = go p' d' (Advance (n-1):is)
>     | otherwise = go p d is
>   go p d (TurnLeft:is) = go p (turnLeft (p,d)) is
>   go p d (TurnRight:is) = go p (turnRight (p,d)) is

I'm going to need to define a few operations for that to pan out.

Let's start with a “state”, *i.e.* where we are on the board.  From
that data we'll define how to turn left or right, changing the current
direction.

> class MonkeyState d where
>   type MSDir d
>   turnLeft,turnRight :: d -> MSDir d

Building on it, we can define our more generic monkey interpretation
of the board.^[There's quite a bit more associated type families and
equivalences than I'd like.  Simplifications welcome!]  We'll define
the read-current-tile operation we'll need to properly move around,
the rest of the actual move-around operations, and two helpers to
convert back to board coordinates and direction so as to compute the
checksum when the path has completed.

> class (MonkeyState (Pos m,Dir m),MSDir (Pos m,Dir m) ~ Dir m)
>       => Monkey m where
>   type Pos m
>   type Dir m
>   readTile :: m -> Pos m -> Char
>   startPos :: m -> Pos m
>   startDir :: m -> Dir m
>   advance :: m -> Pos m -> Dir m -> (Pos m,Dir m)
>   readPos :: m -> Pos m -> V2 Int
>   readDir :: m -> Pos m -> Dir m -> Int

Let's implement them for the simple wraparound board semantics!

The state is a simple pair of position and direction vectors.  I don't
actually need to specify the position, as I don't need it to rotate
the direction.

> instance MonkeyState (a,V2 Int) where
>   type MSDir (a,V2 Int) = V2 Int
>   turnLeft = perp . snd
>   turnRight = negated . perp . snd

The board topology is simple enough.  First, define the two associated
types.

> instance Monkey Board where
>   type Pos Board = V2 Int
>   type Dir Board = V2 Int

Reading a tile simply defers to the underlying array.

>   readTile = (!)

Start position simply scans for a non-empty space.

>   startPos m = fromMaybe (error "No startPos found") $
>                find ((/= ' ') . (m !)) (indices m)

Start direction is a mere constant, using the board as a type proxy.

>   startDir _ = unit _y

The meat is the advance function, that implements the wraparound
semantics.

>   advance m = let (V2 1 1,V2 h w) = bounds m in \p d -> 
>     let p' = fromMaybe (error "Failed to advance") $
>              find ((/= ' ') . (m!))
>              [ (mod <$> (p + k*^d - 1) <*> (V2 h w)) + 1
>                | k <- [1..max h w] ]
>     in (p',d)

Reading “back” the board coordinates doesn't need to do much.

>   readPos _ = id

And reading back the board direction will be implemented with a simple
scan.

>   readDir m p = fromMaybe (error "dir not found") .
>                 elemIndex (startDir m) .
>                 take 4 .
>                 iterate (turnLeft . (p,))

The astute reader has already noticed this code is much more
conservative than I usually do in AoC.  What can I say, it's the end
of the month!

> part1 :: Board -> Path -> Int
> part1 = solve

With part 1 done, we can now get to the painful part of the problem:
folding the board to a cube and following that instead.

I've already done this fairly recently: this is more or less what had
to be done in the [CodinGame summer 2019
contest](https://www.codingame.com/training/easy/detective-pikaptcha-ep4).
The obvious way to do it fast, during the contest as well as for AoC,
is to simply advance on the board as if it were flat, and implement
rotational teleportation when an edge is reached.

As far as AoC goes, this is rather inconvenient, as we then can't use
the same code to solve our puzzle input as we could to solve the
in-statement examples.  So we have to resort to a more defensive style
of coding to avoid too many errors lurking in the code we can't debug
as easily.  Painful.

And players could have different shaped maps!

Let's solve this properly now.  Let's implement the general case.
I'll fold the map around, and wrap it on a cube.

I don't want to have to shift the faces and implement lifelike 3D
coordinates on them, so I'll simplify and keep a discrete $[1..N]^3$
coordinate system, and adjoin a normal vector to disambiguate the
edges.

> data CubePos = CubePos
>   { _cpVec  :: V3 Int
>   ,  cpNorm :: V3 Int
>   }
>   deriving (Eq,Ord)

The current direction can then be an actual 3D direction vector.
Rotation is implemented by cross-product with the normal.

> instance MonkeyState (CubePos,V3 Int) where
>   type MSDir (CubePos,V3 Int) = V3 Int
>   turnLeft (cp,d) = cpNorm cp `cross` d
>   turnRight (cp,d) = d `cross` cpNorm cp

The encompasing structure will then hold the cube side^[Another
painful difference between sample and puzzle input.], for each small
square on one of the faces a link to the corresponding coordinates on
the flat board and a copy of the rotation matrix to convert direction
vectors, and the original flat board to read from.

> data Cube = Cube
>   { cSide  :: !Int
>   , cCube  :: !(Map CubePos (V2 Int,M32 Int))
>   , cBoard :: !Board
>   }

So reading a tile just needs to thread through those two last
structures.

> instance Monkey Cube where
>   type Pos Cube = CubePos
>   type Dir Cube = V3 Int
>   readTile Cube{cCube,cBoard} p = cBoard ! fst (cCube Map.! p)

We'll wrap the board to coordinates of our choice, so this time not
only the starting direction is a constant, but the starting position
as well.  Both using the structure as a phantom type proxy argument.

>   startPos _ = CubePos (V3 0 0 0) (V3 0 0 1)
>   startDir _ = V3 0 1 0

Advancing along the faces is usually as simple as adding the current
direction vector.  Except when it would bring us over an edge, in
which case we rotate current direction and normal.  (We don't really
need to perform any complex computations here: both can be expressed
as being the other's former value, possibly negated.^[This is also
where you could notice I'm either using left-handed 3D coordinates, an
abomination that really ought stay to remain confined in POV-Ray and
not spread out any more, ever, or painting the *inside* of a cube, as
opposed to what the puzzle statement implies.  (AFAICT it doesn't
actually *say*, but I'm looking for trouble.)])

>   advance Cube{cSide} (CubePos p n) d =
>     let p' = p + d in
>       if | minimum p' >= 0 && maximum p' < cSide -> (CubePos p' n,d)
>          | otherwise -> (CubePos p (negated d),n)

Reading back flatboard coordinates is a bit more involved this time,
though barely so as we held along to that information all along.

>   readPos Cube{cCube} p = fst (cCube Map.! p)
>   readDir Cube{cCube} p d =
>     let m = snd (cCube Map.! p)
>     in fromMaybe (error "dir not found") .
>        elemIndex d .
>        map (m !*) .
>        take 4 $
>        iterate (turnRight . (undefined,)) (V2 0 1)

Ok, so we're done, right?

Of course not.  We still have to actually build such a cube.  I'm
using a DFS, tracking flat and 3D coordinates simultaneously along
with the current basis transformation matrix to establish the
matchings all over the surface.

> buildCube :: Board -> Cube
> buildCube cBoard = Cube{..} where
>   cSide | bounds cBoard == (V2 1 1,V2 12 16) = 4
>         | otherwise = 50
>   p0 = startPos cBoard
>   cp0 = CubePos (V3 0 0 0) (V3 0 0 1)
>   m0 = V3 (V2 1 0)
>           (V2 0 1)
>           (V2 0 0)
>   cCube = dfs (Map.singleton cp0 (p0,m0)) [(p0,(cp0,m0))]
>   dfs !cl [] = cl
>   dfs cl ((p,(cp,m)):q) = dfs cl' (q'++q) where
>         cl' = foldl' (\cl0 (p',(cp',m')) -> Map.insert cp' (p',m') cl0) cl q'
>         q' = filter ((`Map.notMember` cl) . fst . snd) $
>              filter ((/= ' ') . (cBoard !) . fst) $
>              filter (inRange (bounds cBoard) . fst) $
>              map (advance3d cp m p) [V2 0 1,V2 1 0,V2 0 (-1),V2 (-1) 0]

The tricky math is left for that last function, `advance3d`.  It's a
generalization of the `advance` method in the `Monkey` class, that
doesn't advance a single direction but instead an entire basis.

Reading guide: `bp` and `bd` stand for (flat) board position and
direction.  `cpv` and `cpn` are the `CubePos` vector and normal.

I'm computing the change using a rotation vector $(rx,ry,rz)$ and a
simplification of [Rodrigues' formula][rodrigues].

[rodrigues]: https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula

>   advance3d :: CubePos -> M32 Int -> V2 Int -> V2 Int
>             -> (V2 Int,(CubePos,M32 Int))
>   advance3d (CubePos cpv cpn) m bp bd = (bp',(cp',m')) where
>     bp' = bp + bd
>     cd = m !* bd
>     cpv' = cpv + cd
>     (cp',m')
>       | minimum cpv' >= 0 && maximum cpv' < cSide = (CubePos cpv' cpn,m)
>       | otherwise = (CubePos cpv (rot !* cpn),rot !*! m) where
>           V3 rx ry rz = cd `cross` cpn
>           rot = V3 (V3    (abs rx) (negate rz)         ry )
>                    (V3         rz     (abs ry) (negate rx))
>                    (V3 (negate ry)         rx     (abs rz))

And *now* we're all set.

> part2 :: Board -> Path -> Int
> part2 = solve . buildCube

A little wrapper to solve both parts.

> main :: IO ()
> main = do
>   (board,path) <- parse <$> getContents
>   print $ part1 board path
>   print $ part2 board path

In a parallel reality where I'm not as rusty on the math, I'd probably
have strived not to drag along both source coordinates and
transformation matrix in every cube face position.  Probably using
homogeneous coordinates, possibly getting grumpy at the requirement to
use a fractional type and opting for an affine matrix+vector
combination instead.

The puzzle author expressing his checksum in terms of source
coordinates has a large part in making this painful.

Oh well.  It's done now.

Hope you enjoyed it, see you tomorrow!
