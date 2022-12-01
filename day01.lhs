---
title: "AoC Day 1: Calorie Counting"
author: Jean-Baptiste Mazon
date: 2022-12-01T22:27:37-01:00
tags: [ "advent of code", aoc2022, haskell ]
description: Lightweight point-freedom
image: aoc-haskell.jpeg
---

Oh dear.  I haven't posted anything since last AoC.  There are
reasons, and it will change, but still…

Anyway.

[Advent of Code][aoc] is back for a new season!  For its first day,
[“Calorie Counting”][aoc1], we're comparing elf backpack contents for
calories.

[aoc]: https://adventofcode.com/
[aoc1]: https://adventofcode.com/2022/day/1

Seems like a perfect opportunity to have fun with Haskell's point-free
notation again.  Let's first have a few imports to clear the floor for
a [literate Haskell][gh] post.

[gh]: https://github.com/jmazon/adventofcode2022/blob/master/day01.lhs

> import Control.Category ((>>>))
> import Data.List.Extra  (sortOn,wordsBy)
> import Data.Ord         (Down(Down))
> 
> main :: IO ()
> main = interact $

Ok, we're right in the pipeline.  We're receiving raw input here, as
in `"1000\n2000\n3000\n\n4000\n…"` per the example.  Parsing will
start simple, let's first split lines:

>   lines >>>

This yields a somewhat more structured list of strings:
`["1000","2000","3000","","4000",…]`.  Let's group them by paragraph,
*aka* backpack.

>   wordsBy null >>>

The Haskell Prelude function `words` splits a string into words,
cutting on spaces.  The `wordsBy` extension generalizes that to work
on arbitrary lists.  Since there's no general way to know what it
means to be a space in a list of something that could be
non-characters, it takes an explicit function from the caller to
answer that.

Here we're using `null`, which detects empty strings.  More
specifically, empty lists, of which empty strings are a special case.

So we have a list of backpacks of food items:
`[["1000","2000","3000"],["4000"],…]`

Let's convert them to actual numbers we can do math on.

>   (map . map) read >>>

That was easy.  We now have `[[1000,2000,3000],[4000],…]`.

How many calories are in a backpack?  Summing the calorie counts of the
individual food items within answers that.

>   map sum >>>

We have `[6000,4000,…]`.  We're really looking for the high-calorie
bags here, so let's sort them.

>   sortOn Down >>>

We have `[24000,11000,10000,…]` now.^[You have to either trust me or
go see the original problem statement here, since those high-calorie
backpacks weren't visible in the truncated input.]

Now let's get a bit of deeper point-free hackery out.

>   flip take >>>

The `take` function takes two arguments: a count and a list.  It
returns the first *count* elements of the list.  `flip`ping it as we
do here reverses the order of its arguments: it now takes a list and a
count, and returns the same result as the unflipped variant.

We're inserting this function in the pipeline, so it's receiving as
its first argument our sorted list of backpack calories.  There's no
second argument yet, it remains a partially applied function.

So we have a function that takes a count and returns that many
elements from a very specific list: the sorted calorie tallies.

>   flip map [1,3] >>>

`map` takes a function and a list, and returns the list of the results
of applying said function to all elements in the input list.  `flip
map` takes a list and a function, and returns… you get the picture.

`flip map [1,3]` assigns that input list to be `[1,3]`, takes a
function and applies.

We are transferred that function from uppipe, so we now have a list of
2 elements: the result of taking the first 1 and 3 elements from the
sorted calorie tallies.  Something like that:
`[[24000],[24000,11000,10000]]`.

>   map sum >>>

If you've made it this far, you understood this.  We get
`[24000,45000]`.

We could keep going and format it properly, but I'll be lazy and count
on our attention span to remember that list's first element is the
answer to part 1 and the second the answer to part 2.

>   show

No `>>>` at the end of the line, this is the end of the pipeline.
`[24000,45000]` is printed out.

This concludes this first day of AoC, with a now traditional
demonstration of point-free coding.  Which, as we can see, is both
extremely powerful and extremely risky[^risk] as far as long-term
legibility goes.

[^risk]: And this year's day 1 readability is impressive compared to
         [last year's](/posts/2021-12-aoc/day01.html)

See you tomorrow!
