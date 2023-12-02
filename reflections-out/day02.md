Day 2
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day02.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *2*

[reflections]: https://github.com/mstksg/advent-of-code-2023/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2023/blob/master/reflections-out/day01.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2023

*[Prompt][d02p]* / *[Code][d02g]* / *[Rendered][d02h]*

[d02p]: https://adventofcode.com/2023/day/2
[d02g]: https://github.com/mstksg/advent-of-code-2023/blob/master/src/AOC/Challenge/Day02.hs
[d02h]: https://mstksg.github.io/advent-of-code-2023/src/AOC.Challenge.Day02.html

This one was definitely a bit too heavy on the parsing side heh. But maybe the
more interesting Haskell insight would be that we can use a nice data type --
`V3` from the *linear* library (`data V3 a = V3 a a a`) to make the actual
solving logic very simple.  I've always held that `V2` and `V3` are pretty much
some of the most useful non-standard data types for advent of code, and they
were pretty helpful today.  They have the most convenient `Num`, `Functor`,
`Applicative`, `Foldable`, and `Traversable` instances!

If we encode each "set" as `V3 <red> <green> <blue>`, then the filters and
checksums become very simple.

For part 1, we need to see if all `V3 Int`s in a line are legal.  We can
implement that as `all isLegal`:

```hasell
isLegal :: V3 Int -> Bool
isLegal colorVec = and do
  allowed <- V3 12 13 14
  amount <- colorVec
  pure (amount <= allowed)
```

It reads a little silly, but you just have to remember that `and` just checks
if every item is true, and here the `do` notation semantics are to compare each
`allowed` and `amount` point-wise, first the red-by-red, then the
green-by-green, and then the blue-by-blue.

Part 2 is a little simpler, we just need to aggregate the max per-color, and
then find the product:

```haskell
calcPower = product . foldr (liftA2 max) 0
```

where `liftA2 max (V3 x y z) (V3 a b c) = V3 (max x a) (max y b) (max z c)`,
and `0` for `V3` is `V3 0 0 0`.  And `product` works like how you'd think.

Going back to parsing, one neat way we can leverage `V3` is with its `Functor`
instance:

```
-- [("red", 1), ("blue", 2), ("green", 6)] => V3 1 6 2
pairUp :: [(String, Int)] -> V3 Int
pairUp pairs = do
    color <- V3 "red" "green" "blue"
    pure $ fromMaybe 0 (lookup color pairs)
```

This performs an action per-color and fills in the spot with the result of
`lookup`, with `0` if the lookup fails.


*[Back to all reflections for 2023][reflections]*

## Day 2 Benchmarks

```
>> Day 02a
benchmarking...
time                 1.376 μs   (1.373 μs .. 1.380 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 1.388 μs   (1.376 μs .. 1.435 μs)
std dev              75.81 ns   (6.071 ns .. 160.5 ns)
variance introduced by outliers: 69% (severely inflated)

* parsing and formatting times excluded

>> Day 02b
benchmarking...
time                 3.878 μs   (3.678 μs .. 4.054 μs)
                     0.992 R²   (0.988 R² .. 0.997 R²)
mean                 3.808 μs   (3.761 μs .. 3.861 μs)
std dev              173.2 ns   (112.0 ns .. 256.3 ns)
variance introduced by outliers: 58% (severely inflated)

* parsing and formatting times excluded
```

