Day 5
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day05.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *[1][day01]* / *[2][day02]* / *5*

[reflections]: https://github.com/mstksg/advent-of-code-2023/blob/master/reflections.md
[day01]: https://github.com/mstksg/advent-of-code-2023/blob/master/reflections-out/day01.md
[day02]: https://github.com/mstksg/advent-of-code-2023/blob/master/reflections-out/day02.md

[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2023

*[Prompt][d05p]* / *[Code][d05g]* / *[Rendered][d05h]*

[d05p]: https://adventofcode.com/2023/day/5
[d05g]: https://github.com/mstksg/advent-of-code-2023/blob/master/src/AOC/Challenge/Day05.hs
[d05h]: https://mstksg.github.io/advent-of-code-2023/src/AOC.Challenge.Day05.html

Another year and another puzzle where I feel like using Haskell's
[data-interval][] is cheating :)  It lets us work with ranged intervals and
gives us types like `IntervalSet` (a set of intervals) and `IntervalMap`
(mapping intervals to values), but with the correct implementations of map
intersection, set intersection, etc. that is aware of how intervals work.  We 
can generate a single `Interval`:

[data-interval]: https://hackage.haskell.org/package/data-interval

```haskell
import Data.Interval (Interval)
import qualified Data.Interval as IV

fromRange :: Int -> Int -> Interval Int
fromRange x len = IV.Finite x IV.<=..< IV.Finite (x + len)
```

The main type we will be using here is the `IntervalMap` type.  We will use it 
to map intervals to deltas: if you fall inside the interval, your value will 
update by `+ delta`.

```haskell
import Data.IntervalMap.Strict (IntervalMap)
import qualified Data.IntervalMap.Strict as IVM

-- | Takes in the (a, b, c) triples that the problem gives map chunks
buildMap :: [(Int, Int, Int)] -> IntervalMap Int Int
buildMap = IVM.fromList
         . map (\(dest, src, len) -> fromRange src len, dest - src)
```

Now we can run it on a single point:

```haskell
convertSingle :: Int -> IntervalMap Int Int -> Int
convertSingle x mp = case IVM.lookup x mp of
  Nothing    -> x           -- the value is not in any known interval
  Just delta -> x + delta   -- that interval has the given delta
```

So part 1 is simply finding the minimum after iterating `convertSingle` across 
every range:

```haskell
day05a :: [IntervalMap Int Int] -> [Int] -> Int
day05a imaps = minimum . map (\s0 -> foldl' convertSingle s0 imaps)
```

Part 2 is where it gets interesting.  We actually want to keep track of an 
`IntervalSet` -- the set of all intervals that we have seeds at.  We need to 
write a function `convertMany` that takes an `IntervalSet` and runs that 
interval set through an `IntervalMap` appropriately.

To do this, we find all of the "misses" (the intervals in the `IntervalSet` 
that aren't mapped) and the "hits" (the intervals in the `IntervalSet` that do 
exist in the map, shifted by the delta) and then recombine it:

```haskell
import Data.IntervalSet (IntervalSet)
import qualified Data.IntervalSet as IVS

convertMany :: IntervalMap Int Int -> IntervalSet Int -> IntervalSet Int
convertMany mp xs = misses <> hits
  where
    -- dummy map corresponding to putting `()` at every interval in our
    -- `IntervalSet`, needed because interval map functions require maps
    tempMap :: IntervalMap Int ()
    tempMap = IVM.fromList . map (,()) . IVS.toList $ xs

    misses = IVM.keysSet $ tempMap `IVM.difference` mp
    hits =
      IVS.fromList
        . map (\(iv, delta) -> IV.mapMonotonic (+ delta) iv)
        . IVM.toList
        $ IVM.intersectionWith const mp tempMap
```

And run it all together with:

```haskell
day05b :: IntervalSet Int -> [IntervalMap Int Int] -> IntervalSet Int
day05b = foldl' convertMany
```


*[Back to all reflections for 2023][reflections]*

## Day 5 Benchmarks

```
>> Day 05a
benchmarking...
time                 17.13 μs   (16.83 μs .. 17.86 μs)
                     0.997 R²   (0.994 R² .. 1.000 R²)
mean                 16.87 μs   (16.80 μs .. 17.13 μs)
std dev              421.7 ns   (78.33 ns .. 880.9 ns)
variance introduced by outliers: 26% (moderately inflated)

* parsing and formatting times excluded

>> Day 05b
benchmarking...
time                 450.8 μs   (450.1 μs .. 451.6 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 450.3 μs   (450.0 μs .. 450.6 μs)
std dev              946.6 ns   (694.4 ns .. 1.270 μs)

* parsing and formatting times excluded
```

