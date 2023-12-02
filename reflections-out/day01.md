Day 1
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day01.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *1*

[reflections]: https://github.com/mstksg/advent-of-code-2023/blob/master/reflections.md


[Available as an RSS Feed][rss]

[rss]: http://feeds.feedburner.com/jle-advent-of-code-2023

*[Prompt][d01p]* / *[Code][d01g]* / *[Rendered][d01h]*

[d01p]: https://adventofcode.com/2023/day/1
[d01g]: https://github.com/mstksg/advent-of-code-2023/blob/master/src/AOC/Challenge/Day01.hs
[d01h]: https://mstksg.github.io/advent-of-code-2023/src/AOC.Challenge.Day01.html

This year's day 1 was definitely a little spicier than most!  But Haskell's
stream processing works well for us, again.

First let's get a nice utility function to make a number the first and last
seen digit:

```haskell
firstAndLast :: [Int] -> Int
firstAndLast xs = head xs * 10 + last xs
```

And now we have to figure out how to extract the digits from each part.  For
part 1, we need only to extract all of the `isDigit` characters:

```haskell
extract1 :: String -> [Int]
extract1 = map digitToInt . filter isDigit
```

For part 2, it's a little bit trickier: we can look at each position and see if
it is a new number string:

```haskell
extract2 :: String -> [Int]
extract2 = mapMaybe isNumberString . tails
  where
    isNumberString :: String -> Maybe Int
    isNumberString str = listToMaybe
      [ d
      | (numStr, d) <-
          [ ("one",1), ("two",2), ("three",3)
          , ("four",4), ("five",5), ("six",6)
          , ("seven",7), ("eight",8), ("nine",9)
          ]
      | numStr `isPrefixOf` str
      ]
```

Note that `tails` is a function that takes `"abc"` and returns
`["abc","bc","c"]`, allowing us to look at each position in order.

And that gives us the solutions:

```haskell
day01 :: (String -> [Int]) -> String -> Int
day01 extractor = sum . map (sum . extractor) . lines

day01a = day01 extract1
day01b = day01 extract2
```



*[Back to all reflections for 2023][reflections]*

## Day 1 Benchmarks

```
Build profile: -w ghc-9.0.2 -O1
In order, the following will be built (use -v for more details):
 - aoc2023-0.1.0.0 (lib) (file README.md changed)
 - aoc2023-0.1.0.0 (exe:aoc2023) (dependency rebuilt)
Preprocessing library for aoc2023-0.1.0.0..
Building library for aoc2023-0.1.0.0..
Preprocessing executable 'aoc2023' for aoc2023-0.1.0.0..
Building executable 'aoc2023' for aoc2023-0.1.0.0..
>> Day 01a
benchmarking...
time                 630.3 μs   (600.2 μs .. 657.1 μs)
                     0.984 R²   (0.976 R² .. 0.991 R²)
mean                 586.8 μs   (572.0 μs .. 602.8 μs)
std dev              56.16 μs   (45.97 μs .. 71.08 μs)
variance introduced by outliers: 74% (severely inflated)

* parsing and formatting times excluded

>> Day 01b
benchmarking...
time                 3.228 ms   (3.207 ms .. 3.274 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 3.229 ms   (3.214 ms .. 3.270 ms)
std dev              71.01 μs   (8.521 μs .. 149.4 μs)

* parsing and formatting times excluded
```

