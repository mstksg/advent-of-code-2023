Day 1
===

<!--
This section is generated and compiled by the build script at ./Build.hs from
the file `./reflections/day01.md`.  If you want to edit this, edit
that file instead!
-->

*[all][reflections]* / *1* / *[2][day02]* / *[5][day05]*

[reflections]: https://github.com/mstksg/advent-of-code-2023/blob/master/reflections.md
[day02]: https://github.com/mstksg/advent-of-code-2023/blob/master/reflections-out/day02.md
[day05]: https://github.com/mstksg/advent-of-code-2023/blob/master/reflections-out/day05.md

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
>> Day 01a
benchmarking...
time                 644.4 μs   (634.7 μs .. 654.0 μs)
                     0.992 R²   (0.985 R² .. 0.997 R²)
mean                 636.6 μs   (615.0 μs .. 658.1 μs)
std dev              72.70 μs   (60.51 μs .. 86.91 μs)
variance introduced by outliers: 80% (severely inflated)

* parsing and formatting times excluded

>> Day 01b
benchmarking...
time                 3.106 ms   (3.086 ms .. 3.145 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 3.094 ms   (3.090 ms .. 3.105 ms)
std dev              22.59 μs   (8.206 μs .. 41.38 μs)

* parsing and formatting times excluded
```

