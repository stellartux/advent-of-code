# Solving Advent of Code 2022 Day 10 in 6 Lines of Awk

[Advent of Code's puzzle for 2022 Day 10](https://adventofcode.com/2022/day/10)
features a toy assembly language that has to be interpreted in a cycle accurate
manner for a toy CPU design. This article is an explanation of how I solved the
puzzle, so if you'd like to try it, I recommend you attempt it before reading
the article.

## The Puzzle

You're provided with an assembly listing for the toy CPU, which is to be
interpreted. It only has two instructions. The `noop` instruction does nothing
for one cycle, the `addx` instruction adds the instruction's parameter to the
`x` register, the only register, which starts set to `1`. The `addx`
instruction takes two cycles to execute, and `x` isn't updated until the second
cycle.

```txt
noop
addx 3
addx -5
```

## Part One

Part one of the puzzle asks for the sum of the values of `x` at the 20th, 60th,
100th, 140th, 180th, and 220th cycles multiplied by the respective cycle count.

The obvious approach is to go through the file line by line, executing each
instruction as described, manually keeping track of the cycle count, however
there is another way to view the instructions. What if instead of splitting the
file at each line, it were split at any whitespace. The example above splits
into an array of `["noop", "addx", 3, "addx", -5]`. The `noop` instruction is a
single token long, and takes a single cycle to execute. The `addx` instruction
is two tokens long and takes two cycles to execute, only modifying its state in
its second cycle.

So what if instead viewing `addx` as one instruction, we view it as two. Now
`addx` is also a no-op, and numerical values denote an instruction one cycle
long which add that numerical value to `x`. Now all the instructions are a
single cycle long, and all non-numerical instructions are no-ops. This produces
the same result as the original interpretation, but is easier to implement, as
all the instructions take a single cycle and execute immediately.

## awk

Now where does awk come into this? If you're not familiar, awk is a programming
language and data processing tool which is part of the POSIX standard and is
included with basically every OS but Windows. It's designed to operate on entire
text files line by line. Since most Advent puzzles have some sort of newline
delimited data as input, awk is a very handy tool to have for Advent of Code,
especially towards the start of the month.

By default, files are read as text data where each line is a *record*, and
each record is split into *fields* by whitespace. Programs written in awk are
arranged into *rules* (statements) which consist of a *pattern* (conditional)
and its *action* (consequent) written inside curly braces. Patterns are
typically a regular expression matching on either the entire record (`$0`) or a
field of the record (`$1`, `$2`, etc), but can be any boolean expression. There
are also the keyword patterns `BEGIN` and `END`, which run before and after the
input file has been read, and actions written without a pattern, which run on
every record. Other than this unorthodox approach to program structure, the awk
syntax should be familiar to most programmers, since not only is it inspired by
C in syntax design, many modern scripting languages were directly influenced by
awk's design.

Awk values are loosely typed, and are strings by default, but coerced to numbers
if the program performs a numerical operation on them. Strings which can't be
converted to a number are coerced to `0`. This is useful for the puzzle, as it
means we can convert every token in the file to a number and add it to the `x`
variable, and any `noop` or `addx` token is converted to `0`, `x += 0` being a
no-op. Handy!

So by changing the *r*ecord *s*eparator (`RS`) from the default of only matching
newlines to a regex that matchs any whitespace, each token in the file is
treated as a record of its own. This means the implementation of the toy CPU
needs only the following code.

```awk
BEGIN { RS = "[[:space:]]+"; x = 1 }
{ x += $0 }
```

Getting the answer to the puzzle means taking the value of `x` multiplied by
the cycle count at every 40th cycle starting from cycle 20. Each record of awk's
input takes one cycle to process, and awk automatically keeps track of the
*n*umber of the *r*ecord in a variable called `NR`. Keeping track of the
running total while going through the file and printing it at the end adds
another two lines to the program. `total` doesn't need to be initialised, as
numerical awk variables are initialised to `0` by default.

```awk
BEGIN { RS = "[[:space:]]+"; x = 1 }
NR % 40 == 20 { total += NR * x }
{ x += $0 }
END { print total }
```

Sure enough, this is all that's needed to solve part one.

```sh
$ awk -f 10.awk example.txt
13140
```

## Part Two

The second part adds a twist to the puzzle, as always. Now the `x` register is
not merely a number, it's the `x` coordinate of a sprite on a screen that is
being drawn by a CRT. Each cycle a "pixel" is drawn to the "screen" (or a
character is printed out), `'#'` if the `x` value is within a pixel of the
pixel currently being drawn, or `'.'` otherwise. There are forty pixels per
line, so a newline needs to be printed every forty characters. By setting the
*o*utput *r*ecord *s*eparator (`ORS`) to the empty string, awk won't print
newlines at the end of each `print` call.

```awk
BEGIN { ORS = ""; RS = "[[:space:]]+"; x = 1 }
{ print((d = x - ((NR - 1) % 40)) < 2 && d > -2 ? "#" : "."); x += $0 }
NR % 40 == 0 { print "\n" }
```

That juggling a variable in the ternary conditional is a bit gnarly, but it's
needed since awk doesn't have a function for calculating the absolute value of
a number, although it's quite easy to write one. JavaScript programmers will
find the syntax familiar.

```awk
function abs(d) { return d < 0 ? -d : d }
```

This can be combined with the code from part one, to make a program which
calculates the answer to both parts in a single pass.

```awk
BEGIN { ORS = ""; RS = "[[:space:]]+"; x = 1 }
NR % 40 == 20 { total += NR * x }
{ print(abs(x - (NR - 1) % 40) < 2 ? "#" : "."); x += $0 }
NR % 40 == 0 { print "\n" }
END { print total "\n" }
function abs(d) { return d < 0 ? -d : d }
```

```sh
$ awk -f 10.awk example.txt
##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....
13140
```
