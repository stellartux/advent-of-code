import { bench, runBenchmarks } from 'https://deno.land/std/testing/bench.ts'
import { solve } from './solution.js'

bench({
  name: 'Day 15 Part 2 Solution',
  runs: 1,
  func: b => {
    b.start()
    console.log(solve([15, 5, 1, 4, 7, 0], 30_000_000))
    b.stop()
  }
})

if (import.meta.main) runBenchmarks()

// 3590ms
