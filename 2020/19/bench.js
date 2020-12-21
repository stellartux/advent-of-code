import { bench, runBenchmarks } from 'https://deno.land/std/testing/bench.ts'
import { loadFile, solve, solveB } from './solution.js'

const [rules, messages] = loadFile('input.txt')

bench({
  name: 'Part 1',
  runs: 10,
  func: (b) => {
    b.start()
    console.log(solve(rules, messages))
    b.stop()
  },
})
// 10 runs avg: 6.8ms

bench({
  name: 'Part 2',
  runs: 1,
  func: (b) => {
    b.start()
    console.log(solveB(rules, messages))
    b.stop()
  },
})
// 17600ms

if (import.meta.main) runBenchmarks()
