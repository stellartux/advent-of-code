const numbers = Deno.readTextFileSync('./1/input.txt')
  .trim()
  .split('\n')
  .map(Number)

function pair(numbers) {
  for (let j = 0; j < numbers.length - 1; j++) {
    for (let i = j + 1; i < numbers.length; i++) {
      const ns = [numbers[j], numbers[i]]
      if (ns[0] + ns[1] === 2020) {
        return ns
      }
    }
  }
}

console.log(pair(numbers).reduce((a, b) => a * b))

function trio(numbers) {
  for (let k = 0; k < numbers.length - 2; k++) {
    for (let j = k + 1; j < numbers.length - 1; j++) {
      if (numbers[k] + numbers[k] > 2020) {
        continue
      }
      for (let i = j + 1; i < numbers.length; i++) {
        const ns = [numbers[k], numbers[j], numbers[i]]
        if (ns.reduce((a, b) => a + b, 0) === 2020) {
          return ns
        }
      }
    }
  }
}

console.log(trio(numbers).reduce((a, b) => a * b))

import { bench, runBenchmarks } from 'https://deno.land/std/testing/bench.ts'

bench({
  name: 'find-trio-original',
  runs: 1,
  func: (b) => {
    b.start()
    for (let k = 0; k < numbers.length - 2; k++) {
      for (let j = k + 1; j < numbers.length - 1; j++) {
        for (let i = j + 1; i < numbers.length; i++) {
          const ns = [numbers[k], numbers[j], numbers[i]]
          if (ns.reduce((a, b) => a + b, 0) === 2020) {
            b.stop()
            return ns
          }
        }
      }
    }
  },
})

bench({
  name: 'find-trio-continue',
  runs: 1,
  func: (b) => {
    b.start()
    trio(numbers)
    b.stop()
  },
})

if (import.meta.main) runBenchmarks()

/*

running 2 benchmarks ...
benchmark find-trio-original ...
    22ms
benchmark find-trio-continue ...
    4ms

*/
