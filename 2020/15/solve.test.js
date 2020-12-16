import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { solve } from './solution.js'

Deno.test({
  name: '1,3,2 = 1',
  fn: function () {
    assertEquals(solve([1, 3, 2]), 1)
  },
})
Deno.test({
  name: '2,1,3 = 10',
  fn: function () {
    assertEquals(solve([2, 1, 3]), 10)
  },
})
Deno.test({
  name: '1,2,3 = 27',
  fn: function () {
    assertEquals(solve([1, 2, 3]), 27)
  },
})
Deno.test({
  name: '2,3,1 = 78',
  fn: function () {
    assertEquals(solve([2, 3, 1]), 78)
  },
})
Deno.test({
  name: '3,2,1 = 438',
  fn: function () {
    assertEquals(solve([3, 2, 1]), 438)
  },
})
Deno.test({
  name: '3,1,2 = 1836',
  fn: function () {
    assertEquals(solve([3, 1, 2]), 1836)
  },
})

// 15,5,1,4,7,0

if (import.meta.main) Deno.runTests()
