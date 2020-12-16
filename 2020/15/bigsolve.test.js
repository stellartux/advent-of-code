import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { solve } from './solution.js'

Deno.test({
  name: "solve([0, 3, 6], 30_000_000)",
  fn: function () {
    assertEquals(solve([0, 3, 6], 30_000_000), 175594)
  }
})

if (import.meta.main) Deno.runTests()
