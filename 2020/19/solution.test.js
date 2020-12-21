import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import * as Solution from './solution.js'

const [exampleRules, exampleMessages] = Solution.loadFile('exampleinput.txt')
const ruleSet = Solution.newRuleSet(exampleRules)

Deno.test({
  name: 'Example for basic strings',
  fn: function () {
    assertEquals(ruleSet[4].test('a'), true)
  },
})

Deno.test({
  name: 'ababbb',
  fn: function () {
    assertEquals(
      ruleSet[0].test(exampleMessages[0]),
      true,
      `Message: ${exampleMessages[0]}\nrule: ${ruleSet[0]}`
    )
  },
})

Deno.test({
  name: 'bababa',
  fn: function () {
    assertEquals(
      ruleSet[0].test(exampleMessages[1]),
      false,
      `Message: ${exampleMessages[1]}\nrule: ${ruleSet[0]}`
    )
  },
})

Deno.test({
  name: 'abbbab',
  fn: function () {
    assertEquals(
      ruleSet[0].test(exampleMessages[2]),
      true,
      `Message: ${exampleMessages[2]}\nrule: ${ruleSet[0]}`
    )
  },
})

Deno.test({
  name: 'aaabbb',
  fn: function () {
    assertEquals(
      ruleSet[0].test(exampleMessages[3]),
      false,
      `Message: ${exampleMessages[3]}\nrule: ${ruleSet[0]}`
    )
  },
})

Deno.test({
  name: 'aaaabbb',
  fn: function () {
    assertEquals(
      ruleSet[0].test(exampleMessages[4]),
      false,
      `Message: ${exampleMessages[4]}\nrule: ${ruleSet[0]}`
    )
  },
})

Deno.test({
  name: 'Example B - part 1',
  fn: function () {
    assertEquals(Solution.solve(...Solution.loadFile('exampleinputb.txt')), 3)
  },
})

Deno.test({
  name: 'Example B - part 2',
  fn: function () {
    assertEquals(Solution.solveB(...Solution.loadFile('exampleinputb.txt')), 12)
  },
})

if (import.meta.main) Deno.runTests()
