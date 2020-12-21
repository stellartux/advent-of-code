export function loadFile(path) {
  return Deno.readTextFileSync(path)
    .trim()
    .split('\n\n')
    .map((txt) => txt.split('\n'))
}

const stringLiteralRule = /\d+:\s+\"(.*)\"/

export function newRuleSet(ruleStrings) {
  const ruleset = []
  const stringRules = ruleStrings.filter((rule) => stringLiteralRule.test(rule))
  const nonStringRules = ruleStrings
    .filter((rule) => !stringLiteralRule.test(rule))
    .reduce((map, rule) => map.set(...rule.split(': ')), new Map())

  for (const rule of stringRules) {
    const [key, value] = rule.split(': ')
    ruleset[key] = new RegExp(`^${value.slice(1, -1)}$`)
  }

  return new Proxy(ruleset, {
    get: function (target, number, receiver) {
      if (target.hasOwnProperty(number)) {
        return target[number]
      }

      target[number] = new RegExp(
        `^(${nonStringRules
          .get(number)
          .split(' ')
          .map((token) =>
            token === '|' ? '|' : receiver[token].source.slice(1, -1)
          )
          .join('')})$`
      )
      return target[number]
    },
  })
}

export function solve(rules, messages) {
  const ruleset = newRuleSet(rules)
  return messages.reduce(
    (count, message) => count + ruleset[0].test(message),
    0
  )
}

const [rules, messages] = loadFile('input.txt')

export function solveB(rules, messages) {
  const ruleset = newRuleSet(rules)
  ruleset[8] = new RegExp(`^(${ruleset[42].source.slice(1, -1)})+$`)
  ruleset[11] = new RegExp(
    '^'.concat(
      `(${ruleset[42].source.slice(1, -1)}`.repeat(10),
      `${ruleset[31].source.slice(1, -1)})?`.repeat(9),
      ruleset[31].source.slice(1, -1),
      ')$'
    )
  )
  return messages.reduce(
    (count, message) => count + ruleset[0].test(message),
    0
  )
}
