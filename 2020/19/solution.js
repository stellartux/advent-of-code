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
  let nonStringRules = ruleStrings
    .filter((rule) => !stringLiteralRule.test(rule))
    .reduce((map, rule) => map.set(rule.split(': ')[0], rule), new Map())

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
        `^(${nonStringRules.get(number)
          .split(': ')[1]
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

console.log(solve(...loadFile('input.txt')))
