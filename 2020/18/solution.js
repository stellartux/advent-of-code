const exprs = Deno.readTextFileSync('input.txt').trim().split('\n')

function evalu(expr) {
  while (/\(/.test(expr)) {
    expr = expr.replace(/\([^()]+\)/g, (g) => evalu(g.slice(1, -1)))
  }

  // comment this part out for part 1 answer
  while (/(\d+) \+ (\d+)/.test(expr)) {
    expr = expr.replace(/(\d+) \+ (\d+)/g, (_, a, b) => Number(a) + Number(b))
  }
  // end of commentout block

  let tokens = expr.split(' ')
  let acc = Number(tokens.shift())
  while (tokens.length) {
    switch (tokens.shift()) {
      case '*':
        acc *= tokens.shift()
        break
      case '+':
        acc += Number(tokens.shift())
        break
    }
  }
  return acc
}

console.log(exprs.map(evalu).reduce((a, b) => a + b, 0))
