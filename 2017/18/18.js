function load(filepath) {
  return Deno.readTextFileSync(filepath)
    .trim()
    .split("\n")
    .map((line) => line.split(/\s+/g))
}

class Interpreter {
  program
  ip = 0;
  done = false;
  waiting = false

  constructor(/** @type {string[][]} */ program) {
    this.program = program
  }

  set(x, y) {
    this[x] = this.getValue(y)
    this.ip += 1
  }

  add(x, y) {
    this[x] += this.getValue(y)
    this.ip += 1
  }

  mul(x, y) {
    this[x] *= this.getValue(y)
    this.ip += 1
  }

  mod(x, y) {
    this[x] %= this.getValue(y)
    this.ip += 1
  }

  jgz(x, y) {
    if (this.getValue(x) > 0) {
      this.ip += this.getValue(y)
    } else {
      this.ip += 1
    }
  }

  step() {
    const instr = this.program[this.ip]
    if (instr) {
      const [cmd, ...args] = instr
      return this[cmd](...args)
    } else {
      this.done = true
    }
  }

  getValue(x) {
    const value = Number(x)
    if (!isNaN(value)) {
      return value
    }
    let result = this[x]
    if (result === undefined) {
      result = this[x] = 0
    }
    return result
  }

  #sound

  snd(x) {
    this.#sound = this.getValue(x)
    this.ip += 1
  }

  rcv(x) {
    this.ip += 1
    if (this.getValue(x) !== 0) {
      return this.#sound
    }
  }
}

function partOne(program) {
  const interpreter = new Interpreter(program)
  let value
  do {
    value = interpreter.step()
  } while (!interpreter.done && value === undefined)
  return value
}

function partTwo(program) {
  const interpreter0 = new Interpreter(program)
  const interpreter1 = new Interpreter(program)
  interpreter1.p = 1

  const queue0to1 = []
  const queue1to0 = []

  function snd(queue) {
    return function (x) {
      queue.push(this.getValue(x))
      this.sendCount += 1
      this.ip += 1
    }
  }

  function rcv(queue) {
    return function (x) {
      const result = queue.shift()
      if (result !== undefined) {
        this[x] = result
        this.waiting = false
        this.ip += 1
      } else {
        this.waiting = true
      }
    }
  }

  function locked(queue) {
    return function () {
      return this.done || (this.waiting && queue[0] === undefined)
    }
  }

  interpreter0.sendCount = 0
  interpreter1.sendCount = 0

  interpreter0.snd = snd(queue0to1)
  interpreter0.rcv = rcv(queue1to0)
  interpreter0.locked = locked(queue1to0)

  interpreter1.snd = snd(queue1to0)
  interpreter1.rcv = rcv(queue0to1)
  interpreter1.locked = locked(queue0to1)

  while (!(interpreter0.locked() && interpreter1.locked())) {
    do {
      interpreter0.step()
    } while (!interpreter0.locked())
    do {
      interpreter1.step()
    } while (!interpreter1.locked())
    interpreter0.step()
  }

  return interpreter1.sendCount
}

const input = load(Deno.args[0] ?? "example.txt")
console.log(partOne(input))
console.log(partTwo(input))
