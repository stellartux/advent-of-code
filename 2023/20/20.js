#!/usr/bin/env -S deno run --allow-read
/// <reference lib="es2023" />
///@ts-check

class Pulse {
  sender
  level
  receiver

  /**
   * @param {PulseModule} sender
   * @param {PulseLevel} level
   * @param {PulseModule} receiver
   */
  constructor(sender, level, receiver) {
    this.sender = sender
    this.level = level
    /** @typedef {"low"|"high"} PulseLevel */

    const /** @type {PulseLevel} */ low = 'low'
    const /** @type {PulseLevel} */ high = 'high'

    this.receiver = receiver
  }
  toString() {
    return this.sender.name + ' -' + this.level + '-> ' + this.receiver.name
  }
}

/** @typedef {"low"|"high"} PulseLevel */

const /** @type {PulseLevel} */ low = 'low'
const /** @type {PulseLevel} */ high = 'high'

/** @extends {Array<Pulse>} */
class Queue extends Array {
  #lows = 0
  #highs = 0
  run() {
    for (let pulse = this.shift(); pulse; pulse = this.shift()) {
      console.debug(pulse.toString())
      if (pulse.level === low) {
        ++this.#lows
      } else {
        ++this.#highs
      }
      pulse.receiver.send(this, pulse)
    }
  }
  get result() {
    return this.#lows * this.#highs
  }
  static from(/** @type {Pulse[]} */ pulses) {
    const queue = new Queue()
    queue.push(...pulses)
    return queue
  }
}

function invert(level = high) {
  return level === high ? low : high
}

class PulseModule {
  /** @type {PulseModule[]} */ outputs = []
  name
  /** @param {string} name */
  constructor(name) {
    this.name = name
  }

  /** @abstract */
  addInputs(/** @type {PulseModule[]} */ ..._inputs) { }

  addOutputs(/** @type {PulseModule[]} */ ...outputs) {
    this.outputs.push(...outputs)
  }

  /** @abstract */
  send(/** @type {Queue} */ _queue, /** @type {Pulse} */ _pulse) { }

  pushAll(/** @type {Queue} */ queue, level = low) {
    for (const receiver of this.outputs) {
      queue.push(new Pulse(this, level, receiver))
    }
  }
}

class ButtonModule extends PulseModule {
  send(/** @type {Queue} */ queue) {
    this.pushAll(queue, low)
  }
}

class BroadcastModule extends PulseModule {
  /**
   * @param {Queue} queue
   * @param {Pulse} pulse
   */
  send(queue, { level }) {
    this.pushAll(queue, level)
  }
}

class FlipFlopModule extends PulseModule {
  level = low
  /**
   * @param {Queue} queue
   * @param {Pulse} pulse
   */
  send(queue, { level }) {
    if (level === low) {
      this.level = invert(this.level)
      this.pushAll(queue, this.level)
    }
  }
}

class ConjunctionModule extends PulseModule {
  /** @type {Map<string,PulseLevel>}} */ memory = new Map()

  addInputs(/** @type {PulseModule[]} */ ...inputs) {
    for (const input of inputs) {
      if (!this.memory.has(input.name)) {
        this.memory.set(input.name, low)
      }
    }
  }

  /**
   * @param {Queue} queue
   * @param {Pulse} pulse
   */
  send(queue, { level, sender }) {
    this.memory.set(sender.name, level)
    // @ts-ignore
    const newPulse = this.memory.values().every((/** @type {PulseLevel} */ p) => p === high) ? low : high
    this.pushAll(queue, newPulse)
  }
}

function loadFile(/** @type {string} */ filename) {
  // @ts-ignore
  const config = Deno.readTextFileSync(filename)
  const modules = new Map()
  const connections = [...config.matchAll(/^(?<prefix>[%&]?)(?<sender>\S+) -> (?<receivers>.+)/gm)].map((match) => match.groups)
  let broadcaster
  for (const { prefix, sender } of connections) {
    if (!modules.has(sender)) {
      if (prefix === '%') {
        modules.set(sender, new FlipFlopModule(sender))
      } else if (prefix === '&') {
        modules.set(sender, new ConjunctionModule(sender))
      } else {
        broadcaster = new BroadcastModule(sender)
        modules.set(sender, broadcaster)
      }
    }
  }
  if (!broadcaster) {
    throw new Error('No broadcaster module found in config')
  }
  for (const { sender: name, receivers } of connections) {
    const sender = modules.get(name)
    for (const name of receivers.split(', ')) {
      let receiver = modules.get(name)
      if (!receiver) {
        receiver = new PulseModule(name)
        modules.set(name, receiver)
      }
      sender.addOutputs(receiver)
      receiver.addInputs(sender)
    }
  }
  const button = new ButtonModule('button')
  button.addOutputs(broadcaster)
  return button
}

// @ts-ignore
if (import.meta.main) {
  // @ts-ignore
  const scriptArgs = Deno.args
  if (scriptArgs.length === 0) {
    throw new Error('usage: ./20.js [--debug] FILENAME')
  } else if (scriptArgs[0] !== '--debug') {
    console.debug = function () { }
  }
  const button = loadFile(scriptArgs.at(-1))
  const queue = new Queue()
  for (let i = 0; i < 1000; ++i) {
    button.send(queue)
    queue.run()
  }
  console.log(queue.result)
}
