#!/usr/bin/env -S qjs -m --std
// usage: qjs -m --std 7.js filename.txt
/// <reference lib="es2020" />

export class DirTree {
  #parent

  /** @param {DirTree?} parent */
  constructor(parent) {
    this.#parent = parent
  }

  /** @param {string} filename */
  static load(filename) {
    const root = new DirTree()
    let pwd = root
    if (filename) {
      const fp = std.open(filename, 'r')
      for (let line = fp.getline(); line;) {
        if (/^\$ cd/.test(line)) {
          const dir = line.slice(5)
          if (dir === '/') {
            pwd = root
          } else if (dir === '..') {
            pwd = pwd.#parent
          } else {
            if (!pwd[dir]) {
              pwd[dir] = new DirTree(pwd)
            }
            pwd = pwd[dir]
          }
          line = fp.getline()
        } else if (line === '$ ls') {
          for (line = fp.getline(); line && line[0] !== '$'; line = fp.getline()) {
            const [left, right] = line.split(' ')
            if (left === 'dir') {
              if (!pwd[right]) {
                pwd[right] = new DirTree(pwd)
              }
            } else {
              pwd[right] = parseInt(left)
            }
          }
        }
      }
    } else {
      throw new Error('Filename required')
    }
    return root
  }

  print(name = '/', indent = 0) {
    console.log(`${' '.repeat(indent)}- ${name} (dir)`)
    for (const [name, value] of Object.entries(this)) {
      if (typeof (value) === 'object') {
        value.print(name, indent + 2)
      } else {
        console.log(`${' '.repeat(indent + 2)}- ${name} (file, size=${value})`)
      }
    }
    if (indent === 0) {
      console.log('')
    }
  }

  size() {
    let total = 0
    for (let x of this.walkNumbers()) {
      total += x
    }
    return total
  }

  *walkNodes() {
    for (const [key, node] of Object.entries(this)) {
      if (typeof node !== 'number' && key[0] !== '#') {
        yield* node.walkNodes()
      }
    }
    yield this
  }

  *walkNumbers() {
    for (const node of Object.values(this)) {
      if (typeof node === 'number') {
        yield node
      } else {
        yield* node.walkNumbers()
      }
    }
  }
}

// Find all of the directories with a total size of at most 100000.
// What is the sum of the total sizes of those directories?
function partOne(tree) {
  let total = 0
  for (const node of tree.walkNodes()) {
    const size = node.size()
    if (size <= 100000) {
      total += size
    }
  }
  return total
}

function partTwo(tree) {
  let result = tree.size()
  const neededSpace = result - 40000000
  for (const node of tree.walkNodes()) {
    const size = node.size()
    if (size >= neededSpace) {
      result = Math.min(result, size)
    }
  }
  return result
}

if (scriptArgs[0] === '7.js') {
  const tree = DirTree.load(scriptArgs[1])
  console.log(partOne(tree), partTwo(tree))
}
