let loadFile
let argv
if (globalThis.std) {
  loadFile = globalThis.std.loadFile.bind(globalThis.std)
  argv = globalThis.scriptArgs
} else if (globalThis.Deno) {
  loadFile = function (filename) {
    return String.fromCharCode(...globalThis.Deno.readFileSync(filename))
  }
  argv = globalThis.Deno.args
} else if (globalThis.process) {
  const fs = require('node:fs')
  loadFile = function (filename) {
    return String.fromCharCode(...fs.readFileSync(filename))
  }
  argv = globalThis.process.argv.slice(1)
} else {
  throw new Error("Can't access file system. (try `--std` flag in qjs)")
}

function loadGrid(filename) {
  const grid = loadFile(filename)
    .split('\n')
    .map((line) => line.split('').map((c) => c !== ' ' && c))
  grid.atPosition = ([x, y]) => grid[y]?.[x]
  return grid
}

function add(position, bearing) {
  position[0] += bearing[0]
  position[1] += bearing[1]
  return position
}

function addedTo(position, bearing) {
  return [position[0] + bearing[0], position[1] + bearing[1]]
}

function turn180(bearing) {
  bearing[0] = -bearing[0]
  bearing[1] = -bearing[1]
  return bearing
}

function turn90(bearing) {
  [bearing[0], bearing[1]] = [bearing[1], bearing[0]]
  return bearing
}

function main(filename) {
  const grid = loadGrid(filename)
  const position = [grid[0].indexOf('|'), 0]
  const bearing = [0, 1]
  const path = []
  let steps = 1
  while (true) {
    if (/[A-Z]/.test(grid.atPosition(position))) {
      path.push(grid.atPosition(position))
    }
    if (grid.atPosition(addedTo(position, bearing))
      || grid.atPosition(addedTo(position, turn90(bearing)))
      || grid.atPosition(addedTo(position, turn180(bearing)))) {
      add(position, bearing)
      steps++
    } else {
      console.log(path.join(''))
      console.log(steps)
      return
    }
  }
}

main(argv[argv.length - 1] || 'example.txt')
