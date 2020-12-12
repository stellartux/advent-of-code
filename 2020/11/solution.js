import { Automaton } from 'https://stellartux.github.io/fishtank/src/automaton.js'
import { MOORE } from 'https://stellartux.github.io/fishtank/src/neighbourhood.js'
import { Position } from 'https://stellartux.github.io/fishtank/src/position.js'
import { count } from 'https://stellartux.github.io/utilele/utilele.js'
/** @typedef {import('https://raw.githubusercontent.com/stellartux/fishtank/master/src/grid.js').Grid2D} Grid2D */

//@ts-ignore
let startingState = Deno.readTextFileSync('input.txt').trim()

function toGridData(str) {
  return str.split('\n').map((row) => row.split(''))
}

/**
 * @param {Position} position
 * @param {Grid2D} grid
 */
function rules(position, grid) {
  let value = grid.get(position)
  const neighbourCount = count('#', grid.neighbourValues(position))

  if (value === 'L') {
    if (neighbourCount === 0) {
      value = '#'
    }
  } else if (value === '#') {
    if (neighbourCount >= 4) {
      value = 'L'
    }
  }

  return value
}

const seats = new Automaton(rules, {
  height: startingState.match(/\n/g).length + 1,
  width: startingState.indexOf('\n'),
  data: toGridData(startingState),
  historyLength: 1,
})

seats.terminate()
console.log(count('#', seats.values()))

/**
 * @param {Position} position
 * @param {Grid2D} grid
 */
function otherRules(position, grid) {
  const value = grid.get(position)

  if (value === '.') {
    return value
  }

  const neighbourValues = []

  for (const direction of MOORE) {
    let pos = new Position(position)
    do {
      pos.x += direction.x
      pos.y += direction.y
    } while (grid.has(pos) && grid.get(pos) === '.')

    if (grid.has(pos)) {
      neighbourValues.push(grid.get(pos))
    }
  }

  const total = count('#', neighbourValues)
  if (value === '#') {
    return total >= 5 ? 'L' : '#'
  } else {
    return total === 0 ? '#' : 'L'
  }
}

const otherSeats = new Automaton(otherRules, {
  height: startingState.match(/\n/g).length + 1,
  width: startingState.indexOf('\n'),
  data: toGridData(startingState),
  historyLength: 1,
})

otherSeats.terminate()
console.log(count('#', otherSeats.values()))
