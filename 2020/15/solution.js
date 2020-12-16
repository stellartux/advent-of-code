export function naiveSolve(numbers) {
  while (numbers.length < 2020) {
    numbers.push(rulesOfTheGame(numbers))
  }
  return numbers[2019]
}

function rulesOfTheGame(numbers) {
  const i = numbers.length - 1
  const j = numbers.lastIndexOf(numbers[i], i - 1)
  return j >= 0 ? i - j : 0
}

export function solve(numbers, limit = 2020) {
  // Map of numbers to the last index of that number
  const visited = numbers.reduce((m, c, i) => m.set(c, i), new Map())
  let lastNumberSpoken = numbers[numbers.length - 1]
  let lastIndexOfLastNumberSpoken = -1

  for (let index = numbers.length; index < limit; index++) {
    if (lastIndexOfLastNumberSpoken > -1) {
      lastNumberSpoken = index - lastIndexOfLastNumberSpoken - 1
    } else {
      lastNumberSpoken = 0
    }
    lastIndexOfLastNumberSpoken = visited.has(lastNumberSpoken)
      ? visited.get(lastNumberSpoken)
      : -1
    visited.set(lastNumberSpoken, index)
  }
  return lastNumberSpoken
}

// console.log(naiveSolve([15, 5, 1, 4, 7, 0]))
// console.log(solve([15, 5, 1, 4, 7, 0], 30_000_000))
