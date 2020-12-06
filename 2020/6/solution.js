const groups = Deno.readTextFileSync('input.txt').trim().split('\n\n')

import {
  sum,
  unique
} from 'https://raw.githubusercontent.com/stellartux/utilele/master/utilele.js'

const countUniqueLetters = (word) => word.match(/\w/g).filter(unique).length

const answers = groups.map(countUniqueLetters).reduce(sum, 0)

console.log(answers)

const allAnswered = groups
  .map((group) => {
    const g = group.split('\n')
    if (g.length === 1) {
      return countUniqueLetters(g[0])
    } else {
      return g[0]
        .match(/\w/g)
        .filter(unique)
        .filter((letter) => g.slice(1).every((group) => group.includes(letter)))
        .length
    }
  })
  .reduce(sum, 0)

console.log(allAnswered)
