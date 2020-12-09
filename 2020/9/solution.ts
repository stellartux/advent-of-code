//@ts-ignore
import { sum } from 'https://raw.githubusercontent.com/stellartux/utilele/master/utilele.js'

//@ts-ignore
const numbers = Deno.readTextFileSync('input.txt')
  .trim()
  .split('\n')
  .map(Number)

function hasSumPair(total: number, numbers: number[]): boolean {
  if (numbers.length < 2) {
    return false
  } else if (numbers.slice(1).some((n: number) => n + numbers[0] === total)) {
    return true
  } else {
    return hasSumPair(total, numbers.slice(1))
  }
}

function findMissingSumPair(numbers: number[], preambleLength: number): number {
  for (let i = preambleLength; i < numbers.length; i++) {
    if (!hasSumPair(numbers[i], numbers.slice(i - preambleLength, i))) {
      return numbers[i]
    }
  }
  throw new Error('No number found')
}

const missingNo = findMissingSumPair(numbers, 26)

console.log(missingNo)

function findContiguousSumRange(numbers: number[], needle: number): number[] {
  let start = 0
  let end = 2
  let total = numbers.slice(start, end).reduce(sum)

  while (end < numbers.length) {
    if (total === needle) {
      return numbers.slice(start, end)
    } else if (total < needle) {
      total += numbers[end]
      end += 1
    } else {
      total -= numbers[start]
      start += 1
      if (end - start < 2) {
        total += numbers[end]
        end += 1
      }
    }
  }
  throw new Error('No number found')
}

const range = findContiguousSumRange(numbers, missingNo)

console.log(Math.min(...range) + Math.max(...range))
