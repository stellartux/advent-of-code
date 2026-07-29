const path = Deno.args[0] ?? "example.txt"

const [rangesStr, valuesStr] = Deno.readTextFileSync(path)
  .trim()
  .split(/(?:\r?\n){2,}/, 2)

const values = valuesStr
  .split(/\r?\n/g)
  .map(BigInt)

const ranges = rangesStr
  .split(/\r?\n/g)
  .map((line) => line.split("-", 2).map(BigInt))
  .sort((a, b) => a[0] === b[0] ? Number(a[1] - b[1]) : Number(a[0] - b[0]))

let j = 1n
while (ranges[j]) {
  const left = ranges[j - 1n]
  const right = ranges[j]
  if (left[0] <= right[0] && right[0] <= left[1]) {
    left[1] = left[1] > right[1] ? left[1] : right[1]
    ranges.splice(Number(j), 1)
  } else {
    ++j
  }
}

let partOne = 0
for (const value of values) {
  for (const [min, max] of ranges) {
    if (min <= value && value <= max) {
      ++partOne
      break
    } else if (value < min) {
      break
    }
  }
}
console.log(partOne.toString())

let partTwo = 0n
for (const [min, max] of ranges) {
  partTwo += max - min + 1n
}
console.log(partTwo.toString())
