const slope = Deno.readTextFileSync('input.txt').trim().split('\n')

function ski(slope, xOff = 3, yOff = 1) {
  let x = 0
  let trees = 0

  for (let y = 0; y < slope.length; y += yOff) {
    if (slope[y][x] === '#') {
      trees++
    }
    x = (x + xOff) % slope[y].length
  }
  return trees
}

console.log(`Collided with ${ski(slope)} trees`)

console.log(
  [
    [1, 1],
    [3, 1],
    [5, 1],
    [7, 1],
    [1, 2],
  ]
    .map(([x, y]) => ski(slope, x, y))
    .reduce((a, b) => a * b)
)
