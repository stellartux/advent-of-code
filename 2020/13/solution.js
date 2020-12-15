const file = Deno.readTextFileSync('input.txt').trim()
let [timestamp, buses] = file.split('\n')
timestamp = Number(timestamp)
const busNumbers = buses
  .split(',')
  .filter((x) => x !== 'x')
  .map(Number)

function findBus(timestamp, buses) {
  let time = timestamp
  while (true) {
    for (const bus of buses) {
      if (time % bus === 0) {
        return (time - timestamp) * bus
      }
    }
    time++
  }
}

// console.log(findBus(timestamp, busNumbers))

const otherBuses = buses
  .split(',')
  .map((n, relativeTime) => {
    return (
      n !== 'x' && {
        number: Number(n),
        relativeTime: relativeTime % n,
      }
    )
  })
  .filter((x) => x)
  .sort((left, right) => right.number - left.number)

const N = otherBuses.reduce((prod, bus) => prod * bus.number, 1)

//*
otherBuses.forEach(bus => {
  console.log(`x = ${bus.relativeTime} (mod ${bus.number})`)
})

/*
x = 50 (mod 523)
x = 19 (mod 521)
x = 9 (mod 41)
x = 19 (mod 37)
x = 19 (mod 29)
x = 4 (mod 23)
x = 0 (mod 19)
x = 2 (mod 17)
x = 11 (mod 13)

x = 523t + 50
x = 521t + 19
x = 41t + 9
x = 37t + 19
x = 29t + 19
x = 23t + 4
x = 19t + 0
x = 17t + 2
x = 13t + 11

something something Chinese Remainder Theorem something something
something something linear system of Diophantine equations something something
*/
