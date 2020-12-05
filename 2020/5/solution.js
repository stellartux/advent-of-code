const passes = Deno.readTextFileSync('input.txt').trim().split('\n')

const converter = (letters) => (number) =>
  parseInt(
    number.replaceAll(/./g, (d) => letters.indexOf(d)),
    2
  )

const seatIds = passes.map(
  (pass) =>
    converter('FB')(pass.slice(0, 7)) * 8 + converter('LR')(pass.slice(-3))
)

const highest = Math.max(...seatIds)

console.log(highest)

for (let seat = Math.min(...seatIds); seat < highest; seat++) {
  if (!seatIds.includes(seat) && seatIds.includes(seat + 1) && seatIds.includes(seat - 1)) {
    console.log(seat)
    break
  }
}
