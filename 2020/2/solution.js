const lines = Deno.readTextFileSync('2/input.txt').trim().split('\n')

const total = lines.filter((line) => {
  const [_, min, max, letter, password] = line.match(/(\d+)-(\d+) (\w): (.*)/)
  const letterCount = password.match(new RegExp(letter, 'g'))?.length ?? 0
  return letterCount >= min && letterCount <= max
}).length

console.log(`${total} valid passwords`)

const secondTotal = lines.filter((line) => {
  const [_, index1, index2, letter, password] = line.match(
    /(\d+)-(\d+) (\w): (.*)/
  )
  return (
    password[index1 - 1] !== password[index2 - 1] &&
    (password[index1 - 1] === letter || password[index2 - 1] === letter)
  )
}).length

console.log(`${secondTotal} valid passwords`)
