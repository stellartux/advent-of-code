const passports = Deno.readTextFileSync('input.txt')
  .trim()
  .split(/\n{2,}/)
  .map((passport) =>
    passport
      .split(/\s+/g)
      .reduce((map, pair) => map.set(...pair.split(':')), new Map())
  )

const fields = ['byr', 'iyr', 'eyr', 'hgt', 'hcl', 'ecl', 'pid']

const valid = passports.filter((passport) =>
  fields.every((field) => passport.has(field))
)

console.log(`${valid.length} valid passports`)

const withinRange = (min, max) => (val) => val >= min && val <= max

const validators = {
  byr: withinRange(1920, 2002),
  iyr: withinRange(2010, 2020),
  eyr: withinRange(2020, 2030),
  hgt: (str) => /^(1([5-8]\d|9[0-3])cm|(59|6\d|7[0-6])in)$/.test(str),
  hcl: (str) => /^#[\da-z]{6}$/i.test(str),
  ecl: (str) => /^(amb|b(lu|rn)|gr[ny]|hzl|oth)$/.test(str),
  pid: (str) => /^\d{9}$/.test(str),
}

const strictlyValid = passports.filter((passport) =>
  fields.every(
    (field) => passport.has(field) && validators[field](passport.get(field))
  )
)

console.log(`${strictlyValid.length} strictly valid passports`)

/*
  byr (Birth Year) - four digits; at least 1920 and at most 2002.
  iyr (Issue Year) - four digits; at least 2010 and at most 2020.
  eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
  hgt (Height) - a number followed by either cm or in:
  If cm, the number must be at least 150 and at most 193.
  If in, the number must be at least 59 and at most 76.
  hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
  ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
  pid (Passport ID) - a nine-digit number, including leading zeroes.
  cid (Country ID) - ignored, missing or not.
*/
