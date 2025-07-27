#!/usr/bin/env -s deno --allow-read

const passports = Deno.readTextFileSync(Deno.args[0])
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

console.log(`${valid.length}`)

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

console.log(`${strictlyValid.length}`)
