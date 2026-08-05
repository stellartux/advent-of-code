#!/usr/bin/env -S deno run --allow-read --

const input = Deno.readTextFileSync(Deno.args[0] ?? "example.txt");

function partOne(input: string) {
  return input.matchAll(/mul\((\d{1,3}),(\d{1,3})\)/g)
    .reduce((a, m) => a + (Number(m[1]) * Number(m[2])), 0);
}

function partTwo(input: string) {
  let result = 0;
  const matches = input.matchAll(
    /(?<do>do\(\))|(?<dont>don't\(\))|(?<mul>mul\((?<a>\d{1,3}),(?<b>\d{1,3})\))/g,
  );
  let active = true;
  for (const { groups } of matches) {
    if (groups?.do) {
      active = true;
    } else if (groups?.dont) {
      active = false;
    } else if (groups?.mul && active) {
      result += Number(groups.a) * Number(groups.b);
    }
  }
  return result;
}

console.log(partOne(input));
console.log(partTwo(input));
