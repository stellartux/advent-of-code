#!/usr/bin/env deno run --allow-read

const /** @type {number[]} */ seatIds =
  Deno.readTextFileSync(Deno.args[0])
    .trim()
    .replaceAll(/[FL]/g, '0')
    .replaceAll(/[BR]/g, '1')
    .split('\n')
    .map((/** @type {string} */ s) => parseInt(s, 2))

console.log(Math.max(...seatIds))
console.log(seatIds.find((x, i) => x - seatIds[i + 1] !== 1) - 1)
