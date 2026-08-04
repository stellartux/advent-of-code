#!/usr/bin/env -S deno run --allow-read --

import { Coord } from "../../helpers/coord.ts";
import { UnionFind } from "../../helpers/unionfind.ts";

function load(filepath: string) {
  return Deno.readTextFileSync(filepath)
    .trim()
    .split(/\s+/g)
    .map((line) => new Coord(line.split(",").map(Number)));
}

const input = load(Deno.args[0] ?? "example.txt");

const forest = new UnionFind(input);
for (let i = 0; i < input.length; ++i) {
  const left = input[i]
  for (let j = i + 1; j < input.length; ++j) {
    const right = input[j]
    if (left.manhattanDistance(right) <= 3) {
      forest.unite(left, right)
    }
  }
}

console.log(forest.groupCount);
