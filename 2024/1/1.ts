const rows = Deno.readTextFileSync(Deno.args[0] ?? "example.txt")
  .trim()
  .split("\n")
  .map((line) => line.split(/\s+/).map(Number));

const [col1, col2] = [0, 1]
  .map((x) => rows.map((row) => row[x])).map((col) =>
    col.sort((a, b) => a - b)
  );

let partOne = 0;
for (const [i, x1] of col1.entries()) {
  partOne += Math.abs(x1 - col2[i]);
}
console.log(partOne);

const counts: Record<number, number> = {};

for (const x of col2) {
  counts[x] = (counts[x] || 0) + 1;
}

const partTwo = col1.reduce((a, c) => c * (counts[c] || 0) + a, 0);
console.log(partTwo)
