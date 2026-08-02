#!/usr/bin/env -S deno run --allow-read --

type Direction = 0 | 1 | 2 | 3;

class Guard {
  x;
  y;
  direction;
  constructor(x: number, y: number, direction: Direction) {
    this.x = x;
    this.y = y;
    this.direction = direction;
  }

  moveForward() {
    switch (this.direction) {
      case 0:
        --this.y;
        break;
      case 1:
        ++this.x;
        break;
      case 2:
        ++this.y;
        break;
      case 3:
        --this.x;
        break;
    }
  }

  turnRight() {
    this.direction = ((this.direction + 1) & 3) as Direction;
  }

  isBlocked(grid: Grid) {
    let { x, y } = this;
    if (this.direction === 0) {
      --y;
    } else if (this.direction === 1) {
      ++x;
    } else if (this.direction === 2) {
      ++y;
    } else if (this.direction === 3) {
      --x;
    }
    return grid.get(x, y);
  }

  copy() {
    return new Guard(this.x, this.y, this.direction);
  }
}

class Grid {
  #data;
  width;
  height;
  constructor(width: number, height: number) {
    this.width = width;
    this.height = height;
    this.#data = new Uint8Array(width * height);
  }

  get(x: number, y: number) {
    return Boolean(this.#data[y * this.width + x]);
  }

  set(x: number, y: number) {
    if (!this.inBounds(x, y)) {
      throw new Error(`BoundsError: { x: ${x}, y: ${y} }`);
    }
    this.#data[y * this.width + x] = 1;
    return true;
  }

  inBounds(x: number, y: number) {
    return x >= 0 && x < this.width && y >= 0 && y < this.height;
  }

  keys() {
    return this.entries().map(([key]) => key);
  }

  *entries(): Generator<[{ x: number; y: number }, boolean]> {
    for (let y = 0; y < this.height; ++y) {
      for (let x = 0; x < this.width; ++x) {
        yield [{ x, y }, this.get(x, y)];
      }
    }
  }

  remove(x: number, y: number) {
    this.#data[y * this.width + x] = 0;
  }

  toString(): string {
    let s = "";
    for (let y = 0; y < this.height; ++y) {
      for (let x = 0; x < this.width; ++x) {
        s += this.get(x, y) ? "#" : ".";
      }
      s += "\n";
    }
    return s;
  }
}

function load(path: string) {
  const rows = Deno.readTextFileSync(path).trim().split(/\s+/g);
  const grid = new Grid(rows[0].length, rows.length);
  let guard;
  for (let y = 0; y < rows.length; ++y) {
    for (let x = 0; x < rows[y].length; ++x) {
      const value = rows[y][x];
      if (value === "#") {
        grid.set(x, y);
      } else if (value === "^") {
        guard = new Guard(x, y, 0);
      }
    }
  }
  if (!guard) {
    throw new Error("Couldn't find the guard");
  }
  return { grid, guard };
}

const { grid, guard } = load(Deno.args[0] ?? "example.txt");

class GuardSet {
  #data;
  width;
  height;
  constructor(options: { width: number; height: number }) {
    this.#data = new Uint8Array(options.width * options.height);
    this.width = options.width;
    this.height = options.height;
  }

  get({ x, y }: { x: number; y: number }): Direction[] {
    const result: Direction[] = [];
    const value = this.#data[y * this.width + x];
    for (let i = 0; i < 4; ++i) {
      if (value & (1 << i)) {
        result.push(i as Direction);
      }
    }
    return result;
  }

  has({ x, y, direction }: Guard) {
    return (this.#data[y * this.width + x] & (1 << direction)) !== 0;
  }

  set({ x, y, direction }: Guard) {
    this.#data[y * this.width + x] |= 1 << direction;
  }

  get size() {
    let result = 0;
    for (const value of this.#data) {
      if (value !== 0) {
        ++result;
      }
    }
    return result;
  }

  *keys() {
    for (let y = 0; y < this.height; ++y) {
      for (let x = 0; x < this.width; ++x) {
        if (this.#data[y * this.width + x] !== 0) {
          yield { x, y };
        }
      }
    }
  }
}

function gallivant(grid: Grid, guard: Guard) {
  const visited = new GuardSet(grid);
  guard = guard.copy();
  while (grid.inBounds(guard.x, guard.y)) {
    visited.set(guard);
    while (guard.isBlocked(grid)) {
      guard.turnRight();
    }
    guard.moveForward();
  }
  return visited;
}

const visited = gallivant(grid, guard);

// part one
console.log(visited.size.toString());

function hasLoop(grid: Grid, guard: Guard): boolean {
  guard = guard.copy();
  const visited = new GuardSet(grid);
  while (grid.inBounds(guard.x, guard.y)) {
    if (visited.has(guard)) {
      return true;
    } else {
      visited.set(guard);
    }
    if (guard.isBlocked(grid)) {
      guard.turnRight();
    } else {
      guard.moveForward();
    }
  }
  return false;
}

let partTwo = 0;
for (const { x, y } of visited.keys()) {
  if (x !== guard.x || y !== guard.y) {
    grid.set(x, y);
    if (hasLoop(grid, guard)) {
      ++partTwo;
    }
    grid.remove(x, y);
  }
}

// part two
console.log(partTwo.toString());
