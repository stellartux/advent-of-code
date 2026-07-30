export interface Position {
  x: number;
  y: number;
}

// deno-lint-ignore ban-types
export class Map2D<Value extends {}> implements Map<Position, Value> {
  #rows = new Map<number, Map<number, Value>>();

  constructor(grid?: Value[][]) {
    if (grid) {
      grid.forEach((row, y) =>
        row.forEach((value, x) => this.set({ x, y }, value))
      );
    }
  }

  get bounds(): [maxX: number, maxY: number] {
    let maxX = 0, maxY = 0;
    for (const { x, y } of this.keys()) {
      if (x > maxX) maxX = x;
      if (y > maxY) maxY = y;
    }
    return [maxX, maxY];
  }

  // Map interface

  clear(): void {
    this.#rows.clear();
  }

  delete({ x, y }: Position): boolean {
    return Boolean(this.#rows.get(y)?.delete(x));
  }

  forEach(
    callbackfn: (
      value: Value,
      key: Position,
      map: Map<Position, Value>,
    ) => void,
    // deno-lint-ignore no-explicit-any
    thisArg?: any,
  ): void {
    for (const [key, value] of this.entries()) {
      callbackfn.call(thisArg, value, key, this);
    }
  }

  get({ x, y }: Position) {
    return this.#rows.get(y)?.get(x);
  }

  has({ x, y }: Position) {
    return Boolean(this.#rows.get(y)?.has(x));
  }

  set({ x, y }: Position, value: Value) {
    this.#rows.getOrInsertComputed(y, () => new Map<number, Value>()).set(
      x,
      value,
    );
    return this;
  }

  get size() {
    return this.#rows.values().reduce((size, row) => size + row.size, 0);
  }

  *entries(): MapIterator<[Position, Value]> {
    for (const [y, row] of this.#rows.entries()) {
      for (const [x, val] of row.entries()) {
        yield [{ x, y }, val];
      }
    }
  }

  *keys(): MapIterator<Position> {
    for (const [y, row] of this.#rows.entries()) {
      for (const x of row.keys()) {
        yield { x, y };
      }
    }
  }

  *values(): MapIterator<Value> {
    for (const row of this.#rows.values()) {
      yield* row.values();
    }
  }

  getOrInsert({ x, y }: Position, defaultValue: Value): Value {
    const row = this.#rows.getOrInsertComputed(
      y,
      () => new Map<number, Value>(),
    );
    return row.getOrInsert(x, defaultValue);
  }

  getOrInsertComputed(
    position: Position,
    callback: (key: Position) => Value,
  ): Value {
    const { x, y } = position;
    const row = this.#rows.getOrInsertComputed(
      y,
      () => new Map<number, Value>(),
    );
    return row.getOrInsert(x, callback(position));
  }

  [Symbol.iterator](): MapIterator<[Position, Value]> {
    return this.entries();
  }

  get [Symbol.toStringTag]() {
    return "Map2D";
  }
}
