function* range(
  indices1: number[],
  indices2: number[],
): Generator<number[], void, void> {
  if (indices1.length === 1) {
    for (let i = indices1[0]; i <= indices2[0]; ++i) {
      yield [i];
    }
  } else {
    const i1 = indices1.slice(1);
    const i2 = indices2.slice(1);
    for (let i = indices1[0]; i <= indices2[0]; ++i) {
      for (const rest of range(i1, i2)) {
        yield [i, ...rest];
      }
    }
  }
}

function assertEqualLength(coord1: Coord, coord2: Coord) {
  if (coord1.length !== coord2.length) {
    throw new Error("Can't compare coords with different dimensions.");
  }
}

export class Coord {
  indices;
  length;

  constructor(indices: number[]) {
    this.indices = indices;
    this.length = this.indices.length;
  }

  equals(that: Coord) {
    return this.length === that.length &&
      this.indices.every((x, i) => x === that.indices[i]);
  }

  manhattanDistance(that: Coord) {
    assertEqualLength(this, that);
    let result = 0;
    for (let i = 0; i < this.length; ++i) {
      result += Math.abs(this.indices[i] - that.indices[i]);
    }
    return result;
  }

  add(that: Coord) {
    assertEqualLength(this, that);
    for (const [index, value] of that.indices.entries()) {
      this.indices[index] += value;
    }
    return this;
  }

  sub(that: Coord) {
    assertEqualLength(this, that);
    for (const [index, value] of that.indices.entries()) {
      this.indices[index] -= value;
    }
    return this;
  }

  /** The von Neumann neighbourhood, i.e. no diagonals */
  *neighbours() {
    const offset = Array.from({ length: this.length }, () => 0);
    for (let i = 0; i < this.length; ++i) {
      for (const step of [-1, 1]) {
        offset[i] = step;
        yield new Coord(offset).add(this);
      }
      offset[i] = 0;
    }
  }

  /** open range */
  *range(that: Coord) {
    assertEqualLength(this, that);
    for (const indices of range(this.indices, that.indices)) {
      yield new Coord(indices);
    }
  }

  toString() {
    return `Coord(${this.indices.join(", ")})`;
  }
}
