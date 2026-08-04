export class UnionFind<Value> {
  #parents: Map<number, number> = new Map();
  #values: Value[] = [];

  constructor(iterable?: Iterable<Value>) {
    if (iterable) {
      this.push(...iterable);
    }
  }

  push(...values: Value[]) {
    for (const value of values) {
      const index = this.#values.push(value) - 1;
      this.#parents.set(index, index);
    }
  }

  find(value: Value): Value {
    return this.#values[this.#find(this.#values.indexOf(value))];
  }

  #find(index: number): number {
    const parentIndex = this.#parents.get(index);
    if (parentIndex === undefined) {
      throw new Error("unreachable");
    } else if (parentIndex !== index) {
      const result = this.#find(parentIndex);
      this.#parents.set(index, result);
      return result;
    } else {
      return index;
    }
  }

  unite(value1: Value, value2: Value) {
    let index1 = this.#find(this.#values.indexOf(value1));
    let index2 = this.#find(this.#values.indexOf(value2));
    if (index1 !== index2) {
      if (index1 > index2) {
        [index1, index2] = [index2, index1];
      }
      this.#parents.set(index1, index2);
    }
    return this;
  }

  get groupCount() {
    let result = 0;
    for (const [key, value] of this.#parents.entries()) {
      if (key === value) ++result;
    }
    return result;
  }

  get length() {
    return this.#values.length
  }

  *[Symbol.iterator]() {
    for (const key of this.#parents.keys()) {
      yield [key, this.#values[this.#find(key)]] as [number, Value]
    }
  }
}

// export class UnionFind {
//   constructor(input) {

//   }

// }
