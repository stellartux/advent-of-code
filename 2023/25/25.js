#!/usr/bin/env -S deno run --allow-read
/// <reference lib="es2020" />
/// @ts-check

/** @template T */
export class UnionFind {
  /** @type {Map<T,T>} */ #parent = new Map()
  /** @type {Map<T,number>} */ #rank = new Map()
  #compress = false

  constructor(/** @type {T[]} */...values) {
    for (const value of values) {
      this.#parent.set(value, value)
      this.#rank.set(value, 0)
    }
  }

  add(/** @type {T[]} */...values) {
    for (const value of values) {
      if (!this.#parent.has(value)) {
        this.#parent.set(value, value)
        this.#rank.set(value, 0)
      }
    }
  }

  compress() {
    this.#compress = true
    for (const key of this.#parent.keys()) {
      this.find(key)
    }
  }

  clear() {
    this.#parent.clear()
    this.#rank.clear()
    this.#compress = false
  }

  /**
   * Find the representative node of the union-find
   * @param {T} value
   * @param {boolean} [compress=false] When true, performs path compression from then on.
   * @returns {T|undefined}
   */
  find(value, compress = this.#compress) {
    const parent = this.#parent.get(value)
    if (!parent || parent === value) {
      return parent
    } else {
      const root = this.find(parent, compress)
      if (root !== undefined) {
        if (compress && root !== parent) {
          this.#compress = true
          this.#parent.set(value, root)
        }
        return root
      }
    }
  }

  graphviz() {
    let result = 'graph {\n'
    for (const [node, parent] of this.#parent) {
      if (node !== parent) {
        result += '\t' + JSON.stringify(node) + ' -- ' + JSON.stringify(parent) + '\n'
      }
    }
    return result + '}\n'
  }

  get groupSizes() {
    this.compress()
    /** @type {Map<T,number>} */
    const sizes = new Map()
    for (const root of this.#parent.values()) {
      sizes.set(root, (sizes.get(root) || 0) + 1)
    }
    return sizes.values()
  }

  /** Unite the sets containing the left and right nodes */
  unite(/** @type {T} */ left, /** @type {T} */ right, compress = false) {
    const leftParent = this.find(left, compress)
    const rightParent = this.find(right, compress)
    if (leftParent && rightParent && leftParent !== rightParent) {
      const leftSeniority = this.#rank.get(leftParent) ?? 0
      const rightSeniority = this.#rank.get(rightParent) ?? 0
      if (leftSeniority <= rightSeniority) {
        this.#parent.set(leftParent, rightParent)
      } else {
        this.#parent.set(right, left)
      }
      if (leftSeniority === rightSeniority) {
        this.#rank.set(leftParent, leftSeniority + 1)
      }
      return true
    }
    return false
  }
}

///@ts-ignore
if (import.meta.main) {
  /** @type {UnionFind<string>} */
  const uf = new UnionFind()
  ///@ts-ignore
  for (const line of Deno.readTextFileSync(Deno.args.at(-1)).split('\n')) {
    if (!line) continue
    const [left, ...rights] = line.match(/[a-zA-Z]+/g)
    uf.add(left)
    uf.add(...rights)
    for (const right of rights) {
      uf.unite(left, right, true)
    }
  }
  console.log([...uf.groupSizes].reduce((a, c) => a * c, 1))
}
