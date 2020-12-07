//@ts-ignore
const bags = Deno.readTextFileSync('input.txt').trim().split('\n')

export class BagList extends Map<string, Map<string, number>> {
  constructor(rules: string[]) {
    super()
    rules.forEach((rule: string) => {
      const matchIter = rule.matchAll(/(\d+ )?(\w+ \w+) bag/g)

      const color = matchIter.next().value[2]
      const contents = new Map<string, number>()

      for (const match of matchIter) {
        if (match[1]) {
          contents.set(match[2], Number(match[1]))
        }
      }

      this.set(color, contents)
    })
  }

  hasContents(needleColor: string): string[] {
    return [...this.keys()].filter((color) => this.get(color)?.has(needleColor))
  }

  sizeOf(needleColor: string): number {
    return [...(this.get(needleColor)?.entries() || [])].reduce(
      (total, [color, contents]) => total + contents * this.sizeOf(color),
      1
    )
  }

  holdsBags(bagColor: string): Set<string> {
    const result = new Set<string>()
    let size: number
    this.hasContents(bagColor).forEach((color) => result.add(color))

    do {
      size = result.size
      result.forEach((foundColor) =>
        this.hasContents(foundColor).forEach((color) => result.add(color))
      )
    } while (size < result.size)

    return result
  }
}

const bagList = new BagList(bags)
console.log(
  `Shiny gold bags can appear in ${
    bagList.holdsBags('shiny gold').size
  } different bags.`
)
console.log(
  `Shiny gold bags must have ${
    bagList.sizeOf('shiny gold') - 1
  } bags inside them.`
)
