const elfCount = Deno.args[0] ? Number(Deno.args[0]) : 5;

class Elf {
  id;
  previous: Elf;
  next: Elf;
  constructor(presents: number) {
    this.id = presents;
    this.previous = this;
    this.next = this;
  }

  unshift(previousElf: Elf) {
    previousElf.previous = this.previous;
    previousElf.next = this;
    this.previous = this.previous.next = previousElf;
  }

  push(nextElf: Elf) {
    nextElf.previous = this;
    nextElf.next = this.next;
    this.next = this.next.previous = nextElf;
  }

  pop() {
    return this.next.remove()
  }

  shift() {
    return this.previous.remove()
  }

  remove() {
    this.previous.next = this.next
    this.next.previous = this.previous
    return this
  }
}

function elfRing(elfCount: number) {
  const elf = new Elf(1);
  for (let presents = 2; presents <= elfCount; ++presents) {
    elf.unshift(new Elf(presents));
  }
  return elf;
}

function partOne(elfCount: number) {
  let elf = elfRing(elfCount);
  while (elf !== elf.next) {
    elf.pop();
    elf = elf.next;
  }
  return elf.id;
}

console.log(partOne(elfCount));

function partTwo(elfCount: number) {
  let elf = elfRing(elfCount);
  let target = elf
  for (let i = 0; i < Math.floor(elfCount / 2); i++) {
    target = target.next
  }
  while (elf !== elf.next) {
    target.remove()
    target = target.next
    if (--elfCount % 2 === 0) {
      target = target.next
    }
    elf = elf.next
  }
  return elf.id;
}

console.log(partTwo(elfCount));
