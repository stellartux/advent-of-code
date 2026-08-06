{
  cmd[NR] = $1
  arg[NR] = $2
}

END {
  # part one
  run()
  print +a

  # part two
  for (j = 1; j <= NR; ++j) {
    if (cmd[j] != "acc") {
      cmd[j] = swap(cmd[j])
      if (run() < 0) {
        print +a
        break
      }
      cmd[j] = swap(cmd[j])
    }
  }

}

function swap(cmd) {
  return cmd == "nop" ? "jmp" : "nop"
}

function run(i) {
  a = 0
  delete visited
  for (i = 1; i <= NR;) {
    if (visited[i]) {
      return i
    }
    visited[i] = 1;
    if (cmd[i] == "acc") a += arg[i];
    i += cmd[i] == "jmp" ? arg[i] : 1
  }
  return -1
}
