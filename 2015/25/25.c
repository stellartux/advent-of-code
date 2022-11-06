#include <stdio.h>
#include <stdlib.h>

unsigned long code(unsigned long row, unsigned long column) {
  unsigned long r, c, t, result;
  result = 20151125;
  for (t = 2; true; t++) {
    for (r = 1; r < t; r++) {
      c = t - r;
      if (r == row && c == column) {
        return result;
      }
      result = result * 252533 % 33554393;
    }
  }
}

int main(int argc, char *argv[]) {
  unsigned long row, column;
  if (argc < 3) {
    printf("usage:\n    %s row column\n", argv[0]);
    exit(1);
  }
  row = atoi(argv[1]);
  column = atoi(argv[2]);
  if (!row || !column) {
    exit(1);
  }
  printf("%llu\n", code(row, column));
}
