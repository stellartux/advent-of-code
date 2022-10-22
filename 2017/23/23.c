#include <stdio.h>
#include <stdlib.h>

int main() {
  int b, d, h = 0;
  for (b = 109900; b <= 126900; b += 17) {
    for (d = 2; d < b >> 1; d++) {
      if (b % d == 0) {
        h++;
        break;
      }
    }
  }
  printf("%d\n", h);
}
