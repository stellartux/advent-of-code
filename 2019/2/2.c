#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct Intcode {
  size_t length;
  int64_t buffer[];
} Intcode;

Intcode *LoadIntcode(const char *filepath) {
  Intcode *code;
  uint64_t i, t;
  FILE *fp;
  int c;
  i = 1;
  fp = fopen(filepath, "r");
  if (!fp) {
    fprintf(stderr, "File not found: \"%s\"\n", filepath);
    return NULL;
  }
  while ((c = fgetc(fp)) != EOF) {
    i += (c == ',');
  }
  rewind(fp);
  code = malloc(sizeof(Intcode) + sizeof(int64_t) * i);
  code->length = i;
  i = 0;
  t = 0;
  while ((c = fgetc(fp)) != EOF) {
    if (isdigit(c)) {
      t = 10 * t + (c - '0');
    } else {
      code->buffer[i++] = t;
      t = 0;
    }
  }
  return code;
}

#define PrintIntcode(code) FprintIntcode(code, stdout)
void FprintIntcode(Intcode *code, FILE *fp) {
  size_t i;
  fprintf(fp, "%lu", code->buffer[0]);
  for (i = 1; i < code->length; i++) {
    fprintf(fp, ",%lu", code->buffer[i]);
  }
  putc('\n', fp);
}

void ExecuteIntcode(Intcode *code) {
  size_t i;
  for (i = 0; i < code->length; i += 4) {
    switch (code->buffer[i]) {
    case 1:
      code->buffer[code->buffer[i + 3]] =
          code->buffer[code->buffer[i + 1]] + code->buffer[code->buffer[i + 2]];
      break;
    case 2:
      code->buffer[code->buffer[i + 3]] =
          code->buffer[code->buffer[i + 1]] * code->buffer[code->buffer[i + 2]];
      break;
    default:
      printf("Unknown opcode: %lu\n", code->buffer[i]);
    case 99:
      return;
    }
  }
}

int main(int argc, char *argv[]) {
  Intcode *code;
  uint64_t copy;
  if (argc < 3) {
    exit(1);
  }
  code = LoadIntcode(argv[1]);
  if (code == NULL) {
    exit(1);
  }
  code->buffer[1] = atoi(argv[2]);
  code->buffer[2] = atoi(argv[3]);
  ExecuteIntcode(code);
  printf("%lu\n", code->buffer[0]);
}
