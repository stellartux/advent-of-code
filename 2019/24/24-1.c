#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef uint_fast32_t Eris;

typedef struct ErisArray {
  size_t length;
  size_t bufsize;
  Eris buffer[];
} ErisArray;

static ErisArray *CreateErisArray(size_t bufsize) {
  ErisArray *a;
  if (!bufsize) {
    return NULL;
  }
  a = malloc(sizeof(ErisArray) + bufsize * sizeof(Eris));
  if (!a) {
    return NULL;
  }
  a->length = 0;
  a->bufsize = bufsize;
  return a;
}

static void PushErisArray(ErisArray *a, Eris value) {
  if (a->length + 1 == a->bufsize) {
    a->bufsize *= 2;
    realloc(a, sizeof(ErisArray) + a->bufsize * sizeof(Eris));
  }
  a->buffer[a->length] = value;
  a->length++;
}

#define PrintEris(eris) FprintEris(stdout, eris)
static void FprintEris(FILE *fp, Eris eris) {
  size_t i = 0;
  for (i = 0; i < 25; i++) {
    putc(eris & (1 << i) ? '#' : '.', fp);
    if (i % 5 == 4) {
      putc('\n', fp);
    }
  }
}

static Eris LoadEris(const char *filename) {
  FILE *fp;
  Eris c, x, y, z;
  fp = fopen(filename, "r");
  if (!fp) {
    printf("Could not open \"%s\"", filename);
  }
  x = 0;
  y = 0;
  z = 0;
  while ((c = getc(fp)) != EOF) {
    switch (c) {
    case '#':
      x = 1;
    case '.':
      z |= x << y++;
      x = 0;
    }
  }
  fclose(fp);
  if (y != 25) {
    return -1;
  }
  return z;
}

#define ErisIndex(E, I) (1 & ((E) >> (I)))

static Eris NextEris(Eris eris) {
  Eris result, i, p, t;
  result = 0;
  for (i = 0; i < 25; i++) {
    p = ErisIndex(eris, i);
    t = 0;
    if (i >= 5) {
      t += ErisIndex(eris, i - 5);
    }
    if (i % 5 != 0) {
      t += ErisIndex(eris, i - 1);
    }
    if (i % 5 != 4) {
      t += ErisIndex(eris, i + 1);
    }
    if (i < 20) {
      t += ErisIndex(eris, i + 5);
    }
    if (t == 1 || (p == 0 && t == 2)) {
      result |= 1 << i;
    }
  }
  return result;
}

#define PushNextEris(A) (PushErisArray(A, NextEris(A->buffer[A->length - 1])))

static ssize_t ErisArrayFindIndex(const ErisArray *haystack, Eris needle) {
  ssize_t i;
  for (i = 0; i < haystack->length; i++) {
    if (haystack->buffer[i] == needle) {
      return i;
    }
  }
  return -1;
}

#define PrintErisArray(A) FprintErisArray(stdout, A)
static void FprintErisArray(FILE *fp, const ErisArray *a) {
  size_t i;
  if (a->length > 0) {
    fprintf(fp, "Initial state:\n");
    FprintEris(fp, a->buffer[0]);
  }
  if (a->length > 1) {
    fprintf(fp, "\nAfter 1 minute:\n");
    FprintEris(fp, a->buffer[1]);
  }
  for (i = 2; i < a->length; i++) {
    fprintf(fp, "\nAfter %u minutes:\n", i);
    FprintEris(fp, a->buffer[i]);
  }
}

int main(int argc, char *const argv[]) {
  ErisArray *a;
  Eris e;
  int example, opt, verbose;
  example = false;
  verbose = false;
  if (argc < 2) {
    exit(EXIT_FAILURE);
  }
  while ((opt = getopt(argc, argv, ":hvx")) != -1) {
    switch (opt) {
    case 'v':
      verbose = true;
      break;
    case 'x':
      example = true;
      break;
    case 'h':
      printf("Usage:\n\t%s [options] filename\n"
             "Options:\n"
             "\t-h\thelp\t\tshow this message\n"
             "\t-v\tverbose\t\tshow step by step info\n"
             "\n",
             argv[0]);
      exit(EXIT_SUCCESS);
    case '?':
      fprintf(stderr, "Unknown option: '%c'\n", optopt);
      exit(EXIT_FAILURE);
    }
  }
  a = CreateErisArray(64);
  if ((e = LoadEris(argv[argc - 1])) == -1) {
    fprintf(stderr, "Couldn't open file: \"%s\"", argv[1]);
    exit(EXIT_FAILURE);
  }
  PushErisArray(a, e);
  if (example) {
    PushNextEris(a);
    PushNextEris(a);
    PushNextEris(a);
    PushNextEris(a);
    if (verbose) {
      PrintErisArray(a);
      putchar('\n');
    }
  } else {
    e = NextEris(a->buffer[a->length - 1]);
    while (ErisArrayFindIndex(a, e) == -1) {
      PushErisArray(a, e);
      e = NextEris(a->buffer[a->length - 1]);
    }
    if (verbose) {
      PushErisArray(a, e);
      PrintErisArray(a);
    }
    printf("%lu\n", e);
  }
}
