#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct Marble {
  struct Marble *prev, *next;
  unsigned value;
} Marble;

static Marble *Print(Marble *m, Marble *t) {
  Marble *r;
  r = m;
  do {
    printf((t == r ? "\e[1m(%d)\e[0m\t" : "%d\t"), r->value);
    r = r->next;
  } while (r != m);
  putc('\n', stdout);
}
static Marble *PrintR(Marble *m, Marble *t) {
  Marble *r;
  r = m;
  do {
    printf((t == r ? "\e[1m(%d)\e[0m\t" : "%d\t"), r->value);
    r = r->prev;
  } while (r != m);
  putc('\n', stdout);
}

// Get the marble n marbles to the left.
static Marble *Left(Marble *m, unsigned n) {
  while (n > 0) {
    m = m->prev;
    --n;
  }
  return m;
}

// Get the marble n marbles to the right.
static Marble *Right(Marble *m, unsigned n) {
  while (n > 0) {
    m = m->next;
    --n;
  }
  return m;
}

// Add a marble to the right of the current marble.
// Returns the new marble.
static Marble *Add(Marble *m, unsigned value) {
  Marble *n;
  n = malloc(sizeof(Marble));
  n->prev = m;
  n->next = m->next;
  n->value = value;
  m->next->prev = n;
  m->next = n;
  if (m == m->prev) {
    m->prev = n;
  }
  return n;
}

// Removes the current marble.
static Marble *Remove(Marble *m) {
  Marble *n;
  n = m->next;
  n->prev = m->prev;
  n->prev->next = n;
  free(m);
  return n;
}

int main(int argc, char *argv[]) {
  unsigned *scores, i, limit, players, result;
  Marble *m, *r;
  assert(argc < 3);
  scores = calloc(sizeof(unsigned), atoi(argv[1]));
  r = m = malloc(sizeof(Marble));
  m->prev = m;
  m->next = m;
  m->value = 0;
  players = atoi(argv[1]);
  limit = atoi(argv[2]);
  // printf("[-]\t\e[1m(0)\e[0m\n");
  for (i = 1; i <= limit; ++i) {
    if (i % 23) {
      m = Right(m, 1);
      m = Add(m, i);
    } else {
      m = Left(m, 7);
      scores[(i - 1) % players] += i + m->value;
      m = Remove(m);
    }
    // printf("[%d]\t", (i - 1) % players + 1);
    // Print(r, m);
    // if (i > 20)
    //   getchar();
  }
  result = 0;
  for (i = 0; i < players; i++) {
    if (scores[i] > result) {
      result = scores[i];
    }
  }
  printf("%d\n", result);
}
