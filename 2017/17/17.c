#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef PARTONE
#define PARTONE 0
#endif 

typedef struct Position {
  struct Position *next;
  unsigned long value;
} Position;

static Position *Print(Position *p, Position *t) {
  Position *r;
  r = p;
  do {
    printf((t == r ? "\e[1m(%lu)\e[0m\t" : "%lu\t"), r->value);
    r = r->next;
  } while (r != p);
  putc('\n', stdout);
}

static Position *Right(Position *p, unsigned long n) {
  while (n > 0) {
    p = p->next;
    --n;
  }
  return p;
}

// Add a position to the right of the current position.
// Returns the new position.
static Position *Add(Position *p, unsigned long value) {
  Position *r;
  r = malloc(sizeof(Position));
  r->next = p->next;
  r->value = value;
  p->next = r;
  return r;
}

static Position *Find(Position *p, unsigned long value) {
  Position *r;
  r = p;
  do {
    if (value == r->value) {
      return r;
    }
  } while ((r = r->next) != p);
  return NULL;
}

int main(int argc, char *argv[]) {
  unsigned long i, limit, steps, target;
  Position *p;
  p = malloc(sizeof(Position));
  p->next = p;
  p->value = 0;
  limit = atoi(argv[1]);
  steps = atoi(argv[2]);
  target = argc == 4 ? atoi(argv[3]) : limit;
  for (i = 1; i <= limit; ++i) {
    p = Add(Right(p, steps), i);
  }
  printf("%lu\n", Find(p, target)->next->value);
}
