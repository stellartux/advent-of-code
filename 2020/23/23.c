#ifndef __COSMOPOLITAN__
#include <err.h>
#include <errno.h>
#include <getopt.h>
#include <stdio.h>
#endif

struct Cups {
  struct Cups *prev, *next;
  int value;
};

struct Cups *Cups_create(int value) {
  struct Cups *root, *current, *prev;
  if (!value) {
    return NULL;
  }
  root = malloc(sizeof(struct Cups));
  root->value = value % 10;
  current = root;
  while (value /= 10) {
    prev = malloc(sizeof(struct Cups));
    current->prev = prev;
    prev->next = current;
    current = prev;
    current->value = value % 10;
  }
  current->prev = root;
  root->next = current;
  return current;
}

struct Cups *Cups_find(struct Cups *c, int value) {
  if (c->value == value) {
    return c;
  }
  for (struct Cups *n = c->next; n && n != c; n = n->next) {
    if (n->value == value) {
      return n;
    }
  }
  return NULL;
}

struct Cups *Cups_move(struct Cups *c) {
  struct Cups *d, *t1, *t2, *t3;
  int dval = c->value - 1;
  if (!dval) {
    dval = 9;
  }
  t1 = c->next;
  t2 = t1->next;
  t3 = t2->next;
  c->next = t3->next;
  c->next->prev = c->next;
  while (t1->value == dval || t2->value == dval || t3->value == dval) {
    if (!--dval) {
      dval = 9;
    }
  }
  d = Cups_find(c, dval);
  t3->next = d->next;
  d->next->prev = t3;
  d->next = t1;
  t1->prev = d;
  return c->next;
}

void Cups_print(struct Cups *c) {
  c = Cups_find(c, 1);
  for (struct Cups *n = c->next; n != c; n = n->next) {
    printf("%i", n->value);
  }
  putchar('\n');
}

void Cups_show(struct Cups *c) {
  printf("cups: (%i)", c->value);
  for (struct Cups *n = c->next; n != c; n = n->next) {
    printf(" %i ", n->value);
  }
  putchar('\n');
}

int main(int argc, char *const *argv) {
  struct Cups *c;
  int ch, moves, verbose;
  moves = 100;
  verbose = 0;
  while ((ch = getopt(argc, argv, "hm:v")) > -1) {
    switch (ch) {
    case 'm':
      moves = atoi(optarg);
      break;
    case 'v':
      verbose = 1;
      break;
    default:
    case 'h':
      printf("Usage:\t%s [options] arguments\n\n"
             "Options:\n"
             "\th\thelp\tshow this message\n"
             "\t\tdescription\n",
             argv[0]);
      exit(0);
    case '?':
      err(EINVAL, "Unrecognised option: '%c'\n", optopt);
    }
  }
  argc -= optind;
  argv += optind;
  if (!(c = Cups_create(atoi(argv[0])))) {
    exit(-1);
  }
  while (--moves >= 0) {
    if (verbose) {
      Cups_show(c);
    }
    c = Cups_move(c);
  }
  if (verbose) {
    Cups_show(c);
  }
  Cups_print(c);
}
