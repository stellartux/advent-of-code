#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "/opt/cosmo/libc/x/x.h"
#include "/opt/cosmo/third_party/linenoise/linenoise.h"

#define MICROS_PER_FRAME 200000

#define GetTrack(WORLD, X, Y) (WORLD->data[Y * (WORLD->width + 1) + X])
#define Turn(FACING, TURN) (FACING = (FACING + TURN) & 3)

enum Direction { UP, RIGHT, DOWN, LEFT };

typedef struct Cart {
  unsigned x;
  unsigned y;
  enum Direction facing;
  enum Direction nextTurn;
} Cart;

static char GetFacing(enum Direction dir) {
  switch (dir) {
  case UP:
    return '^';
  case RIGHT:
    return '>';
  case LEFT:
    return '<';
  case DOWN:
    return 'v';
  default:
    return ' ';
  }
}

static void DumpCart(Cart cart) {
  printf("  {\n"
         "    \"x\": %u,\n"
         "    \"y\": %u,\n"
         "    \"facing\": %u,\n"
         "    \"nextTurn\": %u\n"
         "  }",
         cart.x, cart.y, cart.facing, cart.nextTurn);
}

static void PrintCart(Cart cart) {
  printf("\e[%u;%uH\e[36m%c\e[39m\n", cart.y + 1, cart.x + 1,
         GetFacing(cart.facing));
}

static int CompareCarts(Cart left, Cart right) {
  return left.y == right.y ? left.x - right.x : left.y - right.y;
}

typedef struct World {
  unsigned width;
  unsigned height;
  char *data;
  size_t cart_count;
  Cart carts[];
} World;

static size_t CountCarts(const char *data) {
  size_t count = 0;
  while ((data = strpbrk(data, "^>v<"))) {
    ++count;
    ++data;
  }
  return count;
}

static void DumpCarts(World *world) {
  printf("\e[%u;%uH[", world->width, world->height);
  for (size_t i = 0; i < world->cart_count; i++) {
    if (i) {
      putc(',', stdout);
    }
    putc('\n', stdout);
    DumpCart(world->carts[i]);
  }
  puts("\n]\n");
}

static enum Direction GetNextDirection(enum Direction dir) {
  switch (dir) {
  case LEFT:
    return UP;
  case UP:
    return RIGHT;
  case RIGHT:
  case DOWN:
  default:
    return LEFT;
  }
}

static struct World *CreateWorld(char *data) {
  size_t count;
  World *world;
  char *c;
  count = CountCarts(data);
  world = (World *)malloc(sizeof(World) + sizeof(Cart) * count);
  world->width = (strchr(data, '\n') - data);
  world->height = strlen(data) / world->width;
  world->data = data;
  world->cart_count = count;
  for (count = 0, c = data; (c = strpbrk(c, "^>v<")); ++count) {
    world->carts[count].x = (c - data) % (world->width + 1);
    world->carts[count].y = (c - data) / (world->width + 1);
    world->carts[count].nextTurn = LEFT;
    switch (c[0]) {
    case '^':
      world->carts[count].facing = UP;
      break;
    case 'v':
      world->carts[count].facing = DOWN;
      break;
    case '<':
      world->carts[count].facing = LEFT;
      break;
    case '>':
      world->carts[count].facing = RIGHT;
      break;
    default:
      break;
    }
    if (c[0] == '^' || c[0] == 'v') {
      c[0] = '|';
    } else {
      c[0] = '-';
    }
  }
  return world;
}

static void PrintCarts(World *world) {
  for (size_t i = 0; i < world->cart_count; ++i) {
    PrintCart(world->carts[i]);
  }
  puts("\n");
}

static void PrintCollision(Cart *c) {
  printf("\e[%u;%uH\e[31mX\e[39m\n", c->y + 1, c->x + 1);
}

static void PrintWorld(World *world) { printf("\e[1;1H\e[J%s\n", world->data); }

static Cart *CheckCollisions(World *world) {
  for (size_t i = 1; i < world->cart_count; ++i) {
    if (CompareCarts(world->carts[i - 1], world->carts[i]) == 0) {
      return &(world->carts[i]);
    }
  }
  return NULL;
}

static Cart *SortWorld(World *world) {
  Cart left, right;
  int cmp;
  for (size_t i = 1; i < world->cart_count; ++i) {
    left = world->carts[i - 1];
    right = world->carts[i];
    if ((cmp = CompareCarts(left, right)) > 0) {
      world->carts[i - 1] = right;
      world->carts[i] = left;
      if (i > 2) {
        i -= 2;
      } else {
        i = 0;
      }
    } else if (cmp == 0) {
      return &(world->carts[i]);
    }
  }
}

static void StepCart(World *world, size_t i) {
  switch (GetTrack(world, world->carts[i].x, world->carts[i].y)) {
  case '+':
    Turn(world->carts[i].facing, world->carts[i].nextTurn);
    world->carts[i].nextTurn = GetNextDirection(world->carts[i].nextTurn);
    break;
  case '/':
    if (world->carts[i].facing == RIGHT || world->carts[i].facing == LEFT) {
      Turn(world->carts[i].facing, LEFT);
    } else {
      Turn(world->carts[i].facing, RIGHT);
    }
    break;
  case '\\':
    if (world->carts[i].facing == RIGHT || world->carts[i].facing == LEFT) {
      Turn(world->carts[i].facing, RIGHT);
    } else {
      Turn(world->carts[i].facing, LEFT);
    }
    break;
  case '-':
  case '|':
  default:
    break;
  }
  switch (world->carts[i].facing) {
  case UP:
    world->carts[i].y--;
    break;
  case DOWN:
    world->carts[i].y++;
    break;
  case LEFT:
    world->carts[i].x--;
    break;
  case RIGHT:
    world->carts[i].x++;
    break;
  }
}

static Cart *StepWorld(World *world) {
  Cart *c;
  for (size_t i = 0; i < world->cart_count; i++) {
    StepCart(world, i);
    if ((c = CheckCollisions(world))) {
      return c;
    }
  }
  SortWorld(world);
  return NULL;
}

int main(int argc, char *argv[]) {
  char *file, *path;
  World *world;
  Cart *cart;
  path = argc == 1 ? "example.txt" : argv[1];
  file = (char *)xslurp(path, NULL);
  if (file == NULL) {
    printf("Couldn't open file: \"%s\"\n", path);
    exit(1);
  }
  world = CreateWorld(file);
  // PrintWorld(world);
  // PrintCarts(world);
  while (1 || !usleep(MICROS_PER_FRAME)) {
    // PrintWorld(world);
    if ((cart = StepWorld(world))) {
      // PrintCollision(cart);
      break;
    }
    // PrintCarts(world);
  }

  printf(
      "In this example, the location of the first crash is \e[1m%u,%u\e[0m.\n",
      cart->x, cart->y);
}
