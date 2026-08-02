#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct Map {
  int width;
  int height;
  uint8_t data[];
}* Map;

enum GuardDirection {
  UP = '^',
  DOWN = 'v',
  LEFT = '<',
  RIGHT = '>'
};

typedef struct Guard {
  int x;
  int y;
  enum GuardDirection direction;
}* Guard;

typedef struct Input {
  Map map;
  Guard guard;
}* Input;


Guard newGuard(int x, int y, enum GuardDirection dir) {
  Guard guard = malloc(sizeof(struct Guard));
  guard->x = x;
  guard->y = y;
  guard->direction = dir;
  return guard;
}

Guard guardCopy(Guard oldGuard) {
  return newGuard(oldGuard->x, oldGuard->y, oldGuard->direction);
}

void guardPrint(Guard guard) {
  printf("Guard { x = %d, y = %d, direction = '%c' }\n", guard->x, guard->y, guard->direction);
}

static Map newMap(int width, int height) {
  Map m = malloc((sizeof(struct Map)) + width * height);
  m->width = width;
  m->height = height;
  return m;
}

#define mapForEach(map, i) for (int i = 0; i < map->width * map->height; ++i)
static void mapFillValue(Map m, uint8_t val) {
  mapForEach(m, i) {
    m->data[i] = val;
  }
}

#define mapSetValue(m, x, y, val) (m->data[m->width * y + x] = val)

#define mapGetValue(m, x, y) (m->data[m->width * y + x])

static Map mapCopy(Map m) {
  Map result = newMap(m->width, m->height);
  memcpy(result->data, m->data, m->width * m->height);
  return result;
}

static void mapPrint(Map m) {
  for (int y = 0; y < m->height; ++y) {
    for (int x = 0; x < m->width; ++x) {
      putchar(mapGetValue(m, x, y));
    }
    putchar('\n');
  }
}

static Input newInput(Map map, Guard guard) {
  Input input = malloc(sizeof(struct Input));
  input->map = map;
  input->guard = guard;
  return input;
}

static void inputPrint(Input input) {
  for (int y = 0; y < input->map->height; ++y) {
    for (int x = 0; x < input->map->width; ++x) {
      if (y == input->guard->y && x == input->guard->x) {
        putchar(input->guard->direction);
      } else {
        putchar(mapGetValue(input->map, x, y));
      }
    }
    putchar('\n');
  }
}

static Input load(FILE *fp) {
  int width = 0;
  int height = 0;
  bool atStartOfLine = true;
  Map map;
  Guard guard = newGuard(0, 0, UP);
  for (int c = fgetc(fp); c != '\n'; c = fgetc(fp)) {
    if (c != '\r') ++width;
  }
  ++height;
  for (int c = fgetc(fp); c != EOF; c = fgetc(fp)) {
    if (c == '\n') {
      ++height;
      atStartOfLine = true;
    } else {
      atStartOfLine = false;
    }
  }
  if (!atStartOfLine) {
    ++height;
  }
  map = newMap(width, height);
  rewind(fp);
  int y = 0;
  int x = 0;
  for (int c = fgetc(fp); c != EOF; c = fgetc(fp)) {
    switch (c) {
      case '\r':
        break;

      case '\n':
        ++y;
        x = 0;
        break;

      case '#':
        mapSetValue(map, x, y, '#');
        ++x;
        break;

      case '^':
        guard->x = x;
        guard->y = y;
        guard->direction = UP;
      default:
        mapSetValue(map, x, y, '.');
        ++x;
        break;
    }
  }
  return newInput(map, guard);
}

static inline bool pointInBounds(Map map, int x, int y) {
  return x >= 0 && y >= 0 && x < map->width && y < map->height;

}

static inline bool guardInBounds(Map map, Guard guard) {
  return pointInBounds(map, guard->x, guard->y);
}

static bool isBlocked(Map map, Guard guard) {
  int x = guard->x;
  int y = guard->y;
  switch (guard->direction) {
    case UP:    --y; break;
    case RIGHT: ++x; break;
    case DOWN:  ++y; break;
    case LEFT:  --x; break;
  }
  return pointInBounds(map, x, y) && mapGetValue(map, x, y) == '#';
}

static void moveForward(Guard guard) {
  if (guard->direction == UP) {
    guard->y--;
  } else if (guard->direction == LEFT) {
    guard->x--;
  } else if (guard->direction == RIGHT) {
    guard->x++;
  } else if (guard->direction == DOWN) {
    guard->y++;
  }
}

static void turnRight(Guard guard) {
  switch (guard->direction) {
    case UP: guard->direction = RIGHT; break;
    case RIGHT: guard->direction = DOWN; break;
    case DOWN: guard->direction = LEFT; break;
    case LEFT: guard->direction = UP; break;
  }
}

static int partOne(Input input) {
  Map map = input->map;
  Guard guard = guardCopy(input->guard);
  while (guardInBounds(map, guard)) {
    mapSetValue(map, guard->x, guard->y, 'X');
    while (isBlocked(map, guard)) {
      turnRight(guard);
    }
    moveForward(guard);
  }
  int result = 0;
  mapForEach(map, i) {
    if (map->data[i] == 'X') ++result;
  }
  free(guard);
  return result;
}

static bool isDirection(char c) {
  return strchr("<>^v", c) != NULL;
}

static bool hasLoop(Map map, Guard guard) {
  map = mapCopy(map);
  guard = guardCopy(guard);
  bool result = false;
  while (guardInBounds(map, guard)) {
    if (mapGetValue(map, guard->x, guard->y) == guard->direction) {
      result = true;
      break;
    }
    mapSetValue(map, guard->x, guard->y, guard->direction);
    while (isBlocked(map, guard)) {
      turnRight(guard);
    }
    moveForward(guard);
  }
  free(map);
  free(guard);
  return result;
}

static int partTwo(Input input) {
  Map map = input->map;
  Guard guard = input->guard;
  int height = map->height;
  int width = map->width;
  int result = 0;
  for (int y = 0; y < height; ++y) {
    for (int x = 0; x < width; ++x) {
      if ((x != guard->x || y != guard->y) && mapGetValue(map, x, y) == 'X') {
        mapSetValue(map, x, y, '#');
        if (hasLoop(map, guard)) {
          ++result;
        }
        mapSetValue(map, x, y, '.');
      }
    }
  }
  return result;
}

int main(int argc, const char *argv[]) {
  if (argc != 2) {
    fprintf(stderr, "Missing input filename.\n");
    return -1;
  }
  FILE *fp = fopen(argv[1], "r");
  if (fp == NULL) {
    fprintf(stderr, "Could not open '%s'.\n", argv[1]);
    return -1;
  }
  Input input = load(fp);
  fclose(fp);
  int result1 = partOne(input);
  printf("%d\n", result1);
  int result2 = partTwo(input);
  printf("%d\n", result2);
}
