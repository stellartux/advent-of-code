#include <stdio.h>
#include <stdlib.h>

enum Weapon { Dagger, Shortsword, Warhammer, Longsword, Greataxe };
const char *WeaponName[] = {"Dagger", "Shortsword", "Warhammer", "Longsword",
                            "Greataxe"};
static int WeaponCost(enum Weapon weapon) {
  switch (weapon) {
  case Dagger:
    return 8;
  case Shortsword:
    return 10;
  case Warhammer:
    return 25;
  case Longsword:
    return 40;
  case Greataxe:
    return 74;
  default:
    return 0;
  }
}
static int WeaponDamage(enum Weapon weapon) { return weapon + 4; }

enum Armor { NoArmor, Leather, Chainmail, Splintmail, Bandedmail, Platemail };
const char *ArmorName[] = {"None",       "Leather",    "Chainmail",
                           "Splintmail", "Bandedmail", "Platemail"};
static int ArmorCost(enum Armor a) {
  switch (a) {
  case Leather:
    return 13;
  case Chainmail:
    return 31;
  case Splintmail:
    return 53;
  case Bandedmail:
    return 75;
  case Platemail:
    return 102;
  case NoArmor:
  default:
    return 0;
  }
}
static int ArmorArmor(enum Armor a) { return a; }

enum Ring { NoRing, Damage1, Damage2, Damage3, Defense1, Defense2, Defense3 };
const char *RingName[] = {"None",       "Damage +1",  "Damage +2", "Damage +3",
                          "Defense +1", "Defense +2", "Defense +3"};
static int RingCost(enum Ring r) {
  switch (r) {
  case Damage1:
    return 25;
  case Damage2:
    return 50;
  case Damage3:
    return 100;
  case Defense1:
    return 20;
  case Defense2:
    return 40;
  case Defense3:
    return 80;
  default:
    return 0;
  }
}
static int RingDamage(enum Ring r) { return r < 4 ? r : 0; }
static int RingArmor(enum Ring r) { return r >= 4 ? r - 3 : 0; }

struct Equipment {
  enum Weapon weapon;
  enum Armor armor;
  enum Ring ring1;
  enum Ring ring2;
} equipment[1110];

static int EquipmentDamage(const struct Equipment *e) {
  return WeaponDamage(e->weapon) + RingDamage(e->ring1) + RingDamage(e->ring2);
}

static int EquipmentArmor(const struct Equipment *e) {
  return ArmorArmor(e->armor) + RingArmor(e->ring1) + RingArmor(e->ring2);
}

static int EquipmentCost(const struct Equipment *e) {
  return WeaponCost(e->weapon) + ArmorCost(e->armor) + RingCost(e->ring1) +
         RingCost(e->ring2);
}

static void PrintEquipment(const struct Equipment *e) {
  printf("{\n    \"weapon\": \"%s\",\n    \"armor\": \"%s\",\n    \"ring1\": "
         "\"%s\",\n    \"ring2\": \"%s\",\n    \"damageLevel\": %u,\n    "
         "\"armorLevel\": %u,\n    \"cost\": %u\n}\n",
         WeaponName[e->weapon], ArmorName[e->armor], RingName[e->ring1],
         RingName[e->ring2], EquipmentDamage(e), EquipmentArmor(e),
         EquipmentCost(e));
}

struct Monster {
  int hitpoints;
  int damage;
  int armor;
};

struct Player {
  int hitpoints;
  struct Equipment *equipment;
};

// true if player wins
static bool Fight(struct Player *p, struct Monster *m, bool verbose) {
  int d, a, h;
  while (true) {
    d = EquipmentDamage(p->equipment);
    a = m->armor;
    h = a >= d ? 1 : d - a;
    if (h >= m->hitpoints) {
      m->hitpoints = 0;
    } else {
      m->hitpoints -= h;
    }
    if (verbose) {
      printf("\e[2mThe player deals \e[0m%d-%d = %d\e[2m damage; the boss goes "
             "down to \e[0m%d\e[2m hit points.\e[0m\n",
             d, a, h, m->hitpoints);
    }
    if (!m->hitpoints) {
      if (verbose) {
        printf("\nIn this scenario, the player wins!\n");
      }
      return true;
    }
    d = m->damage;
    a = EquipmentArmor(p->equipment);
    h = a >= d ? 1 : d - a;
    if (h >= p->hitpoints) {
      p->hitpoints = 0;
    } else {
      p->hitpoints -= h;
    }
    if (verbose) {
      printf("\e[2mThe boss deals \e[0m%d-%d = %d\e[2m damage; the player goes "
             "down to \e[0m%d\e[2m hit points.\e[0m\n",
             d, a, h, p->hitpoints);
    }
    if (!p->hitpoints) {
      if (verbose) {
        printf("\nIn this scenario, the player loses...\n");
      }
      return false;
    }
  }
}

static void LoadEquipment() {
  int w, a, r1, r2, i;
  i = 0;
  for (w = 0; w < 5; w++) {
    for (a = 0; a < 6; a++) {
      for (r1 = 0; r1 < 7; r1++) {
        for (r2 = 0; r2 < 7; r2++) {
          if (r1 ? r1 == r2 : r2) {
            continue;
          }
          equipment[i++] = (struct Equipment){w, a, r1, r2};
        }
      }
    }
  }
}

static int CompareEquipment(const void *left, const void *right) {
  return EquipmentCost(left) - EquipmentCost(right);
}

int main(int argc, char *argv[]) {
  int i;
  struct Player p;
  struct Monster m;
  if (argc < 2) {
    exit(1);
  }
  LoadEquipment();
  qsort(equipment, 1110, sizeof(struct Equipment), CompareEquipment);
  if (!strcmp("example", argv[1])) {
    /*
      For example, suppose you have 8 hit points, 5 damage, and 5 armor, and
      that the boss has 12 hit points, 7 damage, and 2 armor:
    */
    p = (struct Player){8, &equipment[147]};
    m = (struct Monster){12, 7, 2};
    Fight(&p, &m, true);
  } else if (!strcmp("equipment", argv[1])) {
    putchar('[');
    for (int i = 0; i < 1109; i++) {
      PrintEquipment(&equipment[i]);
      putchar(',');
    }
    PrintEquipment(&equipment[1109]);
    putchar(']');
  } else if (!strcmp("partone", argv[1])) {
    // Find the cheapest winning equipment set
    for (i = 0; i < 1110; i++) {
      p = (struct Player){100, &equipment[i]};
      m = (struct Monster){100, 8, 2};
      if (Fight(&p, &m, false)) {
        p = (struct Player){100, &equipment[i]};
        m = (struct Monster){100, 8, 2};
        Fight(&p, &m, true);
        PrintEquipment(p.equipment);
        break;
      }
    }
  } else if (!strcmp("parttwo", argv[1])) {
    // Find the dearest losing equipment set
    for (i = 1109; i >= 0; i--) {
      p = (struct Player){100, &equipment[i]};
      m = (struct Monster){100, 8, 2};
      if (!Fight(&p, &m, false)) {
        p = (struct Player){100, &equipment[i]};
        m = (struct Monster){100, 8, 2};
        Fight(&p, &m, true);
        PrintEquipment(p.equipment);
        break;
      }
    }
  } else {
    exit(1);
  }
}
