#include <mruby.h>

typedef uint64_t bitmap;

struct regbit {
  bitmap *phi;
  bitmap *src;
  bitmap *dst;
};

typedef struct BitIrep {
  struct regbit normal;
  struct regbit reverse;

  int bnum;
  int bpos;
} mrb_bit_irep;

typedef struct RegResult {
  bitmap src[256];
  bitmap dst[256];
} mrb_reg_result;

typedef struct RegInfo {

} mrb_reginfo;

typedef uint64_t Bloom[4]; 

#define BITMAP_SIZE (sizeof(bitmap) * 8)
#define BITMAP_NUM(n) (((n) / BITMAP_SIZE) + 1)
#define BITMAP_POS(n) ((n) & (BITMAP_SIZE - 1))


