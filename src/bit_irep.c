#include <math.h>
#include <string.h>
#include <assert.h>
#include <mruby/irep.h>
#include <mruby/opcode.h>

static void make_bitmap_irep_aux(mrb_state *mrb, mrb_irep *irep,
				 int inst, int bitpos, int bitoff,
				 struct regbit *bitirep)
{
  int bnum = irep->bitirep.bnum;

  switch (GET_OPCODE(inst)) {
  case OP_PHI:
    bitirep->phi[bitpos] |= (1llu << bitoff);
    break;

  case OP_MOVE:
    bitirep->dst[GETARG_A(inst) * bnum + bitpos] |= (1llu << bitoff);
    bitirep->src[GETARG_B(inst) * bnum + bitpos] |= (1llu << bitoff);
    break;

  case OP_LOADL:
  case OP_LOADI:
  case OP_LOADSYM:
  case OP_LOADNIL:
  case OP_LOADT:
  case OP_LOADF:

  case OP_ARRAY:
  case OP_STRING:
  case OP_HASH:
  case OP_LAMBDA:
  case OP_RANGE:
  case OP_OCLASS:
  case OP_CLASS:
  case OP_MODULE:
  case OP_SCLASS:
  case OP_TCLASS:
    bitirep->dst[GETARG_A(inst) * bnum + bitpos] |= (1llu << bitoff);
    break;

  case OP_GETIV:
  case OP_GETCV:
  case OP_GETCONST:
  case OP_GETMCNST:
    bitirep->dst[GETARG_A(inst) * bnum + bitpos] |= (1llu << bitoff);
    break;

  case OP_SETIV:
  case OP_SETCV:
  case OP_SETCONST:
  case OP_SETMCNST:
    bitirep->src[GETARG_A(inst) * bnum + bitpos] |= (1llu << bitoff);
    break;

  case OP_RETURN:
    bitirep->phi[bitpos] |= (1llu << bitoff);
    bitirep->src[GETARG_A(inst) * bnum + bitpos] |= (1llu << bitoff);
    break;

  case OP_LOADSELF:
    bitirep->dst[GETARG_A(inst) * bnum + bitpos] |= (1llu << bitoff);
    bitirep->src[0 * bnum + bitpos] |= (1llu << bitoff);
    break;

  case OP_ADD:
  case OP_SUB:
  case OP_MUL:
  case OP_DIV:
  case OP_EQ:
  case OP_LT:
  case OP_LE:
  case OP_GT:
  case OP_GE:
    bitirep->dst[GETARG_A(inst) * bnum  + bitpos] |= (1llu << bitoff);
    bitirep->src[GETARG_A(inst) * bnum + bitpos] |= (1llu << bitoff);
    bitirep->src[(GETARG_A(inst) + 1) * bnum + bitpos] |= (1llu << bitoff);
    break;
      
  case OP_ADDI:
  case OP_SUBI:
    bitirep->dst[GETARG_A(inst) * bnum  + bitpos] |= (1llu << bitoff);
    bitirep->src[GETARG_A(inst) * bnum + bitpos] |= (1llu << bitoff);
    break;

  case OP_SENDB:
    bitirep->src[(GETARG_A(inst) + GETARG_C(inst) + 1) * bnum + bitpos] |= (1llu << bitoff);
    // Fall through

  case OP_SEND:
    int argnum = GETARG_C(inst);
    if (argnum == 127) {
      argnum = 1;
    }
	
    bitirep->dst[GETARG_A(inst) * bnum + bitpos] |= (1llu << bitoff);
    if (argnum >= 0) {
      for (int k = 0; k < argnum + 1; k++) {
	bitirep->src[(GETARG_A(inst) + k) * bnum + bitpos] |= (1llu << bitoff);
      }
    }
    break;

  default:
    /*  Do nothing */
    break;

  }
}

int bitmap_ctz(mrb_state *mrb, mrb_irep *irep, bitmap *bit, int start)
{
  int cbit = BITMAP_NUM(start) - 1;
  int coff = BITMAP_POS(start);

  if (irep->bitirep.bnum <= cbit && irep->bitirep.bpos <= coff) {
    return -1;
  }

  if (coff < (sizeof(bitmap) * 8 - 1) && bit[cbit] > (1llu << (coff + 1))) {
    return (__builtin_ctzll(bit[cbit] & ~((1llu << (coff + 1)) - 1)) +
	    cbit * sizeof(bitmap) * 8);
  }
  coff = 0;
  cbit++;
  for (;bit[cbit] == 0; cbit++) {
    /* Do nothing */
  }
  if (cbit < irep->bitirep.bnum) {
    return (__builtin_ctzll(bit[cbit]) +
	    cbit * sizeof(bitmap) * 8);
  }
  else {
    return -1;
  }

}

/* use for PHI */
bitmap *bitmap_mask_with_shift(mrb_state *mrb, mrb_irep *irep, bitmap *dst, bitmap *src, int start)
{
  int bnum = irep->bitirep.bnum;
  start += 1;
  int cbit = BITMAP_NUM(start) - 1;
  int coff = BITMAP_POS(start);
  bitmap mask_lsb = 1llu << coff;

  if (coff == 0 && cbit > 0) {
    memset(dst, 0, (cbit - 1) * sizeof(bitmap));
    dst[cbit - 1] = 1llu << (sizeof(bitmap) * 8 - 1);
  }
  else {
    memset(dst, 0, cbit * sizeof(bitmap));
  }
  
  int ebit = cbit + 1;

  dst[cbit] = src[cbit] -  mask_lsb;
  if (src[cbit] < mask_lsb) {
    for (; ebit < bnum && src[ebit] == 0; ebit++) {
      dst[ebit] = src[ebit] - 1;
    }
    if (ebit < bnum) {
      dst[ebit] = src[ebit] - 1;
      ebit++;
    }
  }

  /* xor for not changed bit to 0 */
  for (int i = cbit; i < ebit; i++) {
    dst[i] ^= src[i];
  }

  /* shift */
  for (int i = cbit; i < ebit - 1; i++) {
    dst[i] >>= 1;
    dst[i] |= ((dst[i + 1] & 1) << (sizeof(bitmap) * 8 - 1));
  }
  dst[ebit - 1] >>= 1;

  memset(dst + ebit, 0, (bnum - ebit) * sizeof(bitmap));

  return dst;
}

/* use for reg lifetime */
bitmap *bitmap_mask_wo_shift(mrb_state *mrb, mrb_irep *irep, bitmap *dst, bitmap *src, int start)
{
  int bnum = irep->bitirep.bnum;
  start += 1;
  int cbit = BITMAP_NUM(start) - 1;
  int coff = BITMAP_POS(start);
  bitmap mask_lsb = 1llu << coff;

  memset(dst, 0, cbit * sizeof(bitmap));

  int ebit = cbit + 1;

  dst[cbit] = src[cbit] - mask_lsb;
  if (src[cbit] < mask_lsb) {
    for (; ebit < bnum && src[ebit] == 0; ebit++) {
      dst[ebit] = src[ebit] - 1;
    }
    if (ebit < bnum) {
      dst[ebit] = src[ebit] - 1;
      ebit++;
    }
  }

  /* xor for not changed bit to 0 */
  for (int i = cbit; i < ebit; i++) {
    dst[i] ^= src[i];
  }

  memset(dst + ebit, 0, (bnum - ebit) * sizeof(bitmap));

  return dst;
}

bitmap *bitmap_and(mrb_state *mrb, mrb_irep *irep, bitmap *dst, bitmap *src0, bitmap *src1)
{
  int bnum = irep->bitirep.bnum;

  for (int i = 0; i < bnum; i++) {
    dst[i] = src0[i] & src1[i];
  }

  return dst;
}

bitmap *bitmap_or(mrb_state *mrb, mrb_irep *irep, bitmap *dst, bitmap *src0, bitmap *src1)
{
  int bnum = irep->bitirep.bnum;

  for (int i = 0; i < bnum; i++) {
    dst[i] = src0[i] | src1[i];
  }

  return dst;
}

bitmap *bitmap_xor(mrb_state *mrb, mrb_irep *irep, bitmap *dst, bitmap *src0, bitmap *src1)
{
  int bnum = irep->bitirep.bnum;

  for (int i = 0; i < bnum; i++) {
    dst[i] = src0[i] ^ src1[i];
  }

  return dst;
}

bitmap *bitmap_set(mrb_state *mrb, mrb_irep *irep, bitmap *dst, bitmap *src)
{
  int bnum = irep->bitirep.bnum;

  for (int i = 0; i < bnum; i++) {
    dst[i] = src[i];
  }

  return dst;
}

bitmap *bitmap_setbit(mrb_state *mrb, mrb_irep *irep, bitmap *dst, int pos)
{
  int cbit = BITMAP_NUM(pos);
  int coff = BITMAP_POS(pos);
  bitmap mask_lsb = 1llu << coff;

  dst[cbit] |= mask_lsb;

  return dst;
}

bitmap *bitmap_resetbit(mrb_state *mrb, mrb_irep *irep, bitmap *dst, int pos)
{
  int cbit = BITMAP_NUM(pos);
  int coff = BITMAP_POS(pos);
  bitmap mask_lsb = 1llu << coff;

  dst[cbit] &= ~mask_lsb;

  return dst;
}

bitmap *bitmap_reversebit(mrb_state *mrb, mrb_irep *irep, bitmap *dst, int pos)
{
  int cbit = BITMAP_NUM(pos);
  int coff = BITMAP_POS(pos);
  bitmap mask_lsb = 1llu << coff;

  dst[cbit] ^= mask_lsb;

  return dst;
}

int bitmap_cmp(mrb_state *mrb, mrb_irep *irep, bitmap *dst, bitmap *src)
{
  int bnum = irep->bitirep.bnum;
  int i = 0;

  for (i = bnum; i >= 0 && dst[i] == src[i]; i--);

  if (i < 0) {
    return 0;
  }
  else {
    return 1 - (dst[i] > src[i]) * 2;
  }
}

void compute_reg_bitmap_aux(mrb_state *mrb, mrb_irep *irep, int pos)
{
  const int bnum = irep->bitirep.bnum;
  bitmap *block_map = __builtin_alloca(bnum * sizeof(bitmap));
  bitmap *src_map = __builtin_alloca(bnum * sizeof(bitmap));
  bitmap *dst_map = __builtin_alloca(bnum * sizeof(bitmap));
  bitmap *tmpbit = __builtin_alloca(bnum * sizeof(bitmap));

  const int isize = irep->ilen;
  const int rnum = irep->nregs;
  const int nphiposr = bitmap_ctz(mrb, irep, irep->bitirep.reverse.phi, irep->ilen - pos - 1);

  const int cbitr = BITMAP_NUM(nphiposr);
  const int coffr = BITMAP_POS(nphiposr);
  const bitmap phi_bitmapr = 1llu << coffr;

  const int nphipos = nphiposr - pos - 1;
  const int cbit = BITMAP_NUM(nphiposr);
  const int coff = BITMAP_POS(nphiposr);
  const bitmap phi_bitmap = 1llu << coffr;

  const int rpos = irep->ilen - pos - 1;
  if (rpos < 0) printf("foo");
  bitmap_mask_wo_shift(mrb, irep, block_map, irep->bitirep.reverse.phi, rpos);
  for (int i = 0; i <rnum; i++) {
    bitmap_and(mrb, irep, dst_map, irep->bitirep.reverse.dst + i * bnum, block_map);
    bitmap_and(mrb, irep, src_map, irep->bitirep.reverse.src + i * bnum, block_map);
    if (bitmap_cmp(mrb, irep, dst_map, src_map) < 0 &&
	bitmap_cmp(mrb, irep, 
		   bitmap_xor(mrb, irep, tmpbit, dst_map, src_map),
		   src_map) < 0) {
      irep->bitirep.reverse.dst[cbitr + i * bnum]  |= phi_bitmapr;
      irep->bitirep.normal.dst[cbit + i * bnum]  |= phi_bitmap;
    }
  }
}

void compute_reg_bitmap_inter_block(mrb_state *mrb, mrb_irep *irep)
{
  int bnum = irep->bitirep.bnum;
  int bpos = irep->bitirep.bnum;

  bitmap *traverse_bit = __builtin_alloca(bnum * sizeof(bitmap));
  for (int cpc = 0; cpc >= 0;
       cpc = bitmap_ctz(mrb, irep, irep->bitirep.normal.phi, cpc)) {
    if (GET_OPCODE(irep->iseq[cpc]) == OP_RETURN) {
      /* RETURN instruction is start of data flow */
      compute_reg_bitmap_aux(mrb, irep, cpc);
    }
  }
}

int mrb_make_bitmap_irep(mrb_state *mrb, mrb_irep *irep)
{
  int isize = irep->ilen;
  int bnum = BITMAP_NUM(isize);
  int bpos = BITMAP_POS(isize);
  int rnum = irep->nregs;
  int bitpos;
  int bitoff;

  /* Allocate bitmap */
  irep->bitirep.bnum = bnum;
  irep->bitirep.bpos = bpos;

  irep->bitirep.normal.phi = mrb_calloc(mrb, 1, bnum * sizeof(bitmap));
  irep->bitirep.normal.src = mrb_calloc(mrb, rnum, bnum * sizeof(bitmap));
  irep->bitirep.normal.dst = mrb_calloc(mrb, rnum, bnum * sizeof(bitmap));

  irep->bitirep.reverse.phi = mrb_calloc(mrb, 1, bnum * sizeof(bitmap));
  irep->bitirep.reverse.src = mrb_calloc(mrb, rnum, bnum * sizeof(bitmap));
  irep->bitirep.reverse.dst = mrb_calloc(mrb, rnum, bnum * sizeof(bitmap));

  /* Scan iseq */
  bitpos = 0;
  bitoff = 0;
  for (int i = 0; i < isize; i++) {
    make_bitmap_irep_aux(mrb, irep, irep->iseq[i], 
			 bitpos, bitoff, &irep->bitirep.normal);
    make_bitmap_irep_aux(mrb, irep, irep->iseq[isize - i - 1], 
			 bitpos, bitoff, &irep->bitirep.reverse);

    bitoff++;
    if (bitoff == BITMAP_SIZE) {
      bitoff = 0;
      bitpos++;
    }
  }

  compute_reg_bitmap_inter_block(mrb, irep);
}

bitmap res[10];
bitmap *test_bitmap_mask_with_shift(mrb_state *mrb, mrb_irep *irep, bitmap *bit, int start)
{
  return bitmap_mask_with_shift(mrb, irep, res, bit, start);
}

bitmap *test_bitmap_mask_wo_shift(mrb_state *mrb, mrb_irep *irep, bitmap *bit, int start)
{
  return bitmap_mask_wo_shift(mrb, irep, res, bit, start);
}

