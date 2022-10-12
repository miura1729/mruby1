#include <math.h>
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

}
