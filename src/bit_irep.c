#include <math.h>
#include <mruby/irep.h>
#include <mruby/opcode.h>

int mrb_make_bitmap_irep(mrb_state *mrb, mrb_irep *irep)
{
  int isize = irep->ilen;
  int bnum = BITMAP_NUM(isize);
  int bpos = BITMAP_POS(isize);
  int rnum = irep->nregs;
  bitmap *bit;
  int bitpos;
  int bitoff;

  /* Allocate bitmap */
  irep->bitirep.bnum = bnum;
  irep->bitirep.bpos = bpos;
  irep->bitirep.phi = mrb_calloc(mrb, 1, bnum * sizeof(bitmap));
  irep->bitirep.src = mrb_calloc(mrb, rnum, bnum * sizeof(bitmap));
  irep->bitirep.dst = mrb_calloc(mrb, rnum, bnum * sizeof(bitmap));

  /* Scan iseq */
  bitpos = 0;
  bitoff = 0;
  for (int i = 0; i < isize; i++) {
    mrb_code inst = irep->iseq[i];
    switch (GET_OPCODE(inst)) {
    case OP_PHI:
      irep->bitirep.phi[bitpos] |= (1 << bitoff);
      break;

    case OP_MOVE:
      irep->bitirep.dst[GETARG_A(inst) * bnum + bitpos] |= (1 << bitoff);
      irep->bitirep.src[GETARG_B(inst) * bnum + bitpos] |= (1 << bitoff);
      break;

    case OP_LOADL:
    case OP_LOADI:
    case OP_LOADSYM:
    case OP_LOADNIL:
    case OP_LOADSELF:
    case OP_LOADT:
    case OP_LOADF:
      irep->bitirep.dst[GETARG_A(inst) * bnum + bitpos] |= (1 << bitoff);
      break;

    case OP_ADD:
    case OP_SUB:
    case OP_MUL:
    case OP_DIV:
      irep->bitirep.dst[GETARG_A(inst) * bnum  + bitpos] |= (1 << bitoff);
      irep->bitirep.src[GETARG_A(inst) * bnum + bitpos] |= (1 << bitoff);
      irep->bitirep.src[GETARG_A(inst) * bnum + bitpos] |= (1 << bitoff);
      break;
      
    case OP_SEND:
      int argnum = GETARG_C(inst);
      if (argnum == 127) {
	argnum = 1;
      }
	
      irep->bitirep.dst[GETARG_A(inst) * bnum + bitpos] |= (1 << bitoff);
      if (argnum >= 0) {
	for (int k = 0; k < argnum + 1; k++) {
	  irep->bitirep.src[(GETARG_A(inst) + k) * bnum + bitpos] |= (1 << bitoff);
	}
      }
      break;

    default:
      /*  Do nothing */
      break;

    }

    bitoff++;
    if (bitoff == BITMAP_SIZE) {
      bitoff = 0;
      bitpos++;
    }
  }

}
