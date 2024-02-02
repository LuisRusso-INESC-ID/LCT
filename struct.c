#ifndef _STRUCT_C
#define _STRUCT_C

#include "LCT.h"

struct LCT {
  nodeT above;
  nodeT below;
  nodeT father; /* Inside splay tree or across if at the root of splay tree */
  int size; /* Negative values indicate that above/below order is swapped */
#ifdef _VERSION_W
  costT w;
  costT d; /* delta for w in sub-tree */
  costT min; /* sub-tree minimum */
#endif /* _VERSION_W */
};

#endif /* _STRUCT_C */
