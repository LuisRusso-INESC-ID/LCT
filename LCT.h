#ifndef LINKCUTTREE_H
#define LINKCUTTREE_H

/* ------------------------------------------------------------ */
/* Configuration */

/* Comment for non-weighted version */
/* #define _VERSION_W */
typedef double costT; /* Type for edge costs. */
#include <float.h>
#define _COST_MAX DBL_MAX /* Maximum value */

/* Do not edit below line */
/* ------------------------------------------------------------ */

#include <stddef.h> /* For size_t */
#include <stdbool.h> /* For bool */

typedef unsigned int nodeT; /* Nodes are numbered 1 to n. */
typedef /*@abstract@*/ struct LCT *LCT;

void initLCT(LCT f, unsigned int n);
size_t sizeofStruct(void);
LCT allocLCT(unsigned int n); /* Destroy with free. */

unsigned int Access(LCT f, nodeT v); /* Return depth of v. */
/* d >= 0 move downwards, d=0 is root. d < 0 move upwards, d=-1 is father. */
nodeT getNode(LCT f, nodeT v, int d);
bool edgeQ(LCT f, nodeT u, nodeT v); /* Is {u,v} an edge in some tree? */
nodeT cut(LCT f, nodeT u, nodeT v); /* Remove edge {u,v}. Return new root. */
void reRoot(LCT f, nodeT v); /* v becomes root. */
nodeT LCA(LCT f, nodeT u, nodeT v); /* Return 0 if not in the same tree. */

#ifndef _VERSION_W
void Link(LCT f, nodeT u, nodeT v); /* Tree containing u looses root. */

#else /* _VERSION_W */
void LinkW(LCT f, nodeT u, nodeT v, costT w);
costT getCost(LCT f, nodeT u, nodeT v); /* Edge {u,v} should exist. */
void update(LCT f, nodeT v, costT w); /* Add w to edges of branch. */
nodeT getMin(LCT f, nodeT v); /* Get the lower node of a minCost edge. */
#endif /* _VERSION_W */

#endif /* LINKCUTTREE_H */
