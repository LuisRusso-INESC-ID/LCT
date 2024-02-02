#include <stdlib.h>
#include <assert.h>

#include "splayTree.h"
#include "macros.h"

#include "struct.c"

/* Private functions for this file. */

static /*@ dependent */ nodeT *getHook(splayT t, nodeT v);
static void setSize(splayT t, nodeT v);
/* Propagates flipA2Bs and weight deltas to sub-trees */
static void propagate(splayT t, nodeT v);
static void rotate(splayT t, nodeT v);

/* Implementation starts here. */
enum childClass classify(splayT t, nodeT v)
{
  if(0 != t[v].father && v == t[t[v].father].above) return above;
  if(0 != t[v].father && v == t[t[v].father].below) return below;
  return across;
}

static /*@ dependent */ nodeT *getHook(splayT t, nodeT v)
{ /*@-immediatetrans */
  if(v == t[t[v].father].above) return &t[t[v].father].above;
  return &t[t[v].father].below;
} /*@=immediatetrans */

bool above2belowQ(splayT t, nodeT v)
{
  return 0 < t[v].size;
}

void flipA2B(splayT t, nodeT v)
{
  t[v].size *= -1;
}

/* ab: is direction above to below? */
int inorderRec(splayT t, nodeT v, nodeT *I, bool ab)
{
  assertM(0 != v, "Nothing to show at 0.");

  /* Variables for recursive calls */
  int r = 0;
  nodeT ra = t[v].above;
  nodeT rb = t[v].below;
  bool rab = ab;
  if(!above2belowQ(t, v)) rab = !ab;
  if(!rab) swapM(nodeT, ra, rb);

  if(0 != ra)
    r += inorderRec(t, ra, I, rab);

  I[r] = v;
  r++;

  if(0 != rb)
    r += inorderRec(t, rb, &I[r], rab);

  return r;
}

#ifdef _VERSION_W
int inorderRecW(splayT t, nodeT v, costT d, costT *I, bool ab)
{
  assertM(0 != v, "Nothing to show at 0.");

  /* Variables for recursive calls */
  int r = 0;
  nodeT ra = t[v].above;
  nodeT rb = t[v].below;
  bool rab = ab;
  if(!above2belowQ(t, v)) rab = !ab;
  if(!rab) swapM(nodeT, ra, rb);

  d += t[v].d;

  if(0 != ra){
    r += inorderRecW(t, ra, d, I, rab);
    I[r] = d + t[ra].w;
    r++;
  }

  if(0 != rb){
    I[r] = d + t[rb].w;
    r++;
    r += inorderRecW(t, rb,  d, &I[r], rab);
  }

  return r;
}

#define miniM(M, V) if((V) < (M)) (M) = (V);
#endif /* _VERSION_W */

void setSize(splayT t, nodeT v)
{
  bool originalA2B = above2belowQ(t, v);
  t[v].size = 1;
  if(0 != t[v].above) t[v].size += abs(t[t[v].above].size);
  if(0 != t[v].below) t[v].size += abs(t[t[v].below].size);
  if(!originalA2B) flipA2B(t, v);
  setMin(t, v);
}

#ifndef _VERSION_W
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
void setMin(/*@unused@*/splayT t, /*@unused@*/nodeT v) {}
#pragma GCC diagnostic pop /*-Wunused-parameter*/
#else /* _VERSION_W */
void setMin(splayT t, nodeT v)
{
  t[v].min = t[v].w;
  if(0 == t[v].father) t[v].min = _COST_MAX;
  if(0 != t[v].above) miniM(t[v].min, t[t[v].above].min + t[v].d);
  if(0 != t[v].below) miniM(t[v].min, t[t[v].below].min + t[v].d);
}
#endif /* _VERSION_W */

void setBelow(splayT t, nodeT u, nodeT v)
{
  assertW(0 == t[u].d, "Inconsistent setBelow");
  assertM(above2belowQ(t, u) && across == classify(t, u),
          "Inconsistent setBelow");
  t[u].below = v;
  setSize(t, u);
}

void propagate(splayT t, nodeT v)
{
  if(!above2belowQ(t, v)) {
    flipA2B(t, v);
    swapM(nodeT, t[v].above, t[v].below);
    flipA2B(t, t[v].above);
    flipA2B(t, t[v].below);
  }

#ifdef _VERSION_W
  t[t[v].above].d += t[v].d;
  t[t[v].above].w += t[v].d;
  t[t[v].above].min += t[v].d;
  t[t[v].below].d += t[v].d;
  t[t[v].below].w += t[v].d;
  t[t[v].below].min += t[v].d;
  t[v].d = (costT) 0;
#endif /* _VERSION_W */
}

static void rotate(splayT t, nodeT v)
{
  assertM(across != classify(t, v), "Trying to rotate node without a father");

  nodeT u = t[v].father;
  if(across != classify(t, u)) *getHook(t, u) = v;

  propagate(t, u); /* Top-down */
  propagate(t, v);
  nodeT *ua = getHook(t, v); /* &t[u].above in Fig. */
  nodeT *vb = &t[v].below;
  if(below == classify(t, v)) vb = &t[v].above;

  *ua = *vb;
  *vb = u;
  t[v].father = t[u].father;
  t[*vb].father = v;
  t[*ua].father = u;

#ifdef _VERSION_W
  swapM(costT, t[v].w, t[u].w);
  if(0 != *ua){
    propagate(t, *ua);
    swapM(costT, t[u].w, t[*ua].w);
    setMin(t, *ua);
  }
  t[v].min = t[u].min;
#endif /* _VERSION_W */
  t[v].size = t[u].size;
  setSize(t, u);
}

/* Dangling else makes code compact. */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdangling-else"
void splay(splayT t, nodeT v)
{
  while(across != classify(t, v)){
    nodeT u = t[v].father;
    if(across != classify(t, u))
      if((classify(t, v) == classify(t, u)) == above2belowQ(t, u))
        rotate(t, u); /* Zig-Zig case */
      else rotate(t, v); /* Zig-Zag case */
    rotate(t, v); /* Both cases and Single Zig */
  }
  propagate(t, v); /* Crucial when skipping while. */

  assertM(above2belowQ(t, v), "Flipped node after splay.");
  assertW(0 == t[v].d, "Delta value in node after splay.");
}
#pragma GCC diagnostic pop /*-Wdangling-else*/

nodeT getNodeOnSplay(splayT t, nodeT v, int d)
{
  propagate(t, v); /* .size + .above Read */
  if(0 > d) d += t[v].size;
  if(0 > d || t[v].size <= d) return 0; /* d of bounds */

  nodeT u = t[v].above;
  propagate(t, u); /* .size Read */
  while(d != t[u].size){
    if(t[u].size > d)
      v = u; /* Automagick propagate(t, v) */
    else {
      d -= t[u].size + 1;
      v = t[v].below;
      propagate(t, v); /* .above Read */
    }

    u = t[v].above;
    propagate(t, u); /* .size Read */
  }
  splay(t, v); /* amortized O guarantee */

  return v;
}
