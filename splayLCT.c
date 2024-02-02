#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>

#include "LCT.h"
/*@access LCT@*/

#include "splayTree.h"
/*@access splayT@*/

#include "macros.h"
#include "struct.c"

void initLCT(LCT f, unsigned int n)
{
  for(unsigned int v = 1; v <= n; v++)
    f[v].size = 1;
}

#include "commonLCT.c"

unsigned int Access(LCT f, nodeT v)
{
  splay((splayT)f, v);
  nodeT p = f[v].father;
  while(0 != p) {
    splay((splayT)f, p);
    setBelow((splayT)f, p, v);
    assertM(f[v].father == p, "Failing father test.");
    splay((splayT)f, v);
    p = f[v].father;
  }

  assertM(above2belowQ((splayT)f, v), "Flipped node after Access.");
  assertW(0 == f[v].d, "Delta value in node after Access.");

  setBelow((splayT)f, v, 0);
  return (unsigned int) f[v].size - 1;
}

nodeT getNode(LCT f, nodeT v, int d)
{
  (void)Access(f, v);
  if(0 > d) d--;
  return getNodeOnSplay(f, v, d);
}

nodeT cut(LCT f, nodeT u, nodeT v)
{
  if(!edgeQ(f, u, v)) return 0; /* {u,v} does not exist */
  if(Access(f, v) < Access(f, u)) swapM(nodeT, u, v); /* u is above */
  (void)Access(f, u);
  f[v].father = 0;
  setMin(f, v);

  return v;
}

void reRoot(LCT f, nodeT v)
{ if(0 != Access(f, v)) flipA2B((splayT) f, v); }

nodeT LCA(LCT f, nodeT u, nodeT v)
{
  if(u == v) return v;
  (void)Access(f, u);
  (void)Access(f, v);
  if(0 == f[u].father) return 0; /* Not connected. */
  splay((splayT)f, u);
  if(0 != f[u].father) return f[u].father;
  return u;
}

#ifndef _VERSION_W
void Link(LCT f, nodeT u, nodeT v)
#else /* _VERSION_W */
void LinkW(LCT f, nodeT u, nodeT v, costT w)
#endif /* _VERSION_W */
{
  assertM(0 == LCA(f, u, v), "Link failed, u and v are on the same tree.");
  reRoot(f, u);
  f[u].father = v;
#ifdef _VERSION_W
  f[u].w = w;
  setMin(f, u);
#endif /* _VERSION_W */
}
#ifdef _VERSION_W

costT getCost(LCT f, nodeT u, nodeT v)
{
  assertM(edgeQ(f, u, v), "Cost for unexisting edge ???");
  (void)Access(f, u);
  (void)Access(f, v);
  return f[u].w;
}

void update(LCT f, nodeT v, costT w)
{
  (void)Access(f, v);
  f[v].d = w;
  f[v].min += w;
}

nodeT getMin(LCT f, nodeT v)
{
  (void)Access(f, v);
  assertM(1 < f[v].size, "Asked for min in single node branch.");

  costT min = f[v].min;
  do{ /*@-realcompare*/
    if(0 != f[v].below && f[v].min == f[f[v].below].min + f[v].d)
      v = f[v].below;
    else v = f[v].above;
  } while(f[v].min != f[v].w);
  v = f[v].father; /* Undo last move */
  splay((splayT)f, v); /* amortized O guarantee */

  if(0 != f[v].below){
    (void)getNodeOnSplay((splayT)f, f[v].below, 0);
    if(f[v].w == min) return f[v].father;
  } /*@=realcompare*/
  return v;
}

#endif /* _VERSION_W */

void checkFatherConsistent(LCT f, unsigned int n)
{
  for(nodeT u = 1; u <= n; u++){
    if(0 != f[u].above){
      assertM(f[f[u].above].father == u, "Lost child pointer");
    }
    if(0 != f[u].below){
      assertM(f[f[u].below].father == u, "Lost child pointer");
    }
  }
}
