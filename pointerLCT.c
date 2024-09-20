#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <bsd/stdlib.h>

#include "LCT.h"
/*@access LCT@*/

#include "macros.h"

struct LCT { /* Info in a node. */
  nodeT father;
  unsigned int stamp; /* Time stamp for LCA */
#if defined _EDGE_W || defined _VERTEX_W
  costT w;
#endif /* defined _EDGE_W || defined _VERTEX_W */
};

void initLCT(LCT f, unsigned int n)
{
  /*@-type*/
  f[0].stamp = n; /* Store the size of the array */
  /*@=type*/
}

#include "commonLCT.c"

unsigned int Access(LCT f, nodeT v)
{
  unsigned int r = 0;
  while(0 != f[v].father){
    r++;
    v = f[v].father;
  }

  return r;
}

nodeT getNode(LCT f, nodeT v, int d)
{
  if(0 <= d) d -= (int)Access(f, v);

  while(0 != d && 0 != v){
    v = f[v].father;
    d++;
  }
  return v;
}

nodeT cut(LCT f, nodeT u, nodeT v)
{
  if(0 == v) v = getNode(f, u, -1);
  if(f[v].father == u) swapM(nodeT, u, v); /* v is above */
  if(f[u].father != v) return 0; /* {u,v} does not exist */
  else f[u].father = 0; /* Remove edge */
  return u;
}

void reRoot(LCT f, nodeT v)
{
#ifdef _EDGE_W
  /*@-fullinitblock*/ struct LCT T = {0}; /*@=fullinitblock*/
#else /* _EDGE_W */
  nodeT T = 0;
#endif /* _EDGE_W */

  while(0 != v){
#ifdef _EDGE_W
    swapM(struct LCT, T, f[v]);
    swapM(nodeT, v, T.father);
#else /* _EDGE_W */
    swapM(nodeT, T, f[v].father);
    swapM(nodeT, v, T);
#endif /* _EDGE_W */
  }
}

nodeT LCA(LCT f, nodeT u, nodeT v)
{
  static unsigned int stamper = 0;
  stamper++; /* Get a new value */
  if(0 == stamper){
    for(unsigned int i = 1; i <= f[0].stamp; i++)
      f[i].stamp = 0; /* clean up stamps */
    stamper++;
  }

  while(0 != v){
    if(f[v].stamp == stamper)
      return v; /* Repeated stamp */
    f[v].stamp = stamper;
    v = f[v].father;
    if(0 != u) swapM(nodeT, u, v);
  }
  return 0; /* u and v are not connected */
}

void Link(LCT f, nodeT u, nodeT v)
{
  assertM(0 == LCA(f, u, v), "Link failed, u and v are on the same tree.");
  reRoot(f, u);
  f[u].father = v;
}

#if defined _EDGE_W || defined _VERTEX_W
void setCost(LCT f, nodeT u, costT w)
{
#ifdef _EDGE_W
  assertM(0 != f[u].father, "No edge to set cost.");
#endif /* _EDGE_W */
  f[u].w = w;
}

#ifdef _EDGE_W
costT getCost(LCT f, nodeT u, nodeT v)
#else /* _EDGE_W */
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
costT getCost(LCT f, nodeT u, /*@unused@*/ nodeT v)
#endif /* _EDGE_W */
{
#ifdef _EDGE_W
  if(0 == v) v = getNode(f, u, -1);
  assertM(edgeQ(f, u, v), "Cost for unexisting edge.");
  if(f[v].father == u) u = v;
#endif /* _EDGE_W */
  return f[u].w;
}
#if ! defined _EDGE_W
#pragma GCC diagnostic pop /*-Wunused-parameter*/
#endif

void update(LCT f, nodeT v, costT w)
{
  while(0 != f[v].father){
    f[v].w += w;
    v = f[v].father;
  }
#ifdef _VERTEX_W
  f[v].w += w;
#endif /* _VERTEX_W */
}

nodeT getMin(LCT f, nodeT v)
{
#ifdef _EDGE_W
  assertM(0 != f[v].father, "Asked for min in single node branch.");
#endif /* _EDGE_W */

  nodeT r = v; /* current min node */
  while(0 != f[v].father){
    if(f[v].w < f[r].w) r = v;
    v = f[v].father;
  }
#ifdef _VERTEX_W
  if(f[v].w < f[r].w) r = v;
#endif /* _VERTEX_W */

  return r;
}
#endif /* defined _EDGE_W || defined _VERTEX_W */
