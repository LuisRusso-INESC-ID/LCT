#define _DEFAULT_SOURCE
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <bsd/stdlib.h>

#include "LCT.h"
#include "splayPrinter.h"

/* #define arc4random_uniform(N) random()%N */

/* Config for cycles */
enum ops {
  OAccess = 0 + 0,
  OgetNode = OAccess + 0,
  OedgeQ = OgetNode + 2,
  Ocut = OedgeQ + 1,
  OreRoot = Ocut + 0,
  OLCA = OreRoot + 2,
  OLink = OLCA + 2,
#if !(defined _EDGE_W || defined _VERTEX_W)
  OLast = OLink,
#else /* defined _EDGE_W || defined _VERTEX_W */
  OgetCost = OLink + 2,
  Oupdate = OgetCost + 0,
  OgetMin = Oupdate + 0,
  OLast = OgetMin
#endif /* defined _EDGE_W || defined _VERTEX_W */
};

/* Classical full test */
/* enum ops { */
/*   OAccess = 0 + 2, */
/*   OgetNode = OAccess + 2, */
/*   OedgeQ = OgetNode + 2, */
/*   Ocut = OedgeQ + 1, */
/*   OreRoot = Ocut + 2, */
/*   OLCA = OreRoot + 2, */
/*   OLink = OLCA + 2, */
/* #if !(defined _EDGE_W || defined _VERTEX_W) */
/*   OLast = OLink, */
/* #else /\* !(defined _EDGE_W || defined _VERTEX_W) *\/ */
/*   OgetCost = OLink + 2, */
/*   Oupdate = OgetCost + 2, */
/*   OgetMin = Oupdate + 2, */
/*   OLast = OgetMin */
/* #endif /\* !(defined _EDGE_W || defined _VERTEX_W) *\/ */
/* }; */

struct edge
{
  int u;
  int v;
};

int
main(int argc, char **argv)
{
  int d;
  unsigned int n;
  unsigned int V = 10;
  unsigned int E = 10;
  unsigned int ops = 5;
  struct edge *G; /* The graph */

  uint32_t rd = 0;
  nodeT v;
  nodeT u;
  LCT F;
#if defined _EDGE_W || defined _VERTEX_W
  int w;
#endif /* defined _EDGE_W || defined _VERTEX_W */

  int count = 0; /* Number of operations */

  if(2 != argc)
    exit(EXIT_FAILURE);

  srandom(42);

  sscanf(argv[1], "%ud", &ops);

  scanf("%ud", &V);
  printf("allocLCT %d\n", V);
  F = allocLCT(V);
  n = V;

  scanf("%ud", &E);
  G = (struct edge *)malloc(E*sizeof(struct edge));

  for(unsigned int i = 0; i < E; i++){
    scanf("%d", &G[i].u);
    scanf("%d", &G[i].v);
  }

  for(unsigned int i = 0; i < ops; i++){
    rd = arc4random_uniform(OLast);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wtype-limits"
    if(rd < OAccess){
      v = 1 + arc4random_uniform(n);
      printf("Access %d\n", v);
      printf("# %d\n", Access(F, v));
      count++;
      continue;
    }

    if(rd < OgetNode){
      v = 1 + arc4random_uniform(n);
      d = Access(F, v);
      d = 1 + arc4random_uniform(4*d) - 2*d;
      printf("getNode %d %d\n", v, d);
      printf("# %d\n", getNode(F, v, d));
      count++;
      continue;
    }

    if(rd < OedgeQ){
      int p = arc4random_uniform(E);
      u = G[p].u;
      v = G[p].v;
      printf("edgeQ %d %d\n", u, v);
      printf("# %s\n", edgeQ(F, u, v) ? "true" : "false");
      count++;
      continue;
    }

    if(rd < Ocut){
      u = 1 + arc4random_uniform(V);
      if(0 < Access(F, u)){
	v = getNode(F, u, -1);
	printf("cut %d %d\n", u, v);
	printf("# %d\n", cut(F, u, v));
	G[E].u = u;
	G[E].v = v;
	E++;
	count++;
      }
      continue;
    }

    if(rd < OreRoot){
      v = 1 + arc4random_uniform(n);
      printf("reRoot %d\n", v);
      reRoot(F, v);
      continue;
    }

    /* if(rd < OconnectedQ){ */
    /*   u = 1 + arc4random_uniform(n); */
    /*   v = 1 + arc4random_uniform(n); */
    /*   printf("connectedQ %d %d\n", u, v); */
    /*   printf("# %s\n", connectedQ(F, u, v) ? "true" : "false"); */
    /*   continue; */
    /* } */

    if(rd < OLCA){
      u = 1 + arc4random_uniform(n);
      v = 1 + arc4random_uniform(n);
      printf("LCA %d %d\n", u, v);
      printf("# %u\n", LCA(F, u, v));
      count++;
      continue;
    }

    if(rd < OLink){
      int p = arc4random_uniform(E);
      u = G[p].u;
      v = G[p].v;
      if(u != v && 0 == LCA(F, u, v)){
	printf("Link %d %d\n", u, v);
	Link(F, u, v);
#if defined _EDGE_W || defined _VERTEX_W
	w = arc4random_uniform(21)-10;
	printf("setCost %d %d\n", u, w);
	setCost(F, u, w);
#endif /* defined _EDGE_W || defined _VERTEX_W */
	/* Remove from E */
	E--;
	G[p] = G[E];
	count++;
      }
      continue;
    }

#if defined _EDGE_W || defined _VERTEX_W
    if(rd < OgetCost){
      v = 1 + arc4random_uniform(n);
      d = Access(F, v);
      if(0 < d){
	d = arc4random_uniform(d);
	u = getNode(F, v, d);
	v = getNode(F, v, 1+d);
	printf("getCost %d %d\n", u, v);
	printf("# %f\n", getCost(F, u, v));
	count++;
      }
      continue;
    }

    if(rd < Oupdate){
      v = 1 + arc4random_uniform(n);
      d = Access(F, v);
      if(0 < d){
	w = arc4random_uniform(3)-1;
	printf("update %d %d\n", v, w);
	update(F, v, w);
	count++;
      }
      continue;
    }

    if(rd < OgetMin){
      v = 1 + arc4random_uniform(n);
      d = Access(F, v);
      if(0 < d){
	printf("getMin %d\n", v);
	u = getMin(F, v);
	v = getNode(F, u, -1);
	printf("# %f\n", getCost(F, u, v));
	count++;
      }
      continue;
    }
#endif /* defined _EDGE_W || defined _VERTEX_W */
#pragma GCC diagnostic pop /*-Wtype-limits*/
  }

  printf("end\n");
  printf("%d\n", count);

  free(G);
  free(F);

  return 0;
}
