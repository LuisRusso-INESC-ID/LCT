#define _DEFAULT_SOURCE
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <bsd/stdlib.h>

#include "LCT.h"
#include "splayPrinter.h"

/* #define arc4random_uniform(N) random()%N */

/* enum ops { */
/*   OAccess = 0 + 0, */
/*   OgetNode = OAccess + 0, */
/*   OedgeQ = OgetNode + 0, */
/*   Ocut = OedgeQ + 1, */
/*   OreRoot = Ocut + 0, */
/*   OLCA = OreRoot + 0, */
/*   OLink = OLCA + 2, */
/* #if !(defined _EDGE_W || defined _VERTEX_W) */
/*   OLast = OLink, */
/* #else /\* defined _EDGE_W || defined _VERTEX_W *\/ */
/*   OsetCost = OLink + 0, */
/*   OgetCost = OsetCost + 0, */
/*   Oupdate = OgetCost + 2, */
/*   OgetMin = Oupdate + 2, */
/*   OLast = OgetMin */
/* #endif /\* defined _EDGE_W || defined _VERTEX_W *\/ */
/* }; */

enum ops {
  OAccess = 0 + 2,
  OgetNode = OAccess + 2,
  OedgeQ = OgetNode + 2,
  Ocut = OedgeQ + 1,
  OreRoot = Ocut + 2,
  OLCA = OreRoot + 2,
  OLink = OLCA + 2,
#if !(defined _EDGE_W || defined _VERTEX_W)
  OLast = OLink,
#else /* defined _EDGE_W || defined _VERTEX_W */
  OsetCost = OLink + 2,
  OgetCost = OsetCost + 2,
  Oupdate = OgetCost + 2,
  OgetMin = Oupdate + 2,
  OLast = OgetMin
#endif /* defined _EDGE_W || defined _VERTEX_W */
};

int
main(int argc, char **argv)
{
  int d;
  unsigned int n = 10;
  unsigned int ops = 5;
  int rd = 0;
  nodeT v;
  nodeT u;
  LCT F;
#if defined _EDGE_W || defined _VERTEX_W
  int w;
#endif /* defined _EDGE_W || defined _VERTEX_W */

  if(3 != argc)
    exit(EXIT_FAILURE);

  /* srandom(42); */

  sscanf(argv[1], "%ud", &n);
  printf("allocLCT %d\n", n);
  F = allocLCT(n);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
  bool repeat = false;
#pragma GCC diagnostic pop /*-Wunused-but-set-variable*/

  sscanf(argv[2], "%ud", &ops);
  for(unsigned int i = 0; i < ops; i++){
    /* for(unsigned int j = 1; j <= n; j++){ */
    /*   for(unsigned int k = 1; k <= n; k++){ */
    /* 	printf("edgeQ %d %d\n", j, k); */
    /*   } */
    /* } */

    /* v = 8; */
    /* if(0 != Access(F, v) && !repeat) */
    /*   printf("getMin %d\n", v); */
    repeat = true;

    rd = arc4random_uniform(OLast);

    if(rd < OAccess){
      repeat = false;
      v = 1 + arc4random_uniform(n);
      printf("Access %d\n", v);
      printf("# %d\n", Access(F, v));
      continue;
    }

    if(rd < OgetNode){
      repeat = false;
      v = 1 + arc4random_uniform(n);
      d = Access(F, v);
      d = 1 + arc4random_uniform(4*d) - 2*d;
      printf("getNode %d %d\n", v, d);
      printf("# %d\n", getNode(F, v, d));
      continue;
    }

    if(rd < OedgeQ){
      repeat = false;
      u = 1 + arc4random_uniform(n);
      v = 1 + arc4random_uniform(n);
      printf("edgeQ %d %d\n", u, v);
      printf("# %s\n", edgeQ(F, u, v) ? "true" : "false");
      continue;
    }

    if(rd < Ocut){
      repeat = false;
      u = 1 + arc4random_uniform(n);
      v = 1 + arc4random_uniform(n);
      if(0 < Access(F, u) && 0 == arc4random_uniform(3)){
	printf("cut %d 0\n", u);
	printf("# %d\n", cut(F, u, 0));
      } else {
	printf("cut %d %d\n", u, v);
	printf("# %d\n", cut(F, u, v));
      }
      continue;
    }

    if(rd < OreRoot){
      repeat = false;
      v = 1 + arc4random_uniform(n);
      printf("reRoot %d\n", v);
      reRoot(F, v);
      continue;
    }

    /* if(rd < OconnectedQ){ */
    /*   repeat = false; */
    /*   u = 1 + arc4random_uniform(n); */
    /*   v = 1 + arc4random_uniform(n); */
    /*   printf("connectedQ %d %d\n", u, v); */
    /*   printf("# %s\n", connectedQ(F, u, v) ? "true" : "false"); */
    /*   continue; */
    /* } */

    if(rd < OLCA){
      u = 1 + arc4random_uniform(n);
      v = 1 + arc4random_uniform(n);
      repeat = false;
      printf("LCA %d %d\n", u, v);
      printf("# %u\n", LCA(F, u, v));
      continue;
    }

    if(rd < OLink){
      u = 1 + arc4random_uniform(n);
      v = 1 + arc4random_uniform(n);
      if(u != v && 0 == LCA(F, u, v)){
	repeat = false;
	printf("Link %d %d\n", u, v);
	Link(F, u, v);
#if defined _EDGE_W || defined _VERTEX_W
	w = arc4random_uniform(21)-10;
	printf("setCost %d %d\n", u, w);
	setCost(F, u, w);
#endif /* defined _EDGE_W || defined _VERTEX_W */
      }
      continue;
    }

#if defined _EDGE_W || defined _VERTEX_W
    if(rd < OsetCost){
      u = 1 + arc4random_uniform(n);
      w = arc4random_uniform(21)-10;
#ifdef _EDGE_W
      if(1 < Access(F, u))
#endif /* _EDGE_W */
	printf("setCost %d %d\n", u, w);
      continue;
    }

    if(rd < OgetCost){
      v = 1 + arc4random_uniform(n);
      d = Access(F, v);
      if(0 < d){
	repeat = false;
	d = arc4random_uniform(d);
	u = getNode(F, v, d);
	v = getNode(F, v, 1+d);
	printf("getCost %d %d\n", u, v);
	printf("# %f\n", getCost(F, u, v));
      }
      continue;
    }

    if(rd < Oupdate){
      v = 1 + arc4random_uniform(n);
      d = Access(F, v);
      if(0 < d){
	repeat = false;
	w = arc4random_uniform(3)-1;
	printf("update %d %d\n", v, w);
	update(F, v, w);
      }
      continue;
    }

    if(rd < OgetMin){
      v = 1 + arc4random_uniform(n);
      d = Access(F, v);
      if(0 < d){
	repeat = false;
	printf("getMin %d\n", v);
	u = getMin(F, v);
	v = getNode(F, u, -1);
	printf("# %f\n", getCost(F, u, v));
      }
      continue;
    }
#endif /* defined _EDGE_W || defined _VERTEX_W */
  }

  printf("end\n");

  return 0;
}
