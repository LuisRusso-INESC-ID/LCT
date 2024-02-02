#define _DEFAULT_SOURCE
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
/* #include <bsd/stdlib.h> */

#include "LCT.h"
#include "splayPrinter.h"

/* #define arc4random_uniform(N) random()%N */

enum ops {
  OAccess = 0 + 2,
  OgetNode = OAccess + 2,
  OedgeQ = OgetNode + 2,
  Ocut = OedgeQ + 1,
  OreRoot = Ocut + 2,
  OLCA = OreRoot + 2,
  OLink = OLCA + 2,
#ifndef _VERSION_W
  OLast = OLink,
#else /* _VERSION_W */
  OgetCost = OLink + 2,
  Oupdate = OgetCost + 2,
  OgetMin = Oupdate + 2,
  OLast = OgetMin
#endif /* _VERSION_W */
};

int
main(int argc, char **argv)
{
  int d;
  unsigned int n = 10;
  unsigned int ops = 5;
  uint32_t rd = 0;
  nodeT v;
  nodeT u;
  LCT F;
#ifdef _VERSION_W
  int w;
#endif /* _VERSION_W */

  if(3 != argc)
    exit(EXIT_FAILURE);

  srandom(42);

  sscanf(argv[1], "%ud", &n);
  printf("allocLCT %d\n", n);
  F = allocLCT(n);

  sscanf(argv[2], "%ud", &ops);
  for(unsigned int i = 0; i < ops; i++){
    rd = arc4random_uniform(OLast);

    if(rd < OAccess){
      v = 1 + arc4random_uniform(n);
      printf("Access %d\n", v);
      printf("# %d\n", Access(F, v));
      continue;
    }

    if(rd < OgetNode){
      v = 1 + arc4random_uniform(n);
      d = Access(F, v);
      d = 1 + arc4random_uniform(4*d) - 2*d;
      printf("getNode %d %d\n", v, d);
      printf("# %d\n", getNode(F, v, d));
      continue;
    }

    if(rd < OedgeQ){
      u = 1 + arc4random_uniform(n);
      v = 1 + arc4random_uniform(n);
      printf("edgeQ %d %d\n", u, v);
      printf("# %s\n", edgeQ(F, u, v) ? "true" : "false");
      continue;
    }

    if(rd < Ocut){
      u = 1 + arc4random_uniform(n);
      v = 1 + arc4random_uniform(n);
      if(0 < Access(F, u) && 0 == arc4random_uniform(3))
	v = getNode(F, u, -1);
      printf("cut %d %d\n", u, v);
      printf("# %d\n", cut(F, u, v));
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
      continue;
    }

    if(rd < OLink){
      u = 1 + arc4random_uniform((n-1));
      v = 1 + u;
      if(u != v && 0 == LCA(F, u, v)){
#ifdef _VERSION_W
	w = arc4random_uniform(21)-10;
	printf("LinkW %d %d %d\n", u, v, w);
	LinkW(F, u, v, w);
#else /* _VERSION_W */
	printf("Link %d %d\n", u, v);
	Link(F, u, v);
#endif /* _VERSION_W */
      }
      continue;
    }

#ifdef _VERSION_W
    if(rd < OgetCost){
      v = 1 + arc4random_uniform(n);
      d = Access(F, v);
      if(0 < d){
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
	printf("getMin %d\n", v);
	u = getMin(F, v);
	v = getNode(F, u, -1);
	printf("# %f\n", getCost(F, u, v));
      }
      continue;
    }
#endif /* _VERSION_W */
  }

  printf("end\n");

  return 0;
}
