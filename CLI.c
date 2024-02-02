#define _POSIX_C_SOURCE 200809L
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "LCT.h"

#ifdef SPRINT
#include "splayPrinter.h"
#endif /* SPRINT */

int
main(void)
{
  char *line = NULL;
  char *tok;
  size_t ln = 0;

  nodeT u;
  nodeT v;

  unsigned int n = 0;
  LCT F = NULL;

  int lnCt = 0; /* Line counter */
  while(0 < getline(&line, &ln, stdin)){
    lnCt++;

#ifdef SPRINT
    /* nameAndPrint(F, n); */
#endif /* SPRINT */

    if('#' == line[0]) continue;

    printf("# %d : %s", lnCt, line);
    tok = strtok(line, " \t\n");

    u = 0;
    v = 0;

    if(0 == strcmp("end", tok))
      break;

    if(0 == strcmp("allocLCT", tok)){
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &n);
      if(NULL != F) free((void *)F);
      F = allocLCT(n);
      continue;
    }

    if(0 == strcmp("Access", tok)){
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &v);
      printf("%d\n", Access(F, v));
      continue;
    }

    if(0 == strcmp("getNode", tok)){
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &v);
      int d;
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%d", &d);
      printf("%d\n", getNode(F, v, d));
      continue;
    }

    if(0 == strcmp("edgeQ", tok)){
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &u);
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &v);
      printf("%s\n", edgeQ(F, u, v) ? "True" : "False");
      continue;
    }

    if(0 == strcmp("cut", tok)){
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &u);
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &v);
      printf("%d\n", cut(F, u, v));
      continue;
    }

    if(0 == strcmp("reRoot", tok)){
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &v);
      reRoot(F, v);
      continue;
    }

    /* if(0 == strcmp("connectedQ", tok)){ */
    /*   tok = strtok(NULL, " \t\n"); */
    /*   sscanf(tok, "%ud", &u); */
    /*   tok = strtok(NULL, " \t\n"); */
    /*   sscanf(tok, "%ud", &v); */
    /*   printf("%s\n", connectedQ(F, u, v) ? "True" : "False"); */
    /* } */

    if(0 == strcmp("LCA", tok)){
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &u);
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &v);
      printf("%d\n", LCA(F, u, v));
      continue;
    }

#ifndef _VERSION_W
    if(0 == strcmp("Link", tok)){
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &u);
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &v);
      tok = strtok(NULL, " \t\n");
      Link(F, u, v);
      continue;
    }
#else /* _VERSION_W */
    if(0 == strcmp("LinkW", tok)){
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &u);
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &v);
      tok = strtok(NULL, " \t\n");
      costT w = 0;
      sscanf(tok, "%lf", &w);
      LinkW(F, u, v, w);
      continue;
    }

    if(0 == strcmp("getCost", tok)){
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &u);
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &v);
      printf("%f\n", getCost(F, u, v));
      continue;
    }

    if(0 == strcmp("update", tok)){
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &v);
      tok = strtok(NULL, " \t\n");
      costT w = 0;
      sscanf(tok, "%lf", &w);
      update(F, v, w);
      continue;
    }

    if(0 == strcmp("getMin", tok)){
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &v);
      u = getMin(F, v);
      v = getNode(F, u, -1);
      printf("%f\n", getCost(F, u, v));
      continue;
    }
#endif /* _VERSION_W */

#ifdef SPRINT
    if(0 == strcmp("print", tok)){
      nameAndPrint(F, n);
      continue;
    }

    if(0 == strcmp("splayPrint", tok)){
      tok = strtok(NULL, " \t\n");
      sscanf(tok, "%ud", &v);
      nameAndSplayP(F, n, v);
      continue;
    }
#endif /* SPRINT */
  }

  if(NULL != F) free((void *)F);
  if(NULL != line) free(line);

  return 0;
}
