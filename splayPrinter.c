#include <stdlib.h>
#include <stdio.h>
#include "splayPrinter.h"

#include "LCT.h"
/*@access LCT@*/

#include "splayTree.h"
/*@access splayT@*/
#include "struct.c"

struct edge {
  nodeT u;
  nodeT v;
#ifdef _VERSION_W
  costT w;
#endif /* _VERSION_W */
};

typedef struct elist *elist;

static elist newEList(void);
static void freeEList(elist l);
static void printEL(elist l, bool dash);
static void append(elist l, struct edge e);

static FILE *pf = NULL;

/* LinkedList code */

typedef struct enode *enode;

struct enode{
  enode nxt;
  struct edge e;
};

struct elist {
  enode start;
  enode *end;
};

static int c = 1;
static char fname[32];

void
nameAndPrint(LCT F, unsigned int n)
{
  sprintf(fname, "LCTree%.3d.tex", c);
  c++;
  printLCT(F, n, fname);
}

void
nameAndSplayP(splayT F, unsigned int n, nodeT v)
{
  sprintf(fname, "SplayTree%.3d.tex", c);
  c++;
  printSplay((splayT)F, n, v, fname);
}

static elist newEList(void)
{
  elist l = (elist)malloc(sizeof (struct elist));
  if(NULL == l)
    exit(EXIT_FAILURE);

  l->start = NULL;
  l->end = &(l->start);

  return l;
}

static void freeEList(elist l)
{
  enode e = l->start;

  while(NULL != e){
    enode nx = e->nxt;
    free(e);
    e = nx;
  }

  free(l);
}

static void append(elist l, struct edge e)
{
  *(l->end) = (enode)malloc(sizeof (struct enode));
  (*(l->end))->e = e;
  (*(l->end))->nxt = NULL;
  l->end = &((*(l->end))->nxt);
}

static void printEL(elist l, bool dash)
{
  enode e = l->start;

  while(NULL != e){
    fprintf(pf, "\\ncline");
    if(dash)
      fprintf(pf, "[linestyle=dashed]");
    fprintf(pf, "{N%d}{N%d}", e->e.v, e->e.u);

#ifdef _VERSION_W
    fprintf(pf, " \\ncput*[npos=0.2]");
    fprintf(pf, "{\\psrotateleft{\\psscalebox{-1 1}{%.1f}}}", e->e.w);
#endif /* _VERSION_W */

    fprintf(pf, "\n");
    e = e->nxt;
  }
}

/* Now printing stuff */

void printLCT(LCT f, unsigned int n, char* fname)
{
  pf = fopen(fname, "w");
  if(NULL == pf)
    exit(EXIT_FAILURE);

  nodeT branches = 0;
  for(nodeT u = 1; u <= n; u++)
    if(across == classify(f, u))
      branches++;

  nodeT *B = malloc(sizeof *B * branches);
  if(NULL == B)
    exit(EXIT_FAILURE);

  unsigned int i = 0;
  for(nodeT u = 1; u <= n; u++)
    if(across == classify(f, u)){
      B[i] = u;
      i++;
    }

  /* Count sort on fathers */
  nodeT *S = malloc(sizeof *S * branches);
  nodeT *F2S = malloc(sizeof *F2S * branches);
  int *c = calloc((size_t)(n+2), sizeof *c);
  if(NULL == c)
    exit(EXIT_FAILURE);

  for(i = 0; i < branches; i++) /* Count */
    c[1+f[B[i]].father]++;

  nodeT u;
  for(u = 1; u <= n; u++) /* Acumulate */
    c[1+u] += c[u];

  for(i = 0; i < branches; i++){ /* Distribute */
    S[c[f[B[i]].father]] = B[i];
    c[f[B[i]].father]++;
  }

  /* Index */
  int j = 0;
  for(i = 0; i < branches; i++){
    F2S[f[S[j]].father] = j;
    while(f[S[j]].father == B[i]) j++;
  }

  /* Printing stuff */
  fprintf(pf, "\\psset{arrows=->}\n");
  fprintf(pf, "\\psscalebox{-1 1}{\\psrotateright{\n");
#ifdef _VERSION_W
  fprintf(pf, "$\\psmatrix[mnode=Circle,emnode=none,radius=3mm,colsep=1.2,rowsep=0.3,arrowscale=2.5]\n");
#else /* _VERSION_W */
  fprintf(pf, "$\\psmatrix[mnode=Circle,emnode=none,radius=3mm,colsep=0.8,rowsep=0.3,arrowscale=2.5]\n");
#endif /* _VERSION_W */
  fprintf(pf, "%%\n");

  int col = 0; /* Column count */
  nodeT *ino = malloc(sizeof *ino * (n+1));
#ifdef _VERSION_W
  costT *inoW = malloc(sizeof *inoW * (n+1));
#endif /* _VERSION_W */
  int *St = malloc(sizeof *St *n);
  int Stn = 0;

  elist bedges = newEList();
  elist cedges = newEList();

  i = branches;
  while(0 < i){
    i--;
    if(0 == f[B[i]].father){
      St[Stn] = B[i];
      Stn++; /* Push */
    }
  }

  ino[col] = 0; /* father for roots */
  col++;
  splayT t = f;
  while(0 < Stn){
    Stn--; /* Pop */
    nodeT u = St[Stn];
    nodeT ft = f[u].father;

    if(ino[col-1] != ft){
      fprintf(pf, "\\\\\n");
      while(ino[col - 1] != ft)
	col--;
      for(int i = 2; i < col; i++)
	putc('&', pf);
    }

    inorderRec(t, u, &ino[col], true);
#ifdef _VERSION_W
    inorderRecW(t, u, 0.0, &inoW[col], true);
#endif /* _VERSION_W */

    if(1 < col){
#ifdef _VERSION_W
      append(cedges, (struct edge){ino[col-1], ino[col], t[u].w});
#else /* _VERSION_W */
      append(cedges, (struct edge){ino[col-1], ino[col]});
#endif /* _VERSION_W */
      putc('&', pf);
    }

    for(i = 0; i < abs(f[u].size); i++){
      if(0 < i){
#ifdef _VERSION_W
      append(bedges, (struct edge){ino[col-1], ino[col], inoW[col-1]});
#else /* _VERSION_W */
      append(bedges, (struct edge){ino[col-1], ino[col]});
#endif /* _VERSION_W */
	putc('&', pf);
      }
      fprintf(pf, " [name=N%d]", ino[col]);
      fprintf(pf, " \\psrotateleft{\\psscalebox{-1 1}{%d}} ", ino[col]);
      /* Add stuff to stack */
      j = c[ino[col]] - 1;
      while(f[S[j]].father == ino[col]){
	St[Stn] = S[j];
	Stn++; /* Push */
	j--;
      }
      col++;
    }
  }
  fprintf(pf, "\n");

  printEL(bedges, false);
  freeEList(bedges);

  printEL(cedges, true);
  freeEList(cedges);

  fprintf(pf, "\\endpsmatrix$\n");
  fprintf(pf, "}}\n");

#ifdef _VERSION_W
  free(inoW);
#endif /* _VERSION_W */
  free(ino);
  free(St);
  free(c);
  free(F2S);
  free(S);
  free(B);
  fclose(pf);
}

static void depthRec(splayT t, nodeT v, int d, int *dp)
{
  if(0 != v){
    dp[v] = d;
    depthRec(t, t[v].above, d+1, dp);
    depthRec(t, t[v].below, d+1, dp);
  }
}

void printSplay(splayT t, unsigned int n, nodeT v, char* fname)
{
  pf = fopen(fname, "w");
  if(NULL == pf)
    exit(EXIT_FAILURE);

  while(0 != t[v].father) v = t[v].father; /* Print at root */

  /* Store node depths in array */
  int *dp = malloc(sizeof *dp * (n+1));
  depthRec(t, v, 0, dp);

  nodeT *ino = malloc(sizeof *ino * abs(t[v].size));

#ifdef _VERSION_W
  costT *inoW = malloc(sizeof *inoW * abs(t[v].size));
#endif /* _VERSION_W */

  inorderRec(t, v, ino, true);
#ifdef _VERSION_W
  inorderRecW(t, v, 0.0, inoW, true);
#endif /* _VERSION_W */

  for(int i = 0; i < abs(t[v].size); i++){
    nodeT v = ino[i];
#ifdef _VERSION_W
    fprintf(pf, "\\rput[tl](%f, %f)", i*2.2, -dp[v]*2.5);
#else /* _VERSION_W */
    fprintf(pf, "\\rput[tl](%f, %f)", i*1.8, -dp[v]*1.6);
#endif /* _VERSION_W */
    fprintf(pf, "{\\rnode{N%.2d}{\n", v);
    fprintf(pf, "\\psframebox[framearc=0.3]{\n");
    fprintf(pf, "\\begin{tabular}{r @{=} l}\n");
    fprintf(pf, "v & %d \\\\ \\hline\n", v);
    fprintf(pf, "size & %d \\\\ \n", t[v].size);
#ifdef _VERSION_W
    fprintf(pf, "\\hline\n");
    fprintf(pf, "d & %.2f \\\\ \\hline\n", t[v].d);
    fprintf(pf, "w & %.2f \\\\ \\hline\n", t[v].w);
    fprintf(pf, "min & %.2f\n", t[v].min);
#endif /* _VERSION_W */
    fprintf(pf, "\\end{tabular}}}}\n");
  }

  fprintf(pf, "%%\n");
  fprintf(pf, "\\SpecialCoor\n");
  for(int i = 0; i < abs(t[v].size); i++){
    nodeT v = ino[i];
    fprintf(pf, "\\cnodeput(N%.2d|0.000000, 1.500000)", v);
    fprintf(pf, "{C%.2d}", v);
    fprintf(pf, "{%d}\n", v);
  }

  fprintf(pf, "%%\n");
  fprintf(pf, "\\psset{arrows=-}\n");
  fprintf(pf, "\\psset{linewidth=0.08}\n");
  fprintf(pf, "\\psset{linestyle=dotted}\n");
  fprintf(pf, "\\psset{linecolor=gray}\n");

  for(int i = 0; i < abs(t[v].size); i++)
    fprintf(pf, "\\ncline{C%.2d}{N%.2d}\n", ino[i], ino[i]);

  fprintf(pf, "%%\n");
  fprintf(pf, "\\psset{linewidth=0.03}\n");
  fprintf(pf, "\\psset{linestyle=solid}\n");
  fprintf(pf, "\\psset{linecolor=black}\n");
  fprintf(pf, "\\psset{arrows=->}\n");
  fprintf(pf, "\\psset{arrowscale=2.5}\n");

  for(int i = 1; i < abs(t[v].size); i++){
    fprintf(pf, "\\ncline{C%.2d}{C%.2d} ", ino[i], ino[i-1]);
#ifdef _VERSION_W
    fprintf(pf, "\\nbput{%.2f}", inoW[i-1]);
#endif /* _VERSION_W */
  fprintf(pf, "\n");
  }

  fprintf(pf, "%%\n");
  fprintf(pf, "\\psset{offset=0.2}\n");

  for(int i = 0; i < abs(t[v].size); i++){
    nodeT v = ino[i];
    if(0 != t[v].father){
      fprintf(pf, "\\ncline{N%.2d}{N%.2d}\n", v, t[v].father);
      fprintf(pf, "\\ncput*{f}\n");
    }
    if(0 != t[v].above){
      fprintf(pf, "\\ncline{N%.2d}{N%.2d}\n", v, t[v].above);
      fprintf(pf, "\\ncput*{a}\n");
    }
    if(0 != t[v].below){
      fprintf(pf, "\\ncline{N%.2d}{N%.2d}\n", v, t[v].below);
      fprintf(pf, "\\ncput*{b}\n");
    }
  }

#ifdef _VERSION_W
  free(inoW);
#endif /* _VERSION_W */
  free(ino);
  free(dp);
  fclose(pf);
}
