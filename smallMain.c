#include <stdlib.h>
#include <stdio.h>

#include "LCT.h"
#include "splayPrinter.h"

int
main(void)
{
#if defined _EDGE_W || defined _VERTEX_W
  static unsigned int n = 10;
  LCT F = NULL;

  F = allocLCT(n);

  nameAndPrint(F, n);

  for(unsigned int i = 1; i < n; i++){
    Link(F, (nodeT)i, (nodeT)i+1);
    setCost(F, (nodeT)i, 0.5);
  }

  nameAndPrint(F, n);
  nameAndSplayP(F, n, 10);

  (void)cut(F, 5, 6);
  Link(F, 2, 6);
  setCost(F, 2, 0.3);
  (void)cut(F, 8, 9);
  Link(F, 2, 9);
  setCost(F, 2, 0.2);
  printLCT(F, n, "nofile");

  (void)cut(F, 6, 7);
  printLCT(F, n, "nofile");
  Link(F, 4, 8);
  setCost(F, 4, 0.2);
  printLCT(F, n, "nofile");

  /*@ +voidabstract*/
  free((void *)F);
  /*@ =voidabstract*/

#endif /* defined _EDGE_W || defined _VERTEX_W */
  return 0;
}
