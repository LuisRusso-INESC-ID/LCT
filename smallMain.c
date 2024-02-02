#include <stdlib.h>
#include <stdio.h>

#include "LCT.h"
#include "splayPrinter.h"

int
main(void)
{
#ifdef _VERSION_W
  static unsigned int n = 10;
  LCT F = NULL;

  F = allocLCT(n);

  nameAndPrint(F, n);

  for(unsigned int i = 1; i < n; i++)
    LinkW(F, (nodeT)i, (nodeT)i+1, 0.5);

  nameAndPrint(F, n);
  nameAndSplayP(F, n, 10);

  (void)cut(F, 5, 6);
  LinkW(F, 2, 6, 0.3);
  (void)cut(F, 8, 9);
  LinkW(F, 2, 9, 0.2);
  printLCT(F, n, "nofile");

  (void)cut(F, 6, 7);
  printLCT(F, n, "nofile");
  LinkW(F, 4, 8, 0.2);
  printLCT(F, n, "nofile");

  /*@ +voidabstract*/
  free((void *)F);
  /*@ =voidabstract*/

#endif /* _VERSION_W */
  return 0;
}
