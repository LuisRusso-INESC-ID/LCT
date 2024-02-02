#ifndef SPLAYPRINTER_H
#define SPLAYPRINTER_H

#include "LCT.h"
#include "splayTree.h"

/* Produces a graphviz representation of the LCT to the file with fname. */
void printLCT(LCT f, unsigned int n, char* fname);

void printSplay(splayT t, unsigned int n, nodeT v, char* fname);

void nameAndPrint(LCT F, unsigned int n);

void nameAndSplayP(splayT F, unsigned int n, nodeT v);

#endif /* SPLAYPRINTER_H */
