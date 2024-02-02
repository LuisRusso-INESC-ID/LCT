#ifndef SPLAYTREE_H
#define SPLAYTREE_H

#include "LCT.h"

typedef /*@abstract@*/ struct LCT *splayT;

enum childClass { above, below, across };

/* Tree preserving functions. */

/* Finds v's class. */
/*@-exportlocal*/
enum childClass classify(splayT t, nodeT v);
/*@=exportlocal*/

/* True when the above below order is reversed. */
bool above2belowQ(splayT t, nodeT v);

/* Return nodes inorder. */
/*@-exportlocal*/
int inorderRec(splayT t, nodeT v, nodeT *I, bool ab);
#ifdef _VERSION_W
int inorderRecW(splayT t, nodeT v, costT d, costT *I, bool ab);
#endif /* _VERSION_W */
/*@=exportlocal*/

/* Call these functions whenever data in the struct changes. */
void setBelow(splayT t, nodeT u, nodeT v);
void setMin(splayT t, nodeT v);

/* Reverse above and below order. */
void flipA2B(splayT f, nodeT v);
void splay(splayT f, nodeT v);
nodeT getNodeOnSplay(splayT , nodeT v, int d);

#endif /* SPLAYTREE_H */
