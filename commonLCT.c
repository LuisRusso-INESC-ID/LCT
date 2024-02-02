/*@access LCT@*/
size_t sizeofStruct(void)
{ LCT f = NULL; return sizeof *f; }

LCT allocLCT(unsigned int n)
{
  LCT f = (LCT) calloc((size_t)(n+1), sizeofStruct());
  if(NULL == f) exit(EXIT_FAILURE);
  /*@-compdef*/ initLCT(f, n);
  return f; /*@=compdef*/
}

bool edgeQ(LCT f, nodeT u, nodeT v)
{ return getNode(f, u, -1) == v || getNode(f, v, -1) == u; }
