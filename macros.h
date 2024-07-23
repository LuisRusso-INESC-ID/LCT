/* Simple swap macro */
#define swapM(T, A, B) {T _mt = A; A = B; B = _mt;}

#define assertM(T, M) {/*@-boolops*/ assert((T) && (M)); /*@=boolops*/}

#if defined _EDGE_W || defined _VERTEX_W
#define assertW(T, M) {/*@-realcompare*/ assertM(T, M); /*@=realcompare*/}
#else /* defined _EDGE_W || defined _VERTEX_W */
#define assertW(T, M) ;
#endif /* defined _EDGE_W || defined _VERTEX_W */
