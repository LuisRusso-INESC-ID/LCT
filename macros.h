/* Simple swap macro */
#define swapM(T, A, B) {T _mt = A; A = B; B = _mt;}

#define assertM(T, M) {/*@-boolops*/ assert((T) && (M)); /*@=boolops*/}

#ifdef _VERSION_W
#define assertW(T, M) {/*@-realcompare*/ assertM(T, M); /*@=realcompare*/}
#else /* _VERSION_W */
#define assertW(T, M) ;
#endif /* _VERSION_W */
