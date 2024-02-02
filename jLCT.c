#include <jni.h>
#include <assert.h>
#include <stdlib.h>

#include "macros.h"
#include "LCT.h"

static LCT getLCT(JNIEnv *env, jobject this)
{
  jclass cls = (*env)->GetObjectClass(env, this);
  jfieldID fid = (*env)->GetFieldID(env, cls, "cLCT", "J");
  assertM(NULL != fid, "Failed to find field.");
  LCT p = (LCT) (*env)->GetLongField(env, this, fid);

  return p;
}

JNIEXPORT void JNICALL Java_jLCT_jinitLCT
(JNIEnv *env, jobject this, jint n)
{ initLCT(getLCT(env, this), n); }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"

JNIEXPORT jint JNICALL Java_jLCT_jsizeofStruct
(JNIEnv *env, jobject this)
{ return (jint)sizeofStruct(); }

JNIEXPORT jlong JNICALL Java_jLCT_jallocLCT
(JNIEnv *env, jobject this, jint n)
{
  LCT p = allocLCT((unsigned int) n);

  return (jlong)p;
}

JNIEXPORT void JNICALL Java_jLCT_jfreeLCT
  (JNIEnv *env, jobject this)
{
  /* printf("Calling LCT free in java\n"); */

  free(getLCT(env, this));
}

#pragma GCC diagnostic pop /*-Wunused-parameter*/

JNIEXPORT jint JNICALL Java_jLCT_jAccess
(JNIEnv *env, jobject this, jint v)
{ return (jint)Access(getLCT(env, this), v); }

JNIEXPORT jint JNICALL Java_jLCT_jgetNode
(JNIEnv *env, jobject this, jint v, jint d)
{ return (jint)getNode(getLCT(env, this), v, d); }

JNIEXPORT jboolean JNICALL Java_jLCT_jedgeQ
(JNIEnv *env, jobject this, jint u, jint v)
{ return (jboolean)edgeQ(getLCT(env, this), u, v); }

JNIEXPORT jint JNICALL Java_jLCT_jcut
(JNIEnv *env, jobject this, jint u, jint v)
{ return (jint)cut(getLCT(env, this), u, v); }

JNIEXPORT void JNICALL Java_jLCT_jreRoot
(JNIEnv *env, jobject this, jint v)
{ reRoot(getLCT(env, this), v); }

JNIEXPORT jint JNICALL Java_jLCT_jLCA
(JNIEnv *env, jobject this, jint u, jint v)
{ return (jint)LCA(getLCT(env, this), u, v); }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wreturn-type"

JNIEXPORT void JNICALL Java_jLCT_jLink
  (JNIEnv *env, jobject this, jint u, jint v)
{
#ifndef _VERSION_W
  Link(getLCT(env, this), u, v);
#endif /* _VERSION_W */
}

JNIEXPORT void JNICALL Java_jLCT_jLinkW
(JNIEnv *env, jobject this, jint u, jint v, jdouble w)
{
#ifdef _VERSION_W
  LinkW(getLCT(env, this), u, v, w);
#endif /* _VERSION_W */
}

JNIEXPORT jdouble JNICALL Java_jLCT_jgetCost
(JNIEnv *env, jobject this, jint u, jint v)
{
#ifdef _VERSION_W
  return (jdouble)getCost(getLCT(env, this), u, v);
#endif /* _VERSION_W */
}

JNIEXPORT void JNICALL Java_jLCT_jupdate
(JNIEnv *env, jobject this, jint v, jdouble w)
{
#ifdef _VERSION_W
  update(getLCT(env, this), v, w);
#endif /* _VERSION_W */
}

JNIEXPORT jint JNICALL Java_jLCT_jgetMin
(JNIEnv *env, jobject this, jint v)
{
#ifdef _VERSION_W
  return (jint)getMin(getLCT(env, this), v);
#endif /* _VERSION_W */
}
#pragma GCC diagnostic pop /*-Wreturn*/
#pragma GCC diagnostic pop /*-Wunused-parameter*/
