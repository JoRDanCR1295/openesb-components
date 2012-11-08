/************************************************************************************
 *
 *   Copyright © 2006 Sun Microsystems, Inc., 4150 Network Circle, Santa Clara, 
 *   California 95054, U.S.A. All rights reserved. Sun Microsystems, Inc. has 
 *   intellectual property rights relating to technology embodied in the product 
 *   that is described in this document. In particular, and without limitation, 
 *   these intellectual property rights may include one or more of the U.S. patents 
 *   listed at http://www.sun.com/patents and one or more additional patents or 
 *   pending patent applications in the U.S. and in other countries. THIS PRODUCT 
 *   CONTAINS CONFIDENTIAL INFORMATION AND TRADE SECRETS OF SUN MICROSYSTEMS, INC. 
 *   USE, DISCLOSURE OR REPRODUCTION IS PROHIBITED WITHOUT THE PRIOR EXPRESS WRITTEN 
 *   PERMISSION OF SUN MICROSYSTEMS, INC.U.S. Government Rights - Commercial 
 *   software.  Government users are subject to the Sun Microsystems, Inc. standard 
 *   license agreement and applicable provisions of the FAR and its supplements.  
 *   Use is subject to license terms.  This distribution may include materials 
 *   developed by third parties. Sun, Sun Microsystems, the Sun logo, Java 
 *   Composite Application Platform Suite,  SeeBeyond, eGate, eInsight, eVision, eTL,
 *   eXchange, eView, eIndex, eBAM and  eWay are trademarks or registered trademarks of
 *   Sun Microsystems, Inc. in the U.S. and other countries. All SPARC trademarks are 
 *   used under license and are trademarks or registered trademarks of SPARC 
 *   International, Inc. in the U.S. and other countries. Products bearing SPARC 
 *   trademarks are based upon architecture developed by Sun Microsystems, Inc. 
 *   UNIX is a registered trademark in the U.S. and other countries, exclusively 
 *   licensed through X/Open Company, Ltd. This product is covered and controlled by 
 *   U.S. Export Control laws and may be subject to the export or import laws in 
 *   other countries.  Nuclear, missile, chemical biological weapons or nuclear 
 *   maritime end uses or end users, whether direct or indirect, are strictly 
 *   prohibited.  Export or reexport to countries subject to U.S. embargo or to 
 *   entities identified on U.S. export exclusion lists, including, but not limited 
 *   to, the denied persons and specially designated nationals lists is strictly 
 *   prohibited.
 *
 *************************************************************************************/
/**
 * Implementation of APIs defined in com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList.h.
 *
 * To be compiled into stcsagjni.dll.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/05/08 06:03:04 $
 */

#include "jni.h"
#include <iostream>

#include "com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList.h"

#include "sagapp.hpp"

#include "stc_sag_jni_util.h"

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList
 * Method:    alloc
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList_alloc
  (JNIEnv * env, jobject obj)
{
  SagApp::NamedItemList *self= new SagApp::NamedItemList();
  jlong result = (jlong)self;

  return result;
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList_free
  (JNIEnv * env, jobject obj)
{
  jclass namedItemListClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItemList");
  jfieldID peer = env->GetFieldID(namedItemListClass, "peer", "J");

  jlong temp = env->GetLongField(obj, peer);
  SagApp::NamedItemList *self = (SagApp::NamedItemList*)temp;

  delete self;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList
 * Method:    getNumberOfItem
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList_getNumberOfItem
  (JNIEnv * env, jobject obj)
{
  jclass namedItemListClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItemList");
  jfieldID peer = env->GetFieldID(namedItemListClass, "peer", "J");

  jlong temp = env->GetLongField(obj, peer);
  SagApp::NamedItemList *self = (SagApp::NamedItemList*)temp;

  return self->getNumberOfItems();
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList
 * Method:    getNamedItem
 * Signature: (I)Lcom/sun/jbi/swiftbc/extensions/SwiftNamedItem;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList_getNamedItem__I
  (JNIEnv *env, jobject obj, jint jItemIndex)
{
  jclass namedItemListClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItemList");
  jfieldID peer = env->GetFieldID(namedItemListClass, "peer", "J");

  jlong temp = env->GetLongField(obj, peer);
  SagApp::NamedItemList *self = (SagApp::NamedItemList*)temp;

  jclass namedItemClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItem");
  jmethodID namedItemConstructor =
    env->GetMethodID(namedItemClass, "<init>", "()V");
  jfieldID namedItemPeer = env->GetFieldID(namedItemClass, "peer", "J");

  jobject jItemObj = env->NewObject(namedItemClass, namedItemConstructor);

  if(jItemObj == NULL) {
    return NULL;
  }
	
  temp = env->GetLongField(jItemObj, namedItemPeer);
  SagApp::NamedItem *myItem = (SagApp::NamedItem*)temp;
  *myItem = self->getNamedItem(jItemIndex);

  return jItemObj;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList
 * Method:    getNamedItem
 * Signature: (Ljava/lang/String;)Lcom/sun/jbi/swiftbc/extensions/SwiftNamedItem;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList_getNamedItem__Ljava_lang_String_2
  (JNIEnv *env, jobject obj, jstring jItemName)
{
  jclass namedItemListClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItemList");
  jfieldID peer = env->GetFieldID(namedItemListClass, "peer", "J");

  jlong temp = env->GetLongField(obj, peer);
  SagApp::NamedItemList *self = (SagApp::NamedItemList*)temp;

  const char *str = env->GetStringUTFChars(jItemName, NULL);

  if(str == NULL) {
    env->ReleaseStringUTFChars(jItemName, str);
    return NULL;
  }

  jclass namedItemClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItem");
  jmethodID namedItemConstructor =
    env->GetMethodID(namedItemClass, "<init>", "()V");
  jfieldID namedItemPeer = env->GetFieldID(namedItemClass, "peer", "J");

  jobject jItemObj = env->NewObject(namedItemClass, namedItemConstructor);

  if(jItemObj == NULL) {
    env->ReleaseStringUTFChars(jItemName, str);
    return NULL;
  }

  temp = env->GetLongField(jItemObj, namedItemPeer);
  SagApp::NamedItem *myItem = (SagApp::NamedItem*)temp;
  *myItem = self->getNamedItem(str);

  env->ReleaseStringUTFChars(jItemName, str);

  return jItemObj;
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList
 * Method:    insert
 * Signature: (Lcom/sun/jbi/swiftbc/extensions/SwiftNamedItem;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList_insert
  (JNIEnv *env, jobject obj, jobject jNamedItemObj)
{
  jclass namedItemListClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItemList");
  jfieldID peer = env->GetFieldID(namedItemListClass, "peer", "J");

  jlong temp = env->GetLongField(obj, peer);
  SagApp::NamedItemList *self = (SagApp::NamedItemList*)temp;

  jclass namedItemClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItem");
  jfieldID namedItemPeer = env->GetFieldID(namedItemClass, "peer", "J");

  temp = env->GetLongField(jNamedItemObj, namedItemPeer);
  SagApp::NamedItem *myItem = (SagApp::NamedItem*)temp;

  self->insert(*myItem);
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList
 * Method:    destroy
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList_destroy__I
  (JNIEnv *env, jobject obj, jint jItemIndex)
{
  jclass namedItemListClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItemList");
  jfieldID peer = env->GetFieldID(namedItemListClass, "peer", "J");

  jlong temp = env->GetLongField(obj, peer);
  SagApp::NamedItemList *self = (SagApp::NamedItemList*)temp;

  self->destroy(jItemIndex);
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList
 * Method:    destroy
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItemList_destroy__Ljava_lang_String_2
  (JNIEnv *env, jobject obj, jstring jItemName)
{
  jclass namedItemListClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItemList");
  jfieldID peer = env->GetFieldID(namedItemListClass, "peer", "J");

  jlong temp = env->GetLongField(obj, peer);
  SagApp::NamedItemList *self = (SagApp::NamedItemList*)temp;

  const char *str = env->GetStringUTFChars(jItemName, NULL);

  self->destroy(str);

  env->ReleaseStringUTFChars(jItemName, str);
}
