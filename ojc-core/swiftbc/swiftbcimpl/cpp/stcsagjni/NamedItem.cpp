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
 * Implementation of APIs defined in com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem.h.
 *
 * To be compiled into stcsagjni.dll.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/05/08 06:03:04 $
 */

#include "jni.h"
#include <iostream>

#include "com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem.h"

#include "sagapp.hpp"

#include "stc_sag_jni_util.h"
/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem
 * Method:    alloc
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem_alloc
  (JNIEnv * env, jobject obj)
{
  SagApp::NamedItem *self= new SagApp::NamedItem();
  jlong result = (jlong)self;

  return result;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem_free
  (JNIEnv * env, jobject obj)
{
  jclass namedItemClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItem");
  jfieldID namedItemPeer = env->GetFieldID(namedItemClass, "peer", "J");

  jlong temp = env->GetLongField(obj, namedItemPeer);
  SagApp::NamedItem *self = (SagApp::NamedItem*)temp;

  delete self;
  
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem
 * Method:    setItemName
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem_setItemName
  (JNIEnv *env, jobject obj, jstring jName)
{
  jclass namedItemClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItem");
  jfieldID namedItemPeer = env->GetFieldID(namedItemClass, "peer", "J");

  jlong temp = env->GetLongField(obj, namedItemPeer);
  SagApp::NamedItem *self = (SagApp::NamedItem*)temp;

  const char *str = env->GetStringUTFChars(jName, NULL);

  if(str == NULL) {
    return;
  }

  self->setItemName(str);
	
  env->ReleaseStringUTFChars(jName, str);
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem
 * Method:    setItemValue
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem_setItemValue
  (JNIEnv *env, jobject obj, jstring jValue)
{
  jclass namedItemClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItem");
  jfieldID namedItemPeer = env->GetFieldID(namedItemClass, "peer", "J");

  jlong temp = env->GetLongField(obj, namedItemPeer);
  SagApp::NamedItem *self = (SagApp::NamedItem*)temp;

  const char *str = env->GetStringUTFChars(jValue, NULL);

  if(str == NULL) {
    return;
  }

  self->setItemValue(str);
	
  env->ReleaseStringUTFChars(jValue, str);
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem
 * Method:    setNamedItemList
 * Signature: (Lcom/sun/jbi/swiftbc/extensions/SwiftNamedItemList;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem_setNamedItemList
  (JNIEnv *env, jobject obj, jobject jListObj)
{
  jclass namedItemClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItem");
  jfieldID namedItemPeer = env->GetFieldID(namedItemClass, "peer", "J");

  jlong temp = env->GetLongField(obj, namedItemPeer);
  SagApp::NamedItem *self = (SagApp::NamedItem*)temp;
	
  jclass namedItemListClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/SwiftNamedItemList");
  jfieldID namedItemListPeer =
    env->GetFieldID(namedItemListClass, "peer", "J");

  temp = env->GetLongField(jListObj, namedItemListPeer);
  SagApp::NamedItemList *myItem = (SagApp::NamedItemList*)temp;
  self->setItemList(*myItem);
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem
 * Method:    getItemName
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem_getItemName
  (JNIEnv * env, jobject obj)
{
  jclass namedItemClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItem");
  jfieldID namedItemPeer = env->GetFieldID(namedItemClass, "peer", "J");

  jlong temp = env->GetLongField(obj, namedItemPeer);
  SagApp::NamedItem *self = (SagApp::NamedItem*)temp;

  return env->NewStringUTF(self->getItemName().c_str());
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem
 * Method:    getItemValue
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem_getItemValue
  (JNIEnv * env, jobject obj)
{
  jclass namedItemClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItem");
  jfieldID namedItemPeer = env->GetFieldID(namedItemClass, "peer", "J");

  jlong temp = env->GetLongField(obj, namedItemPeer);
  SagApp::NamedItem *self = (SagApp::NamedItem*)temp;

  return env->NewStringUTF(self->getItemValue().c_str());
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem
 * Method:    getNamedItemList
 * Signature: ()Lcom/sun/jbi/swiftbc/extensions/SwiftNamedItemList;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem_getNamedItemList
  (JNIEnv * env, jobject obj)
{
  jclass namedItemClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItem");
  jfieldID namedItemPeer = env->GetFieldID(namedItemClass, "peer", "J");

  jlong temp = env->GetLongField(obj, namedItemPeer);
  SagApp::NamedItem *self = (SagApp::NamedItem*)temp;

  jclass namedItemListClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/SwiftNamedItemList");
  jfieldID namedItemListPeer =
    env->GetFieldID(namedItemListClass, "peer", "J");
  jmethodID namedItemListConstructor =
    env->GetMethodID(namedItemListClass, "<init>", "()V");


  jobject jListObj =
        env->NewObject(namedItemListClass, namedItemListConstructor);

  if(jListObj == NULL) {
      return NULL;
  }
	
  temp = env->GetLongField(jListObj, namedItemListPeer);
  SagApp::NamedItemList *list = (SagApp::NamedItemList*)temp;
  *list = self->getNamedItemList();

  return jListObj;
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem
 * Method:    isItemList
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNINamedItem_isItemList
  (JNIEnv * env, jobject obj)
{
  jclass namedItemClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItem");
  jfieldID namedItemPeer = env->GetFieldID(namedItemClass, "peer", "J");

  jlong temp = env->GetLongField(obj, namedItemPeer);
  SagApp::NamedItem *self = (SagApp::NamedItem*)temp;

  return self->isItemList();
}

