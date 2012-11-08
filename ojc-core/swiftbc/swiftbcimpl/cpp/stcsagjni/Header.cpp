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
 * Implementation of APIs defined in com_sun_jbi_swiftbc_extensions_jni_Header.h.
 *
 * To be compiled into stcsagjni.dll.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/05/08 06:03:05 $
 */

#include "jni.h"
#include <iostream>
#include "com_sun_jbi_swiftbc_extensions_jni_Header.h"
#include "sagcontrol.hpp"
#include "stc_sag_jni_util.h"

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Header
 * Method:    alloc
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Header_alloc
  (JNIEnv * env, jobject)
{
  SagProcessControl::Header *self= new SagProcessControl::Header();

  jlong result = (jlong)self;

  return result;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Header
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Header_free
  (JNIEnv * env, jobject obj)
{
  jclass headerClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Header");
  jfieldID headerPeer = env->GetFieldID(headerClass, "peer", "J");

  jlong temp = env->GetLongField(obj, headerPeer);
  SagProcessControl::Header *self = (SagProcessControl::Header*)temp;

  delete self;
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Header
 * Method:    setUserId
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Header_setUserId
  (JNIEnv * env, jobject obj, jstring sUserId)
{
  jclass headerClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Header");
  jfieldID headerPeer = env->GetFieldID(headerClass, "peer", "J");

  jlong temp = env->GetLongField(obj, headerPeer);
  SagProcessControl::Header *self = (SagProcessControl::Header*)temp;

  const char *str = env->GetStringUTFChars(sUserId, NULL);

  if(str == NULL) {
    return;
  }
	
  self->setUserId(str);

  env->ReleaseStringUTFChars(sUserId, str);
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Header
 * Method:    getUserId
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Header_getUserId
  (JNIEnv * env, jobject obj)
{
  jclass headerClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Header");
  jfieldID headerPeer = env->GetFieldID(headerClass, "peer", "J");

  jlong temp = env->GetLongField(obj, headerPeer);
  SagProcessControl::Header *self = (SagProcessControl::Header*)temp;

  return env->NewStringUTF(self->getUserId().c_str());
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Header
 * Method:    setUserAuthentication
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Header_setUserAuthentication
  (JNIEnv * env, jobject obj, jstring sUserAuthentication)
{
  jclass headerClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Header");
  jfieldID headerPeer = env->GetFieldID(headerClass, "peer", "J");

  jlong temp = env->GetLongField(obj, headerPeer);
  SagProcessControl::Header *self = (SagProcessControl::Header*)temp;

  const char *str = env->GetStringUTFChars(sUserAuthentication, NULL);

  if(str == NULL) {
    return;
  }
	
  self->setUserAuthentication(str);

  env->ReleaseStringUTFChars(sUserAuthentication, str);
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Header
 * Method:    getUserAuthentication
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Header_getUserAuthentication
  (JNIEnv * env, jobject obj)
{
  jclass headerClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Header");
  jfieldID headerPeer = env->GetFieldID(headerClass, "peer", "J");

  jlong temp = env->GetLongField(obj, headerPeer);
  SagProcessControl::Header *self = (SagProcessControl::Header*)temp;

  return env->NewStringUTF(self->getUserAuthentication().c_str());
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Header
 * Method:    setLogApplicationId
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Header_setLogApplicationId
  (JNIEnv * env, jobject obj, jstring sLogApplicationId)
{
  jclass headerClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Header");
  jfieldID headerPeer = env->GetFieldID(headerClass, "peer", "J");

  jlong temp = env->GetLongField(obj, headerPeer);
  SagProcessControl::Header *self = (SagProcessControl::Header*)temp;

  const char *str = env->GetStringUTFChars(sLogApplicationId, NULL);

  if(str == NULL) {
    return;
  }
	
  self->setLogApplicationId(str);

  env->ReleaseStringUTFChars(sLogApplicationId, str);
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Header
 * Method:    getLogApplicationId
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Header_getLogApplicationId
  (JNIEnv * env, jobject obj)
{
  jclass headerClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Header");
  jfieldID headerPeer = env->GetFieldID(headerClass, "peer", "J");

  jlong temp = env->GetLongField(obj, headerPeer);
  SagProcessControl::Header *self = (SagProcessControl::Header*)temp;

  return env->NewStringUTF(self->getLogApplicationId().c_str());
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Header
 * Method:    setPrimitive
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Header_setPrimitive
  (JNIEnv * env, jobject obj, jstring sPrimitive)
{
  jclass headerClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Header");
  jfieldID headerPeer = env->GetFieldID(headerClass, "peer", "J");

  jlong temp = env->GetLongField(obj, headerPeer);
  SagProcessControl::Header *self = (SagProcessControl::Header*)temp;

  const char *str = env->GetStringUTFChars(sPrimitive, NULL);

  if(str == NULL) {
    return;
  }
	
  self->setPrimitive(str);

  env->ReleaseStringUTFChars(sPrimitive, str);
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Header
 * Method:    getPrimitive
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Header_getPrimitive
  (JNIEnv * env, jobject obj)
{
  jclass headerClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Header");
  jfieldID headerPeer = env->GetFieldID(headerClass, "peer", "J");

  jlong temp = env->GetLongField(obj, headerPeer);
  SagProcessControl::Header *self = (SagProcessControl::Header*)temp;

  return env->NewStringUTF(self->getPrimitive().c_str());
}



/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Header
 * Method:    getCompatMode
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Header_getCompatMode
  (JNIEnv * env, jobject obj)
{
  jclass headerClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Header");
  jfieldID headerPeer = env->GetFieldID(headerClass, "peer", "J");

  jlong temp = env->GetLongField(obj, headerPeer);
  SagProcessControl::Header *self = (SagProcessControl::Header*)temp;

  //return env->NewStringUTF(self->getCompatMode().c_str());
  return self->getCompatMode();
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Header
 * Method:    toString
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Header_toString
  (JNIEnv * env, jobject obj)
{
  jclass headerClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Header");
  jfieldID headerPeer = env->GetFieldID(headerClass, "peer", "J");

  jlong temp = env->GetLongField(obj, headerPeer);
  SagProcessControl::Header *self = (SagProcessControl::Header*)temp;

  return env->NewStringUTF(self->toString().c_str());
}
