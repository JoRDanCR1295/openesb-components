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
 * Implementation of APIs defined in com_stc_connector_sagadapter_jni_Envelope.h.
 *
 * To be compiled into stcsagjni.dll.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/05/08 06:03:05 $
 */

#include "jni.h"
#include <iostream>

#include "com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope.h"

#include "sagapp.hpp"

#include "stc_sag_jni_util.h"

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    alloc
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_alloc
  (JNIEnv * env, jobject obj)
{
  SagApp::Envelope *self= new SagApp::Envelope();

  jlong result = (jlong)self;

  return result;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_free
  (JNIEnv * env, jobject obj)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  delete self;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    setApplicationId
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_setApplicationId
  (JNIEnv * env, jobject obj, jstring jValue)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  const char *str = env->GetStringUTFChars(jValue, NULL);

  if(str == NULL) {
    return;
  }
	
  self->setApplicationId(str);

  env->ReleaseStringUTFChars(jValue, str);
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    setContextId
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_setContextId
  (JNIEnv * env, jobject obj, jstring jValue)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  const char *str = env->GetStringUTFChars(jValue, NULL);

  if(str == NULL) {
    return;
  }

  self->setContextId(str);
	
  env->ReleaseStringUTFChars(jValue, str);
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    setMsgFormat
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_setMsgFormat
  (JNIEnv * env, jobject obj, jstring jValue)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  const char *str = env->GetStringUTFChars(jValue, NULL);

  if(str == NULL) {
    return;
  }
	
  self->setMsgFormat(str);

  env->ReleaseStringUTFChars(jValue, str);
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    setSender
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_setSender
  (JNIEnv * env, jobject obj, jstring jValue)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  const char *str = env->GetStringUTFChars(jValue, NULL);

  if(str == NULL) {
    return;
  }
	
  self->setSender(str);

  env->ReleaseStringUTFChars(jValue, str);
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    setSenderAuth
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_setSenderAuth
  (JNIEnv * env, jobject obj, jstring jValue)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  const char *str = env->GetStringUTFChars(jValue, NULL);

  if(str == NULL) {
    return;
  }
	
  self->setSenderAuth(str);

  env->ReleaseStringUTFChars(jValue, str);
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    setReceiver
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_setReceiver
  (JNIEnv * env, jobject obj, jstring jValue)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  const char *str = env->GetStringUTFChars(jValue, NULL);

  if(str == NULL) {
    return;
  }
	
  self->setReceiver(str);

  env->ReleaseStringUTFChars(jValue, str);
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    setLocalAuth
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_setLocalAuth
  (JNIEnv * env, jobject obj, jstring jValue)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  const char *str = env->GetStringUTFChars(jValue, NULL);

  if(str == NULL) {
    return;
  }
	
  self->setLocalAuth(str);

  env->ReleaseStringUTFChars(jValue, str);
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    setApplicationStatus
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_setApplicationStatus
  (JNIEnv * env, jobject obj, jstring jValue)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  const char *str = env->GetStringUTFChars(jValue, NULL);

  if(str == NULL) {
    return;
  }
	
  self->setApplicationStatus(str);

  env->ReleaseStringUTFChars(jValue, str);
}
/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    getApplicationId
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_getApplicationId
  (JNIEnv * env, jobject obj)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  return env->NewStringUTF(self->getApplicationId().c_str());
}
/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    getContextId
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_getContextId
  (JNIEnv * env, jobject obj)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  return env->NewStringUTF(self->getContextId().c_str());
}
/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    getMsgFormat
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_getMsgFormat
  (JNIEnv * env, jobject obj)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");
  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  return env->NewStringUTF(self->getMsgFormat().c_str());
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    getSender
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_getSender
  (JNIEnv * env, jobject obj)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  return env->NewStringUTF(self->getSender().c_str());
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    getSenderAuth
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_getSenderAuth
  (JNIEnv * env, jobject obj)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  return env->NewStringUTF(self->getSenderAuth().c_str());
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    getReceiver
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_getReceiver
  (JNIEnv * env, jobject obj)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  return env->NewStringUTF(self->getReceiver().c_str());
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    getMsgRef
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_getMsgRef
  (JNIEnv * env, jobject obj)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  return env->NewStringUTF(self->getMsgRef().c_str());
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    getLocalAuth
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_getLocalAuth
  (JNIEnv * env, jobject obj){
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  return env->NewStringUTF(self->getLocalAuth().c_str());
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    getApplicationStatus
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_getApplicationStatus
  (JNIEnv * env, jobject obj){
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  return env->NewStringUTF(self->getApplicationStatus().c_str());
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    getNamedItemList
 * Signature: ()Lcom/sun/jbi/swiftbc/extensions/SwiftNamedItemList;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_getNamedItemList
  (JNIEnv * env, jobject obj)
{
  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp = env->GetLongField(obj, envelopePeer);
  SagApp::Envelope *self = (SagApp::Envelope*)temp;

  jclass namedItemListClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNINamedItemList");
  jmethodID namedItemListConstructor =
    env->GetMethodID(namedItemListClass, "<init>", "()V");
  jfieldID namedItemListPeer =
    env->GetFieldID(namedItemListClass, "peer", "J");

  jobject jNamedItemListObj = 
    env->NewObject(namedItemListClass, namedItemListConstructor);

  if(jNamedItemListObj == NULL) {
    return NULL;
  }
	
  temp = env->GetLongField(jNamedItemListObj, namedItemListPeer);
  SagApp::NamedItemList *myItem = (SagApp::NamedItemList*)temp;
  *myItem = self->getNamedItemList();

  return jNamedItemListObj;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    getRoutingListRequest
 * Signature: ()Lcom/sun/jbi/swiftbc/extensions/SwiftRoutingList;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_getRoutingListRequest
 (JNIEnv *env, jobject obj)
{
    // deprecated - not implemented
    // todo if it is required
    return NULL;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope
 * Method:    getRoutingListResponse
 * Signature: ()Lcom/sun/jbi/swiftbc/extensions/SwiftRoutingList;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIEnvelope_getRoutingListResponse
  (JNIEnv *env, jobject obj)
{
    // deprecated - not implemented
    // todo if it is required
    return NULL;
}

/*
JNIEXPORT void JNICALL
Java_com_stc_connector_sagadapter_jni_Envelope_throwEx(JNIEnv * envenv, jobject obj)
{
  jclass cls = env->FindClass("com/sun/jbi/swiftbc/extensions/jni/ExcStatus");
	
  if(cls == NULL) {
    return;
  }

  env->ThrowNew(cls, "thrown from C code");
  env->DeleteLocalRef(cls);
}
*/
