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
 * Implementation of APIs defined in com_sun_jbi_swiftbc_extensions_jni_HandleClient.h.
 *
 * To be compiled into stcsagjni.dll.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/05/08 06:03:05 $
 */

#include "jni.h"
#include <iostream>

#include "com_sun_jbi_swiftbc_extensions_jni_HandleClient.h"

#include "stc_sag_jni_util.h"

#include "sagapp.hpp"


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleClient
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleClient_free
  (JNIEnv * env, jobject obj)
{
  jclass handleClientClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleClient");
  jfieldID handleClientPeer = env->GetFieldID(handleClientClass, "peer", "J");

  jlong temp = env->GetLongField(obj, handleClientPeer);
  SagApp::HandleClient *self = (SagApp::HandleClient*)temp;

  delete self;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleClient
 * Method:    putRequest
 * Signature: (Lcom/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage;)J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleClient_putRequest
  (JNIEnv *env, jobject obj, jobject jRequestObj)
{
  jclass cls = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleClient");
  jfieldID FID_HandleClient_peer = env->GetFieldID(cls, "peer", "J");
  jlong temp = env->GetLongField(obj, FID_HandleClient_peer);
  SagApp::HandleClient *self = (SagApp::HandleClient*)temp;

  jclass Message_Class =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage");
  jfieldID FID_Message_peer = env->GetFieldID(Message_Class, "peer", "J");
  temp = env->GetLongField(jRequestObj, FID_Message_peer);
  SagApp::Message *request = (SagApp::Message*)temp;
	
  SagApp::Token_t token;

  try
    {
      self->putRequest(token, *request);
    }
  catch (SagApp::ExcStatus &ex)
    {
      throwSagException(env, ex);
      return NULL;
    }

  return token;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleClient
 * Method:    getAnyResponse
 * Signature: (Lcom/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage;)J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleClient_getAnyResponse__Lcom_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage_2
  (JNIEnv *env, jobject obj, jobject jResponseObj)
{
  jclass cls = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleClient");
  jfieldID FID_HandleClient_peer = env->GetFieldID(cls, "peer", "J");
  jlong temp = env->GetLongField(obj, FID_HandleClient_peer);
  SagApp::HandleClient *self = (SagApp::HandleClient*)temp;

  jclass Message_Class =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage");
  jfieldID FID_Message_peer = env->GetFieldID(Message_Class, "peer", "J");
  temp = env->GetLongField(jResponseObj, FID_Message_peer);
  SagApp::Message *response = (SagApp::Message*)temp;
	
  SagApp::Token_t token;

  try
    {
      self->getAnyResponse(token, *response);
    }
  catch (SagApp::ExcStatus &ex)
    {
      throwSagException(env, ex);
      return NULL;
    }

  return token;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleClient
 * Method:    getAnyResponse
 * Signature: (JLcom/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage;)J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleClient_getAnyResponse__JLcom_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage_2
  (JNIEnv *env, jobject obj, jlong timeout, jobject jResponseObj)
{
  jclass cls = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleClient");
  jfieldID FID_HandleClient_peer = env->GetFieldID(cls, "peer", "J");
  jlong temp = env->GetLongField(obj, FID_HandleClient_peer);
  SagApp::HandleClient *self = (SagApp::HandleClient*)temp;

  jclass Message_Class =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage");
  jfieldID FID_Message_peer = env->GetFieldID(Message_Class, "peer", "J");
  temp = env->GetLongField(jResponseObj, FID_Message_peer);
  SagApp::Message *response = (SagApp::Message*)temp;
	
  SagApp::Token_t token;

  try
    {
      self->getAnyResponse(token, *response);
    }
  catch (SagApp::ExcStatus &ex)
    {
      throwSagException(env, ex);
      return NULL;
    }

  return token;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleClient
 * Method:    getResponse
 * Signature: (JLcom/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleClient_getResponse__JLcom_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage_2
  (JNIEnv *env, jobject obj, jlong token, jobject jResponseObj)
{
  jclass cls = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleClient");
  jfieldID FID_HandleClient_peer = env->GetFieldID(cls, "peer", "J");
  jlong temp = env->GetLongField(obj, FID_HandleClient_peer);
  SagApp::HandleClient *self = (SagApp::HandleClient*)temp;

  jclass Message_Class =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage");
  jfieldID FID_Message_peer = env->GetFieldID(Message_Class, "peer", "J");
  temp = env->GetLongField(jResponseObj, FID_Message_peer);
  SagApp::Message *response = (SagApp::Message*)temp;

  try
    {
      self->getResponse((long)token, *response);
    }
  catch (SagApp::ExcStatus &ex)
    {
      throwSagException(env, ex);
    }
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleClient
 * Method:    getResponse
 * Signature: (JJLcom/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleClient_getResponse__JJLcom_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage_2
  (JNIEnv *env, jobject obj, jlong timeout, jlong token, jobject jResponseObj)
{
  jclass cls = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleClient");
  jfieldID FID_HandleClient_peer = env->GetFieldID(cls, "peer", "J");
  jlong temp = env->GetLongField(obj, FID_HandleClient_peer);
  SagApp::HandleClient *self = (SagApp::HandleClient*)temp;

  jclass Message_Class =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage");
  jfieldID FID_Message_peer = env->GetFieldID(Message_Class, "peer", "J");
  temp = env->GetLongField(jResponseObj, FID_Message_peer);
  SagApp::Message *response = (SagApp::Message*)temp;

  try
    {
      self->getResponse((long)timeout, (long)token, *response);
    }
  catch (SagApp::ExcStatus &ex)
    {
      throwSagException(env, ex);
    }
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleClient
 * Method:    call
 * Signature: (Lcom/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage;Lcom/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleClient_call
  (JNIEnv *env, jobject obj, jobject jRequestObj, jobject jResponseObj)
{
  jclass cls = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleClient");
  jfieldID FID_HandleClient_peer = env->GetFieldID(cls, "peer", "J");
  jlong temp = env->GetLongField(obj, FID_HandleClient_peer);
  SagApp::HandleClient *self = (SagApp::HandleClient*)temp;

  jclass Message_Class =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage");
  jfieldID FID_Message_peer = env->GetFieldID(Message_Class, "peer", "J");
  temp = env->GetLongField(jRequestObj, FID_Message_peer);
  SagApp::Message *request = (SagApp::Message*)temp;

  temp = env->GetLongField(jResponseObj, FID_Message_peer);
  SagApp::Message *response = (SagApp::Message*)temp;

  try
    {
      self->call(*request, *response);	
    }
  catch (SagApp::ExcStatus &ex)
    {
      throwSagException(env, ex);
    }
}




/*
 * Class:     com_stc_connector_sagadapter_jni_HandleClient
 * Method:    getAnyResponse
 * Signature: (JLcom/stc/connector/sagadapter/jni/SagApp::Message;)J
 */
JNIEXPORT jlong JNICALL 
Java_com_stc_connector_sagadapter_jni_HandleClient_getAnyResponse__JLcom_stc_connector_sagadapter_jni_Message_2
(JNIEnv *env, jobject obj, jlong timeout, jobject jResponseObj)
{
  jclass cls = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleClient");
  jfieldID FID_HandleClient_peer = env->GetFieldID(cls, "peer", "J");
  jlong temp = env->GetLongField(obj, FID_HandleClient_peer);
  SagApp::HandleClient *self = (SagApp::HandleClient*)temp;

  jclass Message_Class =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage");
  jfieldID FID_Message_peer = env->GetFieldID(Message_Class, "peer", "J");
  temp = env->GetLongField(jResponseObj, FID_Message_peer);
  SagApp::Message *response = (SagApp::Message*)temp;

  SagApp::Token_t token;

  try
    {
      self->getAnyResponse((long)timeout, token, *response);
    }
  catch (SagApp::ExcStatus &ex)
    {
      throwSagException(env, ex);
    }
	
  return token;
}


