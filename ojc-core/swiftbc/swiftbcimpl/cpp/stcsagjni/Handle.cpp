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
 * Implementation of APIs defined in com_sun_jbi_swiftbc_extensions_jni_Handle.h.
 *
 * To be compiled into stcsagjni.dll.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/05/08 06:03:04 $
 */

#include "jni.h"
#include <iostream>
#include "com_sun_jbi_swiftbc_extensions_jni_Handle.h"
#include "sagcontrol.hpp"
#include "stc_sag_jni_util.h"

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_free
  (JNIEnv * env, jobject obj)
{
  jclass handleClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Handle");
  jfieldID handlePeer = env->GetFieldID(handleClass, "peer", "J");

  jlong temp = env->GetLongField(obj, handlePeer);
  SagProcessControl::Handle *self = (SagProcessControl::Handle*)temp;

  delete self;
}
// the new sag 5.0.0 stuff (putRequest, getResponse, getAnyResponse, etc) can be finalized/exposed 
// if they are required/requested

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    putRequest
 * Signature: (Lcom/sun/jbi/swiftbc/extensions/jni/Primitive;)J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_putRequest
  (JNIEnv * env, jobject obj, jobject primitive)
{
  jclass cls = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Handle");
  jfieldID FID_Handle_peer = env->GetFieldID(cls, "peer", "J");
  jlong temp = env->GetLongField(obj, FID_Handle_peer);
  SagProcessControl::Handle *self = (SagProcessControl::Handle*)temp;

  jclass Primitive_Class =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Primitive");
  jfieldID FID_Primitive_peer = env->GetFieldID(Primitive_Class, "peer", "J");
  temp = env->GetLongField(primitive, FID_Primitive_peer);
  SagProcessControl::Primitive *request = (SagProcessControl::Primitive*)temp;
	
  SagProcessControl::Token_t token;

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
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    getAnyResponse
 * Signature: (Lcom/sun/jbi/swiftbc/extensions/jni/Primitive;)J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_getAnyResponse__Lcom_sun_jbi_swiftbc_extensions_jni_Primitive_2
  (JNIEnv * env, jobject obj, jobject primitive)
{
  jclass cls = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Handle");
  jfieldID FID_Handle_peer = env->GetFieldID(cls, "peer", "J");
  jlong temp = env->GetLongField(obj, FID_Handle_peer);
  SagProcessControl::Handle *self = (SagProcessControl::Handle*)temp;

  jclass Primitive_Class =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Primitive");
  jfieldID FID_Primitive_peer = env->GetFieldID(Primitive_Class, "peer", "J");
  temp = env->GetLongField(primitive, FID_Primitive_peer);
  SagProcessControl::Primitive *response = (SagProcessControl::Primitive*)temp;
	
  SagProcessControl::Token_t token;

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
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    getResponse
 * Signature: (JLcom/sun/jbi/swiftbc/extensions/jni/Primitive;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_getResponse__JLcom_sun_jbi_swiftbc_extensions_jni_Primitive_2
  (JNIEnv *env, jobject obj, jlong token, jobject primitive)
{
  jclass cls = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Handle");
  jfieldID FID_Handle_peer = env->GetFieldID(cls, "peer", "J");
  jlong temp = env->GetLongField(obj, FID_Handle_peer);
  SagProcessControl::Handle *self = (SagProcessControl::Handle*)temp;

  jclass Primitive_Class =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Primitive");
  jfieldID FID_Primitive_peer = env->GetFieldID(Primitive_Class, "peer", "J");
  temp = env->GetLongField(primitive, FID_Primitive_peer);
  SagProcessControl::Primitive *response = (SagProcessControl::Primitive*)temp;

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
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    getAnyResponse
 * Signature: (JLcom/sun/jbi/swiftbc/extensions/jni/Primitive;)J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_getAnyResponse__JLcom_sun_jbi_swiftbc_extensions_jni_Primitive_2
  (JNIEnv *env, jobject obj, jlong msTimeOut, jobject primitive)
{
  jclass cls = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Handle");
  jfieldID FID_Handle_peer = env->GetFieldID(cls, "peer", "J");
  jlong temp = env->GetLongField(obj, FID_Handle_peer);
  SagProcessControl::Handle *self = (SagProcessControl::Handle*)temp;

  jclass Primitive_Class =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Primitive");
  jfieldID FID_Primitive_peer = env->GetFieldID(Primitive_Class, "peer", "J");
  temp = env->GetLongField(primitive, FID_Primitive_peer);
  SagProcessControl::Primitive *response = (SagProcessControl::Primitive*)temp;

  SagProcessControl::Token_t token;

  try
    {
      self->getAnyResponse((long)msTimeOut, token, *response);
    }
  catch (SagApp::ExcStatus &ex)
    {
      throwSagException(env, ex);
    }
	
  return token;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    getResponse
 * Signature: (JJLcom/sun/jbi/swiftbc/extensions/jni/Primitive;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_getResponse__JJLcom_sun_jbi_swiftbc_extensions_jni_Primitive_2
  (JNIEnv *env, jobject obj, jlong msTimeOut, jlong token, jobject primitive)
{
  jclass cls = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Handle");
  jfieldID FID_Handle_peer = env->GetFieldID(cls, "peer", "J");
  jlong temp = env->GetLongField(obj, FID_Handle_peer);
  SagProcessControl::Handle *self = (SagProcessControl::Handle*)temp;

  jclass Primitive_Class =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Primitive");
  jfieldID FID_Primitive_peer = env->GetFieldID(Primitive_Class, "peer", "J");
  temp = env->GetLongField(primitive, FID_Primitive_peer);
  SagProcessControl::Primitive *response = (SagProcessControl::Primitive*)temp;

  try
    {
      self->getResponse((long)msTimeOut, (long)token, *response);
    }
  catch (SagApp::ExcStatus &ex)
    {
      throwSagException(env, ex);
    }
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    call
 * Signature: (Lcom/sun/jbi/swiftbc/extensions/jni/Primitive;Lcom/sun/jbi/swiftbc/extensions/jni/Primitive;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_call
  (JNIEnv *env, jobject obj, jobject primitiveRequest, jobject primitiveResponse)
{
  jclass cls = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Handle");
  jfieldID FID_Handle_peer = env->GetFieldID(cls, "peer", "J");
  jlong temp = env->GetLongField(obj, FID_Handle_peer);
  SagProcessControl::Handle *self = (SagProcessControl::Handle*)temp;

  jclass Primitive_Class =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Primitive");
  jfieldID FID_Primitive_peer = env->GetFieldID(Primitive_Class, "peer", "J");
  temp = env->GetLongField(primitiveRequest, FID_Primitive_peer);
  SagProcessControl::Primitive *request = (SagProcessControl::Primitive*)temp;

  temp = env->GetLongField(primitiveResponse, FID_Primitive_peer);
  SagProcessControl::Primitive *response = (SagProcessControl::Primitive*)temp;

  try
    {
      self->call(*request, *response);	
    }
  catch (SagApp::ExcStatus &ex)
    {
      throwSagException(env, ex);
    }
}
