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
 * Implementation of APIs defined in com_sun_jbi_swiftbc_extensions_jni_Primitive.h.
 *
 * To be compiled into stcsagjni.dll.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/05/08 06:03:04 $
 */

#include "jni.h"
#include <iostream>
#include "com_sun_jbi_swiftbc_extensions_jni_Primitive.h"
#include "sagcontrol.hpp"
#include "stc_sag_jni_util.h"

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Primitive
 * Method:    alloc
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Primitive_alloc
  (JNIEnv * env, jobject obj)
{
  SagProcessControl::Primitive *self= new SagProcessControl::Primitive();

  jlong result = (jlong)self;

  return result;
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Primitive
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Primitive_free
  (JNIEnv * env, jobject obj)
{
  jclass primitiveClass =
    env->FindClass("com/stc/connector/sagadapter/jni/Primitive");
  jfieldID primitivePeer = env->GetFieldID(primitiveClass, "peer", "J");

  jlong temp = env->GetLongField(obj, primitivePeer);
  SagProcessControl::Primitive *self = (SagProcessControl::Primitive*)temp;

  delete self;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Primitive
 * Method:    getHeader
 * Signature: ()Lcom/sun/jbi/swiftbc/extensions/jni/Header;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Primitive_getHeader
  (JNIEnv * env, jobject obj)
{
  jclass primitiveClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Primitive");
  jfieldID primitivePeer = env->GetFieldID(primitiveClass, "peer", "J");

  jlong temp = env->GetLongField(obj, primitivePeer);
  SagProcessControl::Primitive *self = (SagProcessControl::Primitive*)temp;

  jclass headerClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Header");
  jfieldID headerPeer = env->GetFieldID(headerClass, "peer", "J");
  jmethodID headerConstructor = env->GetMethodID(headerClass, "<init>", "()V");

  jobject jHeaderObj = 
    env->NewObject(headerClass, headerConstructor);

  if(jHeaderObj == NULL) {
    return NULL;
  }
	
  temp = env->GetLongField(jHeaderObj, headerPeer);
  SagProcessControl::Header *myArg = (SagProcessControl::Header*)temp;

  *myArg = self->getHeader();

  return jHeaderObj;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Primitive
 * Method:    getText
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Primitive_getText
  (JNIEnv * env, jobject obj)
{
  jclass primitiveClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Primitive");
  jfieldID primitivePeer = env->GetFieldID(primitiveClass, "peer", "J");

  jlong temp = env->GetLongField(obj, primitivePeer);
  SagProcessControl::Primitive *self = (SagProcessControl::Primitive*)temp;

  return env->NewStringUTF(self->getText().c_str());
}
