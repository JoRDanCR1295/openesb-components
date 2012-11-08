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
 * Implementation of APIs defined in com_sun_jbi_swiftbc_extensions_jni_SAGJNILetter.h.
 *
 * To be compiled into stcsagjni.dll.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/05/08 06:03:05 $
 */

#include "jni.h"
#include <iostream>

#include "com_sun_jbi_swiftbc_extensions_jni_SAGJNILetter.h"

#include "sagapp.hpp"

#include "stc_sag_jni_util.h"

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNILetter
 * Method:    alloc
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNILetter_alloc
  (JNIEnv * env, jobject obj)
{
  SagApp::Letter *self= new SagApp::Letter();

  jlong result = (jlong)self;

  return result;
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNILetter
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNILetter_free
  (JNIEnv * env, jobject obj)
  {
  jclass letterClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNILetter");
  jfieldID letterPeer = env->GetFieldID(letterClass, "peer", "J");

  jlong temp = env->GetLongField(obj, letterPeer);
  SagApp::Letter *self = (SagApp::Letter*)temp;

  delete self;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNILetter
 * Method:    set
 * Signature: (ILjava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNILetter_set
  (JNIEnv * env, jobject obj, jint jLength, jstring jBuffer)
{
  jclass letterClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNILetter");
  jfieldID letterPeer = env->GetFieldID(letterClass, "peer", "J");

  jlong temp = env->GetLongField(obj, letterPeer);
  SagApp::Letter *self = (SagApp::Letter*)temp;

  const char *str = env->GetStringUTFChars(jBuffer, NULL);

  if(str == NULL) {
    return;
  }
	
  self->set((long)jLength, str);

  env->ReleaseStringUTFChars(jBuffer, str);
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNILetter
 * Method:    getNativeBuffer
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNILetter_getNativeBuffer
  (JNIEnv * env, jobject obj)
{
  jclass letterClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNILetter");
  jfieldID letterPeer = env->GetFieldID(letterClass, "peer", "J");

  jlong temp = env->GetLongField(obj, letterPeer);
  SagApp::Letter *self = (SagApp::Letter*)temp;

  const char *str = self->getBuffer();

  return env->NewStringUTF(str ? str : "");
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNILetter
 * Method:    getLength
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNILetter_getLength
  (JNIEnv * env, jobject obj)
{
  jclass letterClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNILetter");
  jfieldID letterPeer = env->GetFieldID(letterClass, "peer", "J");

  jlong temp = env->GetLongField(obj, letterPeer);
  SagApp::Letter *self = (SagApp::Letter*)temp;

  return (jint)self->getLength();
}

