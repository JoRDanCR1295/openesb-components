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
 * Implementation of APIs defined in com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage.h.
 *
 * To be compiled into stcsagjni.dll.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/05/08 06:03:05 $
 */

#include "jni.h"
#include <iostream>

#include "com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage.h"

#include "sagapp.hpp"

#include "stc_sag_jni_util.h"

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage
 * Method:    alloc
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage_alloc
  (JNIEnv * env, jobject obj)
{
  SagApp::Message *self= new SagApp::Message();

  jlong result = (jlong)self;

  return result;
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage_free
  (JNIEnv * env, jobject obj)
{
  jclass messageClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage");
  jfieldID messagePeer = env->GetFieldID(messageClass, "peer", "J");

  jlong temp = env->GetLongField(obj, messagePeer);
  SagApp::Message *self = (SagApp::Message*)temp;

  delete self;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage
 * Method:    setLetter
 * Signature: (Lcom/sun/jbi/swiftbc/extensions/SwiftLetter;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage_setLetter
  (JNIEnv * env, jobject  obj, jobject jLetterObj)
{
  jclass messageClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage");
  jfieldID messagePeer = env->GetFieldID(messageClass, "peer", "J");

  jlong temp = env->GetLongField(obj, messagePeer);
  SagApp::Message *self = (SagApp::Message*)temp;

  jclass letterClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNILetter");
  jfieldID letterPeer = env->GetFieldID(letterClass, "peer", "J");

  jlong temp2 = env->GetLongField(jLetterObj, letterPeer);
  SagApp::Letter *myArg = (SagApp::Letter*)temp2;

  SagApp::Letter &myLetter = self->getLetter();
  myLetter = *myArg;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage
 * Method:    setEnvelope
 * Signature: (Lcom/sun/jbi/swiftbc/extensions/SwiftEnvelope;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage_setEnvelope
  (JNIEnv * env, jobject obj, jobject jEnvelopeObj)
{
  jclass messageClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage");
  jfieldID messagePeer = env->GetFieldID(messageClass, "peer", "J");

  jlong temp = env->GetLongField(obj, messagePeer);
  SagApp::Message *self = (SagApp::Message*)temp;

  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");

  jlong temp2 = env->GetLongField(jEnvelopeObj, envelopePeer);
  SagApp::Envelope *myArg = (SagApp::Envelope*)temp2;

  SagApp::Envelope &myEnvelope = self->getEnvelope();
  myEnvelope = *myArg;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage
 * Method:    getLetter
 * Signature: ()Lcom/sun/jbi/swiftbc/extensions/SwiftLetter;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage_getLetter
  (JNIEnv * env, jobject obj)
{
  jclass messageClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage");
  jfieldID messagePeer = env->GetFieldID(messageClass, "peer", "J");

  jlong temp = env->GetLongField(obj, messagePeer);
  SagApp::Message *self = (SagApp::Message*)temp;

  jclass letterClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNILetter");
  jfieldID letterPeer = env->GetFieldID(letterClass, "peer", "J");
  jmethodID letterConstructor = env->GetMethodID(letterClass, "<init>", "()V");

  jobject jLetterObj = 
    env->NewObject(letterClass, letterConstructor);

  if(jLetterObj == NULL) {
    return NULL;
  }
	
  temp = env->GetLongField(jLetterObj, letterPeer);
  SagApp::Letter *myArg = (SagApp::Letter*)temp;

  *myArg = self->getLetter();

  return jLetterObj;
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage
 * Method:    getEnvelope
 * Signature: ()Lcom/sun/jbi/swiftbc/extensions/SwiftEnvelope;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIMessage_getEnvelope
  (JNIEnv * env, jobject obj)
{
  jclass messageClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIMessage");
  jfieldID messagePeer = env->GetFieldID(messageClass, "peer", "J");

  jlong temp = env->GetLongField(obj, messagePeer);
  SagApp::Message *self = (SagApp::Message*)temp;

  jclass envelopeClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/SAGJNIEnvelope");
  jfieldID envelopePeer = env->GetFieldID(envelopeClass, "peer", "J");
  jmethodID envelopeConstructor = env->GetMethodID(envelopeClass, "<init>", "()V");

  jobject jEnvelopeObj = 
    env->NewObject(envelopeClass, envelopeConstructor);

  if(jEnvelopeObj == NULL)
    {
      return NULL;
    }
	
  temp = env->GetLongField(jEnvelopeObj, envelopePeer);
  SagApp::Envelope *myArg = (SagApp::Envelope*)temp;

  *myArg = self->getEnvelope();

  return jEnvelopeObj;
}
