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
 * Implementation of APIs defined in com_sun_jbi_swiftbc_extensions_jni_ProcessControlHandleFactory.h.
 *
 * To be compiled into stcsagjni.dll.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/05/08 06:03:05 $
 */

#include "jni.h"

#include "com_sun_jbi_swiftbc_extensions_jni_ProcessControlHandleFactory.h"

#include "sagcontrol.hpp"
#include "stc_sag_jni_util.h"
/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_ProcessControlHandleFactory
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_ProcessControlHandleFactory_free
  (JNIEnv * env, jobject obj);

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_ProcessControlHandleFactory
 * Method:    newFactory
 * Signature: (Ljava/lang/String;)Lcom/sun/jbi/swiftbc/extensions/jni/ProcessControlHandleFactory;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_ProcessControlHandleFactory_newFactory
  (JNIEnv *env, jclass cls, jstring jConfigFile)
{
  jclass handleFactoryClass = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/ProcessControlHandleFactory");
  jfieldID handleFactoryPeer =
    env->GetFieldID(handleFactoryClass, "peer", "J");
  jmethodID handleFactoryConstructor = env->GetMethodID(cls, "<init>", "()V");

  const char *configFile = env->GetStringUTFChars(jConfigFile, NULL);

  jobject obj = env->NewObject(cls, handleFactoryConstructor);

  if(obj == NULL) {
    env->ReleaseStringUTFChars(jConfigFile, configFile);
    return NULL;
  }

  int a_nArg = 2;
  const char *attc_chzArg[2];
  attc_chzArg[0] = "-SagTransport";
  attc_chzArg[1] = configFile;

  try {
    SagProcessControl::HandleFactory *handleFactory =
      SagProcessControl::HandleFactory::newFactory(a_nArg, attc_chzArg);
    env->SetLongField(obj, handleFactoryPeer, (jlong)handleFactory);
  } catch (SagApp::ExcStatus &ex) {
    throwSagException(env, ex);
    env->ReleaseStringUTFChars(jConfigFile, configFile);
    return NULL;
  }

  env->ReleaseStringUTFChars(jConfigFile, configFile);
  return obj;
}
/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_ProcessControlHandleFactory
 * Method:    newHandle
 * Signature: ()Lcom/sun/jbi/swiftbc/extensions/jni/Handle;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_ProcessControlHandleFactory_newHandle
  (JNIEnv * env, jobject obj)
{
  jclass handleFactoryClass = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/ProcessControlHandleFactory");
  jfieldID handleFactoryPeer =
    env->GetFieldID(handleFactoryClass, "peer", "J");

  jlong temp = env->GetLongField(obj, handleFactoryPeer);
  SagProcessControl::HandleFactory *self =
    (SagProcessControl::HandleFactory*)temp;

  jclass handleClass = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/Handle");
  jmethodID handleConstructor =
    env->GetMethodID(handleClass, "<init>", "()V");
  jfieldID handlePeer =
    env->GetFieldID(handleClass, "peer", "J");

  jobject clientObj = env->NewObject(handleClass,
                                     handleConstructor);

  if(clientObj == NULL) {
    return NULL;
  }

  try {
      SagProcessControl::Handle *handle = self->newHandle();
      env->SetLongField(clientObj, handlePeer, (jlong)handle);
  } catch (SagApp::ExcStatus &ex) {
    throwSagException(env, ex);
    return NULL;
  }

  return clientObj;

}
