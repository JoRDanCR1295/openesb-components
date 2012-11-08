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
 * Implementation of APIs defined in com_sun_jbi_swiftbc_extensions_jni_HandleFactory.h.
 *
 * To be compiled into stcsagjni.dll.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/05/08 06:03:04 $
 */

#include "jni.h"
#include <iostream>
#include <stdlib.h>

#include "com_sun_jbi_swiftbc_extensions_jni_HandleFactory.h"

#include "sagapp.hpp"

#include "stc_sag_jni_util.h"
/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleFactory
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleFactory_free
  (JNIEnv * env, jobject obj)
{
  jclass handleFactoryClass =
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleFactory");
  jfieldID handleFactoryPeer = env->GetFieldID(handleFactoryClass,
					       "peer",
					       "J");

  jlong temp = env->GetLongField(obj, handleFactoryPeer);
  SagApp::HandleFactory *self = (SagApp::HandleFactory*)temp;

  delete self;

}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleFactory
 * Method:    newFactory
 * Signature: (Ljava/lang/String;)Lcom/sun/jbi/swiftbc/extensions/jni/HandleFactory;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleFactory_newFactory
  (JNIEnv * env, jclass cls, jstring jConfigFile)
{
  jclass handleFactoryClass = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleFactory");
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
    SagApp::HandleFactory *handleFactory =
      SagApp::HandleFactory::newFactory(a_nArg, attc_chzArg);
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
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleFactory
 * Method:    newHClient
 * Signature: (J)Lcom/sun/jbi/swiftbc/extensions/jni/HandleClient;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleFactory_newHClient
  (JNIEnv * env, jobject obj, jlong timeout)
{
  jclass handleFactoryClass = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleFactory");
  jfieldID handleFactoryPeer =
    env->GetFieldID(handleFactoryClass, "peer", "J");

  jlong temp = env->GetLongField(obj, handleFactoryPeer);
  SagApp::HandleFactory *self = (SagApp::HandleFactory*)temp;

  jclass handleClientClass = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleClient");
  jmethodID handleClientConstructor =
    env->GetMethodID(handleClientClass, "<init>", "()V");
  jfieldID handleClientPeer =
    env->GetFieldID(handleClientClass, "peer", "J");

  jobject clientObj = env->NewObject(handleClientClass,
                                     handleClientConstructor);

  if(clientObj == NULL) {
    return NULL;
  }

  try {
      SagApp::HandleClient *handleClient = self->newHClient((long)timeout);
      env->SetLongField(clientObj, handleClientPeer, (jlong)handleClient);
  } catch (SagApp::ExcStatus &ex) {
    throwSagException(env, ex);
    return NULL;
  }

  return clientObj;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleFactory
 * Method:    newHServer
 * Signature: (Ljava/lang/String;)Lcom/sun/jbi/swiftbc/extensions/jni/HandleServer;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleFactory_newHServer
  (JNIEnv * env, jobject obj, jstring jmessage_partner)
{
  jclass handleFactoryClass = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleFactory");
  jfieldID handleFactoryPeer =
    env->GetFieldID(handleFactoryClass, "peer", "J");

  jlong temp = env->GetLongField(obj, handleFactoryPeer);
  SagApp::HandleFactory *self = (SagApp::HandleFactory*)temp;

  const char *message_partner = env->GetStringUTFChars(jmessage_partner, NULL);

  jclass handleServerClass = 
    env->FindClass("com/sun/jbi/swiftbc/extensions/jni/HandleServer");
  jmethodID handleServerConstructor =
    env->GetMethodID(handleServerClass, "<init>", "()V");
  jfieldID handleServerPeer =
    env->GetFieldID(handleServerClass, "peer", "J");

  jobject serverObj =
    env->NewObject(handleServerClass, handleServerConstructor);

  if(serverObj == NULL) {
    env->ReleaseStringUTFChars(jmessage_partner, message_partner);
    return NULL;
  }

  try {
    SagApp::HandleServer *handleServer = self->newHServer(message_partner);
    env->SetLongField(serverObj, handleServerPeer, (jlong)handleServer);
  } catch (SagApp::ExcStatus &ex) {
    throwSagException(env, ex);
    env->ReleaseStringUTFChars(jmessage_partner, message_partner);
    return NULL;
  }

  env->ReleaseStringUTFChars(jmessage_partner, message_partner);
  return serverObj;
}


/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleFactory
 * Method:    setEnvVar
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleFactory_setEnvVar
  (JNIEnv * env, jclass cls, jstring envVar)
 {
  char *env_var = (char *)env->GetStringUTFChars(envVar, NULL);
  int stat = putenv(env_var);
  if (stat) {
    // Throw exception?
  }
  env->ReleaseStringUTFChars(envVar, env_var);
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleFactory
 * Method:    getEnvVar
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleFactory_getEnvVar
  (JNIEnv * env, jclass cls, jstring envVar)
{
  const char *env_var = env->GetStringUTFChars(envVar, NULL);
  char *ret = getenv(env_var);
  env->ReleaseStringUTFChars(envVar, env_var);
  return env->NewStringUTF(ret);
}
