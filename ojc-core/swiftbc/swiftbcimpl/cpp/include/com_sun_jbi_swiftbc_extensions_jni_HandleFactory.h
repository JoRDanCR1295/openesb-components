/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class com_sun_jbi_swiftbc_extensions_jni_HandleFactory */

#ifndef _Included_com_sun_jbi_swiftbc_extensions_jni_HandleFactory
#define _Included_com_sun_jbi_swiftbc_extensions_jni_HandleFactory
#ifdef __cplusplus
extern "C" {
#endif
/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleFactory
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleFactory_free
  (JNIEnv *, jobject);

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleFactory
 * Method:    newFactory
 * Signature: (Ljava/lang/String;)Lcom/sun/jbi/swiftbc/extensions/jni/HandleFactory;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleFactory_newFactory
  (JNIEnv *, jclass, jstring);

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleFactory
 * Method:    newHClient
 * Signature: (J)Lcom/sun/jbi/swiftbc/extensions/jni/HandleClient;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleFactory_newHClient
  (JNIEnv *, jobject, jlong);

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleFactory
 * Method:    newHServer
 * Signature: (Ljava/lang/String;)Lcom/sun/jbi/swiftbc/extensions/jni/HandleServer;
 */
JNIEXPORT jobject JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleFactory_newHServer
  (JNIEnv *, jobject, jstring);

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleFactory
 * Method:    setEnvVar
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleFactory_setEnvVar
  (JNIEnv *, jclass, jstring);

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_HandleFactory
 * Method:    getEnvVar
 * Signature: (Ljava/lang/String;)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_HandleFactory_getEnvVar
  (JNIEnv *, jclass, jstring);

#ifdef __cplusplus
}
#endif
#endif
