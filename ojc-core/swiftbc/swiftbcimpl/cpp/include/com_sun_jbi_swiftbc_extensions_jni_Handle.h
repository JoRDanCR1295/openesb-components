/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class com_sun_jbi_swiftbc_extensions_jni_Handle */

#ifndef _Included_com_sun_jbi_swiftbc_extensions_jni_Handle
#define _Included_com_sun_jbi_swiftbc_extensions_jni_Handle
#ifdef __cplusplus
extern "C" {
#endif
/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_free
  (JNIEnv *, jobject);

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    putRequest
 * Signature: (Lcom/sun/jbi/swiftbc/extensions/jni/Primitive;)J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_putRequest
  (JNIEnv *, jobject, jobject);

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    getAnyResponse
 * Signature: (Lcom/sun/jbi/swiftbc/extensions/jni/Primitive;)J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_getAnyResponse__Lcom_sun_jbi_swiftbc_extensions_jni_Primitive_2
  (JNIEnv *, jobject, jobject);

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    getResponse
 * Signature: (JLcom/sun/jbi/swiftbc/extensions/jni/Primitive;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_getResponse__JLcom_sun_jbi_swiftbc_extensions_jni_Primitive_2
  (JNIEnv *, jobject, jlong, jobject);

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    getAnyResponse
 * Signature: (JLcom/sun/jbi/swiftbc/extensions/jni/Primitive;)J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_getAnyResponse__JLcom_sun_jbi_swiftbc_extensions_jni_Primitive_2
  (JNIEnv *, jobject, jlong, jobject);

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    getResponse
 * Signature: (JJLcom/sun/jbi/swiftbc/extensions/jni/Primitive;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_getResponse__JJLcom_sun_jbi_swiftbc_extensions_jni_Primitive_2
  (JNIEnv *, jobject, jlong, jlong, jobject);

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_Handle
 * Method:    call
 * Signature: (Lcom/sun/jbi/swiftbc/extensions/jni/Primitive;Lcom/sun/jbi/swiftbc/extensions/jni/Primitive;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_Handle_call
  (JNIEnv *, jobject, jobject, jobject);

#ifdef __cplusplus
}
#endif
#endif
