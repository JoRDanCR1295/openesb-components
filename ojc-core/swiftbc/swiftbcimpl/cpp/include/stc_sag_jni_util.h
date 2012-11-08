#ifndef STC_SAG_JNI_UTIL_H
#define STC_SAG_JNI_UTIL_H

#include <jni.h>

#include <iostream>

#include "sagapp.hpp"

inline jint
throwSagException(JNIEnv *env, SagApp::ExcStatus &ex)
{
  env->ExceptionDescribe();
  env->ExceptionClear();

  jclass excStatusClass =
	  env->FindClass("com/sun/jbi/swiftbc/extensions/jni/ExcStatus");
  if(excStatusClass == NULL) {
    return NULL;
  }

  jmethodID excStatusConstructor =
    env->GetMethodID(excStatusClass, "<init>", "(JILjava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");
  if (excStatusConstructor == NULL) {
    return NULL;
  }

  jstring code = env->NewStringUTF(ex.getCode().c_str());
  jstring data = env->NewStringUTF(ex.getData().c_str());
  jstring text = env->NewStringUTF(ex.getText().c_str());
  jstring action = env->NewStringUTF(ex.getAction().c_str());
  jobject excStatusObj =
    env->NewObject(excStatusClass,
                   excStatusConstructor,
                   (jlong)ex.getToken(),
                   ex.getSeverity(),
                   code,
                   data,
                   text,
                   action);
  if (excStatusObj == NULL) {
    return NULL;
  }

  jint result = env->Throw((jthrowable)excStatusObj); 

  return result;
}

/* inline jint */
/* throwSagException(JNIEnv *env, SagProcessControl::ExcStatus &ex) */
/* { */
/*   env->ExceptionDescribe(); */
/*   env->ExceptionClear(); */

/*   jclass excStatusClass = */
/*     env->FindClass("com/sun/jbi/swiftbc/extensions/jni/ExcStatus"); */
/*   if(excStatusClass == NULL) { */
/*     return NULL; */
/*   } */

/*   jmethodID excStatusConstructor = */
/*     env->GetMethodID(excStatusClass, "<init>", "(JILjava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V"); */
/*   if (excStatusConstructor == NULL) { */
/*     return NULL; */
/*   } */

/*   jstring code = env->NewStringUTF(ex.getCode().c_str()); */
/*   jstring data = env->NewStringUTF(ex.getData().c_str()); */
/*   jstring text = env->NewStringUTF(ex.getText().c_str()); */
/*   jstring action = env->NewStringUTF(ex.getAction().c_str()); */
/*   jobject excStatusObj = */
/*     env->NewObject(excStatusClass, */
/*                    excStatusConstructor, */
/*                    (jlong)ex.getToken(), */
/*                    ex.getSeverity(), */
/*                    code, */
/*                    data, */
/*                    text, */
/*                    action); */
/*   if (excStatusObj == NULL) { */
/*     return NULL; */
/*   } */

/*   jint result = env->Throw((jthrowable)excStatusObj); */

/*   return result; */
/* } */

#endif
