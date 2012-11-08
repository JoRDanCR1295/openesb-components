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
 * Implementation of APIs defined in com_stc_connector_sagadapter_jni_ExcStatus.h.
 *
 * To be compiled into stcsagjni.dll.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/05/08 06:03:04 $
 */

// stcsagjni.cpp : Defines the entry point for the DLL application.
//
#include "jni.h"

#include <iostream>

#include "sagapp.hpp"
#include "sagappltypes.hpp"
#include "stc_sag_jni_util.h"
#include "com_sun_jbi_swiftbc_extensions_jni_ExcStatus.h"

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_ExcStatus
 * Method:    throwit
 * Signature: (JILjava/lang/String;Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_ExcStatus_throwit
  (JNIEnv * env, jclass classObj, jlong token, jint severity, jstring code,
 jstring data, jstring text, jstring action)
{
   SagApp::ExcStatus *self= new SagApp::ExcStatus();

   self->setToken((long)token);

   if (severity == 0) {
     self->setSeverity(SagApp::c_success);
   } else if (severity == 1) {
     self->setSeverity(SagApp::c_transient);
   } else if (severity == 2) {
     self->setSeverity(SagApp::c_logic);
   } else if (severity == 3) {
     self->setSeverity(SagApp::c_fatal);
   } else {
     self->setSeverity(SagApp::c_success);
   }

   const char *str = env->GetStringUTFChars(code, NULL);
   if (str == NULL) {
     return;
   }
   self->setCode(str);
   env->ReleaseStringUTFChars(code, str);

   const char *str2 = env->GetStringUTFChars(data, NULL);
   if (str2 == NULL) {
     return;
   }
   self->setData(str2);
   env->ReleaseStringUTFChars(data, str2);

   const char *str3 = env->GetStringUTFChars(text, NULL);
   if (str3 == NULL) {
     return;
   }
   self->setText(str3);
   env->ReleaseStringUTFChars(text, str3);

   const char *str4 = env->GetStringUTFChars(action, NULL);
   if (str4 == NULL) {
     return;
   }
   self->setAction(str4);
   env->ReleaseStringUTFChars(action, str4);

   throwSagException(env, *self);
}
