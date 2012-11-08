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
 * Implementation of APIs defined in com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep.h.
 *
 * To be compiled into stcsagjni.dll.
 *
 * @author Harry Liu (harry.liu@sun.com)
 * @version cvs revision: $Revision: 1.1 $ Last Modified: $Date: 2007/05/08 06:03:05 $
 */

#include "com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep.h"
// this class is deprecated for sag 5.0.0

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep
 * Method:    alloc
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep_alloc
  (JNIEnv *, jobject)
{
   // todo if it is required
   return 0;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep
 * Method:    free
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep_free
  (JNIEnv *, jobject){
   // todo if it is required
}
/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep
 * Method:    setVisit
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep_setVisit
  (JNIEnv *, jobject, jint)
{
   // todo if it is required
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep
 * Method:    getVisit
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep_getVisit
  (JNIEnv *, jobject)
{
   // todo if it is required
   return 0;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep
 * Method:    setPlugIn
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep_setPlugIn
  (JNIEnv *, jobject, jstring)
{
   // todo if it is required
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep
 * Method:    getPlugIn
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep_getPlugIn
  (JNIEnv *, jobject)
{
   // todo if it is required
   return NULL;
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep
 * Method:    setPrimitive
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep_setPrimitive
  (JNIEnv *, jobject, jstring)
{
   // todo if it is required
}

/*
 * Class:     com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep
 * Method:    getPrimitive
 * Signature: ()Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_com_sun_jbi_swiftbc_extensions_jni_SAGJNIRoutingStep_getPrimitive
  (JNIEnv *, jobject)
{
   // todo if it is required
   return NULL;
}
