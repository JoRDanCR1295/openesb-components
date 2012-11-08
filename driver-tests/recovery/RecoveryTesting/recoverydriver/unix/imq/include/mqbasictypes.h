/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.  
 *
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with 
 * your own identifying information: 
 * "Portions Copyrighted [year] [name of copyright owner]"
 */

/*
 * @(#)mqbasictypes.h	1.9 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

#ifndef MQ_BASICTYPES_H
#define MQ_BASICTYPES_H

/*
 * defines MQ basic types
 */

#if ((defined(__SUNPRO_CC) && (__SUNPRO_CC_COMPAT == 5)) \
         || defined(__SUNPRO_C)) \
    && defined(sun) && (defined(sparc) || defined(i386))
#ifndef SOLARIS
#define SOLARIS
#endif
#endif

#if (defined(__GNUC__) || defined (__GNUG__)) && defined(__linux__)
#ifndef LINUX
#define LINUX
#endif
#endif

//######hpux-dev######
#if (defined(__hpux))
#ifndef HPUX
#define HPUX
#endif
#endif


#if defined(_MSC_VER) && defined(_WIN32)
#ifndef WIN32
#define WIN32
#endif
#endif

#ifdef SOLARIS
#include <inttypes.h>
#endif
#ifdef LINUX
#include <stdint.h>
#endif
//#####hpux-dev#####
#ifdef HPUX
#include <inttypes.h>
#endif



#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

//#####hpux-dev#####
#if defined(SOLARIS) || defined(LINUX) || defined(HPUX)
typedef int32_t   MQBool;
typedef int8_t    MQInt8;
typedef int16_t   MQInt16;
typedef int32_t   MQInt32;
typedef int64_t   MQInt64;
typedef uint32_t  MQUint32;
#elif defined(WIN32)
typedef __int32           MQBool;
typedef __int8            MQInt8;
typedef __int16           MQInt16;
typedef __int32           MQInt32;
typedef __int64           MQInt64;
typedef unsigned __int32  MQUint32;
#else
#error unknown platform
#endif

//#####hpux-dev#####
#if defined(SOLARIS) || defined(LINUX) || defined(WIN32) || defined(HPUX)
typedef float   MQFloat32;
typedef double  MQFloat64;
typedef char    MQChar;

#define MQ_TRUE  1
#define MQ_FALSE 0
#else
#error unknown platform
#endif

//#####hpux-dev#####
/** internal use only */ 
#if defined(WIN32)
#if defined(MQ_EXPORT_DLL_SYMBOLS)
#define EXPORTED_SYMBOL __declspec(dllexport)
#else
#define EXPORTED_SYMBOL __declspec(dllimport)
#endif /* defined(MQ_EXPORT_DLL_SYMBOLS) */
#elif defined(SOLARIS) || defined(LINUX) || defined(HPUX)
#define EXPORTED_SYMBOL 
#else
#error unknown platform
#endif  /* defined(WIN32) */


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* MQ_BASICTYPES_H */
