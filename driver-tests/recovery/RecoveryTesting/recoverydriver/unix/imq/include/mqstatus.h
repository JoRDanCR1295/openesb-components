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
 * @(#)mqstatus.h	1.14 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

#ifndef MQ_STATUS_H
#define MQ_STATUS_H

/*
 * declarations of C interface for error handling 
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#include "mqtypes.h"

/**
 * Returns MQ_TRUE iff status represents an error.
 *
 * @param status the result of an MQ function call to check
 * @return MQ_TRUE iff status represents an error
 */
EXPORTED_SYMBOL MQBool 
MQStatusIsError(const MQStatus status);


/**
 * Returns the 32 bit error code associated with status.
 *
 * @param status the result of an MQ function call to check
 * @return the 32 bit error code associated with status.
 */
EXPORTED_SYMBOL MQError 
MQGetStatusCode(const MQStatus status);

/**
 * Returns a string explanation of status.  The caller is responsible
 * for freeing the returned string by calling MQFreeString.
 *
 * @param status the result of an MQ function call to check
 * @return the string explanation of status */
EXPORTED_SYMBOL MQString 
MQGetStatusString(const MQStatus status);


/**
 * Gets error trace. The caller is responsible for freeing 
 * the returned string by calling MQFreeString.
 *
 * @return the error trace or NULL if no error trace */
EXPORTED_SYMBOL MQString 
MQGetErrorTrace();


/**
 * Frees a MQString that was returned by MQGetStatusString
 * or MQGetErrorTrace
 * 
 * @param string the MQString to free.  It must have been
 *        returned by MQGetStatusString or MQGetErrorTrace */
EXPORTED_SYMBOL void
MQFreeString(MQString string);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* MQ_STATUS_H */
