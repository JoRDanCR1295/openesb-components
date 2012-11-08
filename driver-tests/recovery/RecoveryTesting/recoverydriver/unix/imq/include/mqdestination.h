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
 * @(#)mqdestination.h	1.11 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

#ifndef MQ_DESTINATION_H
#define MQ_DESTINATION_H

/*
 * declarations of C interface for destination
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "mqtypes.h"

/**
 * Frees the destination object specified by destinationHandle.
 *
 * @param destinationHandle the destination to free.
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQFreeDestination(MQDestinationHandle destinationHandle);

/**
 * Get the destination type of a destination 
 *
 * @param destinationHandle the destination to type from
 * @param destinationType the output parameter for the type
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus
MQGetDestinationType(const MQDestinationHandle destinationHandle,
                     MQDestinationType *       destinationType);

/**
 * Get the destination name of a destination. The returned  
 * destinationName is a copy which the caller is responsible
 * to free by calling MQFreeString
 *
 * @param destinationHandle the destination to get name from
 * @param destinationName the output parameter for the name 
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus
MQGetDestinationName(const MQDestinationHandle destinationHandle,
                     MQString *                destinationName);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* MQ_DESTINATION_H */
