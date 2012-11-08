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
 * @(#)mqbytes-message.h	1.9 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

#ifndef MQ_BYTES_MESSAGE_H
#define MQ_BYTES_MESSAGE_H

/*
 * declarations of C interface for bytes message 
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "mqtypes.h"

/**
 * Creates a new bytes message.
 *
 * @param messageHandle the output parameter for the newly created message
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */   
EXPORTED_SYMBOL MQStatus 
MQCreateBytesMessage(MQMessageHandle * messageHandle);

/**
 * Gets the bytes from a bytes message.  This call is only valid if
 * messageHandle refers to a message whose type is
 * MQBytesMessageType.  The bytes that are returned are not a copy.  The
 * caller should not modify the bytes or attempt to free them.
 *
 * @param messageHandle the handle of the message to retrieve the bytes from
 * @param messageBytes the output parameter for the bytes of the message
 * @param messageBytesSize the number of bytes that are present in messageBytes
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */   
EXPORTED_SYMBOL MQStatus 
MQGetBytesMessageBytes(const MQMessageHandle messageHandle,
                         const MQInt8 ** messageBytes,
                         MQInt32 *       messageBytesSize);

/**
 * Sets the bytes for a bytes message.  This call is only valid if
 * messageHandle refers to a message whose type is
 * MQBytesMessageType.  A copy of the bytes is made, so the caller
 * can manipulate messageBytes after this function returns.
 *
 * @param messageHandle the handle of the message to set the bytes for
 * @param messageBytes the bytes to set for the message body
 * @param messageBytesSize the number of bytes that are present in messageBytes
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */   
EXPORTED_SYMBOL MQStatus 
MQSetBytesMessageBytes(const MQMessageHandle messageHandle,
                         const MQInt8 *        messageBytes,
                         MQInt32               messageBytesSize);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* MQ_BYTES_MESSAGE_H */
