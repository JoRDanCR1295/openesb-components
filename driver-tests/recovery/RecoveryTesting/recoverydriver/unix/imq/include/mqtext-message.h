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
 * @(#)mqtext-message.h	1.9 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

#ifndef MQ_TEXT_MESSAGE_H
#define MQ_TEXT_MESSAGE_H

/*
 * declarations of C interface for text message
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "mqtypes.h"

/**
 * Creates a new text message.
 *
 * @param messageHandle the output parameter for the newly created message
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */   
EXPORTED_SYMBOL MQStatus 
MQCreateTextMessage(MQMessageHandle * messageHandle);
  
/**
 * Gets the bytes from a bytes message.  This call is only valid if
 * messageHandle refers to a message whose type is MQTextMessageType.
 * The string that is returned is not a copy.  The caller should not
 * modify the bytes or attempt to free it.  The string is a NULL-
 * terminated UTF8-encoded string.
 *
 * @param messageHandle the handle of the message to retrieve the text from
 * @param messageText the output parameter for the message text
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */   
EXPORTED_SYMBOL MQStatus 
MQGetTextMessageText(const MQMessageHandle messageHandle,
                     ConstMQString *       messageText);

/**
 * Sets the text for a text message.  This call is only valid if
 * messageHandle refers to a message whose type is
 * MQTextMessageType.  A copy of the string is made, so the caller
 * can manipulate messageText after this function returns.  messageText
 * should be a NULL-terminated UTF8-encoded string.
 *
 * @param messageHandle the handle of the message to set the text 
 * @param messageText the string to set the message text to
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */   
EXPORTED_SYMBOL MQStatus 
MQSetTextMessageText(const MQMessageHandle messageHandle,
                     ConstMQString         messageText);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* MQ_TEXT_MESSAGE_H */
