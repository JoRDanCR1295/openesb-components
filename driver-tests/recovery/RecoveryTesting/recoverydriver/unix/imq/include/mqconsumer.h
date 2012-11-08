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
 * @(#)mqconsumer.h	1.11 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

#ifndef MQ_CONSUMER_H
#define MQ_CONSUMER_H

/*
 * declarations of C interface for message consumer
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "mqtypes.h"
  
/**
 * Closes the message consumer.  
 *
 * @param consumerHandle the handle to the consumer to close
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQCloseMessageConsumer(MQConsumerHandle consumerHandle);

/**
 * Waits until the consumer specified by consumerHandle receives a
 * message and returns this message in messageHandle.  If there is
 * already a message pending for this consumer, then this call returns
 * it immediately and does not block.  If an exception occurs, such as
 * the connection closing before a message arrives, then this call
 * returns with an error.
 *
 * @param consumerHandle the handle to the consumer to wait for a message
 *        to arrive
 * @param messageHandle the output parameter that contains the received
 *        message
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQReceiveMessageWait(const MQConsumerHandle consumerHandle, 
                     MQMessageHandle *      messageHandle);

/**
 * Waits for up to timeoutMilliSeconds milliseconds until the consumer
 * specified by consumerHandle receives a message and returns this
 * message in messageHandle.  If there is already a message pending
 * for this consumer, then this call returns it immediately and does
 * not block.  If an exception occurs before a message arrives or the
 * timeout expires, such as the connection closing, then this call
 * returns with an error.
 *
 * @param consumerHandle the handle to the consumer to wait for a message
 *        to arrive
 * @param timeoutMilliSeconds the number of milliseconds to wait for a
 *        message to arrive for this consumer         
 * @param messageHandle the output parameter that contains the received
 *        message
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQReceiveMessageWithTimeout(const MQConsumerHandle consumerHandle, 
                            MQInt32                timeoutMilliSeconds,
                            MQMessageHandle *      messageHandle);

/**
 * If a message is pending for the consumer, then this call
 * immediately returns it.  Otherwise, it immediately returns an
 * error.
 *
 * @param consumerHandle the handle to the consumer to wait for a message
 *        to arrive
 * @param messageHandle the output parameter that contains the received
 *        message
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQReceiveMessageNoWait(const MQConsumerHandle consumerHandle, 
                       MQMessageHandle *      messageHandle);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* MQ_CONSUMER_H */
