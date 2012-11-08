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
 * @(#)mqsession.h	1.21 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

#ifndef MQ_SESSION_H
#define MQ_SESSION_H

/*
 * declarations of C interface for session
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "mqtypes.h"
#include "mqcallback-types.h"

/**
 * Closes the session.  This closes all producers and consumers
 * created from this session.  This will force all threads associated
 * with this session that are blocking in the library (e.g. a consumer
 * calling MQReceiveMessageWait) to return.
 *
 * @param sessionHandle the handle to the session to close
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQCloseSession(MQSessionHandle sessionHandle);

/**
 * Creates a destination with the given name and type.
 *
 * @param sessionHandle the handle to the session for which to
 *        create the destination
 * @param destinationName the name of the destination
 * @param destinationType MQ_QUEUE_DESTINATION or MQ_TOPIC_DESTINATION
 * @param destinationHandle the output handle of the newly created
 *        destination.
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQCreateDestination(const MQSessionHandle sessionHandle,
                    ConstMQString         destinationName,
                    MQDestinationType     destinationType,
                    MQDestinationHandle * destinationHandle);

/**
 * Creates a temporary destination of the given type. 
 *
 * @param sessionHandle the handle to the session for which to
 *        create the temporary destination
 * @param destinationType MQ_QUEUE_DESTINATION or MQ_TOPIC_DESTINATION
 * @param destinationHandle the output handle of the newly created
 *        temporary destination.
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQCreateTemporaryDestination(const MQSessionHandle sessionHandle,
                             MQDestinationType     destinationType,
                             MQDestinationHandle * destinationHandle);

/**
 * Creates a message producer with a specified destination.
 *
 * @param sessionHandle the handle to the session for which to
 *        create the message producer
 * @param destinationHandle the destination to which the created producer
 *        will send messages
 * @param producerHandle the output handle of the newly created
 *        producer
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQCreateMessageProducerForDestination(
                           const MQSessionHandle     sessionHandle,
                           const MQDestinationHandle destinationHandle,
                           MQProducerHandle *        producerHandle);

/**
 * Creates a message producer with no specified destination.
 *
 * @param sessionHandle the handle to the session for which to
 *        create the message producer
 * @param producerHandle the output handle of the newly created
 *        producer
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */  
EXPORTED_SYMBOL MQStatus 
MQCreateMessageProducer(const MQSessionHandle sessionHandle,
                          MQProducerHandle *    producerHandle);

/**
 * Creates a message consumer with the given properties for
 * synchronous receiving.
 *
 * @param sessionHandle the handle to the session for which to
 *        create the message consumer
 * @param destinationHandle the destination on which the consumer
 *        receives messages
 * @param messageSelector the message selector
 * @param noLocal MQ_TRUE iff the consumer should not receive 
 *        messages sent by a producer on this connection
 * @param consumerHandle the output handle to the newly creaated
 *        consumer
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */    
EXPORTED_SYMBOL MQStatus 
MQCreateMessageConsumer(const MQSessionHandle     sessionHandle,
                        const MQDestinationHandle destinationHandle,
                        ConstMQString             messageSelector,
                        MQBool                    noLocal,
                        MQConsumerHandle *        consumerHandle);

/**
 * Creates a durable message consumer with the given properties
 * for synchronous receiving.
 *
 * @param sessionHandle the handle to the session for which to
 *        create the message consumer
 * @param destinationHandle the destination on which the consumer
 *        receives messages
 * @param durableName the name of the durable subscriber.  
 * @param messageSelector the messages selector
 * @param noLocal MQ_TRUE iff the consumer should not receive 
 *        messages sent by a producer on this connection
 * @param consumerHandle the output handle to the newly creaated
 *        consumer
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQCreateDurableMessageConsumer(
                      const MQSessionHandle     sessionHandle,
                      const MQDestinationHandle destinationHandle,
                      ConstMQString             durableName,
                      ConstMQString             messageSelector,
                      MQBool                    noLocal, 
                      MQConsumerHandle *        consumerHandle);

/**
 * Creates a message consumer for asynchronous receiving.  The session
 * that is represented by the sessionHandle must be created with 
 * MQ_SESSION_ASYNC_RECEIVE
 *
 * @param sessionHandle the handle to the session for which to
 *        create the message consumer.
 * @param destinationHandle the destination on which the consumer
 *        receives messages
 * @param messageSelector the messages selector
 * @param noLocal MQ_TRUE iff the consumer should not receive
 *        messages sent by a producer on this connection
 * @param messageListener the message listener callback function
 * @param messageListenerCallbackData void * data pointer that to be
 *        passed to the message listener function when it is called
 * @param consumerHandle the output handle to the newly creaated
 *        consumer
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus
MQCreateAsyncMessageConsumer(const MQSessionHandle     sessionHandle,
                             const MQDestinationHandle destinationHandle,
                             ConstMQString             messageSelector,
                             MQBool                    noLocal,
                             MQMessageListenerFunc     messageListener,
                             void *                    listenerCallbackData,
                             MQConsumerHandle *        consumerHandle);

/**
 * Creates a durable message consumer for asynchronous receiving.  
 * The session that is represented by sessionHandle must be created
 * with MQ_SESSION_ASYNC_RECEIVE
 *
 * @param sessionHandle the handle to the session for which to
 *        create the message consumer
 * @param destinationHandle the destination on which the consumer
 *        receives messages
 * @param durableName the name of the durable subscriber.
 * @param messageSelector the message selector
 * @param noLocal MQ_TRUE iff the consumer should not receive
 *        messages sent by a producer on this connection
 * @param messageListener the mesage listener callback function
 * @param messageListenerCallbackData void * data pointer that to be
 *        passed to the message listener function when it is called
 * @param consumerHandle the output handle to the newly creaated
 *        consumer
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus
MQCreateAsyncDurableMessageConsumer(
                           const MQSessionHandle     sessionHandle,
                           const MQDestinationHandle destinationHandle,
                           ConstMQString             durableName,
                           ConstMQString             messageSelector,
                           MQBool                    noLocal,
                           MQMessageListenerFunc     messageListener,
                           void *                    listenerCallbackData,
                           MQConsumerHandle *        consumerHandle);


/**
 * Unsubscribes the durable message consumer with the given durable
 * name.  This deletes all messages at the broker that were being
 * stored for this durable consumer.  This function cannot be called
 * if there is an active consumer with the specified durable name.
 *
 * @param sessionHandle the handle to the session to use to 
 *        unsubscribe the durable consumer.
 * @param durableName the name of the durable subscriber.  
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQUnsubscribeDurableMessageConsumer(const MQSessionHandle sessionHandle,
                                    ConstMQString const durableName);

/**
 * Stops message delivery in this session, and restarts message
 *  delivery with the oldest unacknowledged message. 
 *
 * All consumers deliver messages in a serial order. Acknowledging
 * a received message automatically acknowledges all messages that
 * have been delivered to the client. 
 *
 * Restarting a session causes it to take the following actions: 
 * .  Stop message delivery 
 * .  Mark all messages that might have been delivered but not acknowledged
 *    as "redelivered" 
 * .  Restart the delivery sequence including all unacknowledged messages
 *    that had been previously delivered. Redelivered messages do not have
 *    to be delivered in exactly their original delivery order. 
 *
 * @param sessionHandle the handle to the session
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus
MQRecoverSession(const MQSessionHandle sessionHandle);


/**
 * Commits all messages done in this transaction
 *
 * @param sessionHandle the handle to the session
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus
MQCommitSession(const MQSessionHandle sessionHandle);


/**
 * Rolls back any messages done in this transaction
 *
 * @param sessionHandle the handle to the session
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus
MQRollbackSession(const MQSessionHandle sessionHandle);


/**
 * Get session acknowledge mode
 *
 * @param sessionHandle the handle to the session
 * @param ackMode output parameter
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus
MQGetAcknowledgeMode(const MQSessionHandle sessionHandle, MQAckMode * ackMode);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* MQ_SESSION_H */