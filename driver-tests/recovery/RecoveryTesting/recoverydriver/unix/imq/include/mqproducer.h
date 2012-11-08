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
 * @(#)mqproducer.h	1.14 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 
 
#ifndef MQ_PRODUCER_H
#define MQ_PRODUCER_H

/*
 * declarations of C interface for message producer
 */ 

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "mqtypes.h"
#include "mqmessage.h"
  
/**
 * Closes the message producer.  
 *
 * @param producerHandle the handle to the producer to close
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQCloseMessageProducer(MQProducerHandle producerHandle);

/**
 * Has the producer specified by producerHandle send the message
 * specified by messageHandle to the producer's destination with
 * the default message properties.  This call can only be used with 
 * a producer that has a specified destination at creation time (i.e.
 * producers created by calling MQCreateMessageProducerForDestination)
 *
 * @param producerHandle the handle to the producer to close
 * @param messageHandle the message to send
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQSendMessage(const MQProducerHandle producerHandle,
              const MQMessageHandle  messageHandle);

/**
 * Has the producer specified by producerHandle send the message
 * specified by messageHandle to the producer's destination with the
 * specified message properties.  This call can only be used with a
 * producer that has a specified destination at creation time (i.e.
 * producers created by calling MQCreateMessageProducerForDestination)
 *
 * @param producerHandle the handle to the producer to close
 * @param messageHandle the message to send
 * @param msgDeliveryMode the persistent delivery mode of the
 *        message.  Options are MQ_NON_PERSISTENT_DELIVERY and
 *        MQ_PERSISTENT_DELIVERY
 * @param msgPriority the priority of the message. There are 10 levels
 *        of priority, with 0 lowest and 9 highest. The default level
 *        is 4. A JMS provider tries to deliver higher-priority
 *        messages before lower-priority ones, but does not have to
 *        deliver messages in exact order of priority.
 * @param msgTimeToLive the message's lifetime (in milliseconds)
 *        If the specified value is zero, the message never expires.
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQSendMessageExt(const MQProducerHandle producerHandle,
                 const MQMessageHandle  messageHandle,
                 MQDeliveryMode         msgDeliveryMode,
                 MQInt8                 msgPriority,
                 MQInt64                msgTimeToLive);

/**
 * Has the producer specified by producerHandle send the message
 * specified by messageHandle to the destination specified by
 * destinationHandle with the default message properties. This 
 * call can only be used with a producer that does not have a
 * specified destination at creation time (i.e. producers created
 * by calling MQCreateMessageProducer)
 *
 * @param producerHandle the handle to the producer to close
 * @param messageHandle the message to send
 * @param destinationHandle the destination to send the message to
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQSendMessageToDestination(const MQProducerHandle    producerHandle,
                           const MQMessageHandle     messageHandle,
                           const MQDestinationHandle destinationHandle);

/**
 * Has the producer specified by producerHandle send the message
 * specified by messageHandle to the destination specified by
 * destinationHandle with the specified message properties.  This 
 * call can only be used with a producer that does not have a specified
 * destination at creation time (i.e. producers created by calling
 * MQCreateMessageProducer)
 *
 * @param producerHandle the handle to the producer to close
 * @param messageHandle the message to send
 * @param destinationHandle the destination to send the message to
 * @param msgDeliveryMode the persistent delivery mode of the
 *        message.  Options are MQ_NON_PERSISTENT_DELIVERY and
 *        MQ_PERSISTENT_DELIVERY
 * @param msgPriority the priority of the message. There are 10 levels
 *        of priority, with 0 lowest and 9 highest. The default level
 *        is 4. A JMS provider tries to deliver higher-priority
 *        messages before lower-priority ones, but does not have to
 *        deliver messages in exact order of priority.
 * @param msgTimeToLive the number of milliseconds until the
 *        message expires.  If the specified value is zero, the message
 *        never expires.
 * @return the status of the function call.  Pass this value to
 *         MQStatusIsError to determine if the call was
 *         successful.  */
EXPORTED_SYMBOL MQStatus 
MQSendMessageToDestinationExt(const MQProducerHandle    producerHandle,
                              const MQMessageHandle     messageHandle,
                              const MQDestinationHandle destinationHandle,
                              MQDeliveryMode            msgDeliveryMode,
                              MQInt8                    msgPriority,
                              MQInt64                   msgTimeToLive);
  
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* MQ_PRODUCER_H */

