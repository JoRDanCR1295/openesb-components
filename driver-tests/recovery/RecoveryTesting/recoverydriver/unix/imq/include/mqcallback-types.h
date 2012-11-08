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
 * @(#)mqcallback-types.h	1.15 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

#ifndef MQ_CALLBACK_TYPES_H
#define MQ_CALLBACK_TYPES_H

/*
 * defines MQ C-API callback types 
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "mqtypes.h"

/**
 * This callback is used to notify the user that an exception occurred.
 *
 * @param connectionHandle a handle to the connection on which the
 *        connection exception occurred
 * @param exception the connection exception that occurred
 * @param callbackData whatever void * pointer that was passed to
 *        MQCreateConnection
 * @see MQCreateConnection.  */
typedef void (*MQConnectionExceptionListenerFunc)(
                                const MQConnectionHandle  connectionHandle,
                                MQStatus                  exception,
                                void *                    callbackData );

/**
 * This callback is used for asynchronous receiving messages
 *
 * @param sessionHandle a handle to the session
 * @param consumerHandle a handle to the message consumer 
 * @param messageHandle a handle to the message 
 * @param callbackData whatever void * pointer that was passed to
 *        MQCreateAsyncMessageConsumer or MQCreateAsyncDurableMessageConsumer
 * @see MQCreateAsyncMessageConsumer and MQCreateAsyncDurableMessageConsumer. */
typedef MQError (*MQMessageListenerFunc)(const MQSessionHandle  sessionHandle,
                                         const MQConsumerHandle consumerHandle,
                                         MQMessageHandle        messageHandle,
                                         void *                 callbackData);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* MQ_CALLBACK_TYPES_H */
