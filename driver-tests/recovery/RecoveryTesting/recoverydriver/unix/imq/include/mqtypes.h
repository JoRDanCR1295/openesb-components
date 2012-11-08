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
 * @(#)mqtypes.h	1.17 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

#ifndef MQ_TYPES_H
#define MQ_TYPES_H

/*
 * defines MQ C-API types
 */

#include "mqbasictypes.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/** Enumeration of the basic types that can be stored in a properties object.*/
typedef enum _MQType {MQ_BOOL_TYPE,    
                      MQ_INT8_TYPE, MQ_INT16_TYPE, MQ_INT32_TYPE, MQ_INT64_TYPE, 
                      MQ_FLOAT32_TYPE, MQ_FLOAT64_TYPE, 
                      MQ_STRING_TYPE,
                      MQ_INVALID_TYPE} MQType;
  
/** The different types of messages that can be received */
typedef enum _MQMessageType {MQ_TEXT_MESSAGE = 0,
                             MQ_BYTES_MESSAGE = 1,
                             MQ_MESSAGE = 3,
                             MQ_UNSUPPORTED_MESSAGE = 2} MQMessageType;

/** An enumeration of the various acknowledgement modes */
typedef enum _MQAckMode {MQ_AUTO_ACKNOWLEDGE    = 1, 
                         MQ_CLIENT_ACKNOWLEDGE  = 2, 
                         MQ_DUPS_OK_ACKNOWLEDGE = 3,
                         MQ_SESSION_TRANSACTED  = 0} MQAckMode;

/** Enumeration of  delivery modes */
typedef enum _MQDeliveryMode {MQ_NON_PERSISTENT_DELIVERY = 1,
                              MQ_PERSISTENT_DELIVERY     = 2} MQDeliveryMode;

/** Enumeration of  destination types */
typedef enum _MQDestinationType {MQ_QUEUE_DESTINATION,
                                 MQ_TOPIC_DESTINATION} MQDestinationType;

/** An enumeration of session receiving modes */
typedef enum _MQReceiveMode {MQ_SESSION_SYNC_RECEIVE=0,
                             MQ_SESSION_ASYNC_RECEIVE=1} MQReceiveMode;

/** A MQString is a NULL terminated UTF8 encoded character string */
typedef MQChar *        MQString;
typedef const MQChar *  ConstMQString;

typedef MQUint32 MQError;

/** Status struct */
typedef struct _MQStatus {
  MQError errorCode;
} MQStatus;


typedef MQInt32 MQObjectHandle;

/** Properties handle */
typedef struct _MQPropertiesHandle {
  MQObjectHandle handle;
} MQPropertiesHandle;

/** Connection handle */
typedef struct _MQConnectionHandle {
  MQObjectHandle handle;
} MQConnectionHandle;

/** Session handle */
typedef struct _MQSessionHandle {
  MQObjectHandle handle;
} MQSessionHandle;

/** Destination handle */
typedef struct _MQDestinationHandle {
  MQObjectHandle handle;
} MQDestinationHandle;

/** Publisher handle */
typedef struct _MQProducerHandle {
  MQObjectHandle handle;
} MQProducerHandle;

/** Subscriber handle */
typedef struct _MQConsumerHandle {
  MQObjectHandle handle;
} MQConsumerHandle;

/** Message handle */
typedef struct _MQMessageHandle {
  MQObjectHandle handle;
} MQMessageHandle;

/** An invalid HANDLE value, which can be used to initialize any of
    the above handles */
#define MQ_INVALID_HANDLE {(MQInt32)0xFEEEFEEE}


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* MQ_TYPES_H */
