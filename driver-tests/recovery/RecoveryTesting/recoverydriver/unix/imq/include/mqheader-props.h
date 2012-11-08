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
 * @(#)mqheader-props.h	1.10 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

#ifndef MQ_HEADER_PROPERTIES_H
#define MQ_HEADER_PROPERTIES_H

/*
 * defines constants for message headers
 */

static const char * MQ_PERSISTENT_HEADER_PROPERTY          = "MQPersistent";    /* MQBool */
static const char * MQ_REDELIVERED_HEADER_PROPERTY         = "MQRedelivered";   /* MQBool */
static const char * MQ_EXPIRATION_HEADER_PROPERTY          = "MQExpiration";    /* MQInt64 */
static const char * MQ_PRIORITY_HEADER_PROPERTY            = "MQPriority";      /* MQInt8 */
static const char * MQ_TIMESTAMP_HEADER_PROPERTY           = "MQTimestamp";     /* MQInt64 */
static const char * MQ_MESSAGE_ID_HEADER_PROPERTY          = "MQMessageID";     /* MQString */
static const char * MQ_CORRELATION_ID_HEADER_PROPERTY      = "MQCorrelationID"; /* MQString */
static const char * MQ_MESSAGE_TYPE_HEADER_PROPERTY        = "MQType";          /* MQString  */


#endif /* MQ_HEADER_PROPERTIES_H */
