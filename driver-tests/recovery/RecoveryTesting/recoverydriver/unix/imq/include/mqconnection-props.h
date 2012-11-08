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
 * @(#)mqconnection-props.h	1.16 04/09/07
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

#ifndef MQ_CONNECTION_PROPERTIES_H
#define MQ_CONNECTION_PROPERTIES_H

/*
 * defines constants for connection properties
 */

static const char * MQ_BROKER_HOST_PROPERTY             = "MQBrokerHostName"; /* MQString */
static const char * MQ_BROKER_PORT_PROPERTY             = "MQBrokerHostPort"; /* MQInt32  */
static const char * MQ_SERVICE_PORT_PROPERTY            = "MQServicePort";    /* MQInt32  */
static const char * MQ_CONNECTION_TYPE_PROPERTY         = "MQConnectionType"; /* MQString */
static const char * MQ_ACK_TIMEOUT_PROPERTY             = "MQAckTimeout";     /* MQInt32 in millisecond */
static const char * MQ_ACK_ON_PRODUCE_PROPERTY          = "MQAckOnProduce";        /* MQBool */
static const char * MQ_ACK_ON_ACKNOWLEDGE_PROPERTY      = "MQAckOnAcknowledge";    /* MQBool */
static const char * MQ_CONNECTION_FLOW_COUNT_PROPERTY         = "MQConnectionFlowCount";        /* MQInt32 */
static const char * MQ_CONNECTION_FLOW_LIMIT_ENABLED_PROPERTY = "MQConnectionFlowLimitEnabled"; /* MQBool  */
static const char * MQ_CONNECTION_FLOW_LIMIT_PROPERTY         = "MQConnectionFlowLimit";        /* MQInt32 */
static const char * MQ_PING_INTERVAL_PROPERTY           = "MQPingInterval";   /* MQInt32 in second */


/** SSL */
static const char * MQ_SSL_BROKER_IS_TRUSTED            = "MQSSLIsHostTrusted";        /* MQBool */
static const char * MQ_SSL_CHECK_BROKER_FINGERPRINT     = "MQSSLCheckHostFingerprint"; /* MQBool */
static const char * MQ_SSL_BROKER_CERT_FINGERPRINT      = "MQSSLHostCertFingerprint";  /* MQString */


/** connection metadata properties to be used with MQGetMetaData   */
static const char * MQ_NAME_PROPERTY            = "MQ_NAME";
static const char * MQ_VERSION_PROPERTY         = "MQ_VERSION";
static const char * MQ_MAJOR_VERSION_PROPERTY   = "MQ_VMAJOR";
static const char * MQ_MINOR_VERSION_PROPERTY   = "MQ_VMINOR";
static const char * MQ_MICRO_VERSION_PROPERTY   = "MQ_VMICRO";
static const char * MQ_SERVICE_PACK_PROPERTY    = "MQ_SVCPACK";
static const char * MQ_UPDATE_RELEASE_PROPERTY  = "MQ_URELEASE";

#endif /* MQ_CONNECTION_PROPERTIES_H */
