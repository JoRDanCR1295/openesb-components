/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)Constants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jca;

/**
 *
 * Constants defined
 */
public class Constants {
    
    //
    // Scheme portion of the connection url for each JMS provider types
    //
    public static final String SCHEME_JMS_PROVIDER_SUNMQ       = "mq";    
    public static final String SCHEME_JMS_PROVIDER_STCMS       = "stcms";
    public static final String SCHEME_JMS_PROVIDER_JBOSS       = "jnp";
    public static final String SCHEME_JMS_PROVIDER_WAVE        = "tcp";
    public static final String SCHEME_JMS_PROVIDER_WEBLOGIC    = "t3";
    public static final String SCHEME_JMS_PROVIDER_WEBSPHERE   = "wmq";
    public static final String SCHEME_JMS_GENERIC_JNDI         = "jndi";
        
    //
    // JMS JCA option values
    //
    public static final String SUBSCRIPTION_DURABILITY_DURABLE    = "Durable";
    public static final String SUBSCRIPTION_DURABILITY_NONDURABLE = "NonDurable";
    
    public static final String CONCURRENCTY_SERIAL              = "serial";
    public static final String CONCURRENCTY_CONNECTION_CONSUMER = "cc";
    public static final String CONCURRENCTY_SYNC                = "sync";
    
    // JMSJCA RA Options
    public static final String JMSJCA_RA_OPTION_NoXA = "JMSJCA.NoXA";
    public static final String JMSJCA_RA_OPTION_LocatorClass = "JMSJCA.LocatorClass";
    public static final String JMSJCA_RA_OPTION_redeliveryredirect = "JMSJCA.redeliveryredirect";
    public static final String JMSJCA_RA_OPTION_redeliveryhandling = "JMSJCA.redeliveryhandling";
    public static final String JMSJCA_RA_OPTION_concurrencymode = "JMSJCA.concurrencymode";
    public static final String JMSJCA_RA_OPTION_ACC = "JMSJCA.ACC";
    public static final String JMSJCA_RA_OPTION_IgnoreTx = "JMSJCA.IgnoreTx";
    public static final String JMSJCA_RA_OPTION_BypassRA = "JMSJCA.BypassRA";
    public static final String JMSJCA_RA_OPTION_Strict = "JMSJCA.Strict";
    public static final String JMSJCA_RA_OPTION_poolmaxsize="JMSJCA.poolmaxsize"; 
    public static final String JMSJCA_RA_OPTION_pooltimeout="JMSJCA.pooltimeout"; 
    public static final String JMSJCA_RA_OPTION_poolminsize="JMSJCA.poolminsize"; 
    public static final String JMSJCA_RA_OPTION_QueueCF="JMSJCA.QueueCF"; 
    public static final String JMSJCA_RA_OPTION_TopicCF="JMSJCA.TopicCF"; 
    public static final String JMSJCA_RA_OPTION_UnifiedCF="JMSJCA.UnifiedCF"; 

    // JMSJCA MCF properties as options
    public static final String JMSJCA_MCF_OPTION_ProducerPooling  = "ProducerPooling";
    public static final String JMSJCA_MCF_OPTION_IdleTimeout      = "IdleTimeout";

    // JMSJCA AS properties as options
    public static final String JMSJCA_AS_OPTION_ConcurrencyMode = "ConcurrencyMode";
    
    // JNDI related properties
    public static final String JAVA_NAMING_FACTORY_INITIAL       = "java.naming.factory.initial";
    public static final String JAVA_NAMING_PROVIDER_URL          = "java.naming.provider.url";
    public static final String JAVA_NAMING_SECURITY_PRINCIPAL    = "java.naming.security.principal";
    public static final String JAVA_NAMING_SECURITY_CREDENTIALS  = "java.naming.security.credentials";
    
}
