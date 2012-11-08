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
 * @(#)JMSBCRuntimeConfigurationMBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.mbeans;

import javax.management.InvalidAttributeValueException;
import javax.management.MBeanException;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.SimpleType;

import com.sun.jbi.internationalization.Messages;


/**
 * MBean interface for run-time configuration
 *
 * @author Sun
 */
public interface JMSBCRuntimeConfigurationMBean extends RuntimeConfigurationMBean{

    public static final Messages mMessages =
        Messages.getMessages(JMSBCRuntimeConfigurationMBean.class);
	
		// Application configuration row fields
		static final String APPLICATION_CONFIG_CONNECTION_URL = "connectionURL";
		static final String APPLICATION_CONFIG_CONNECTION_FACTORY_NAME = "connectionFactoryName";
		static final String APPLICATION_CONFIG_INITIAL_CONTEXT_FACTORY = "initialContextFactory";
		static final String APPLICATION_CONFIG_PROVIDER_URL = "providerURL";
		static final String APPLICATION_CONFIG_SECURITY_PRINCIPAL = "securityPrincipal";
		static final String APPLICATION_CONFIG_SECURITY_CREDENTIALS = "securityCredentials";
		static final String APPLICATION_CONFIG_USERNAME = "username";
		static final String APPLICATION_CONFIG_PASSWORD = "password";
		static final String APPLICATION_CONFIG_JNDIENV= "jndienv";
		static final String[] APPLICATION_CONFIG_ROW_NAMES = new String[] {
	    	APPLICATION_CONFIG_CONNECTION_URL,
	    	APPLICATION_CONFIG_CONNECTION_FACTORY_NAME,
	    	APPLICATION_CONFIG_INITIAL_CONTEXT_FACTORY,
	    	APPLICATION_CONFIG_PROVIDER_URL,
	    	APPLICATION_CONFIG_SECURITY_PRINCIPAL,
	    	APPLICATION_CONFIG_SECURITY_CREDENTIALS,
	    	APPLICATION_CONFIG_USERNAME,
	    	APPLICATION_CONFIG_PASSWORD,
	    	APPLICATION_CONFIG_JNDIENV
	    	};
    
    static final String[] APPLICATION_CONFIG_ROW_DESCS = new String[] {
    	mMessages.getString("APPLICATION_CONFIG_CONNECTION_URL"),
    	mMessages.getString("APPLICATION_CONFIG_CONNECTION_FACTORY_NAME"),
    	mMessages.getString("APPLICATION_CONFIG_INITIAL_CONTEXT_FACTORY"),
    	mMessages.getString("APPLICATION_CONFIG_PROVIDER_URL"),
    	mMessages.getString("APPLICATION_CONFIG_SECURITY_PRINCIPAL"),
    	mMessages.getString("APPLICATION_CONFIG_SECURITY_CREDENTIALS"),
    	mMessages.getString("APPLICATION_CONFIG_USERNAME"),
    	mMessages.getString("APPLICATION_CONFIG_PASSWORD"),
    	mMessages.getString("APPLICATION_CONFIG_JNDIENV")
    	};

    static final OpenType[] APPLICATION_CONFIG_ROW_TYPES = new OpenType[] {
    	SimpleType.STRING,
    	SimpleType.STRING, 
    	SimpleType.STRING,
    	SimpleType.STRING, 
    	SimpleType.STRING,
    	SimpleType.STRING, 
    	SimpleType.STRING,
    	SimpleType.STRING,
    	SimpleType.STRING
    	};

    // Attribute names
    public static final String CONFIG_THREADS = "Threads";
    public static final String CONFIG_DEFAULT_REDELIVERY = "DefaultRedeliveryHandling";
    public static final String CONFIG_FORCE_CONCURRENCY_MODE = "ForceConcurrencyMode";
    public static final String CONFIG_FORCE_MAX_CONCURRENT_CONSUMERS = "ForceMaxConcurrentConsumers";

    /**
      * Get the number of threads
      *
      * @return an Integer indicating the number of threads
      */
    public Integer getThreads();
    
    /** Set the number of threads
      *
      * @param an Integer indicating the number of threads
      * @throws InvalidAttributeValueException if the value is not a valid integer
      * @throws MBeanAttribute for other errors
      */
    public void setThreads(Integer val) throws InvalidAttributeValueException, MBeanException;

    /**
     * Return the default Redelivery behaviour
     */
    public String getDefaultRedeliveryHandling();

    /** Set the default Redelivery behaviour
     *
     * @param The default retry behaviour (e.g. "5:5000; 15:60000")
     * @throws InvalidAttributeValueException if the value is not a valid integer
     * @throws MBeanAttribute for other errors
     */
    public void setDefaultRedeliveryHandling(String val) throws InvalidAttributeValueException, MBeanException;

    /**
     * Return the forced concurrency mode, if any
     */
    public String getForceConcurrencyMode();

    /** Set the forced concurrency mode
     *
     * @param The forced concurrency mode, if any ("", "sync", "cc", "serial")
     * @throws InvalidAttributeValueException if the value is not a valid
     * @throws MBeanAttribute for other errors
     */
    public void setForceConcurrencyMode(String val) throws InvalidAttributeValueException, MBeanException;
    
    /**
     * Return the number of forced concurrent consumers, if any (-1 if not set)
     */
    public Integer getForceMaxConcurrentConsumers();

    /** Set the forced number of max concurrent consumers (-1 if not set)
     *
     * @param The forced max concurrent consumers
     * @throws InvalidAttributeValueException if the value is not a valid
     * @throws MBeanAttribute for other errors
     */
    public void setForceMaxConcurrentConsumers(Integer val) throws InvalidAttributeValueException, MBeanException;

}
