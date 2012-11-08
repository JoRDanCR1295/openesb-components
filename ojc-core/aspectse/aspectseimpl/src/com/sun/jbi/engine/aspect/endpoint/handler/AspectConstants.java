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
 * @(#)AspectConstants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.handler;

/**
 * This class has all the constants used by the AspectSEEndpointHandler and its subclasses.
 *
 * @author karthikeyan s
 */

public interface AspectConstants {   
    
    /* Advice Specific TAG */
    public static final String ADVICE_ATTR_TYPE = "type";
    
    public static final String ADVICE_ATTR_CONFIG_FILE = "configurationFile";
    
    public static final String ADVICE_ATTR_ORDER = "order";
    
    /* Configuration file Tags */
    public static final String CONFIG_TAG = "config";
    
    public static final String PROPERTY_TAG = "property";
    
    public static final String PROPERTY_ATTR_NAME = "name";
    
    public static final String PROPERTY_ATTR_VALUE = "value";
    
    public static final String RULESET_TAG = "ruleset";
    
    public static final String RULE_TAG = "rule";
    
    public static final String RULE_TAG_DESTINATION = "destination";
    
    public static final String RULE_TAG_DESTINATION_ATTR = "id";
    
    public static final String XPATH_TAG = "xpath";
    
    public static final String XPATH_ATTR_KEY_EXPRESSION = "keyExpression";    
    
    /* Logging Advice Tags */
    public static final String LOG_TAG_VERBOSITY = "verbosity";
    
    public static final String LOG_TAG_ROTATIONPOLICY = "rotationpolicy";
    
    public static final String LOG_TAG_FILE = "logfile";
    
    /* Logging Advice Values */
    public static final String LOG_VAL_INFO = "INFO";
    
    public static final String LOG_VAL_DEBUG = "DEBUG";
    
    public static final String LOG_VAL_ERROR = "ERROR";
    
    public static final String LOG_VAL_DAILY = "DAILY";
    
    public static final String LOG_VAL_WEEKLY = "WEEKLY";
    
    /* Retry Advice Tags */
    public static final String RETRY_PROP_RATE = "rate";
    
    public static final String RETRY_PROP_TIMEOUT = "timeout";
    
    /* Throttling Advice Tags */
    public static final String THROTTLE_POLLING_TIMER = "throttle_polling_timer";
    public static final String THROTTLE_RATE = "throttle_rate";
    public static final String MAX_THROTTLE_QUEUE_SIZE = "queue-length";
}
