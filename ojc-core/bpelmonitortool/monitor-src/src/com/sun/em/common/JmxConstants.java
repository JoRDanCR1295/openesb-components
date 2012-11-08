/*
 * Copyright 2007 Sun Microsystems, Inc. All rights reserved.
 */

package com.sun.em.common;

/**
 * @author ylee
 *
 */
public interface JmxConstants {

    public static final String JMX_OPARTION_INVOKE = "invoke";
    public static final int JMX_OPARTION_INVOKE_INDEX = 0;
    public static final String JMX_OPARTION_CREATEMBEAN = "createMBean";
    public static final int JMX_OPARTION_CREATEMBEAN_INDEX = 1;
    public static final String JMX_OPARTION_UNREGISTERMBEAN = "unregisterMBean";
    public static final int JMX_OPARTION_UNREGISTERMBEAN_INDEX = 2;
    public static final String JMX_OPARTION_GETOBJECTINSTANCE = "getObjectInstance";
    public static final int JMX_OPARTION_GETOBJECTINSTANCE_INDEX = 3;
    public static final String JMX_OPARTION_QUERYMBEANS = "queryMBeans";
    public static final int JMX_OPARTION_QUERYMBEAN_INDEX = 4;
    public static final String JMX_OPARTION_QUERYNAMES = "queryNames";
    public static final int JMX_OPARTION_QUERYNAMES_INDEX = 5;
    public static final String JMX_OPARTION_ISREGISTERED = "isRegistered";
    public static final int JMX_OPARTION_ISREGISTERED_INDEX = 6;
    public static final String JMX_OPARTION_GETMBEANCOUNT = "getMBeanCount";
    public static final int JMX_OPARTION_GETMBEANCOUNT_INDEX = 7;
    public static final String JMX_OPARTION_GETATTRIBUTE = "getAttribute";
    public static final int JMX_OPARTION_GETATTRIBUTE_INDEX = 8;
    public static final String JMX_OPARTION_GETATTRIBUTES = "getAttributes";
    public static final int JMX_OPARTION_GETATTRIBUTES_INDEX = 9;
    public static final String JMX_OPARTION_SETATTRIBUTE = "setAttribute";
    public static final int JMX_OPARTION_SETATTRIBUTE_INDEX = 10;
    public static final String JMX_OPARTION_SETATTRIBUTES = "setAttributes";
    public static final int JMX_OPARTION_SETATTRIBUTES_INDEX = 11;
    public static final String JMX_OPARTION_GETDEFAULTDOMAIN = "getDefaultDomain";
    public static final int JMX_OPARTION_GETDEFAULTDOMAIN_INDEX = 12;
    public static final String JMX_OPARTION_GETDOMAINS = "getDomains";
    public static final int JMX_OPARTION_GETDOMAINS_INDEX = 13;
    public static final String JMX_OPARTION_GETMBEANINFO = "getMBeanInfo";
    public static final int JMX_OPARTION_GETMBEANINFO_INDEX = 14;
    public static final String JMX_OPARTION_ISINSTANCEOF = "isInstanceOf";
    public static final int JMX_OPARTION_ISINSTANCEOF_INDEX = 15;
    public static final String JMX_OPARTION_ADDNOTIFICATIONLISTENER = "addNotificationListener";
    public static final int JMX_OPARTION_ADDNOTIFICATIONLISTENE_INDEX = 16;
    public static final String JMX_OPARTION_REMOVENOTIFICATIONLISTENER = "removeNotificationListener";
    public static final int JMX_OPARTION_REMOVENOTIFICATIONLISTENER_INDEX = 17;
    public static final String JMX_OPARTION_REMOVENOTIFICATIONLISTENERS = "removeNotificationListeners";
    public static final int JMX_OPARTION_REMOVENOTIFICATIONLISTENERS_INDEX = 18;

    public static final String MESSAGE_TERGET_MONITOR = "Monitor";
    public static final String MESSAGE_TERGET_DEPLOYER = "Deployer";
    public static final String MESSAGE_TERGET_PREFIX = "EnterpriseMonitor";
    public static final String MESSAGE_TERGET_URI = 
        "http://ICANEnterpriseMonitor.Sun.com";

    public static final String MESSAGE_TERGET_MONITOR_OP = "JmxOpName";
    public static final String OP_NAME = "OperationName";
    public static final String OBJECT_NAME = "ObjectName";
    public static final String METHOD_NAME = "TargetMethodName";
    public static final String PARAMETER = "Parameter";
    public static final String PARAMETER_TYPE = "ParameterType";
    public static final String NULL_VALUE = "NullIndicator";
    
    
    public static final String RESULT = "OpertionResult";
    public static final String RETURN_VALUE = "OpertionValue";
    public static final String RESULT_OK = "OK";
    public static final String RESULT_FAILED = "FAILED";
    
    public static final String HOSTING_MBEANSERVER_FACTORY = "sunJIS.jmx.hosting.server";
    
}
