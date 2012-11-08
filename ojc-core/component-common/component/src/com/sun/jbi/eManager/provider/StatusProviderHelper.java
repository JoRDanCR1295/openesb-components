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
 * @(#)StatusProviderHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.eManager.provider;

import java.util.HashMap;
import java.util.Map;

import javax.management.InstanceAlreadyExistsException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.MalformedObjectNameException;
import javax.management.NotCompliantMBeanException;
import javax.management.ObjectName;
import javax.xml.namespace.QName;

/**
 *
 * @author Sun Microsystems
 */
public class StatusProviderHelper {

    public static final String PROVISIONING_ID = "Provider";
    public static final String CONSUMING_ID = "Consumer";


    String shortDisplayName;
//    String type;
//    String name;
    ObjectName objectName;
    MBeanServer connection;

    // Direct references to the objects getting registered as MBeans.
    static Map providers = new HashMap();

    /**
     * Create an instance, providing an MBeanServer to use
     * @deprecated Use the constructors with additional shortDisplayName argument
     */
    public StatusProviderHelper(String componentType, String componentName, MBeanServer mbeanServer) throws MalformedObjectNameException {
        this(null, componentType, componentName, mbeanServer);
    }


    /**
     * Create an instance, providing an MBeanServer to use
     * @param shortDisplayNm a short display name of the component, as succinct as posssible. As a guideline 16 characters max.
     * @param componentType The type of component, e.g. whether it is a binding or engine - use the constants
     * defined in the  StatusProviderMBean, e.g. COMPONENT_TYPE_BINDING | COMPONENT_TYPE_ENGINE
     * @param componentName the JBI component name
     * @param mBeanServer a reference to the MBean server to use
     */
    public StatusProviderHelper(String shortDisplayNm, String componentType, String componentName, MBeanServer mbeanServer) throws MalformedObjectNameException {
        super();
        this.shortDisplayName = shortDisplayNm;
//        this.type = componentType;
//        this.name = componentName;
        this.objectName = StatusProvider.constructObjectName(componentType, componentName);
        this.connection = mbeanServer;
    }


    /* StatusProviderHelper instance, use standard JMX bootstrapping to determine an MBeanServer.
     */
    public StatusProviderHelper(String shortDisplayNm, String componentType, String componentName) throws MalformedObjectNameException {
        super();
        this.shortDisplayName = shortDisplayNm;
//        this.type = componentType;
//        this.name = componentName;
        this.objectName = StatusProvider.constructObjectName(componentType, componentName);
        this.connection = (MBeanServer)MBeanServerFactory.findMBeanServer(null).get(0);
    }
    
    /** Alternative constructor for StatusProviderHelper
     *  Uses the object name passed in
     * @param shortDisplayNm, a short display name of the component
     * @param objectName, the object name of the component Status MBean
     * @param mbeanServer, the MBean server to use 
     */
    public StatusProviderHelper(String shortDisplayNm, ObjectName objectName, MBeanServer mbeanServer) {
        super();
        this.shortDisplayName = shortDisplayNm;
        this.objectName = objectName;
        this.connection = mbeanServer;
    } 
    
    /** Alternative constructor for StatusProviderHelper
     *  Uses standard JMX boostrapping to determine an MBeanServer
     *  Uses the object name passed in
     * @param shortDisplayNm, a short display name of the component
     * @param objectName, the object name of the component Status MBean
     */
    public StatusProviderHelper(String shortDisplayNm, ObjectName objectName) {
        super();
        this.shortDisplayName = shortDisplayNm;
        this.objectName = objectName;
        this.connection = (MBeanServer)MBeanServerFactory.findMBeanServer(null).get(0);
    } 

    /**
     * Register the Status Provider MBean for this component
     * Note that to catch all JMX related exception one can use JMException
     */
    public void registerMBean() throws InstanceAlreadyExistsException, MBeanRegistrationException, NotCompliantMBeanException {
        if(connection != null) {
            // register MBean only if it is not already registered.
            if(this.connection.isRegistered(this.objectName) == false) {
                StatusProvider provider = new StatusProvider(shortDisplayName);
                providers.put(objectName, provider);
                this.connection.registerMBean(provider, this.objectName);
            }
        }
    }

    /**
     * Register the Status Provider MBean for this component with performance instrumentation enabling.
     * Note that to catch all JMX related exception one can use JMException
     * @param callback The callback object for getting performance measurement for endpoints.
     * @param categories An array of String - each entry in the array specifies the category of measurement to be contained in the performance instrumenation measurement TabularData returned by the component.
     */
    public void registerMBean(String [] categories, PerformanceMeasurement callback) throws InstanceAlreadyExistsException, MBeanRegistrationException, NotCompliantMBeanException {
        registerMBean();
        ((StatusProvider)providers.get(objectName)).setPerformanceMeasurementCategories(categories);
        ((StatusProvider)providers.get(objectName)).setPeformanceMeasurementCallback(callback);
    }
    
    /**
     * Unregister the Status Provider MBean for this component
     * Note that to catch all JMX related exception one can use JMException
     */
    public void unregisterMBean() throws InstanceNotFoundException, MBeanRegistrationException {
        if(connection != null) {
            // unregister MBean only if it is not already registered.
            if(this.connection.isRegistered(objectName) == true) {
                this.connection.unregisterMBean(this.objectName);
                providers.remove(objectName);
            }
        }
    }

    public StatusReporting getStatusReporter() {
        return (StatusReporting) providers.get(objectName);
    }

    /**
     * @deprecated use createProvisioningEndpointIdentifier or createConsumingEndpointIdentifier
     * Get a unique endpoint identifer for use with add/remove endpoints and
     * data retrieval
     * @param serviceName qualified service name the endpoint belongs to
     * @param portName the local name of the port
     * @return unique endpoint identifier string.
     */
    public String createEndpointIdentifier(QName serviceName, String portName) {
        return serviceName.getNamespaceURI() + "," + serviceName.getLocalPart() + "," + portName;
    }

    /**
     * Get a unique provisioning endpoint identifer for use with add/remove endpoints and
     * data retrieval
     * @param serviceName qualified service name the endpoint belongs to
     * @param portName the local name of the port
     * @return unique endpoint identifier string.
     */
    public String createProvisioningEndpointIdentifier(QName serviceName, String portName) {
        return serviceName.getNamespaceURI() + "," + serviceName.getLocalPart()
            + "," + portName + "," + PROVISIONING_ID;
    }

    /**
     * Get a unique consuming endpoint identifer for use with add/remove endpoints and
     * data retrieval
     * @param serviceName qualified service name the endpoint belongs to
     * @param portName the local name of the port
     * @return unique endpoint identifier string.
     */
    public String createConsumingEndpointIdentifier(QName serviceName, String portName) {
        return serviceName.getNamespaceURI() + "," + serviceName.getLocalPart()
            + "," + portName + "," + CONSUMING_ID;
    }

    /**
     * get parameter signatures based on the object type class
     *
     * @param params
     * @return
     */
    private String[] getSignatures(Object[] params) {
        if (params == null || params.length == 0) {
            return null;
        }
        String[] signatures = new String[params.length];
        for (int index = 0; index < params.length; index++) {
            if(params[index] == null) {
                signatures[index] = "java.lang.Object";
            } else {
                signatures[index] = params[index].getClass().getName();
            }
        }
        return signatures;
    }

    private Object invokeWithParameters(ObjectName objName, 
                                        String operationName, 
                                        Object[] parameters) throws Exception {
        Object result = null;
        String[] signature = this.getSignatures(parameters);
        if(this.connection != null) {
            result = connection.invoke(objName,
                    operationName,
                    parameters,
                    signature);
        }
        return result;
    }


    private Object getAttribute(ObjectName objName, String attributeName) throws Exception {
        Object result = null;
        if(this.connection != null) {
            result = connection.getAttribute(objName, attributeName);
        }
        return result;
    }


    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getProvisioningEndpoints()
     */
    public String[] getProvisioningEndpoints() throws Exception {
        String[] result = null;
        result = (String[])this.getAttribute(objectName, "ProvisioningEndpoints");
        return result;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getConsumingEndpoints()
     */
    public String[] getConsumingEndpoints() throws Exception {
        String[] result = null;
        result = (String[])this.getAttribute(objectName, "ConsumingEndpoints");
        return result;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getTotalRequests()
     */
    public long getTotalRequests() throws Exception {
        Long result = null;
        result = (Long)this.getAttribute(objectName, "TotalRequests");
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getTotalReplies()
     */
    public long getTotalReplies() throws Exception {
        Long result = null;
        result = (Long)this.getAttribute(objectName, "TotalReplies");
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getTotalErrors()
     */
    public long getTotalErrors() throws Exception {
        Long result = null;
        result = (Long)this.getAttribute(objectName, "TotalErrors");
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getTotalDone()
     */
    public long getTotalDone() throws Exception {
        Long result = null;
        result = (Long)this.getAttribute(objectName, "TotalDone");
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getTotalSentRequests()
     */
    public long getTotalSentRequests() throws Exception {
        Long result = null;
        result = (Long)this.getAttribute(objectName, "TotalSentRequests");
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getTotalSentReplies()
     */
    public long getTotalSentReplies() throws Exception {
        Long result = null;
        result = (Long)this.getAttribute(objectName, "TotalSentReplies");
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getTotalSentErrors()
     */
    public long getTotalSentErrors() throws Exception {
        Long result = null;
        result = (Long)this.getAttribute(objectName, "TotalSentErrors");
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getTotalSentDones()
     */
    public long getTotalSentDones() throws Exception {
        Long result = null;
        result = (Long)this.getAttribute(objectName, "TotalSentDones");
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getTotalReceivedRequests()
     */
    public long getTotalReceivedRequests() throws Exception {
        Long result = null;
        result = (Long)this.getAttribute(objectName, "TotalReceivedRequests");
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getTotalReceivedReplies()
     */
    public long getTotalReceivedReplies() throws Exception {
        Long result = null;
        result = (Long)this.getAttribute(objectName, "TotalReceivedReplies");
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getTotalReceivedErrors()
     */
    public long getTotalReceivedErrors() throws Exception {
        Long result = null;
        result = (Long)this.getAttribute(objectName, "TotalReceivedErrors");
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getTotalReceivedDones()
     */
    public long getTotalReceivedDones() throws Exception {
        Long result = null;
        result = (Long)this.getAttribute(objectName, "TotalReceivedDones");
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getSentRequests(java.lang.String)
     */
    public long getSentRequests(String endpoint) throws Exception {
        Long result = null;
        String parameters[] = new String[1];
        parameters[0] = endpoint;
        result = (Long)this.invokeWithParameters(objectName, "getSentRequests", parameters);
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getSentReplies(java.lang.String)
     */
    public long getSentReplies(String endpoint) throws Exception {
        Long result = null;
        String parameters[] = new String[1];
        parameters[0] = endpoint;
        result = (Long)this.invokeWithParameters(objectName, "getSentReplies", parameters);
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getSentErrors(java.lang.String)
     */
    public long getSentErrors(String endpoint) throws Exception {
        Long result = null;
        String parameters[] = new String[1];
        parameters[0] = endpoint;
        result = (Long)this.invokeWithParameters(objectName, "getSentErrors", parameters);
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getSentDones(java.lang.String)
     */
    public long getSentDones(String endpoint) throws Exception {
        Long result = null;
        String parameters[] = new String[1];
        parameters[0] = endpoint;
        result = (Long)this.invokeWithParameters(objectName, "getSentDones", parameters);
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getReceivedRequests(java.lang.String)
     */
    public long getReceivedRequests(String endpoint) throws Exception {
        Long result = null;
        String parameters[] = new String[1];
        parameters[0] = endpoint;
        result = (Long)this.invokeWithParameters(objectName, "getReceivedRequests", parameters);
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getReceivedReplies(java.lang.String)
     */
    public long getReceivedReplies(String endpoint) throws Exception {
        Long result = null;
        String parameters[] = new String[1];
        parameters[0] = endpoint;
        result = (Long)this.invokeWithParameters(objectName, "getReceivedReplies", parameters);
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getReceivedErrors(java.lang.String)
     */
    public long getReceivedErrors(String endpoint) throws Exception {
        Long result = null;
        String parameters[] = new String[1];
        parameters[0] = endpoint;
        result = (Long)this.invokeWithParameters(objectName, "getReceivedErrors", parameters);
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.eManager.provider.StatusProviderMBean#getReceivedDones(java.lang.String)
     */
    public long getReceivedDones(String endpoint) throws Exception {
        Long result = null;
        String parameters[] = new String[1];
        parameters[0] = endpoint;
        result = (Long)this.invokeWithParameters(objectName, "getReceivedDones", parameters);
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }
 }
