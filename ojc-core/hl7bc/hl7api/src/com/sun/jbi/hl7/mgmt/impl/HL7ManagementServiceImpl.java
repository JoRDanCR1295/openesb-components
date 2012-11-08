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
 * @(#)$Id: HL7ManagementServiceImpl.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.hl7.mgmt.impl;

import com.sun.esb.management.api.configuration.ConfigurationService;
import com.sun.esb.management.common.ManagementRemoteException;
import com.sun.esb.management.common.Service;
import com.sun.esb.management.impl.configuration.ConfigurationServiceImpl;
import com.sun.jbi.hl7.mgmt.api.HL7ManagementService;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import javax.management.MBeanServerConnection;
import javax.management.ObjectName;

/**
 *
 * @author ylee
 */
public class HL7ManagementServiceImpl implements HL7ManagementService, Service {

    protected static final String SUSPEND_ENDPOINT = "suspend";
    protected static final String RESUME_ENDPOINT = "resume";
    protected static final String IS_ENDPOINT_ACTIVE = "isEndpointActive";
    protected static final String LIST_ENDPOINTS = "listEndpoints";
    protected static final String LIST_ACTIVE_ENDPOINTS = "listActiveEndpoints";
    protected static final String LIST_INACTIVE_ENDPOINTS = "listInactiveEndpoints";
    protected static final String LIST_SERVICE_UNITS = "listServiceUnits";
    protected static final String GET_EXTSYS_CONN_STATUS = "getExternalSystemConnStatus";
    protected static final String LIST_EXTSYS_CONNECTIONS = "listExternalSystemConnections";
    protected static final String GET_ENDPOINTS = "getEndpoints";
    protected static final String GET_SEQUENCE_NUMBER = "getSequenceNumber";
    protected static final String COMPONENT_NAME = "sun-hl7-binding";
    protected static final String CONN_MBEAN_PATTERN = "com.sun.jbi:JbiName=server,ComponentName=sun-hl7-binding,ControlType=Custom,ComponentType=Installed,InstalledType=Binding,CustomControlName=HL7Service-*";

    protected String targetName = "server";
    protected static final String ADMINISTRATION_KEY = "Administration";
    protected static final String HL7SERVICE = "HL7Service-";

    protected MBeanServerConnection connection;
    protected boolean isRemoteConnection;
    protected HL7ManagementServiceImpl serviceInstance = null;
    protected ConfigurationService configurationService;

    public HL7ManagementServiceImpl() {
    }

    public HL7ManagementServiceImpl(MBeanServerConnection theConnection,
            boolean isRemoteConnection) {
        this.connection = theConnection;
        this.isRemoteConnection = isRemoteConnection;
    }

    /**
     * Creates an instance of a service
     *
     * @param connection
     *            The MBeanServerConnection
     * @param isRemoteConnection
     *            true if it is a remote connection, false if not
     * @return the Service object
     * @throws ManagementRemoteException
     *             on error
     */
    public HL7ManagementService getService(
            MBeanServerConnection connection, boolean isRemoteConnection)
            throws ManagementRemoteException {
        if (serviceInstance == null) {
            serviceInstance = new HL7ManagementServiceImpl(connection, isRemoteConnection);
        }
        return serviceInstance;
    }

    public ConfigurationService getConfigurationService() throws ManagementRemoteException {
        if ( configurationService==null ) {
            configurationService = new ConfigurationServiceImpl(connection, isRemoteConnection);
        }
        return configurationService;
    }

    /**
     * Suspend a consuming endpoint No-op for provisioning endpoints and
     * operation will return false
     *
     * @param consumingEndpointName
     *            an unique consuming endpoint identifier
     * @param targetName
     * @param targetInstanceName
     * @return boolean indicating if suspend succeeds
     * @throws ManagementRemoteException
     */
    public boolean suspend(String consumingEndpointName, String targetName,
            String targetInstanceName) throws ManagementRemoteException {
        Boolean resultObject = Boolean.TRUE;
        consumingEndpointName = stripQuotes(consumingEndpointName);
        ConfigurationService configurationService = getConfigurationService();
        Object[] params = {consumingEndpointName};
        String[] signatures = {"java.lang.String"};
        Boolean result = (Boolean) configurationService.invokeExtensionMBeanOperation(
                COMPONENT_NAME,
                ADMINISTRATION_KEY, SUSPEND_ENDPOINT, params, signatures,
                targetName, targetInstanceName);

        return resultObject.booleanValue();
    }

    /**
     * Resume a consuming endpoint No-op for provisioning endpoints and
     * operation will return false.
     *
     * @param consumingEndpointName
     *            an unique consuming endpoint identifier
     * @param targetName
     * @param targetInstanceName
     * @return boolean indicating if resume succeeds
     * @throws ManagementRemoteException
     */
    public boolean resume(String consumingEndpointName, String targetName,
            String targetInstanceName) throws ManagementRemoteException {
        Boolean resultObject = Boolean.TRUE;
        consumingEndpointName = stripQuotes(consumingEndpointName);
        Object[] params = {consumingEndpointName};
        String[] signatures = {"java.lang.String"};
        Boolean result = (Boolean) getConfigurationService().invokeExtensionMBeanOperation(
                COMPONENT_NAME,
                ADMINISTRATION_KEY,RESUME_ENDPOINT, params, signatures,
                targetName, targetInstanceName);

        return resultObject.booleanValue();
    }

    /**
     * Returns true if an consuming endpoint is active, or false if it is
     * suspended Always return true for provisioning endpoints.
     *
     * @param consumingEndpointNamean
     *            unique consuming endpoint identifier
     * @param targetName
     * @param targetInstanceName
     * @return true if endpoint is active, false if it is suspended
     * @throws ManagementRemoteException
     */
    public boolean isEndpointActive(String consumingEndpointName,
            String targetName, String targetInstanceName)
            throws ManagementRemoteException {
        Boolean resultObject = Boolean.FALSE;
        Object[] params = {consumingEndpointName};
        String[] signatures = {"java.lang.String"};
        resultObject = (Boolean) getConfigurationService().invokeExtensionMBeanOperation(
                COMPONENT_NAME,
                ADMINISTRATION_KEY, IS_ENDPOINT_ACTIVE, params,
                signatures, targetName, targetInstanceName);
        return resultObject.booleanValue();
    }

    /**
     * Returns an array of unique endpoint names for active consuming endpoints.
     *
     * @param targetName
     * @param targetInstanceName
     * @return an array of unique endpoint names for active consuming endpoints
     * @throws ManagementRemoteException
     */
    public List<String> listActiveEndpoints(String targetName,
            String targetInstanceName) throws ManagementRemoteException {
        String[] endpoints = null;
        Object[] params = null;
        String[] signatures = null;
        endpoints = (String[]) getConfigurationService().invokeExtensionMBeanOperation(
                COMPONENT_NAME,
                ADMINISTRATION_KEY, LIST_ACTIVE_ENDPOINTS, params,
                signatures, targetName, targetInstanceName);
        List<String> endpointsList = new ArrayList<String>();
        if ( endpoints!=null ) {
            for ( String activeEndpoint : endpoints ) {
                endpointsList.add("\""+activeEndpoint+"\"");
            }
        }
        return endpointsList;
    }

    /**
     * Returns an array of unique endpoint names for inactive (suspended)
     * consuming endpoints.
     *
     * @param targetName
     * @param targetInstanceName
     * @return an array of unique endpoint names for inactive(suspended)
     *         consuming endpoints
     * @throws ManagementRemoteException
     */
    public List<String> listInactiveEndpoints(String targetName,
            String targetInstanceName) throws ManagementRemoteException {
        String[] endpoints = null;
        Object[] params = null;
        String[] signatures = null;
        endpoints = (String[]) getConfigurationService().invokeExtensionMBeanOperation(
                COMPONENT_NAME,
                ADMINISTRATION_KEY, LIST_INACTIVE_ENDPOINTS, params,
                signatures, targetName, targetInstanceName);
        List<String> endpointsList = new ArrayList<String>();
        if ( endpoints!=null ) {
            for ( String activeEndpoint : endpoints ) {
                endpointsList.add("\""+activeEndpoint+"\"");
            }
        }
        return endpointsList;
    }

    public List<String> getEndpoints(String serviceUnit, String targetName, String targetInstanceName) throws ManagementRemoteException {
        List<String> endpointsList = new ArrayList<String>();
        Object[] params = {serviceUnit};
        String[] signatures = null;
        List<String> endpoints = (List<String>) getConfigurationService().invokeExtensionMBeanOperation(
                COMPONENT_NAME,
                ADMINISTRATION_KEY, GET_ENDPOINTS, params,
                signatures, targetName, targetInstanceName);
        if ( endpoints!=null ) {
            for ( String endpoint : endpoints ) {
                endpointsList.add("\""+endpoint+"\"");
            }
        }
        return endpointsList;
    }

    public List<String> listEndpoints(String targetName, String targetInstanceName) throws ManagementRemoteException {
        List<String> endpointsList = new ArrayList<String>();
        Object[] params = null;
        String[] signatures = null;
        List<String> endpoints = (List<String>) getConfigurationService().invokeExtensionMBeanOperation(
                COMPONENT_NAME,
                ADMINISTRATION_KEY, LIST_ENDPOINTS, params,
                signatures, targetName, targetInstanceName);
        if ( endpoints!=null ) {
            for ( String endpoint : endpoints ) {
                endpointsList.add("\""+endpoint+"\"");
            }
        }
        return endpointsList;
    }


    public List<String> getHl7Servers(String targetName, String targetInstanceName) throws ManagementRemoteException {
        List<String> servers = new ArrayList<String>();
        try {
            String pattern = "com.sun.jbi:JbiName=server,ComponentName=sun-hl7-binding,ControlType=Custom,ComponentType=Installed,InstalledType=Binding,CustomControlName=HL7Service-*";
            Set<ObjectName> objNames = connection.queryNames(new ObjectName(pattern), null);
            for (ObjectName objName : objNames ) {
                //System.out.println("ObjectName: "+objName.getCanonicalName());
                String prop = objName.getKeyProperty("CustomControlName");
                String[] tokens = prop.split("-");
                if ( tokens!=null ) {
                    String server = tokens[1] + " " + tokens[2];
                    servers.add(server);
                }
            }
        } catch(Exception e) {
            e.printStackTrace();
        }
        return servers;
    }

    public ConcurrentHashMap<String, String> getExternalSystemConnectionStatus(String serverLocation, long serverPort, String targetName, String targetInstanceName) throws ManagementRemoteException {
        ConcurrentHashMap<String,String> extSystemConnections = new ConcurrentHashMap<String,String>();
        // lookup all the ConnMonitor mbeans
        try {
            String objName = "com.sun.jbi:JbiName=server,ComponentName=sun-hl7-binding,ControlType=Custom,ComponentType=Installed,InstalledType=Binding,CustomControlName=HL7Service-"+serverLocation+"-"+serverPort;
            Object[] params = null;
            String[] sigs = null;
            extSystemConnections = (ConcurrentHashMap)connection.invoke(new ObjectName(objName),"getExternalSystemConnStatus", params, sigs);
        } catch(Exception e) {
            e.printStackTrace();
        }

        return extSystemConnections;
    }

    public ConcurrentHashMap<String, String> getMessageReceivedTimestamps(String targetName, String targetInstanceName) throws ManagementRemoteException {
        ConcurrentHashMap<String,String> msgTimestamps = new ConcurrentHashMap<String,String>();
        // lookup all the ConnMonitor mbeans
        try {
            String pattern = CONN_MBEAN_PATTERN;
            Set<ObjectName> objNames = connection.queryNames(new ObjectName(pattern), null);
            for (ObjectName objName : objNames ) {
                Object[] params = null;
                String[] sigs = null;
                Map<String,String> map = (Map<String,String>)connection.invoke(objName, "getMessageReceivedTimeStamp", params, sigs);
                if (map!=null ) {
                    msgTimestamps.putAll(map);
                }
            }
        } catch(Exception e) {
            e.printStackTrace();
        }
        return msgTimestamps;
    }

    public ConcurrentHashMap<String, String> getMessageSentTimestamps(String targetName, String targetInstanceName) throws ManagementRemoteException {
        ConcurrentHashMap<String,String> msgTimestamps = new ConcurrentHashMap<String,String>();
        try {
            String pattern = CONN_MBEAN_PATTERN;
            Set<ObjectName> objNames = connection.queryNames(new ObjectName(pattern), null);
            for (ObjectName objName : objNames ) {
                Object[] params = null;
                String[] sigs = null;
                Map<String,String> map = (Map<String,String>)connection.invoke(objName, "getACKMessageSentTimeStamp", params, sigs);
                if (map!=null ) {
                    msgTimestamps.putAll(map);
                }
            }
        } catch(Exception e) {
            e.printStackTrace();
        }
        return msgTimestamps;
    }

    public long getSequenceNumber(String serviceUnit, String targetName, String targetInstanceName) throws ManagementRemoteException {
        long seqno = -1;
        Object[] params = {serviceUnit};
        String[] signatures = {"java.lang.String"};
        seqno = (Long) getConfigurationService().invokeExtensionMBeanOperation(
                COMPONENT_NAME,
                ADMINISTRATION_KEY, GET_SEQUENCE_NUMBER, params,
                signatures, targetName, targetInstanceName);
        return seqno;
    }

    public List<String> listExternalSystemConnections(String targetName, String targetInstanceName) throws ManagementRemoteException {
        List<String> extSystemConnections = new ArrayList<String>();
        // lookup all the ConnMonitor mbeans
        try {
            String pattern = CONN_MBEAN_PATTERN;
            Set<ObjectName> objNames = connection.queryNames(new ObjectName(pattern), null);
            for (ObjectName objName : objNames ) {
                //System.out.println("ObjectName: "+objName.getCanonicalName());
                Object[] params = null;
                String[] sigs = null;
                Map<String,String> map = (Map<String,String>)connection.invoke(objName, "getExternalSystemConnStatus", params, sigs);
                if (map!=null ) {
                    for (Iterator iter=map.keySet().iterator(); iter.hasNext(); ) {
                        String clientId = (String)iter.next();
                        extSystemConnections.add(clientId);
                    }
                }
            }
        } catch(Exception e) {
            e.printStackTrace();
        }
        
        return extSystemConnections;
    }

    public List<String> listServiceUnits(String targetName, String targetInstanceName) throws ManagementRemoteException {
        List<String> serviceUnits;
        Object[] params = null;
        String[] signatures = null;
        serviceUnits = (List) getConfigurationService().invokeExtensionMBeanOperation(
                COMPONENT_NAME,
                ADMINISTRATION_KEY, LIST_SERVICE_UNITS, params,
                signatures, targetName, targetInstanceName);
        return serviceUnits;
    }

    private String stripQuotes(String str) {
        return str.replaceAll("\"", "");
    }

}
