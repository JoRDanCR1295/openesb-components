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
 * @(#)StatusProvider.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.management;

import java.io.IOException;
import java.io.Serializable;
import java.util.Iterator;
import java.util.Set;

import javax.management.AttributeNotFoundException;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanServer;
import javax.management.MBeanServerConnection;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.management.QueryExp;
import javax.management.ReflectionException;

import com.sun.jbi.cam.connectors.ServerConnector;
import com.sun.jbi.cam.connectors.LocalServerConnector;
import com.sun.jbi.cam.common.EqualsUtil;
import com.sun.jbi.cam.common.resources.Messages;
import java.util.logging.Logger;

/**
 * @author ylee
 * @author Graj
 *
 */
public class StatusProvider implements Serializable {

    private static final String OBJECT_NAME_PREFIX = "com.sun.ebi:ServiceType=Status,InstallationType="; //$NON-NLS-1$
    private static final String OBJECT_NAME_SUFFIX = ",IdentificationName="; //$NON-NLS-1$

    private String hostName;
    private String httpAdminPort;
    private String userName;
    private String password;

    private String componentType;
    private String componentName;
    private String componentShortName;

    private ServerConnector connector;
    private String objectNameString = StatusProvider.OBJECT_NAME_PREFIX;
    private ObjectName objectName = null;

    private Logger logger = Logger.getLogger(StatusProvider.class.getName());


    /**
     *
     * @param type
     * @param name
     */
    public StatusProvider(String type, String name) {
        this(type,name,null);
    }

    /**
     *
     * @param type
     * @param name
     * @param connector
     */
    public StatusProvider(String type, String name,ServerConnector connector) {
        this.componentType = type;
        this.componentName = name;
        if ( connector==null ) {
            this.connector = new LocalServerConnector();
        } else {
            this.connector = connector;
        }
        this.hostName = this.connector.getHostName();
        this.httpAdminPort = this.connector.getPort();
        this.userName = this.connector.getUserName();
        this.password = this.connector.getPassword();

        this.objectNameString += (type+StatusProvider.OBJECT_NAME_SUFFIX+name);
        try {
            this.initialize();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * @param hostName
     * @param httpAdminPort
     * @param userName
     * @param password
     */
    public StatusProvider(String type,
                          String name,
                          String hostName,
                          String httpAdminPort,
                          String userName,
                          String password) {
        this.componentType = type;
        this.componentName = name;
        this.hostName = hostName;
        this.httpAdminPort = httpAdminPort;
        this.userName = userName;
        this.password = password;
        this.objectNameString += (type+StatusProvider.OBJECT_NAME_SUFFIX+name);
        try {
            this.initialize();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }



    /**
     * @return Returns the password.
     */
    public String getPassword() {
        return this.password;
    }

    /**
     * @param password The password to set.
     */
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * @return Returns the userName.
     */
    public String getUserName() {
        return this.userName;
    }

    /**
     * @param userName The userName to set.
     */
    public void setUserName(String userName) {
        this.userName = userName;
    }

    /**
     * @return Returns the componentName.
     */
    public String getComponentName() {
        return this.componentName;
    }

    /**
     * @return Returns the componentType.
     */
    public String getComponentType() {
        return this.componentType;
    }

    /**
     * @return Returns the hostName.
     */
    public String getHostName() {
        return this.hostName;
    }

    /**
     * @return Returns the httpAdminPort.
     */
    public String getHttpAdminPort() {
        return this.httpAdminPort;
    }

    private void initialize() throws MalformedObjectNameException, AttributeNotFoundException, InstanceNotFoundException, MBeanException, ReflectionException, IOException {
        QueryExp queryExpression = null;
        Set set = null;
        String resultObject = null;
        ObjectName objName = new ObjectName(this.objectNameString);

        if (this.connector != null) {
            MBeanServerConnection cn = this.connector.getConnection();
            if(cn != null) {
                set = cn.queryNames(objName, queryExpression);
                Iterator iterator = set.iterator();
                if((iterator != null) && (iterator.hasNext() == true)) {
                    objName = (ObjectName) iterator.next();
                    if(objName != null) {
                        this.objectName = objName;
                    }
                }
            } else {
                logger.warning(Messages.getString("StatusProvider.ConnectionFailed")); //$NON-NLS-1$
            }
            /////////////////////////////////////////////////////////////////////////////
            // TODO: need to make components implement a getShortDisplayName() operation
            // == Start In lieu of this ==
            /////////////////////////////////////////////////////////////////////////////
            if((this.componentName != null) && (this.componentName.length() > 16)) {
                this.componentShortName = this.componentName.substring(0, 16);
            } else {
                this.componentShortName = this.componentName;
            }
            // == End In lieu of this ==
            /////////////////////////////////////////////////////////////////////////////
        } 
    }

    /**
     * @return Returns the objectName.
     */
    public ObjectName getObjectName() {
        return this.objectName;
    }

    /**
     * @return Returns the Short Display Name.
     */
    public String getShortDisplayName() {
        return componentShortName;
    }

    /**
     * get parameter signatures based on the object type class
     *
     * @param params
     * @return
     */
    public String[] getSignatures(Object[] params) {
        if (params == null || params.length == 0) {
            return null;
        }
        String[] signatures = new String[params.length];
        for (int index = 0; index < params.length; index++) {
            if(params[index] == null) {
                signatures[index] = "java.lang.Object"; //$NON-NLS-1$
            } else {
                signatures[index] = params[index].getClass().getName();
            }
        }
        return signatures;
    }

    private String[] endpointInvoke(ObjectName objectName, String operationName, Object[] parameters) {
        String result[] = null;
        Throwable throwable = null;
        String[] signature = null;

        if(objectName == null) {
            return null;
        }

        try {
            if (this.connector != null) {
                MBeanServerConnection connection = this.connector.getConnection();
                result = (String[])connection.invoke(objectName,
                        operationName,
                        parameters,
                        signature);
            } 
        } catch (Exception e) {
            e.printStackTrace();
        }
        return result;
    }

    private long invokeWithParameters(ObjectName objectName, String operationName, Object[] parameters) {
        Long result = new Long(0);
        Throwable throwable = null;
        //System.out.println("StatusProvider::invokeWithParameters() -"+operationName);

        if(objectName == null) {
            return -1L;
        }

        String[] signature = this.getSignatures(parameters);
        try {
            if(this.connector != null) {
                MBeanServerConnection connection = this.connector.getConnection();
                result = (Long)connection.invoke(objectName,
                        operationName,
                        parameters,
                        signature);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    private long invoke(ObjectName objectName, String operationName, Object[] parameters) {
        Long result = new Long(0);
        Throwable throwable = null;
        String[] signature = null;

        if(objectName == null) {
            return -1L;
        }

        try {
            if(this.connector != null) {
                MBeanServerConnection connection = this.connector.getConnection();
                result = (Long)connection.invoke(objectName,
                        operationName,
                        parameters,
                        signature);
            } 
        } catch (Exception e) {
            e.printStackTrace();
        }
        if(result != null) {
            return result.longValue();
        }
        return 0L;
    }

    //  Retrieves the list of provisioning endpoints for that component
    public String[] getProvisioningEndpoints() {
        String[] result = null;
        result = this.endpointInvoke(objectName,
                             "getProvisioningEndpoints", //$NON-NLS-1$
                             null);
        return result;
    }

    //  Retrieves the list of consuming endpoints for that component
    public String[] getConsumingEndpoints() {
        String[] result = null;
        result = this.endpointInvoke(objectName,
                             "getConsumingEndpoints", //$NON-NLS-1$
                             null);
        return result;
    }

    //  Retrieves the total number of requests handled
    public long getTotalRequests() {
        long result = 0L;
        result = this.invoke(objectName, "getTotalRequests", null); //$NON-NLS-1$
        return result;
    }

    //  Retrieves the total number of replies sent out
    public long getTotalReplies() {
        long result = 0L;
        result = this.invoke(objectName, "getTotalReplies", null); //$NON-NLS-1$
        return result;
    }

    //  Retrieves the total number of messages with faults
    public long getTotalErrors() {
        long result = 0L;
        result = this.invoke(objectName, "getTotalErrors", null); //$NON-NLS-1$
        return result;
    }

    //  Retrieves the total number of messages successfully processed.
    public long getTotalDone() {
        long result = 0L;
        result = this.invoke(objectName, "getTotalDone", null); //$NON-NLS-1$
        return result;
    }

    //  Retrieves the total number of message requests sent.
    public long getTotalSentRequests() {
        long result = 0L;
        result = this.invoke(objectName, "getTotalSentRequests", null); //$NON-NLS-1$
        return result;
    }

    //  Retrieves the total number of message replies sent.
    public long getTotalSentReplies() {
        long result = 0L;
        result = this.invoke(objectName, "getTotalSentReplies", null); //$NON-NLS-1$
        return result;
    }

    //  Retrieves the total number of errors sent.
    public long getTotalSentErrors() {
        long result = 0L;
        result = this.invoke(objectName, "getTotalSentErrors", null); //$NON-NLS-1$
        return result;
    }

    //  Retrieves the total number of messages sent that were successfully processed.
    public long getTotalSentDones() {
        long result = 0L;
        result = this.invoke(objectName, "getTotalSentDones", null); //$NON-NLS-1$
        return result;
    }

    //  Retrieves the total number of requests received that were successfully processed.
    public long getTotalReceivedRequests() {
        long result = 0L;
        result = this.invoke(objectName, "getTotalReceivedRequests", null); //$NON-NLS-1$
        return result;
    }

    //  Retrieves the total number of received replies.
    public long getTotalReceivedReplies() {
        long result = 0L;
        result = this.invoke(objectName, "getTotalReceivedReplies", null); //$NON-NLS-1$
        return result;
    }

    //  Retrieves the total number of received errors.
    public long getTotalReceivedErrors() {
        long result = 0L;
        result = this.invoke(objectName, "getTotalReceivedErrors", null); //$NON-NLS-1$
        return result;
    }

    //  Retrieves the total number of receives successfully processed.
    public long getTotalReceivedDones() {
        long result = 0L;
        result = this.invoke(objectName, "getTotalReceivedDones", null); //$NON-NLS-1$
        return result;
    }

    //  Retrieves the number of sent requests for the specified endpoint.
    public long getSentRequests(String endpoint) {
        long result = 0L;
        Object[] params = new Object[1];
        params[0] = endpoint;
        result = this.invokeWithParameters(objectName,
                             "getSentRequests", //$NON-NLS-1$
                             params);
        return result;
    }

    //  Retrieves the number of sent replies for the specified endpoint.
    public long getSentReplies(String endpoint) {
        long result = 0L;
        Object[] params = new Object[1];
        params[0] = endpoint;
        result = this.invokeWithParameters(objectName,
                             "getSentReplies", //$NON-NLS-1$
                             params);
        return result;
    }

    //  Retrieves the number of sent errors for the specified endpoint.
    public long getSentErrors(String endpoint) {
        long result = 0L;
        Object[] params = new Object[1];
        params[0] = endpoint;
        result = this.invokeWithParameters(objectName,
                             "getSentErrors", //$NON-NLS-1$
                             params);
        return result;
    }

    //  Retrieves the number of sent successfully processed for the specified endpoint.
    public long getSentDones(String endpoint) {
        long result = 0L;
        Object[] params = new Object[1];
        params[0] = endpoint;
        result = this.invokeWithParameters(objectName,
                             "getSentDones", //$NON-NLS-1$
                             params);
        return result;
    }

    //  Retrieves the number of received requests for the specified endpoint.
    public long getReceivedRequests(String endpoint) {
        long result = 0L;
        Object[] params = new Object[1];
        params[0] = endpoint;
        result = this.invokeWithParameters(objectName,
                             "getReceivedRequests", //$NON-NLS-1$
                             params);
        return result;
    }
    //  Retrieves the number of received replies for the specified endpoint.
    public long getReceivedReplies(String endpoint) {
        long result = 0L;
        Object[] params = new Object[1];
        params[0] = endpoint;
        result = this.invokeWithParameters(objectName,
                             "getReceivedReplies", //$NON-NLS-1$
                             params);
        return result;
    }

    //  Retrieves the number of received errors for the specified endpoint.
    public long getReceivedErrors(String endpoint) {
        long result = 0L;
        Object[] params = new Object[1];
        params[0] = endpoint;
        result = this.invokeWithParameters(objectName,
                             "getReceivedErrors", //$NON-NLS-1$
                             params);
        return result;
    }

    //  Retrieves the number of received successfully processed for the specified endpoint.
    public long getReceivedDones(String endpoint) {
        long result = 0L;
        Object[] params = new Object[1];
        params[0] = endpoint;
        result = this.invokeWithParameters(objectName,
                             "getReceivedDones", //$NON-NLS-1$
                             params);
        return result;
    }

    public void dump() {
        logger.info(Messages.getString("StatusProvider.SLANT_LINE")); //$NON-NLS-1$
        logger.info(Messages.getString("StatusProvider.StatusProvider")); //$NON-NLS-1$
        logger.info(Messages.getString("StatusProvider.SLANT_LINE")); //$NON-NLS-1$
        logger.info(Messages.getString("StatusProvider.ComponentType")+componentType); //$NON-NLS-1$
        logger.info(Messages.getString("StatusProvider.ComponentName")+componentName); //$NON-NLS-1$
        logger.info(Messages.getString("StatusProvider.ComponentShortName")+componentShortName); //$NON-NLS-1$
        logger.info(Messages.getString("StatusProvider.HostName")+hostName); //$NON-NLS-1$
        logger.info(Messages.getString("StatusProvider.HttpAdminPort")+httpAdminPort); //$NON-NLS-1$
        logger.info(Messages.getString("StatusProvider.UserName")+userName); //$NON-NLS-1$
        logger.info(Messages.getString("StatusProvider.ObjectName")+objectName.toString()); //$NON-NLS-1$
        logger.info(Messages.getString("StatusProvider.SLANT_LINE")); //$NON-NLS-1$
    }

    public boolean equals(Object aThat) {
        // check for self-comparison
        if (this == aThat)
            return true;

        // use instanceof instead of getClass here for two reasons
        // 1. if need be, it can match any supertype, and not just one class;
        // 2. it renders an explict check for "that == null" redundant, since
        // it does the check for null already - "null instanceof [type]" always
        // returns false. (See Effective Java by Joshua Bloch.)
        if (!(aThat instanceof StatusProvider))
            return false;
        // Alternative to the above line :
        // if ( aThat == null || aThat.getClass() != this.getClass() ) return
        // false;

        // cast to native object is now safe
        StatusProvider that = (StatusProvider) aThat;

        // now a proper field-by-field evaluation can be made
        return  EqualsUtil.areEqual(this.componentType, that.componentType)
                && EqualsUtil.areEqual(this.componentName, that.componentName);
        /*
                && EqualsUtil.areEqual(this.componentShortName, that.componentShortName)
                && EqualsUtil.areEqual(this.httpAdminPort, that.httpAdminPort)
                && EqualsUtil.areEqual(this.userName, that.userName)
                && EqualsUtil.areEqual(this.password, that.password);*/
    }


    public static void main(String[] args) {
    }
}
