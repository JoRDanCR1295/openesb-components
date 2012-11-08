/*
 * Copyright (c) 2007 Sun Microsystems, Inc.
 * All Rights Reserved.
 *
 * This program, and all the routines referenced herein,
 * are the proprietary properties and trade secrets of
 * Sun Microsystems.
 *
 * Except as provided for by license agreement, this
 * program shall not be duplicated, used, or disclosed
 * without  written consent signed by an officer of
 * Sun Microsystems.
 */
package com.sun.caps.management.common;

import java.io.IOException;
import java.io.Serializable;

import javax.management.InstanceNotFoundException;
import javax.management.MBeanException;
import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.management.ReflectionException;

import com.sun.caps.management.common.jbi.JBIRemoteException;

/**
 * @author graj
 *
 */
public abstract class AbstractServiceImpl implements Serializable {
    static final long                         serialVersionUID = -1L;

    /** i18n */
    private static I18NBundle       I18NBUNDLE      = null;


    /** remote MBeanServer connection */
    protected transient MBeanServerConnection remoteConnection;

    /** is this a local or remote connection */
    protected boolean                         isRemoteConnection;

    /** Constructor - Constructs a new instance of AbstractServiceImpl */
    public AbstractServiceImpl() {
        this(null, false);
    }

    /**
     * Constructor - Constructs a new instance of AbstractServiceImpl
     *
     * @param serverConnection
     */
    public AbstractServiceImpl(MBeanServerConnection serverConnection) {
        this(serverConnection, false);
    }

    /**
     * Constructor - Constructs a new instance of AbstractServiceImpl
     *
     * @param serverConnection
     * @param isRemoteConnection
     */
    public AbstractServiceImpl(MBeanServerConnection serverConnection,
            boolean isRemoteConnection) {
        this.remoteConnection = serverConnection;
        this.isRemoteConnection = isRemoteConnection;
    }

    /**
     * is this a local or remote connection
     *
     * @return true if remote, false if local
     */
    protected boolean isRemoteConnection() {
        return this.isRemoteConnection;
    }

    // //////////////////////////////////////////////////////
    // -- Common Operations --
    // //////////////////////////////////////////////////////
    /**
     * gives the I18N bundle
     *
     * @return I18NBundle object
     */
    protected I18NBundle getI18NBundle() {
        // lazzy initialize the Client
        if (I18NBUNDLE == null) {
            I18NBUNDLE = new I18NBundle("com.sun.caps.management.client");
        }
        return I18NBUNDLE;
    }

    /**
     * returns mbean server connection.
     *
     * @throws IllegalStateException
     *             on error
     * @return mbeanserver interface
     */
    protected MBeanServerConnection getMBeanServerConnection()
            throws IllegalStateException {
        if (this.remoteConnection == null) {
            throw new IllegalStateException(
                    "caps.management.client.jmx.connection.not.open");
        }
        return this.remoteConnection;
    }

    /**
     * Test whether an mbean is registered.
     *
     * @param objectName
     * @return true when the mbean is registered, false otherwise
     * @throws JBIRemoteException
     */
    protected boolean isRegistered(ObjectName objectName) {
        boolean result = false;
        Boolean resultObject = null;
        try {
            resultObject = (Boolean) this.remoteConnection
                    .isRegistered(objectName);
            if (resultObject != null) {
                result = resultObject.booleanValue();
            }
        } catch (IOException e) {
            result = false;
        } catch (RuntimeException exception) {
            result = false;
        }

        return result;
    }

    /**
     * return JBI Admin UI MBean object name
     * @return object name of the JBI Admin UI MBean
     * @throws ManagementRemoteException
     */
    protected ObjectName getJbiAdminUiMBeanObjectName() throws ManagementRemoteException {
        return CAPSJMXObjectNames.getJbiAdminUiMBeanObjectName();
    }

    /**
     * open connection, invokes the operation on mbean and closes connection.
     * This should not be used if the connection is already opened or not to be
     * close after the invoke operation.
     *
     * @return result object
     * @param objectName
     *            object name
     * @param operationName
     *            operation name
     * @param params
     *            parameters
     * @param signature
     *            signature of the parameters
     * @throws JBIRemoteException
     *             on error
     */
    protected Object invokeMBeanOperation(ObjectName objectName,
            String operationName, Object[] params, String[] signature)
            throws ManagementRemoteException {

        Object result = null;
        if (this.remoteConnection != null) {
            try {
                result = this.remoteConnection.invoke(objectName,
                        operationName, params, signature);
            } catch (InstanceNotFoundException jmxException) {
                throw ManagementRemoteException.filterJmxExceptions(jmxException);
            } catch (MBeanException jmxException) {
                throw ManagementRemoteException.filterJmxExceptions(jmxException);
            } catch (ReflectionException jmxException) {
                throw ManagementRemoteException.filterJmxExceptions(jmxException);
            } catch (IOException jmxException) {
                throw ManagementRemoteException.filterJmxExceptions(jmxException);
            } finally {
            }
        }

        return result;

    }

    /**
     * Single param operation invocation.
     *
     * @return result object
     * @param objectName
     *            object name
     * @param operationName
     *            operation name
     * @param param
     *            String parameter
     * @throws ManagementRemoteException
     *             on error
     */
    protected Object invokeMBeanOperation(ObjectName objectName,
            String operationName, String param)
            throws ManagementRemoteException {

        Object[] params = new Object[1];
        params[0] = param;

        String[] signature = new String[1];
        signature[0] = "java.lang.String";

        return this.invokeMBeanOperation(objectName, operationName, params,
                signature);
    }

}
