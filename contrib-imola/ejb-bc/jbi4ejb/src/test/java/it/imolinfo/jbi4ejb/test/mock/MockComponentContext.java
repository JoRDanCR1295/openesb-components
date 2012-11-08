/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.test.mock;

import java.util.MissingResourceException;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.MBeanNames;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanServer;
import javax.naming.InitialContext;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

/**
 * The ComponentContext - Mock class (for testing pourpose).
 * Example of use:
 *         // Creates and sets the Mock context 
 *        MockComponentContext context = new  MockComponentContext();
 *        context.setInstallRoot(suDeployFile.getAbsolutePath());        
 *        RuntimeContext.getInstance().setComponentContext(context);
 * 
 */
public class MockComponentContext implements ComponentContext {

    /** The get M bean names. */
    private boolean getMBeanNames;

    /** The get M bean server. */
    private boolean getMBeanServer;

    /** The get naming context. */
    private boolean getNamingContext;

    /** The get transaction manager. */
    private boolean getTransactionManager;
    
    private String installRoot;

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#activateEndpoint(javax.xml.namespace.QName, java.lang.String)
     */
    public ServiceEndpoint activateEndpoint(QName serviceName,
            String endpointName) throws JBIException {
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#deactivateEndpoint(javax.jbi.servicedesc.ServiceEndpoint)
     */
    public void deactivateEndpoint(ServiceEndpoint endpoint)
            throws JBIException {
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#deregisterExternalEndpoint(javax.jbi.servicedesc.ServiceEndpoint)
     */
    public void deregisterExternalEndpoint(ServiceEndpoint externalEndpoint)
            throws JBIException {
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getComponentName()
     */
    public String getComponentName() {
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getDeliveryChannel()
     */
    public DeliveryChannel getDeliveryChannel() throws MessagingException {
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getEndpoint(javax.xml.namespace.QName, java.lang.String)
     */
    public ServiceEndpoint getEndpoint(QName service, String name) {
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getEndpointDescriptor(javax.jbi.servicedesc.ServiceEndpoint)
     */
    public Document getEndpointDescriptor(ServiceEndpoint endpoint)
            throws JBIException {
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getEndpoints(javax.xml.namespace.QName)
     */
    public ServiceEndpoint[] getEndpoints(QName interfaceName) {
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getEndpointsForService(javax.xml.namespace.QName)
     */
    public ServiceEndpoint[] getEndpointsForService(QName serviceName) {
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getExternalEndpoints(javax.xml.namespace.QName)
     */
    public ServiceEndpoint[] getExternalEndpoints(QName interfaceName) {
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getExternalEndpointsForService(javax.xml.namespace.QName)
     */
    public ServiceEndpoint[] getExternalEndpointsForService(QName serviceName) {
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getInstallRoot()
     */
    public String getInstallRoot() {
        return installRoot;
    }
    
    

    public void setInstallRoot(String installRoot) {
        this.installRoot = installRoot;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getLogger(java.lang.String, java.lang.String)
     */
    public Logger getLogger(String suffix, String resourceBundleName)
            throws MissingResourceException, JBIException {
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getMBeanNames()
     */
    public MBeanNames getMBeanNames() {
        getMBeanNames = true;
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getMBeanServer()
     */
    public MBeanServer getMBeanServer() {
        getMBeanServer = true;
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getNamingContext()
     */
    public InitialContext getNamingContext() {
        getNamingContext = true;
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getTransactionManager()
     */
    public Object getTransactionManager() {
        getTransactionManager = true;
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#getWorkspaceRoot()
     */
    public String getWorkspaceRoot() {
        return null;
    }

    /**
     * Checks if is get M bean names.
     * 
     * @return true, if is get M bean names
     */
    public boolean isGetMBeanNames() {
        return getMBeanNames;
    }

    /**
     * Checks if is get M bean server.
     * 
     * @return true, if is get M bean server
     */
    public boolean isGetMBeanServer() {
        return getMBeanServer;
    }

    /**
     * Checks if is get naming context.
     * 
     * @return true, if is get naming context
     */
    public boolean isGetNamingContext() {
        return getNamingContext;
    }

    /**
     * Checks if is get transaction manager.
     * 
     * @return true, if is get transaction manager
     */
    public boolean isGetTransactionManager() {
        return getTransactionManager;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#registerExternalEndpoint(javax.jbi.servicedesc.ServiceEndpoint)
     */
    public void registerExternalEndpoint(ServiceEndpoint externalEndpoint)
            throws JBIException {
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentContext#resolveEndpointReference(org.w3c.dom.DocumentFragment)
     */
    public ServiceEndpoint resolveEndpointReference(DocumentFragment epr) {
        return null;
    }

    /**
     * Sets the get M bean names.
     * 
     * @param getMBeanNames
     *            the new get M bean names
     */
    public void setGetMBeanNames(boolean getMBeanNames) {
        this.getMBeanNames = getMBeanNames;
    }

    /**
     * Sets the get M bean server.
     * 
     * @param getMBeanServer
     *            the new get M bean server
     */
    public void setGetMBeanServer(boolean getMBeanServer) {
        this.getMBeanServer = getMBeanServer;
    }

    /**
     * Sets the get naming context.
     * 
     * @param getNamingContext
     *            the new get naming context
     */
    public void setGetNamingContext(boolean getNamingContext) {
        this.getNamingContext = getNamingContext;
    }

    /**
     * Sets the get transaction manager.
     * 
     * @param getTransactionManager
     *            the new get transaction manager
     */
    public void setGetTransactionManager(boolean getTransactionManager) {
        this.getTransactionManager = getTransactionManager;
    }
}
