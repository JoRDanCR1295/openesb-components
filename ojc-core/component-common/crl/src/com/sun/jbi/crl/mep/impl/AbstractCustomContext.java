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
 * @(#)AbstractCustomContext.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

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
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractCustomContext implements ComponentContext {
	private ComponentContext mComponentCtx;
	
	protected AbstractCustomContext(ComponentContext ctx) {
		mComponentCtx = ctx;
	}
	
    /** @see javax.jbi.component.ComponentContext#activateEndpoint(javax.xml.namespace.QName, java.lang.String) */
    public ServiceEndpoint activateEndpoint(QName serviceName,
                                            String endpointName) 
            throws JBIException {
        return getComponentContext().activateEndpoint(serviceName, endpointName);
    }

    /** @see javax.jbi.component.ComponentContext#deactivateEndpoint(javax.jbi.servicedesc.ServiceEndpoint) */
    public void deactivateEndpoint(ServiceEndpoint endpoint)
            throws JBIException {
        getComponentContext().deactivateEndpoint(endpoint);
    }

    /** @see javax.jbi.component.ComponentContext#deregisterExternalEndpoint(javax.jbi.servicedesc.ServiceEndpoint) */
    public void deregisterExternalEndpoint(ServiceEndpoint externalEndpoint)
            throws JBIException {
        getComponentContext().deregisterExternalEndpoint(externalEndpoint);
    }

    /** @see javax.jbi.component.ComponentContext#getComponentName() */
    public String getComponentName() {
        return getComponentContext().getComponentName();
    }

    /** @see javax.jbi.component.ComponentContext#getDeliveryChannel() */
    public DeliveryChannel getDeliveryChannel() throws MessagingException {
        return getComponentContext().getDeliveryChannel();
    }

    /** @see javax.jbi.component.ComponentContext#getEndpoint(javax.xml.namespace.QName, java.lang.String) */
    public ServiceEndpoint getEndpoint(QName service, String name) {
        return getComponentContext().getEndpoint(service, name);
    }

    /** @see javax.jbi.component.ComponentContext#getEndpointDescriptor(javax.jbi.servicedesc.ServiceEndpoint) */
    public Document getEndpointDescriptor(ServiceEndpoint endpoint)
            throws JBIException {
        return getComponentContext().getEndpointDescriptor(endpoint);
    }

    /** @see javax.jbi.component.ComponentContext#getEndpoints(javax.xml.namespace.QName) */
    public ServiceEndpoint[] getEndpoints(QName interfaceName) {
        return getComponentContext().getEndpoints(interfaceName);
    }

    /** @see javax.jbi.component.ComponentContext#getEndpointsForService(javax.xml.namespace.QName) */
    public ServiceEndpoint[] getEndpointsForService(QName serviceName) {
        return getComponentContext().getEndpointsForService(serviceName);
    }

    /** @see javax.jbi.component.ComponentContext#getExternalEndpoints(javax.xml.namespace.QName) */
    public ServiceEndpoint[] getExternalEndpoints(QName interfaceName) {
        return getComponentContext().getExternalEndpoints(interfaceName);
    }

    /** @see javax.jbi.component.ComponentContext#getExternalEndpointsForService(javax.xml.namespace.QName) */
    public ServiceEndpoint[] getExternalEndpointsForService(QName serviceName) {
        return getComponentContext().getExternalEndpointsForService(serviceName);
    }

    /** @see javax.jbi.component.ComponentContext#getInstallRoot() */
    public String getInstallRoot() {
        return getComponentContext().getInstallRoot();
    }

    /** @see javax.jbi.component.ComponentContext#getLogger(java.lang.String, java.lang.String) */
    public Logger getLogger(String suffix, String resourceBundleName)
            throws MissingResourceException, JBIException {
        return getComponentContext().getLogger(suffix, resourceBundleName);
    }

    /** @see javax.jbi.component.ComponentContext#getMBeanNames() */
    public MBeanNames getMBeanNames() {
        return getComponentContext().getMBeanNames();
    }

    /** @see javax.jbi.component.ComponentContext#getMBeanServer() */
    public MBeanServer getMBeanServer() {
        return getComponentContext().getMBeanServer();
    }

    /** @see javax.jbi.component.ComponentContext#getNamingContext() */
    public InitialContext getNamingContext() {
        return getComponentContext().getNamingContext();
    }

    /** @see javax.jbi.component.ComponentContext#getTransactionManager() */
    public Object getTransactionManager() {
        return getComponentContext().getTransactionManager();
    }

    /** @see javax.jbi.component.ComponentContext#getWorkspaceRoot() */
    public String getWorkspaceRoot() {
        return getComponentContext().getWorkspaceRoot();
    }

    /** @see javax.jbi.component.ComponentContext#registerExternalEndpoint(javax.jbi.servicedesc.ServiceEndpoint) */
    public void registerExternalEndpoint(ServiceEndpoint externalEndpoint)
            throws JBIException {
        getComponentContext().registerExternalEndpoint(externalEndpoint);
    }

    /** @see javax.jbi.component.ComponentContext#resolveEndpointReference(org.w3c.dom.DocumentFragment) */
    public ServiceEndpoint resolveEndpointReference(DocumentFragment epr) {
        return getComponentContext().resolveEndpointReference(epr);
    }

    protected ComponentContext getComponentContext() {
        return mComponentCtx;
    }
}
