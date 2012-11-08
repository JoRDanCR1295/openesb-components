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
 * MockComponentContextBasic.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.component;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

import javax.jbi.JBIException;
import javax.jbi.management.MBeanNames;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.naming.InitialContext;
import javax.xml.namespace.QName;
import java.util.MissingResourceException;
import java.util.logging.Logger;


public class MockComponentContextBasic implements MockComponentContext {
    private DeliveryChannel deliveryChannel;
    private MBeanNames mbeanNames;
    private ServiceEndpoint[] serviceEndpoints;

    /**
     * Sets the DeliveryChannel of the ComponentContext that is returned
     * when components call ComponentContext.getDeliveryChannel();
     * @param deliveryChannel The mock DeliveryChannel
     */
    public void setDeliveryChannel(DeliveryChannel deliveryChannel) {
        this.deliveryChannel = deliveryChannel;
    }

    /**
     * Sets the MBeanNames of the ComponentContext that is returned
     * when components call ComponentContext.getMBeanNames();
     * @param mbeanNames The mock MBeanNames
     */
    public void setMBeanNames(MBeanNames mbeanNames) {
        this.mbeanNames = mbeanNames;
    }

    /**
     * Sets the ServiceEndpoints of the ComponentContext that is returned
     * when components call
     * ComponentContext.getEndpoint(),
     * ComponentContext.getEndpoints(),
     * ComponentContext.getEndpointsForService(),
     * ComponentContext.getExternalEndpoints(), or
     * ComponentContext.getExternalEndpointsForService().
     * @param serviceEndpoints The mock ServiceEndpoint array
     */
    public void setEndpoints(ServiceEndpoint[] serviceEndpoints) {
        this.serviceEndpoints = serviceEndpoints;
    }

    public void setEndpoint(ServiceEndpoint serviceEndpoint) {
        this.serviceEndpoints = new ServiceEndpoint[] {serviceEndpoint};
    }

    public ServiceEndpoint activateEndpoint(QName qName, String string)
        throws JBIException {
        return null;
    }

    public void deactivateEndpoint(ServiceEndpoint serviceEndpoint)
        throws JBIException {
    }

    public void registerExternalEndpoint(ServiceEndpoint serviceEndpoint)
        throws JBIException {
    }

    public void deregisterExternalEndpoint(ServiceEndpoint serviceEndpoint)
        throws JBIException {
    }

    public ServiceEndpoint resolveEndpointReference(
        DocumentFragment documentFragment) {
        return null;
    }

    public String getComponentName() {
        return "";
    }

    public DeliveryChannel getDeliveryChannel() throws MessagingException {
        return deliveryChannel;
    }

    public ServiceEndpoint getEndpoint(QName qName, String string) {
        if (serviceEndpoints == null) {
            return null;
        } else {
            return serviceEndpoints[0];
        }
    }

    public Document getEndpointDescriptor(ServiceEndpoint serviceEndpoint)
        throws JBIException {
        return null;
    }

    public ServiceEndpoint[] getEndpoints(QName qName) {
        return serviceEndpoints;
    }

    public ServiceEndpoint[] getEndpointsForService(QName qName) {
        return serviceEndpoints;
    }

    public ServiceEndpoint[] getExternalEndpoints(QName qName) {
        return serviceEndpoints;
    }

    public ServiceEndpoint[] getExternalEndpointsForService(QName qName) {
        return serviceEndpoints;
    }

    public String getInstallRoot() {
        return "";
    }

    public Logger getLogger(String name, String resourceBundleName)
        throws MissingResourceException, JBIException {
        return Logger.getLogger(name, resourceBundleName);
    }

    public MBeanNames getMBeanNames() {
        return mbeanNames;
    }

    public MBeanServer getMBeanServer() {
        return MBeanServerFactory.newMBeanServer();
    }

    public InitialContext getNamingContext() {
        return null;
    }

    public Object getTransactionManager() {
        return new Object();
    }

    public String getWorkspaceRoot() {
        return "";
    }
}
