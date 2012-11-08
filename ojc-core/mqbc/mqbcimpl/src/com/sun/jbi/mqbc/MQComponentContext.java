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
 */

/*
 * @(#)MQComponentContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc;

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

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

/**
 * A ComponentContext decorator.
 *
 * @author Noel.Ang@sun.com
 */
public final class MQComponentContext implements ComponentContext {
    private final ComponentContext wrappee;
    private final MessagingChannel messagingChannel;
    private volatile boolean useSeparateTransactionBranches = false;

    MQComponentContext(ComponentContext jbiContext,
                       MessagingChannel messagingChannel) {
        assert jbiContext != null;
        assert messagingChannel != null;
        wrappee = jbiContext;
        this.messagingChannel = messagingChannel;
    }

    public MessagingChannel getMessagingChannel() {
        return messagingChannel;
    }

    public boolean getUseSeparateTransactionBranches() {
        return useSeparateTransactionBranches;
    }
    
    public void setUseSeparateTransactionBranches(boolean separate) {
        useSeparateTransactionBranches = separate;
    }

    public ServiceEndpoint activateEndpoint(QName qName, String s)
            throws JBIException {
        return wrappee.activateEndpoint(qName, s);
    }

    public void deactivateEndpoint(ServiceEndpoint serviceEndpoint)
            throws JBIException {
        wrappee.deactivateEndpoint(serviceEndpoint);
    }

    public void registerExternalEndpoint(ServiceEndpoint serviceEndpoint)
            throws JBIException {
        wrappee.registerExternalEndpoint(serviceEndpoint);
    }

    public void deregisterExternalEndpoint(ServiceEndpoint serviceEndpoint)
            throws JBIException {
        wrappee.deregisterExternalEndpoint(serviceEndpoint);
    }

    public ServiceEndpoint resolveEndpointReference(DocumentFragment documentFragment) {
        return wrappee.resolveEndpointReference(documentFragment);
    }

    public String getComponentName() {
        return wrappee.getComponentName();
    }

    public DeliveryChannel getDeliveryChannel() throws MessagingException {
        return wrappee.getDeliveryChannel();
    }

    public ServiceEndpoint getEndpoint(QName qName, String s) {
        return wrappee.getEndpoint(qName, s);
    }

    public Document getEndpointDescriptor(ServiceEndpoint serviceEndpoint)
            throws JBIException {
        return wrappee.getEndpointDescriptor(serviceEndpoint);
    }

    public ServiceEndpoint[] getEndpoints(QName qName) {
        return wrappee.getEndpoints(qName);
    }

    public ServiceEndpoint[] getEndpointsForService(QName qName) {
        return wrappee.getEndpointsForService(qName);
    }

    public ServiceEndpoint[] getExternalEndpoints(QName qName) {
        return wrappee.getExternalEndpoints(qName);
    }

    public ServiceEndpoint[] getExternalEndpointsForService(QName qName) {
        return wrappee.getExternalEndpointsForService(qName);
    }

    public String getInstallRoot() {
        return wrappee.getInstallRoot();
    }

    public Logger getLogger(String s, String s1)
            throws MissingResourceException, JBIException {
        return wrappee.getLogger(s, s1);
    }

    public MBeanNames getMBeanNames() {
        return wrappee.getMBeanNames();
    }

    public MBeanServer getMBeanServer() {
        return wrappee.getMBeanServer();
    }

    public InitialContext getNamingContext() {
        return wrappee.getNamingContext();
    }

    public Object getTransactionManager() {
        return wrappee.getTransactionManager();
    }

    public String getWorkspaceRoot() {
        return wrappee.getWorkspaceRoot();
    }
}
