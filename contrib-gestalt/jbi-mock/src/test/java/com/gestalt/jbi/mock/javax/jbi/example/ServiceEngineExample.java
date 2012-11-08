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
 * ServiceEngineExample.java - ver 1.0 - 2006
 *
 * Copyright 2006-2007 Gestalt, LLC. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.gestalt.jbi.mock.javax.jbi.example;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

import javax.jbi.JBIException;
import javax.jbi.component.*;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;

import javax.management.ObjectName;

import javax.xml.namespace.QName;


/**
 * Service Engine Component Example implementing all the required
 * and optional JBI interfaces. Used in Unit tests to ensure
 * Mock JBI classes are written to cover a JBI Component.
 * This is the incorrect way to create a ServiceEngine. Typically
 * the Bootstrap and Component pieces are separated into two classes.
 */
public class ServiceEngineExample implements Bootstrap, Component,
    ComponentLifeCycle, ServiceUnitManager {
    private ComponentContext context;
    private InstallationContext installContext;

    public void cleanUp() throws JBIException {
    }

    public ObjectName getExtensionMBeanName() {
        return null;
    }

    public void init(InstallationContext installationContext)
        throws JBIException {
        this.installContext = installationContext;

        String componentName = this.installContext.getComponentName();
    }

    public void onInstall() throws JBIException {
    }

    public void onUninstall() throws JBIException {
    }

    public void init(ComponentContext componentContext)
        throws JBIException {
        this.context = componentContext;

        ServiceEndpoint[] serviceEndpoints = context.getEndpoints(new QName(
                    "localPart"));
        serviceEndpoints[0].getEndpointName();
    }

    public void shutDown() throws JBIException {
    }

    /**
     * Don't unit test the start method because there is currently
     * no mock support for the accept method.
     * Instead unit test the handleMessageExchange() method.
     * @throws JBIException
     */
    public void start() throws JBIException {
        MessageExchange me = context.getDeliveryChannel().accept();
        handleMessageExchange(me);
    }

    public void handleMessageExchange(MessageExchange me) {
        // Handle the incoming MessageExchange...
    }

    public void stop() throws JBIException {
    }

    public ComponentLifeCycle getLifeCycle() {
        return null;
    }

    public Document getServiceDescription(ServiceEndpoint serviceEndpoint) {
        return null;
    }

    public ServiceUnitManager getServiceUnitManager() {
        return null;
    }

    public boolean isExchangeWithConsumerOkay(ServiceEndpoint serviceEndpoint,
        MessageExchange messageExchange) {
        return false;
    }

    public boolean isExchangeWithProviderOkay(ServiceEndpoint serviceEndpoint,
        MessageExchange messageExchange) {
        return false;
    }

    public ServiceEndpoint resolveEndpointReference(
        DocumentFragment documentFragment) {
        return null;
    }

    public String deploy(String string, String string1)
        throws DeploymentException {
        return null;
    }

    public void init(String string, String string1) throws DeploymentException {
    }

    public void shutDown(String string) throws DeploymentException {
    }

    public void start(String string) throws DeploymentException {
    }

    public void stop(String string) throws DeploymentException {
    }

    public String undeploy(String string, String string1)
        throws DeploymentException {
        return null;
    }
}
