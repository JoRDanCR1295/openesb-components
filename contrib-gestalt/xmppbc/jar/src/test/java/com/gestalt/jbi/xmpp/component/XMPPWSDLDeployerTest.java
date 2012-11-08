/**
 *   xmpp-binding-component - XMPP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.xmpp.component;

import com.gestalt.jbi.component.AbstractComponent;
import com.gestalt.jbi.component.manager.AbstractServiceUnitManager;
import com.gestalt.jbi.component.manager.Endpoint;
import com.gestalt.jbi.component.manager.ServiceUnit;
import com.gestalt.jbi.xmpp.extensions.XMPPExtensionRegistry;
import com.gestalt.jbi.xmpp.extensions.XMPPOperation;
import com.gestalt.jbi.xmpp.extensions.XMPPOperationInput;
import com.gestalt.jbi.xmpp.extensions.XMPPOperationOutput;

import junit.framework.TestCase;

import org.jmock.Expectations;
import org.jmock.Mockery;

import org.jmock.integration.junit4.JUnit4Mockery;

import org.jmock.lib.legacy.ClassImposteriser;

import java.io.File;

import java.net.URL;

import java.util.Collection;
import java.util.Map;

import javax.jbi.messaging.MessageExchange;

import javax.wsdl.Definition;

import javax.xml.namespace.QName;


public class XMPPWSDLDeployerTest extends TestCase {
    private static final String WSDL_RESOURCE = "/XmppForecastSearchBotWSDL.wsdl";
    private static final QName SERVICE_NAME = new QName("http://j2ee.netbeans.org/wsdl/XmppForecastSearchBotWSDL",
            "XmppForecastSearchBotWSDLService");
    private static final String ENDPOINT = "XmppForecastSearchBotWSDLPort";
    private Mockery context = new JUnit4Mockery();
    private XMPPComponent component = new XMPPComponent();
    private XMPPWSDLDeployer deployer = new XMPPWSDLDeployer(component);
    private File fileWsdl;

    public void setUp() throws Exception {
        URL url = this.getClass().getResource(WSDL_RESOURCE);
        assertNotNull(url);
        fileWsdl = new File(url.getFile());
        assertNotNull(fileWsdl);
    }

    public void testCreateExtensionRegistry() throws Exception {
        XMPPExtensionRegistry reg = (XMPPExtensionRegistry) deployer.createExtensionRegistry();
        assertNotNull(reg);
    }

    public void testCreateEndpoint() throws Exception {
        XMPPEndpoint endpoint = deployer.createEndpoint(SERVICE_NAME, null,
                ENDPOINT, null, fileWsdl, null, null);
        assertNotNull(endpoint);
    }

    public void testAddEndpoint() throws Exception {
        context.setImposteriser(ClassImposteriser.INSTANCE);

        final AbstractComponent abstractComp = context.mock(AbstractComponent.class);
        final AbstractServiceUnitManager suManager = context.mock(AbstractServiceUnitManager.class);

        context.checking(new Expectations() {

                {
                    allowing(abstractComp).getServiceUnitManager();
                    will(returnValue(suManager));
                    allowing(suManager)
                        .registerEndpoint(with(any(String.class)),
                        with(any(Endpoint.class)));
                }
            });

        ServiceUnit su = new ServiceUnit("name", "", abstractComp);
        deployer.addEndpoint(su, fileWsdl, SERVICE_NAME, ENDPOINT, null,
            MessageExchange.Role.CONSUMER);
        assertEquals(1, su.getEndpoints().size());
    }

    public void testGetXMPPOperations() throws Exception {
        Map<QName, XMPPOperation> operations = deployer.getXMPPOperations(deployer.getWsdlDefinition(
                    fileWsdl), SERVICE_NAME.toString(), ENDPOINT);
        assertNotNull(operations);
        assertEquals(1, operations.size());

        Collection<XMPPOperation> coll = operations.values();
        XMPPOperation op = (XMPPOperation) (coll.toArray())[0];
        assertNotNull(op.getXMPPOperationInput());
        assertNotNull(op.getXMPPOperationOutput());
    }

    public void testSetEndpoingInputsOutputs() throws Exception {
        Definition wsdl = deployer.getWsdlDefinition(fileWsdl);
        Map<QName, XMPPOperation> operations = deployer.getXMPPOperations(wsdl,
                SERVICE_NAME.toString(), ENDPOINT);
        XMPPEndpoint endpoint = deployer.createEndpoint(SERVICE_NAME, null,
                ENDPOINT, null, fileWsdl, null, null);

        deployer.setEndpointInputsOutputs(endpoint, operations.values());

        Map<XMPPOperation, XMPPOperationInput> inputs = endpoint.getXMPPOperationInputs();
        Map<XMPPOperation, XMPPOperationOutput> outputs = endpoint.getXMPPOperationOutputs();

        assertNotNull(inputs);
        assertEquals(1, inputs.size());
        assertNotNull(outputs);
        assertEquals(1, outputs.size());
    }
}
