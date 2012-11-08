/*
 * @(#)PartnerLinkExtensibilityElementTest.java        $Revision: 1.1 $ $Date: 2008/11/24 12:49:36 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.shared.wsdl.extension.bpel;

import javax.wsdl.Definition;
import javax.wsdl.PortType;
import javax.wsdl.WSDLException;
import javax.wsdl.factory.WSDLFactory;
import javax.xml.namespace.QName;

import nu.xom.Element;

import org.junit.Before;
import org.junit.Test;
//import static org.junit.Assert.*;
import static nu.xom.tests.XOMTestCase.*;

import org.openesb.components.rules4jbi.shared.util.XOMUtils;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/11/24 12:49:36 $
 * 
 * @since 0.3
 */
public class PartnerLinkExtensibilityElementTest {
    
    private static final String TEST_TARGET_NAMESPACE = "http://www.example.org/test";

    private PartnerLinkExtensibilityElement extensibilityElement = null;
    
    private Definition definition = null;
    
    @Before
    public void setUp() throws Exception {
        WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        definition = wsdlFactory.newDefinition();
        
        definition.setTargetNamespace(TEST_TARGET_NAMESPACE);
        definition.addNamespace("tns", TEST_TARGET_NAMESPACE);
        definition.addNamespace("plnk", PartnerLinkExtensibilityElement.NAMESPACE_URI);
        
        PortType portType = definition.createPortType();
        portType.setQName(new QName(TEST_TARGET_NAMESPACE, "FooPortType"));
        portType.setUndefined(false);
        definition.addPortType(portType);
        
        extensibilityElement = new PartnerLinkExtensibilityElement();
    }

    @Test
    public void parse() throws WSDLException {
        String partnerLink = "<plnk:partnerLinkType xmlns:plnk='http://docs.oasis-open.org/wsbpel/2.0/plnktype' "
                + "name='FooServiceType'>"
                + "<plnk:role name='FooServiceProvider' portType='tns:FooPortType'/>"
                + "</plnk:partnerLinkType>";
        
        extensibilityElement.parse(XOMUtils.toElement(partnerLink), definition);
        
        assertEquals("FooServiceType", extensibilityElement.getTypeName());
        
        assertEquals("FooServiceProvider", extensibilityElement.getRoleName());
        
        PortType parsedPortType = extensibilityElement.getPortType();
        
        assertNotNull(parsedPortType);
        
        assertEquals(new QName(TEST_TARGET_NAMESPACE, "FooPortType"), parsedPortType.getQName());
    }

    @Test
    public void createPartnerLink() {
        String expected = "<plnk:partnerLinkType xmlns:plnk='http://docs.oasis-open.org/wsbpel/2.0/plnktype' "
                + "name='FooServiceType'>"
                + "<plnk:role name='FooServiceProvider' portType='tns:FooPortType'/>"
                + "</plnk:partnerLinkType>";

        Element result = PartnerLinkExtensibilityElement.createPartnerLink(
                "FooServiceType", "FooServiceProvider", "tns:FooPortType");

        assertEquals(XOMUtils.toElement(expected), result);
    }
}
