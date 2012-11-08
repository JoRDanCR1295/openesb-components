/*
 * @(#)Rules4JBIServiceEndpointTest.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
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

package org.openesb.components.rules4jbi.shared.config;

import org.openesb.components.rules4jbi.shared.util.XOMUtils;
import javax.xml.namespace.QName;
import nu.xom.Element;
import nu.xom.Nodes;
import nu.xom.converters.DOMConverter;
import org.junit.Before;
import org.junit.Test;
//import static org.junit.Assert.*;
import static nu.xom.tests.XOMTestCase.*;
import org.w3c.dom.DocumentFragment;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:18 $
 * 
 * @since 0.1
 */
public class Rules4JBIServiceEndpointTest {

    private Rules4JBIServiceEndpoint endpoint1;
    
    private Rules4JBIServiceEndpoint endpoint2;
    
    
    public Rules4JBIServiceEndpointTest() {
    }

    @Before
    public void setUp() {
        endpoint1 = 
            new Rules4JBIServiceEndpoint("http://example.org", "HelloPortType", "HelloService", "HelloPort");
        
        endpoint2 = 
            new Rules4JBIServiceEndpoint("http://whatnot", "echoInterface", "echoService", "echoEndpoint");
    }

    @Test
    public void getAsReference() {
        String expected = "<jbi:end-point-reference xmlns:jbi='http://java.sun.com/xml/ns/jbi'"
                + " xmlns:s='http://example.org'"
                + " jbi:end-point-name='HelloPort' jbi:service-name='s:HelloService'/>";

        DocumentFragment reference = endpoint1.getAsReference(null);
        Nodes nodes = DOMConverter.convert(reference);
        
        assertEquals(1, nodes.size());
        assertEquals(XOMUtils.toElement(expected), nodes.get(0));
        
        expected = "<jbi:end-point-reference xmlns:jbi='http://java.sun.com/xml/ns/jbi'"
                + " xmlns:s='http://whatnot'"
                + " jbi:end-point-name='echoEndpoint' jbi:service-name='s:echoService'/>";

        reference = endpoint2.getAsReference(null);
        nodes = DOMConverter.convert(reference);
        
        assertEquals(1, nodes.size());
        assertEquals(XOMUtils.toElement(expected), nodes.get(0));
    }

    @Test
    public void createEndpointReference() {
        String expected = "<jbi:end-point-reference xmlns:jbi='http://java.sun.com/xml/ns/jbi'"
                + " xmlns:s='http://www.example.org/xml/ns/echo'"
                + " jbi:end-point-name='echoPort' jbi:service-name='s:echoService'/>";
                
        Element result = Rules4JBIServiceEndpoint.createEndpointReference(
                "http://www.example.org/xml/ns/echo", "echoService", "echoPort");
        assertEquals(XOMUtils.toElement(expected), result);
    }

    @Test
    public void getEndpointName() {
        assertEquals("HelloPort", endpoint1.getEndpointName());
        assertEquals("echoEndpoint", endpoint2.getEndpointName());
    }

    @Test
    public void getInterfaces() {
        assertEquals(1, endpoint1.getInterfaces().length);
        assertEquals(new QName("http://example.org", "HelloPortType"), endpoint1.getInterfaces()[0]);
        
        assertEquals(1, endpoint2.getInterfaces().length);
        assertEquals(new QName("http://whatnot", "echoInterface"), endpoint2.getInterfaces()[0]);
    }

    @Test
    public void getServiceName() {
        assertEquals(new QName("http://example.org", "HelloService"), endpoint1.getServiceName());
        assertEquals(new QName("http://whatnot", "echoService"), endpoint2.getServiceName());
    }
}
