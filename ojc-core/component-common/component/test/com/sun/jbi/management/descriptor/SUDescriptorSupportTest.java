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
 * @(#)SUDescriptorSupportTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.management.descriptor;

import junit.framework.*;
import java.io.File;
import javax.xml.namespace.QName;

/**
 *
 * @author Sun Microsystems
 */
public class SUDescriptorSupportTest extends TestCase {
    
    public SUDescriptorSupportTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(SUDescriptorSupportTest.class);
        
        return suite;
    }

    public void testBCDescriptor() throws ConfigurationException {
        String testSUDeploymentPath = new File("test/com/sun/jbi/management/descriptor/bc_descriptor").getAbsolutePath();
        SUDescriptorSupport support = new SUDescriptorSupport(testSUDeploymentPath);
        boolean bc = support.isBindingComponentDescriptor();
        
        
        assertTrue("Descriptor could not be located", support.isDescriptorPresent());
        assertTrue("binding-component attribute not consistent", bc);
        
        Consumes[] consumes = support.getConsumes();
        
        assertEquals(1, consumes.length);
        assertEquals(new QName("urn:Test2TargetNamespace", "Test2PortType", "ifns"), consumes[0].getInterfaceName());
        assertEquals(new QName("urn:Test2TargetNamespace", "Test2Service", "servicens"), consumes[0].getServiceName());
        assertEquals("Test2Port", consumes[0].getEndpointName());
        assertFalse(consumes[0].isProvider());
        
        Provides[] provides = support.getProvides();
        assertEquals(1, provides.length);
        assertEquals(new QName("http://www.xmethods.net/sd/CurrencyExchangeService.wsdl", "CurrencyExchangePortType", "ifns"), provides[0].getInterfaceName());
        assertEquals(new QName("http://www.xmethods.net/sd/CurrencyExchangeService.wsdl", "CurrencyExchangeService", "toplevelservicens"), provides[0].getServiceName());
        assertEquals("CurrencyExchangePort", provides[0].getEndpointName());
        assertTrue(provides[0].isProvider());
        
        EndpointIdentifier[] svcs = support.getServices();
        assertEquals(2, svcs.length);
        assertEquals(new QName("http://www.xmethods.net/sd/CurrencyExchangeService.wsdl", "CurrencyExchangePortType", "ifns"), svcs[0].getInterfaceName());        
        assertEquals(new QName("urn:Test2TargetNamespace", "Test2PortType", "ifns"), svcs[1].getInterfaceName());
    }    

/*    
    public testSEDescriptor() {
    }
*/
    
}
