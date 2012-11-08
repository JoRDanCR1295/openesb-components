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
 * @(#)EndpointConfigurationSUDescriptorTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.packaging;

import junit.framework.*;
import java.util.List;
import com.sun.jbi.sapbc.Endpoint.EndpointType;
import javax.xml.namespace.QName;

/**
 *
 * @author sweng
 */
public class EndpointConfigurationSUDescriptorTest extends TestCase {
    EndpointConfigurationSUDescriptor instance = null;
    
    public EndpointConfigurationSUDescriptorTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        instance = new EndpointConfigurationSUDescriptor("test/com/sun/jbi/sapbc/packaging/descriptors");
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EndpointConfigurationSUDescriptorTest.class);
        
        return suite;
    }

    /**
     * Test of addEndpoint method, of class com.sun.jbi.sapbc.packaging.EndpointConfigurationSUDescriptor.
     */
    public void testAddEndpoint() {
        System.out.println("Testing addEndpoint");
        
        QName interfaceNameOut = new QName("http://localhost/sapbctest/SAPOut", "portTypeSAPOut");
        QName serviceOut = new QName("http://localhost/sapbctest/SAPOut", "serviceSAPOut");
        EndpointData p = new EndpointDataImpl(interfaceNameOut, 
                                              serviceOut,
                                              "portSAPOut",
                                               EndpointType.OUTBOUND);
        instance.addEndpoint(p);
        QName interfaceNameIn = new QName("http://localhost/sapbctest/SAPIn", "portTypeSAPIn");
        QName serviceIn = new QName("http://localhost/sapbctest/SAPIn", "portTypeSAPIn");
        p = new EndpointDataImpl(interfaceNameIn, 
                                 serviceIn,
                                 "portSAPIn",
                                  EndpointType.OUTBOUND);
        instance.addEndpoint(p);
        assertEquals(4, instance.endpoints().size());
        
        System.out.println("Successfully tested addEndpooint");
    }

    /**
     * Test of endpoints method, of class com.sun.jbi.sapbc.packaging.EndpointConfigurationSUDescriptor.
     */
    public void testEndpoints() {
        System.out.println("Testing endpoints");
        
        List result = instance.endpoints();
        assertEquals(2, result.size());
        assertTrue(result.get(0) instanceof EndpointData);
        assertTrue(result.get(1) instanceof EndpointData);
        
        System.out.println("Successfully tested endpoints");
    }
}
