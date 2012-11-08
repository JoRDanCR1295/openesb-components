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
 * @(#)EndpointDataImplTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.packaging;

import com.sun.jbi.sapbc.Endpoint.EndpointType;
import javax.xml.namespace.QName;
import junit.framework.*;

/**
 *
 * @author sweng
 */
public class EndpointDataImplTest extends TestCase {
    EndpointDataImpl instance = null;
    
    public EndpointDataImplTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        QName interfaceNameOut = new QName("http://localhost/sapbctest/SAPOut", "portTypeSAPOut");
        QName serviceOut = new QName("http://localhost/sapbctest/SAPOut", "serviceSAPOut");
        instance = new EndpointDataImpl(interfaceNameOut, 
                                         serviceOut,
                                         "portSAPOut",
                                         EndpointType.OUTBOUND);
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EndpointDataImplTest.class);
        
        return suite;
    }

    /**
     * Test of getInterface method, of class com.sun.jbi.sapbc.packaging.EndpointDataImpl.
     */
    public void testGetInterface() {
        System.out.println("Testing getInterface");
        
        String expResult = "{http://localhost/sapbctest/SAPOut}portTypeSAPOut";
        String result = instance.getInterface().toString();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested getInterface");
        
    }

    /**
     * Test of getService method, of class com.sun.jbi.sapbc.packaging.EndpointDataImpl.
     */
    public void testGetService() {
        System.out.println("Testing getService");
        
        String expResult = "{http://localhost/sapbctest/SAPOut}serviceSAPOut";
        String result = instance.getService().toString();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested getService");
    }

    /**
     * Test of getEndpoint method, of class com.sun.jbi.sapbc.packaging.EndpointDataImpl.
     */
    public void testGetEndpoint() {
        System.out.println("Testing getEndpoint");
        
        String expResult = "portSAPOut";
        String result = instance.getEndpoint();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested getEndpoint");
    }

    /**
     * Test of getDirection method, of class com.sun.jbi.sapbc.packaging.EndpointDataImpl.
     */
    public void testGetDirection() {
        System.out.println("Testing getDirection");
        
        EndpointType expResult = EndpointType.OUTBOUND;
        EndpointType result = instance.getDirection();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested getDirection");
    }
    
}
