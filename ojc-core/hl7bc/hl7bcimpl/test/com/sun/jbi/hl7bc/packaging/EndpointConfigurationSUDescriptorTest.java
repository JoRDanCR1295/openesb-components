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

package com.sun.jbi.hl7bc.packaging;

import junit.framework.*;
import java.util.ArrayList;
import java.util.List;
import com.sun.jbi.hl7bc.Endpoint.EndpointType;
import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.management.descriptor.Consumes;
import com.sun.jbi.management.descriptor.Provides;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.jbi.management.descriptor.ConfigurationException;

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
        instance = new EndpointConfigurationSUDescriptor("test/com/sun/jbi/hl7bc/packaging/descriptors");
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EndpointConfigurationSUDescriptorTest.class);
        
        return suite;
    }

    /**
     * Test of addEndpoint method, of class com.sun.jbi.hl7bc.packaging.EndpointConfigurationSUDescriptor.
     */
    public void testAddEndpoint() {
        System.out.println("Testing addEndpoint");
        
        EndpointData p = new EndpointDataImpl("{http://localhost/hl7bctest/HL7Out}portTypeHL7Out", 
                                              "{http://localhost/hl7bctest/HL7Out}serviceHL7Out",
                                              "portHL7Out","myConfObject",
                                               1);
        instance.addEndpoint(p);
        p = new EndpointDataImpl("{http://localhost/hl7bctest/HL7In}portTypeHL7In", 
                                 "{http://localhost/hl7bctest/HL7In}portTypeHL7In",
                                 "portHL7In","myConfObject",
                                  0);
        instance.addEndpoint(p);
        assertEquals(4, instance.endpoints().size());
        
        System.out.println("Successfully tested addEndpooint");
    }

    /**
     * Test of endpoints method, of class com.sun.jbi.hl7bc.packaging.EndpointConfigurationSUDescriptor.
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
