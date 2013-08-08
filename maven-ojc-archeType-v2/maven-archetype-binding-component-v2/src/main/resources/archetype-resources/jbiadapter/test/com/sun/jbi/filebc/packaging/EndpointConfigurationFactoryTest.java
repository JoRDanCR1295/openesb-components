#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(${symbol_pound})EndpointConfigurationFactoryTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.packaging;

import junit.framework.*;
import java.io.File;

/**
 *
 * @author sweng
 */
public class EndpointConfigurationFactoryTest extends TestCase {

    public EndpointConfigurationFactoryTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(EndpointConfigurationFactoryTest.class);

        return suite;
    }

    /**
     * Test of getEndpointConfiguration method, of class com.sun.jbi.filebc.packaging.EndpointConfigurationFactory.
     */
    public void testGetEndpointConfiguration() throws Exception {
        System.out.println("Testing getEndpointConfiguration");

        String suRootDir = new File("test/com/sun/jbi/filebc/packaging/descriptors").getAbsolutePath();
        EndpointConfiguration result = EndpointConfigurationFactory.getEndpointConfiguration(suRootDir);
        assertTrue(result instanceof EndpointConfigurationSUDescriptor);

        System.out.println("Successfully tested getEndpointConfiguration");
    }
}
