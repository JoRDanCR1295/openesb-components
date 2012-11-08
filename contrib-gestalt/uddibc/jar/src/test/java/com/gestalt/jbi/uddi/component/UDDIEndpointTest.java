/**
 *   uddi-binding-component - UDDI Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
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
package com.gestalt.jbi.uddi.component;

import com.gestalt.jbi.uddi.extensions.UDDIAddress;
import com.gestalt.jbi.uddi.extensions.UDDIBinding;
import com.gestalt.jbi.uddi.extensions.UDDIOperation;
import com.gestalt.jbi.uddi.extensions.UDDIOperationInput;
import com.gestalt.jbi.uddi.extensions.UDDIOperationOutput;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.uddi4j.client.UDDIProxy;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;


/**
 * Author: cgallemore Date: Apr 16, 2007
 */
public class UDDIEndpointTest extends TestCase {
    public static final String UDDI_NS_URI = "http://schemas.sun.com/jbi/wsdl-extensions/uddi/";
    public static final String ADDRESS = "address";
    public static final String BINDING = "binding";
    public UDDIProxy inquiryProxy;
    public UDDIBinding uddiBinding;
    public UDDIAddress uddiAddress;
    public UDDIOperation uddiOperationA;
    public UDDIOperationInput uddiOperationInputA;
    public UDDIOperationOutput uddiOperationOutputA;
    public UDDIOperation uddiOperationB;
    public UDDIOperationInput uddiOperationInputB;
    public UDDIOperationOutput uddiOperationOutputB;
    public Map<QName, UDDIOperation> uddiOperations;

    public UDDIEndpointTest(String whichTest) {
        super(whichTest);
    }

    /**
     * Used by JUnit to determine which tests to run.
     *
     * @return Test suite to be run by JUnit
     */
    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(new UDDIEndpointTest("testGetterAndSetter"));

        return suite;
    }

    public void setUp() {
    }

    public void testGetterAndSetter() {
        UDDIEndpoint uddiEndpoint = new UDDIEndpoint(null, null, null, null,
                null, null, null);
        inquiryProxy = new UDDIProxy();
        uddiBinding = new UDDIBinding();
        uddiAddress = new UDDIAddress();
        uddiOperationA = new UDDIOperation();
        uddiOperationInputA = new UDDIOperationInput();
        uddiOperationOutputA = new UDDIOperationOutput();
        uddiOperationB = new UDDIOperation();
        uddiOperationInputB = new UDDIOperationInput();
        uddiOperationOutputB = new UDDIOperationOutput();
        uddiOperations = new HashMap<QName, UDDIOperation>();

        // Test Get, Set UDDIAddress
        uddiEndpoint.setUDDIAddress(uddiAddress);
        assertNotNull("getUDDIAddress was null", uddiEndpoint.getUDDIAddress());
        assertTrue("getUDDIAddres is incorrect",
            uddiEndpoint.getUDDIAddress().equals(uddiAddress));

        // Test Get, Set UDDIBinding
        uddiEndpoint.setUDDIBinding(uddiBinding);
        assertNotNull("getUDDIBinding was null", uddiEndpoint.getUDDIBinding());
        assertTrue("getUDDIBinding is incorrect",
            uddiEndpoint.getUDDIBinding().equals(uddiBinding));

        // Test Get, Set UDDIOperationInput
        uddiEndpoint.setUDDIOperationInput(uddiOperationA, uddiOperationInputA);
        uddiEndpoint.setUDDIOperationInput(uddiOperationB, uddiOperationInputB);
        assertTrue("getUDDIOperationInput was incorrect",
            uddiEndpoint.getUDDIOperationInput(uddiOperationA)
                        .equals(uddiOperationInputA));
        assertTrue("getUDDIOperationInput was incorrect",
            uddiEndpoint.getUDDIOperationInput(uddiOperationB)
                        .equals(uddiOperationInputB));

        // Test Get, Set UDDIOperationOutput
        uddiEndpoint.setUDDIOperationOutput(uddiOperationA, uddiOperationOutputA);
        uddiEndpoint.setUDDIOperationOutput(uddiOperationB, uddiOperationOutputB);
        assertTrue("uddiOperationOutpu was incorrect",
            uddiEndpoint.getUDDIOperationOutput(uddiOperationA)
                        .equals(uddiOperationOutputA));
        assertTrue("uddiOperationOutput was incorrect",
            uddiEndpoint.getUDDIOperationOutput(uddiOperationB)
                        .equals(uddiOperationOutputB));

        // Test Get, Set UDDIOperation
        QName qNameA = new QName(UDDI_NS_URI, ADDRESS);
        QName qNameB = new QName(UDDI_NS_URI, BINDING);
        uddiOperations.put(qNameA, uddiOperationA);
        uddiOperations.put(qNameB, uddiOperationB);
        uddiEndpoint.setUDDIOperations(uddiOperations);
        assertTrue("Size of getUddiOperations was incorrect",
            uddiEndpoint.getOperations().size() == uddiOperations.size());

        Map<QName, UDDIOperation> returnedUddiOperations = uddiEndpoint.getOperations();
        assertTrue("Operation was incorrect",
            returnedUddiOperations.get(qNameA).equals(uddiOperationA));
        assertTrue("Operation was incorrect",
            returnedUddiOperations.get(qNameB).equals(uddiOperationB));
    }
}
