/**
 *   uddi-binding-component-extensions - Extensions for the UDDI Binding Component
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
package com.gestalt.jbi.uddi.extensions;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import javax.xml.namespace.QName;


/**
 * Author: cgallemore
 * Date: Apr 16, 2007
 */
public class UDDIOperationOutputTest extends TestCase {
    public static final String UDDI_NS_URI = "http://schemas.sun.com/jbi/wsdl-extensions/uddi/";
    boolean isRequired = false;

    public UDDIOperationOutputTest(String whichTest) {
        super(whichTest);
    }

    /**
     * Used by JUnit to determine which tests to run.
     * @return Test suite to be run by JUnit
     */
    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(new UDDIOperationOutputTest("testGetterAndSetter"));

        return suite;
    }

    public void setUp() {
    }

    /**
     * Test the Getter and Setter for Element Type
     */
    public void testGetterAndSetter() {
        UDDIOperationOutput uddiOperationOutput = new UDDIOperationOutput();

        //Test Get and Set ElementType
        QName qName = new QName(UDDI_NS_URI, "output");
        assertNotNull("getElementType was null",
            uddiOperationOutput.getElementType());
        assertTrue("getElementType was incorrect",
            uddiOperationOutput.getElementType().equals(qName));

        //Test Get and Set Required
        uddiOperationOutput.setRequired(isRequired);
        assertFalse("getRequired returned true",
            uddiOperationOutput.getRequired());
        isRequired = true;
        uddiOperationOutput.setRequired(isRequired);
        assertTrue("getRequired returned false",
            uddiOperationOutput.getRequired());
    }
}
