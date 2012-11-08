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
public class UDDIAddressTest extends TestCase {
    public static final String UDDI_NS_URI = "http://schemas.sun.com/jbi/wsdl-extensions/uddi/";
    public static final String PUBLISH_URI = "Gestalt Publish";
    public static final String INQUIRY_URI = "Gestalt Inquiry";
    boolean isRequired = false;

    public UDDIAddressTest(String whichTest) {
        super(whichTest);
    }

    /**
     * Used by JUnit to determine which tests to run.
     * @return Test suite to be run by JUnit
     */
    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(new UDDIAddressTest("testGetterAndSetter"));

        return suite;
    }

    public void setUp() {
    }

    /**
     * Test the Getter and Setter for Element Type
     */
    public void testGetterAndSetter() {
        UDDIAddress uddiAddress = new UDDIAddress();

        //Test Get and Set ElementType
        QName qName = new QName(UDDI_NS_URI, "address");
        assertNotNull("getElementType was null", uddiAddress.getElementType());
        assertTrue("getElementType was incorrect",
            uddiAddress.getElementType().equals(qName));

        //Test Get and Set Required
        uddiAddress.setRequired(isRequired);
        assertFalse("getRequired returned true", uddiAddress.getRequired());
        isRequired = true;
        uddiAddress.setRequired(isRequired);
        assertTrue("getRequired returned false", uddiAddress.getRequired());

        //Test Get and Set PublishURI
        uddiAddress.setPublishUri(PUBLISH_URI);
        assertTrue("getPublishUri was incorrect",
            uddiAddress.getPublishUri().equals(PUBLISH_URI));

        //Test Get and Set InquiryURI
        uddiAddress.setInquiryUri(INQUIRY_URI);
        assertTrue("getInquiryURY was incorrect",
            uddiAddress.getInquiryUri().equals(INQUIRY_URI));
    }
}
