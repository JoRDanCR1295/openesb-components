/**
 *   sip-binding-component-extensions - Extensions for the SIP Binding Component
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
package com.gestalt.jbi.sip.extensions;

import com.ibm.wsdl.Constants;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import javax.xml.namespace.QName;


/**
 * Author: cgallemore
 * Date: May 8, 2007
 */
public class SIPOperationOutputTest extends TestCase {
    QName qName = new QName(SIPConstants.SIP_NS_URI, Constants.ELEM_OUTPUT);
    private Boolean isRequired = true;
    private String message = "Hello";
    SIPOperationOutput sipOperationOutput;

    public SIPOperationOutputTest(String whichTest) {
        super(whichTest);
    }

    public void setUp() {
        sipOperationOutput = new SIPOperationOutput();
    }

    static public Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(new SIPOperationOutputTest("testGetterAndSetter"));

        return suite;
    }

    public void testGetterAndSetter() {
        assertTrue("getElementType was incorrect",
            sipOperationOutput.getElementType().equals(qName));

        sipOperationOutput.setRequired(isRequired);
        assertTrue("getRequired is not correct",
            sipOperationOutput.getRequired());
    }
}
