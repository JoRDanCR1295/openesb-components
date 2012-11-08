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
public class SIPBindingTest extends TestCase {
    QName qName = new QName(SIPConstants.SIP_NS_URI, Constants.ELEM_BINDING);
    private Boolean isRequired = true;
    SIPBinding sipBinding;

    public SIPBindingTest(String whichTest) {
        super(whichTest);
    }

    public void setUp() {
        sipBinding = new SIPBinding();
    }

    static public Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(new SIPBindingTest("testGetterAndSetter"));

        return suite;
    }

    public void testGetterAndSetter() {
        assertTrue("getElementType was incorrect",
            sipBinding.getElementType().equals(qName));

        sipBinding.setRequired(isRequired);
        assertTrue("getRequired is not correct", sipBinding.getRequired());
    }
}
