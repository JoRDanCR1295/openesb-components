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

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import javax.xml.namespace.QName;


/**
 * Author: cgallemore
 * Date: May 8, 2007
 */
public class SIPAddressTest extends TestCase {
    String ELEM_ADDRESS = "address";
    QName qName = new QName(SIPConstants.SIP_NS_URI, ELEM_ADDRESS);
    private String proxyDomain = "b-0498";
    private String password = "password";
    private String username = "username";
    private int timeOut = 5000;
    private int port = 5222;
    private Boolean isRequired = true;
    SIPAddress sipAddress;

    public SIPAddressTest(String whichTest) {
        super(whichTest);
    }

    public void setUp() {
        sipAddress = new SIPAddress();
    }

    static public Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(new SIPAddressTest("testGetterAndSetter"));

        return suite;
    }

    public void testGetterAndSetter() {
        assertTrue("getElementType was incorrect",
            sipAddress.getElementType().equals(qName));

        sipAddress.setRequired(isRequired);
        assertTrue("getRequired is not correct", sipAddress.getRequired());

        sipAddress.setProxydomain(proxyDomain);
        assertTrue("getDomain was incorrect",
            sipAddress.getProxydomain().equals(proxyDomain));

        sipAddress.setPassword(password);
        assertTrue("getPassword was incorrect",
            sipAddress.getPassword().equals(password));

        sipAddress.setProxyport(port);
        assertTrue("getPort was incorrect",
            sipAddress.getProxyport().equals(port));

        sipAddress.setProxytimeout(timeOut);
        assertTrue("getResource was incorrect",
            sipAddress.getProxytimeout().equals(timeOut));

        sipAddress.setUsername(username);
        assertTrue("getUsername was incorrect",
            sipAddress.getUsername().equals(username));
    }
}
