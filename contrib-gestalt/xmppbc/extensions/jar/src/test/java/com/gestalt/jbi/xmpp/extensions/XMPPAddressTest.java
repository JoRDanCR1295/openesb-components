/**
 *   xmpp-binding-component-extensions - Extensions for the XMPP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
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
package com.gestalt.jbi.xmpp.extensions;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import javax.xml.namespace.QName;


/**
 * Author: cgallemore
 * Date: May 8, 2007
 */
public class XMPPAddressTest extends TestCase {
    private static final int SERVER_PORT = 5222;
    private static final String ELEM_ADDRESS = "address";
    QName qName = new QName(XMPPConstants.XMPP_NS_URI, ELEM_ADDRESS);
    String domain = "b-0498";
    String password = "password";
    String username = "username";
    boolean isRequired = true;
    String resource = "resource";
    String group = "room@chat.bar.com";
    XMPPAddress xmppAddress;

    public XMPPAddressTest(String whichTest) {
        super(whichTest);
    }

    public void setUp() {
        xmppAddress = new XMPPAddress();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(new XMPPAddressTest("testGetterAndSetter"));

        return suite;
    }

    public void testGetterAndSetter() {
        assertTrue("getElementType was incorrect",
            xmppAddress.getElementType().equals(qName));

        xmppAddress.setRequired(isRequired);
        assertTrue("getRequired is not correct", xmppAddress.getRequired());

        xmppAddress.setDomain(domain);
        assertTrue("getDomain was incorrect",
            xmppAddress.getDomain().equals(domain));

        xmppAddress.setPassword(password);
        assertTrue("getPassword was incorrect",
            xmppAddress.getPassword().equals(password));

        xmppAddress.setPort(SERVER_PORT);
        assertTrue("getPort was incorrect",
            xmppAddress.getPort().equals(SERVER_PORT));

        xmppAddress.setResource(resource);
        assertTrue("getResource was incorrect",
            xmppAddress.getResource().equals(resource));

        xmppAddress.setUsername(username);
        assertTrue("getUsername was incorrect",
            xmppAddress.getUsername().equals(username));

        xmppAddress.setGroup(group);
        assertTrue("getGroupName was incorrect",
            xmppAddress.getGroup().equals(group));
    }
}
