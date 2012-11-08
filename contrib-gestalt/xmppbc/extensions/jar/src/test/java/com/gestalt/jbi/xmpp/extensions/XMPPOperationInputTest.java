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

import com.ibm.wsdl.Constants;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import javax.xml.namespace.QName;


/**
 * Author: cgallemore
 * Date: May 8, 2007
 */
public class XMPPOperationInputTest extends TestCase {
    QName qName = new QName(XMPPConstants.XMPP_NS_URI, Constants.ELEM_INPUT);
    private Boolean isRequired = true;
    private String jabberId = "user";
    private String packetId = "1234";
    private String message = "Hello from user";
    private String groupChat = "yes";
    XMPPOperationInput xmppOperationInput;

    public XMPPOperationInputTest(String whichTest) {
        super(whichTest);
    }

    public void setUp() {
        xmppOperationInput = new XMPPOperationInput();
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(new XMPPOperationInputTest("testGetterAndSetter"));

        return suite;
    }

    public void testGetterAndSetter() {
        assertTrue("getElementType was incorrect",
            xmppOperationInput.getElementType().equals(qName));

        xmppOperationInput.setRequired(isRequired);
        assertTrue("getRequired is not correct",
            xmppOperationInput.getRequired());

        xmppOperationInput.setJabberId(jabberId);
        assertTrue("getFrom is not correct",
            xmppOperationInput.getJabberId().equals(jabberId));

        xmppOperationInput.setPacketId(packetId);
        assertTrue("getId is not correct",
            xmppOperationInput.getPacketId().equals(packetId));

        xmppOperationInput.setMessage(message);
        assertTrue("getMessage is not correct",
            xmppOperationInput.getMessage().equals(message));

        xmppOperationInput.setGroupChat(groupChat);
        assertTrue("getGroupChat is not correct",
            xmppOperationInput.getGroupChat().equals(groupChat));
    }
}
