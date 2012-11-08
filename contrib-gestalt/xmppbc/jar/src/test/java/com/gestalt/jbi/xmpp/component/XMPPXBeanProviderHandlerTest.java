/**
 *   xmpp-binding-component - XMPP Binding Component
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
package com.gestalt.jbi.xmpp.component;

import com.gestalt.jbi.component.handler.AbstractMessageExchangeHandler;
import com.gestalt.jbi.xmpp.extensions.XMPPAddress;

import junit.framework.TestCase;

import java.lang.reflect.Method;

import java.util.Map;


/**
 * author: cgallemore
 * Date: Dec 20, 2007
 */
public class XMPPXBeanProviderHandlerTest extends TestCase {

    /**
     * Test extracting the content for a Send/Receive message operation.
     * @throws Exception
     */
    public void testSendReceiveMessageParseContent() throws Exception {
        XMPPXBeanProviderHandler handler = new XMPPXBeanProviderHandler(new MockXMPPEndpoint());
        Class clazz = handler.getClass();
        Method getParseXML = clazz.getDeclaredMethod("parseXML", String.class);
        getParseXML.setAccessible(true);

        Map<String, String> map = (Map<String, String>) getParseXML.invoke(handler,
                XMPPTestConstants.SEND_MESSAGE);
        assertEquals("Size of Map was incorrect: ", 5, map.size());
        assertEquals("Alias was incorrect: ", "bob", map.get("alias"));
        assertEquals("JabberId was incorrect: ", "jthorn@localhost/spark",
            map.get("jabberId"));
        assertEquals("Message was incorrect: ", "Hello World!!!",
            map.get("message"));
        assertEquals("Group Chat was incorrect: ", "yes", map.get("groupChat"));
        assertEquals("OperationName was incorrect: ", "sendMessage", map.get("operationName"));
    }

    /**
     * Test extracting the content for a Send/Receive message operation.
     * @throws Exception
     */
    public void testSendReceiveComplexMessageParseContent() throws Exception {
        XMPPXBeanProviderHandler handler = new XMPPXBeanProviderHandler(new MockXMPPEndpoint());
        Class clazz = handler.getClass();
        Method getParseXML = clazz.getDeclaredMethod("parseXML", String.class);
        getParseXML.setAccessible(true);

        Map<String, String> map = (Map<String, String>) getParseXML.invoke(handler,
                XMPPTestConstants.COMPLEX_CONTENT);
//        assertEquals("Size of Map was incorrect: ", 5, map.size());
        assertEquals("Alias was incorrect: ", "bob", map.get("alias"));
        assertEquals("JabberId was incorrect: ", "jthorn@localhost/spark",
            map.get("jabberId"));
        assertEquals("Message was incorrect: ", "<content><message>This is complex Content</message></content>",
            map.get("message"));
        assertEquals("Group Chat was incorrect: ", "yes", map.get("groupChat"));
        assertEquals("OperationName was incorrect: ", "sendMessage", map.get("operationName"));
    }

    /**
     * Test extracting the content for a Create/Destroy group operation.
     * @throws Exception
     */
    public void testCreateDestroyGroupParseContent() throws Exception {
        XMPPXBeanProviderHandler handler = new XMPPXBeanProviderHandler(new MockXMPPEndpoint());
        Class clazz = handler.getClass();
        Method getParseXML = clazz.getDeclaredMethod("parseXML", String.class);
        getParseXML.setAccessible(true);

        Map<String, String> map = (Map<String, String>) getParseXML.invoke(handler,
                XMPPTestConstants.CREATE_GROUP);
        assertEquals("Size of map incorrect: ", 2, map.size());
        assertEquals("Alias was incorrect: ", "bob", map.get("alias"));
        assertEquals("GroupList was incorrect: ", "rooma@conference.localhost",
            map.get("groupList"));
    }

    /**
     * Test extracting the content for a Invite/Kick/Join Operation.
     * @throws Exception
     */
    public void testInviteJoinKickParseContent() throws Exception {
        XMPPXBeanProviderHandler handler = new XMPPXBeanProviderHandler(new MockXMPPEndpoint());
        Class clazz = handler.getClass();
        Method getParseXML = clazz.getDeclaredMethod("parseXML", String.class);
        getParseXML.setAccessible(true);

        Map<String, String> map = (Map<String, String>) getParseXML.invoke(handler,
                XMPPTestConstants.INVITE);
        assertEquals("Map size was incorrect: ", 3, map.size());
        assertEquals("Alias was incorrect: ", "bob", map.get("alias"));
        assertEquals("UserList was incorrect: ", "jthorn@localhost/spark",
            map.get("userList"));
        assertEquals("RoomName was incorrect: ", "roomb@conference.localhost",
            map.get("roomName"));
    }

    /**
     * Mock Endpoint class used for testing.
     */
    private class MockXMPPEndpoint extends XMPPEndpoint {
        XMPPAddress xmppAddress = new XMPPAddress();

        public MockXMPPEndpoint() {
            super(null, null, null, null, null, null, null, true);
            xmppAddress.setUsername("bob");
        }

        public XMPPAddress getXMPPAddress() {
            return xmppAddress;
        }

        public Map getOperations() {
            return null;
        }

        public void activate() throws Exception {
        }

        public void deactivate() throws Exception {
        }

        public AbstractMessageExchangeHandler createConsumerExchangeHandler() {
            return null;
        }

        public AbstractMessageExchangeHandler createProviderExchangeHandler() {
            return null;
        }
    }
}
