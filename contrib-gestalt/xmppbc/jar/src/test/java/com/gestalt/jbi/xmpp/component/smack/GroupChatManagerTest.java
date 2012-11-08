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
package com.gestalt.jbi.xmpp.component.smack;

import junit.framework.TestCase;

import org.junit.Test;

import java.lang.reflect.Method;


/**
 * Test class for the GroupChatManager.
 *
 * @author tchase
 * @version 1.0.0 10/19/2007
 */
public class GroupChatManagerTest extends TestCase {
    @Test
    public void testGetKey() throws Exception {
        GroupChatManager chatManager = new GroupChatManager();
        Class chatManagerClass = chatManager.getClass();
        String checkKey = "roomaperson";

        Method getKeyMethod = chatManagerClass.getDeclaredMethod("getKey",
                String.class, String.class);
        getKeyMethod.setAccessible(true);

        String returnKey = (String) getKeyMethod.invoke(chatManager, "RoomA",
                "person");

        assertEquals("The keys were not equal.", checkKey, returnKey);
    }

    @Test
    public void testGetAlias() throws Exception {
        GroupChatManager chatManager = new GroupChatManager();
        String jid = "bob@localhost.com/Spark";

        String alias = chatManager.getAlias(jid);
        assertEquals("Alias was not correct", "bob", alias);
    }
}
