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

/** 
 * Defines the valid attributes that would comprise an incoming XML message.
 * These attributes are used with the XBean Provider Handler.
 * User: cgallemore
 * Date: Dec 20, 2007
 */

public class XMPPAttributes {
    // Add NM Properties correpsonds to the below OperationExecutor properties
    public static final String NM_PROP_JABBER_ID = "org.glassfish.openesb.xmpp.jabber.id";
    // XMPP Attributes that the OperationExecutor handles.
    public static final String JABBER_ID = "jabberId";
    public static final String STATIC_JABBER_ID = "staticJabberId";
    public static final String NM_JABBER_ID = "nmJabberId";
    public static final String MESSAGE = "message";
    public static final String GROUP_CHAT = "groupChat";
    public static final String GROUP_LIST = "groupList";
    public static final String USER_LIST = "userList";
    public static final String ROOM_NAME = "roomName";
    public static final String ALIAS = "alias";
    public static final String OPERATION_NAME = "operationName";
}
