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
 * author: cgallemore
 * Date: Dec 20, 2007
 */
public class XMPPTestConstants {
    static final String SEND_MESSAGE =
        "<message jabberId='jthorn@localhost/spark' operationName='sendMessage' groupChat='yes'>" +
        "<body>Hello World!!!</body>" +
        "</message>";
    static final String CREATE_GROUP =
        "<message groupList='rooma@conference.localhost'>" +
        "</message>";
    static final String INVITE =
        "<message userList='jthorn@localhost/spark' roomName='roomb@conference.localhost'>" +
        "</message>";
    static final String COMPLEX_CONTENT =
        "<message jabberId='jthorn@localhost/spark' operationName='sendMessage' groupChat='yes'>" +
                "<body><content><message>This is complex Content</message></content></body>" +
        "</message>";
}
