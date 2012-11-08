/**
 *   sip-binding-component - SIP Binding Component
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
package com.gestalt.jbi.sip;

import junit.framework.TestCase;


public class SIPSessionTest extends TestCase {
    public void testInitClearIsAvailable() {
        String uri1 = "uri1";
        SIPSession session = new SIPSession(uri1, null);

        assertTrue("Session Should be Pending state when first created",
            session.isPending());

        session.establish();
        assertTrue("Session should be in Established state",
            session.isEstablished());
    }
}
