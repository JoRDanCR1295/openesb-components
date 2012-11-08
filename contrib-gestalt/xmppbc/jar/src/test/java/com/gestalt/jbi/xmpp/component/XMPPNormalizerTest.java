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

import junit.framework.TestCase;

import org.jivesoftware.smack.packet.Presence;

import java.util.*;


/**
 * @author cgallemore
 * @since<pre>Sep 19, 2007<pre>
 */
public class XMPPNormalizerTest extends TestCase {
    private static final String PAX_RIVER = "exercise_A@conference.j1demo/pax_river";
    private static final String PT_MUGU = "exercise_A@conference.j1demo/pt_mugu";
    private static final String EGLIN = "exercise_A@conference.j1demo/eglin";
    private static final String STARTED = "Started";
    private static final String STARTING = "Starting";
    private static final String AVAILABLE = "available: ";
    private static final String CHAT = "chat";
    private Map<String, Presence> map = new HashMap<String, Presence>();
    private String expectedResponse = "<OccupantList xmlns=\"http://xml.netbeans.org/schema/1.0/extensions/xmppbc\">" +
        "<Occupant>" + "<occupant>" + PAX_RIVER + "</occupant>" + "<presence>" +
        AVAILABLE + "available" + "</presence>" + "</Occupant>" + "<Occupant>" +
        "<occupant>" + EGLIN + "</occupant>" + "<presence>" + AVAILABLE + CHAT +
        " (" + STARTED + ")" + "</presence>" + "</Occupant>" + "<Occupant>" +
        "<occupant>" + PT_MUGU + "</occupant>" + "<presence>" + AVAILABLE +
        CHAT + " (" + STARTING + ")" + "</presence>" + "</Occupant>" +
        "</OccupantList>";

    public void setUp() {
        Presence presenceAvailable = new Presence(Presence.Type.available,
                null, 1, Presence.Mode.available);
        Presence presenceStarting = new Presence(Presence.Type.available,
                "Starting", 1, Presence.Mode.chat);
        Presence presenceStarted = new Presence(Presence.Type.available,
                "Started", 1, Presence.Mode.chat);
        map.put(PAX_RIVER, presenceAvailable);
        map.put(EGLIN, presenceStarted);
        map.put(PT_MUGU, presenceStarting);
    }

    /**
     * Test that the XMPP Normalizer properly constructs the complex message
     * type (OccupantList).
     */
    public void testCreateNormalizedMessage() {
        String response = XMPPNormalizer.createNormalizedOccupantPresence(map);
        assertEquals("List was not the same", expectedResponse, response);
    }

    public void tearDown() {
        map.clear();
    }
}
