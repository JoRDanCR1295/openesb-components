
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

import com.gestalt.jbi.xmpp.component.XMPPConsumerHandler;
import com.gestalt.jbi.xmpp.component.XMPPEndpoint;
import com.gestalt.jbi.xmpp.component.AbstractXMPPConsumerHandler;

import org.jivesoftware.smack.PacketListener;
import org.jivesoftware.smack.packet.Packet;
import org.jivesoftware.smack.packet.Presence;

import org.jivesoftware.smackx.muc.ParticipantStatusListener;

import java.util.Map;
import java.util.Set;


/**
 * Handles the presence notifications and status of users in a GroupChat.
 *
 * A presence notification Represents XMPP presence packets (the call back
 * is processPacket(Packet packet).  Every presence packet has a type,
 * which is one of the following values:
 *                  * available (Default)
 *                  * unavailable
 *                  * subscribe
 *                  * subscribed
 *                  * unsubscribe
 *                  * unsubscribed
 *                  * error
 *
 * A number of presence Attributes are optional:
 *   1. Status - Free form text describing a user's presence (e.g. be right back).
 *   2. Priority - Non negative numerical priority of a sender's resource.
 *   3. Mode - One of five presence modes: available (the default), chat, away,
 *             xa (extended away) and dnd (do not disturb).
 *
 * Status refers to a users status in a room (e.g the user being kicked, banned, etc.)
 * Currently we are only monitoring the status notification for users joining,
 * leaving, or being kicked from a room.
 *
 * @author cgallemore
 * @since<pre>Sep 10, 2007<pre>
 */
public class PresenceManager implements PacketListener,
    ParticipantStatusListener {
    private String roomName;
    private Map<XMPPEndpoint, AbstractXMPPConsumerHandler> handlers;

    /**
     * Constructor
     *
     * @param roomName - The name of the room we are monitoring.
     * @param groupChatManager - The GroupChatManager
     */
    public PresenceManager(String roomName, GroupChatManager groupChatManager) {
        this.roomName = roomName;
        this.handlers = groupChatManager.getLifeCycle().getHandlers();
    }

    /**
     * Call back for anytime a users presence is updated.  Currently we are
     * only monitoring users in a room, if a user updates his presence this gets
     * called and we process the incoming Presence packet.
     * @param packet - Presence Packet.
     */
    public void processPacket(Packet packet) {
        if (packet instanceof Presence) {
            Presence presence = (Presence) packet;
            processNotification(presence, null, null, true);
        }
    }

    /**
     * Called when a new room occupant has joined the room. Note: Take in
     * consideration that when you join a room you will receive the list
     * of current occupants in the room. This message will be sent
     * for each occupant.
     *
     * @param participant - The participant that joined the room
     */
    public void joined(String participant) {
        processNotification(null, participant, "joined", false);
    }

    /**
     * Called when a room occupant has left the room on its own.
     * This means that the occupant was neither kicked nor banned from the room.
     *
     * @param participant - The participant that left the room.
     */
    public void left(String participant) {
        processNotification(null, participant, "left", false);
    }

    /**
     * Called when a room participant has been kicked from the room.
     * This means that the kicked participant is no longer participating in the room.
     *
     * @param participant - the participant that was kicked from the room
     * @param actor - The moderator that kicked the user from the room
     * @param reason - The reason for kicking the user out of the room.
     */
    public void kicked(String participant, String actor, String reason) {
        processNotification(null, participant,
            "kicked for: " + reason + " by: " + actor, false);
    }

    /**
     * Processes all notifications to be sent to all consumer handlers.
     *
     * @param presence - The Presence Packet (null if N/A)
     * @param participant - The participant involved in the notification (null if N/A).
     * @param notification - The notification response (null if N/A)
     * @param isPresencePacket - Is this a presence notification or a
     *                           status notification (e.g. joined, left, kicked, etc.)
     */
    private void processNotification(Presence presence, String participant,
        String notification, boolean isPresencePacket) {
        Set<XMPPEndpoint> keys = handlers.keySet();

        for (XMPPEndpoint endpoint : keys) {
            AbstractXMPPConsumerHandler handler = handlers.get(endpoint);

            if (isPresencePacket) {
                handler.processPresenceNotification(presence);
            } else {
                handler.processStatusNotification(participant, roomName,
                    notification);
            }
        }
    }

    public void voiceGranted(String participant) {
    }

    public void voiceRevoked(String participant) {
    }

    public void banned(String participant, String actor, String reason) {
    }

    public void membershipGranted(String participant) {
    }

    public void membershipRevoked(String participant) {
    }

    public void moderatorGranted(String participant) {
    }

    public void moderatorRevoked(String participant) {
    }

    public void ownershipGranted(String participant) {
    }

    public void ownershipRevoked(String participant) {
    }

    public void adminGranted(String participant) {
    }

    public void adminRevoked(String participant) {
    }

    public void nicknameChanged(String participant, String newNickname) {
    }
}
