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

import com.gestalt.jbi.xmpp.component.XMPPComponentLifeCycle;
import com.gestalt.jbi.xmpp.component.XMPPEndpoint;
import com.gestalt.jbi.xmpp.component.XMPPNormalizer;

import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.XMPPException;
import org.jivesoftware.smack.packet.Presence;

import org.jivesoftware.smackx.Form;
import org.jivesoftware.smackx.muc.MultiUserChat;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * Manages all, if any, MultiUserChat connections.  A MultiUserChat is a
 * conversation that takes place among many users in a virtual room
 *
 * @author cgallemore
 * @since
 *
 * <pre>
 * Aug 20, 2007&lt;pre&gt;
 *
 */
public class GroupChatManager {
    private Logger log = Logger.getLogger(GroupChatManager.class.getName());
    private XMPPComponentLifeCycle lifeCycle;
    private ArrayList<String> keys = new ArrayList<String>();
    private InvitationManager invitationManager;
    private PresenceManager presenceManager;
    private ConcurrentHashMap<String, PresenceManager> presenceWatchers = new ConcurrentHashMap<String, PresenceManager>();
    private XMPPEndpoint endpoint;

    /**
     * Package level constructor used for unit testing methods that do not require
     * an XMPPEndpoint.
     */
    GroupChatManager() {
    }

    public GroupChatManager(XMPPEndpoint endpoint) {
        lifeCycle = (XMPPComponentLifeCycle) endpoint.getServiceUnit()
                                                     .getComponent()
                                                     .getLifeCycle();
        this.endpoint = endpoint;
    }

    /**
     * Parses the JID (bob@localhost/spark) to just return the alias (bob).
     *
     * @param to -
     *            The JID (bob@localhost/spark)
     * @return String - the Alias (e.g. bob)
     */
    protected String getAlias(String to) {
        String pattern = "@";
        Pattern compiledPattern = Pattern.compile(pattern);
        Matcher matcher = compiledPattern.matcher(to);

        if (matcher.find()) {
            return to.substring(0, matcher.start());
        } else {
            return to;
        }
    }

    /**
     * Returns the Map of MultiUserChats
     *
     * @return - Map<String, MultiUserChat>
     */
    private Map<String, MultiUserChat> getMultiUserChats() {
        return lifeCycle.getGroupChats();
    }

    /**
     * Puts a new MultiUserChat in the Map
     *
     * @param key -
     *            Unique key to store the MUC instance
     * @param xmppConnection -
     *            XMPPConnection
     * @param roomName -
     *            The name of the room.
     */
    private void setMultiUserChat(String key, String roomName,
        XMPPConnection xmppConnection) {
        if (!keys.contains(key)) {
            keys.add(key);
        }

        lifeCycle.setGroupChats(key, roomName, xmppConnection);
    }

    /**
     * Used in conjunction with the leave operation.  This allows the user
     * to leave the desired group chat.
     *
     * @param roomName -
     *            The name of the room to leave
     * @param alias -
     *            The alias of the user
     * @param xmppConn -
     *            XMPPConnection instance
     */
    public void leaveChatRoom(String alias, String roomName,
        XMPPConnection xmppConn) {
        String key = getKey(roomName, alias);
        MultiUserChat newChat = getMultiUserChats().get(key);

        try {
            newChat.join(alias);
            removePresenceWatchers(roomName, newChat);
        } catch (XMPPException e) {
            log.warning("Error joining chat " + e);
        }

        newChat.leave();
    }

    /**
     * Removes a Chat from the Map
     * @param key - The key for the MultiUserChat to remove
     */
    private void removeChat(String key) {
        lifeCycle.removeGroupChat(key);
    }

    /**
     * Sends a message to the chat room
     *
     * @param roomName -
     *            The group chat you wish to send a message to
     * @param alias -
     *            The alias you are using in the room
     * @param payload -
     *            The message content
     * @throws XMPPException -
     *             an XMPPException
     */
    public void sendMessage(String roomName, String alias, String payload)
        throws XMPPException {
        String key = getKey(roomName, alias);
        MultiUserChat newChat = getMultiUserChats().get(key);

        if (!newChat.isJoined()) {
            newChat.join(alias);
        }

        newChat.sendMessage(payload);
    }

    /**
     * Joins an already existing room.
     *
     * @param roomName -
     *            The name of the room to join
     * @param alias -
     *            Your alias for the room
     * @param xmppConnection -
     *            The XMPPConnection object to create the new MultiUserChat
     */
    public void joinRoom(String roomName, String alias,
        XMPPConnection xmppConnection) {
        String key = getKey(roomName, alias);
        setMultiUserChat(key, roomName, xmppConnection);

        MultiUserChat newChat = getMultiUserChats().get(key);

        try {
            if (!newChat.isJoined()) {
                newChat.join(alias);
            } else {
                log.fine("Already joined");
            }
        } catch (XMPPException e) {
            log.warning("Error joining room ");
        }
    }

    /**
     * Kick a user from a room.
     *
     * @param roomName -
     *            The room in which the user is participating in.
     * @param user -
     *            The user you wish to kick from the room
     * @param reason -
     *            Why the user is being kicked from the room.
     * @param alias -
     *            The alias you are logged into the room as.
     * @throws XMPPException -
     *             an XMPPException
     */
    public void kickParticipant(String roomName, String user, String reason,
        String alias) throws XMPPException {
        String key = getKey(roomName, alias);
        MultiUserChat newChat = getMultiUserChats().get(key);
        newChat.kickParticipant(user, reason);
    }

    /**
     * Sends invitations to join a group to other users. The invitation is
     * for one room, but the invitation can have mulitple users.  For example
     * I want to send an invitation to Bob and Alice to join roomA.
     *
     * @param userList -
     *            List of users to invite to a group
     * @param roomName -
     *            The name of the room you wish to invite them to.
     * @param alias -
     *            The name you are logged into the room as.
     */
    public void sendInvitation(List<String> userList, String roomName,
        String alias) {
        String key = getKey(roomName, alias);
        MultiUserChat newChat = getMultiUserChats().get(key);
        newChat.addInvitationRejectionListener(invitationManager);

        for (String name : userList) {
            String message = "You have been invited to join " + roomName;
            newChat.invite(name, message);
        }
    }

    /**
     * Creates the room according to some default configuration,
     * assign the requesting user as the room owner, and add the owner to the room.
     *
     * This will also return a response (true || false) whether to room was successfully
     * created
     *
     * @param roomName - The name of the room you want to create - NOTE: This must
     * be a fully qualifed JID (e.g. conferenc_room_a@conference.bar.com)
     * @param alias - The alias you will be asigned in the room
     * @param xmppConnection - The XMPP Connection you are using to create the chat
     * @return response - String representation wheter the create groups was successful
     * (true || false).
     */
    public String createGroups(String roomName, String alias,
        XMPPConnection xmppConnection) {
        String response = "true";
        String key = getKey(roomName, alias);
        setMultiUserChat(key, roomName, xmppConnection);

        MultiUserChat newChat = getMultiUserChats().get(key);
        if (newChat.isJoined()) {
            log.fine(roomName + " has already been created and " + alias + " is already joined.");
            return response;
        }

        try {
            newChat.create(alias);
            newChat.sendConfigurationForm(new Form(Form.TYPE_SUBMIT));
            addPresenceManager(roomName, newChat);
        } catch (XMPPException e) {
            response = "false";
            log.warning("Error creating room: " + roomName + " for user: " +
                alias + ", check and make sure your roomName is correct " +
                "(e.g. roomName@conference.localhost)");
            e.printStackTrace();
        }

        return response;
    }

    /**
     * Adds a ParticpantListener and a ParticpantStatusListner everytime a
     * group is created.  This allows the user to be notified of all users and
     * their status of a given room.
     *
     * @param roomName - The name of the room to add listeners
     * @param chat - The MultiUserChat for this room.
     */
    private void addPresenceManager(String roomName, MultiUserChat chat) {
        PresenceManager presenceManager = new PresenceManager(roomName, this);
        chat.addParticipantListener(presenceManager);
        chat.addParticipantStatusListener(presenceManager);
        presenceWatchers.putIfAbsent(roomName, presenceManager);
    }

    /**
     * Removes ParticipantListener and ParticpantStatusListener for a given
     * group chat.
     * @param roomName - The room name for this multi user chat.
     * @param chat - The MultiUserChat object associated with this chat.
     */
    private void removePresenceWatchers(String roomName, MultiUserChat chat) {
        if (presenceWatchers.containsKey(roomName)) {
            PresenceManager manager = presenceWatchers.get(roomName);
            chat.removeParticipantListener(manager);
            chat.removeParticipantStatusListener(manager);
        }
    }

    /**
     * Destroys a Group Chat.  You must be the owner of the room to destroy,
     * otherwise you will receive a "Forbidden" error (403).
     *
     * @param roomName - The name of the room to destroy - NOTE: Must be fully
     * qualified JID (e.g. roomName@conference.bar.com).
     * @param alias - The alias of the owner
     @return response - String representation wheter the destroy groups was successful
     * (true || false).
     */
    public String destroyGroups(String roomName, String alias) {
        String response = "true";
        String key = getKey(roomName, alias);
        MultiUserChat chat = getMultiUserChats().get(key);

        try {
            chat.destroy("Destroying room", null);
            removePresenceWatchers(roomName, chat);
        } catch (XMPPException e) {
            response = "false";
            log.warning("Error destroying room");
            e.printStackTrace();
        }

        return response;
    }

    /**
     * Gets all the Occupants associated with the given roomName, You must be
     * joined to the room already.
     * @param roomName - The name of the chat room
     * @param alias - The alias you are known in the room as
     * @return - A String representation of the OccupantList complex message type
     * for all Occupants and their presence for the room.
     */
    public String getOccupants(String roomName, String alias) {
        String key = getKey(roomName, alias);
        MultiUserChat chat = getMultiUserChats().get(key);

        Iterator<String> occupants = chat.getOccupants();
        Map<String, Presence> map = new HashMap<String, Presence>();

        while (occupants.hasNext()) {
            String occupant = occupants.next();
            Presence presence = chat.getOccupantPresence(occupant);
            map.put(occupant, presence);
        }

        return XMPPNormalizer.createNormalizedOccupantPresence(map);
    }

    /**
     * Generates a key to store the MultiUserChat instance in the Map
     *
     * @param roomName -
     *            the name of the room
     * @param alias -
     *            The alias you are logged in as
     * @return String - key.
     */
    private String getKey(String roomName, String alias) {
        StringBuilder builder = new StringBuilder();
        builder.append(roomName.toLowerCase());
        builder.append(alias);

        return builder.toString();
    }

    /**
     * Returns the keys for all MultiUserChats we are managing.
     * @return List<String>
     */
    public List<String> getKeys() {
        return keys;
    }

    /**
     * Removes the MultiUserChat we were managing from the map.
     * @param key -
     */
    public void removeMultiUserChats(String key) {
        String roomName = keys.get(keys.indexOf(key));
        removePresenceWatchers(roomName, getMultiUserChats().get(roomName));
        removeChat(roomName);
    }

    public InvitationManager getInvitationManager() {
        return invitationManager;
    }

    public void setInvitationManager(InvitationManager invitationManager) {
        this.invitationManager = invitationManager;
    }

    public PresenceManager getPresenceManager() {
        return presenceManager;
    }

    public void setPresenceManager(PresenceManager presenceManager) {
        this.presenceManager = presenceManager;
    }

    public XMPPEndpoint getEndpoint() {
        return endpoint;
    }

    public void setEndpoint(XMPPEndpoint endpoint) {
        this.endpoint = endpoint;
    }

    public XMPPComponentLifeCycle getLifeCycle() {
        return lifeCycle;
    }

    public void setLifeCycle(XMPPComponentLifeCycle lifeCycle) {
        this.lifeCycle = lifeCycle;
    }
}
