package com.gestalt.jbi.xmpp.component.smack;

import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.packet.Message;

import org.jivesoftware.smackx.muc.InvitationListener;
import org.jivesoftware.smackx.muc.InvitationRejectionListener;

import java.util.logging.Logger;


/**
 * Handles the processing of an incoming invitation to join a group chat and for
 * rejections of invitations that were sent out.
 *
 * @author cgallemore
 * @since<pre>Sep 10, 2007<pre>
 */
public class InvitationManager implements InvitationRejectionListener,
    InvitationListener {
    private Logger log = Logger.getLogger(InvitationManager.class.getName());
    private GroupChatManager manager;

    /**
     * Constructor
     * @param manager - GroupChatManager
     */
    public InvitationManager(GroupChatManager manager) {
        this.manager = manager;
    }

    /**
     * Callback received when a user rejects our invitation to join a group. We
     * are just logging this for now.
     *
     * @param invitee -
     *            the user that rejected the invitation.
     * @param reason -
     *            The reason the user rejected the invitation.
     */
    public void invitationDeclined(String invitee, String reason) {
        log.info("Invitation was declined by: " + invitee +
            " for the following reason: " + reason);
    }

    /**
     * Callback method for all InvitationListeners. When the callback is called
     * we will automatically try to join the room. It will be left up to the
     * user to decide wheter or not they stay joined or leave the room.
     *
     * @param xmppConnection -
     *            The XMPPConnection
     * @param room -
     *            The room the invitation is for
     * @param inviter -
     *            The person iviting you to the room
     * @param reason -
     *            The reason for the invitation
     * @param password -
     *            The password needed for the room (only used for private rooms)
     * @param message -
     *            The Message
     */
    public void invitationReceived(XMPPConnection xmppConnection, String room,
        String inviter, String reason, String password, Message message) {
        log.info("Received Invitation to join " + room);

        String alias = manager.getAlias(message.getTo());
        manager.joinRoom(room, alias, xmppConnection);
    }
}
