package com.gestalt.jbi.xmpp.component;

import com.gestalt.jbi.xmpp.extensions.XMPPOperation;

import org.jivesoftware.smack.ChatManager;
import org.jivesoftware.smack.XMPPConnection;
import org.jivesoftware.smack.packet.Message;
import org.jivesoftware.smack.packet.XMPPError;
import org.jivesoftware.smack.Chat;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * Responsible for Perfroming the various operations that the XMPP BC is responsible for.
 *
 * author: cgallemore
 * Date: Dec 21, 2007
 */
public class OperationExecutor {
    private static final Logger log = Logger.getLogger(OperationExecutor.class.getName());

    /**
     * Executes the given Operation.
     *
     * @param operationName - The Name of the operation to perform.
     * @param attributes - A map of name value pairs that are required for the given operation
     * @param handler - The IProviderHandler requesting the opertion to be executed.
     * @return - String:  Returns a response, if any.
     * @throws Exception - Throws Exception.
     */
    public static String performOperation(XMPPOperation.OperationName operationName,
        Map<String, String> attributes, IProviderHandler handler) throws Exception {
        String response = "";
        String message = attributes.get(XMPPAttributes.MESSAGE);
        String to = attributes.get(XMPPAttributes.JABBER_ID);

        // Jabber ID from Message Payload takes precedence over nm or static ID if defined
        if (to == null) {
            // nm property takes precedence over static id.
            to = attributes.get(XMPPAttributes.NM_JABBER_ID);
            if (to == null ) {
                to = attributes.get(XMPPAttributes.STATIC_JABBER_ID);
            }
        }
        String groupChat = attributes.get(XMPPAttributes.GROUP_CHAT);
        String groupList = attributes.get(XMPPAttributes.GROUP_LIST);
        String userList = attributes.get(XMPPAttributes.USER_LIST);
        String roomName = attributes.get(XMPPAttributes.ROOM_NAME);
        String alias = attributes.get(XMPPAttributes.ALIAS);

        switch (operationName) {
        case createGroup:

            String[] createGroups = groupList.split(",");

            for (String room : createGroups) {
                response = handler.getGroupChatManager()
                                  .createGroups(room, alias,
                        handler.getXMPPConnection());
            }

            break;

        case destroyGroup:

            String[] destroyGroups = groupList.split(",");

            for (String room : destroyGroups) {
                response = handler.getGroupChatManager()
                                  .destroyGroups(room, alias);
            }

            break;

        case invite:

            String[] inviteUsers = userList.split(",");
            List<String> list = new ArrayList<String>();

            for (String user : inviteUsers) {
                list.add(user);
            }

            handler.getGroupChatManager().sendInvitation(list, roomName, alias);

            break;

        case joinGroup:

            String[] joinGroups = groupList.split(",");

            for (String name : joinGroups) {
                handler.getGroupChatManager()
                       .joinRoom(name, alias, handler.getXMPPConnection());
            }

            break;

        case kick:

            String[] kickUsers = userList.split(",");

            for (String user : kickUsers) {
                handler.getGroupChatManager()
                       .kickParticipant(roomName, user,
                    "You are being kicked out of this room", alias);
            }

            break;

        case leaveGroup:

            String[] leaveGroups = groupList.split(",");

            for (String name : leaveGroups) {
                log.info(alias + " is trying to leave " + name);
                handler.getGroupChatManager()
                       .leaveChatRoom(alias, name, handler.getXMPPConnection());
            }

            break;

        case receiveMessage:
            break;

        case getOccupantsPresence:
            response = handler.getGroupChatManager()
                              .getOccupants(roomName, alias);

            break;

        case sendMessage:

            if ((groupChat != null) && groupChat.equalsIgnoreCase("yes")) {
                log.log(Level.INFO,
                    "Processing InOnly Group Chat: to: " + to + " alias: " +
                    alias);
                handler.getGroupChatManager().sendMessage(to, alias, message);
            } else {
                sendMessage(to, Message.Type.normal, null, message, true, handler);
            }

            break;
        }

        return response;
    }

    /**
     * Sends a message to another client
     *
     * @param to -
     *            Who the message is to
     * @param type -
     *            Message Type
     * @param error -
     *            XMPPError
     * @param payload -
     *            Content of the message.
     * @param store -
     * @throws Exception
     */
    public static void sendMessage(String to, Message.Type type, XMPPError error,
        String payload, boolean store, IProviderHandler handler) throws Exception {
        log.info("XMPP provider is creating chat to send message to " + to);
        XMPPConnection conn = handler.getXMPPConnection();
        ChatManager chatMgr = conn.getChatManager();
        Chat chat = chatMgr.createChat(to, null);
        // Chat chat = handler.getXMPPConnection().getChatManager().createChat(to, null);
        Message message = getMessage(to, type, error, payload, store, handler);
        log.log(Level.INFO,
            "XMPP Provider about to send message[" + to + " : " +
            message.getPacketID() + " : " + payload + "]");
        chat.sendMessage(message);
    }

    /**
     * Creates a new XMPP Message
     *
     * @param to -
     *            Who the message is to
     * @param type -
     *            Message.Type
     * @param error -
     *            XMPPError
     * @param payload -
     *            Content of the Message
     * @param store -
     * @return - Message - an XMPP Message
     */
    private static Message getMessage(String to, Message.Type type, XMPPError error,
        String payload, boolean store, IProviderHandler handler) {
        Message message = new Message();

        if (Message.Type.error.equals(type)) {
            message.setType(type);
            message.setError(error);
        }

        message.setBody(payload);

        if (store) {
            XMPPEndpoint.registerWaitingHandler(message.getPacketID(), to, handler);
        }

        return message;
    }
}
