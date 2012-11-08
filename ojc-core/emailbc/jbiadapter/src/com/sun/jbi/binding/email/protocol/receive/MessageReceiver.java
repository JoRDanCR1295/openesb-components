/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)MessageReceiver.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.binding.email.protocol.receive;

import com.sun.jbi.binding.email.protocol.*;
import com.sun.jbi.binding.email.protocol.receive.imap.IMAPEndpoint;
import com.sun.jbi.binding.email.protocol.receive.pop3.POP3Endpoint;
import com.sun.jbi.binding.email.protocol.wsdl.IMAPAddress;
import com.sun.jbi.binding.email.protocol.wsdl.POP3Address;
import com.sun.jbi.binding.email.protocol.EmailBCConstants;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;

import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.mail.internet.MimeMessage;
import javax.xml.namespace.QName;

import com.sun.jbi.binding.email.EmailBCEndpoint;
import com.sun.jbi.binding.email.I18n;
import com.sun.jbi.common.descriptor.EndpointInfo;
import java.util.HashMap;
import java.lang.Thread.State;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;
import javax.mail.FetchProfile;
import javax.mail.Flags;
import javax.mail.Flags.Flag;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Store;
import javax.xml.transform.Source;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public abstract class MessageReceiver implements Runnable {

    private EmailBCEndpoint mEmailEndpoint;
    // atomic flag to enable/disable receiving
    protected AtomicBoolean mReceivingEnabled = new AtomicBoolean(false);
    protected AtomicBoolean hasAckPending = new AtomicBoolean(false);
    protected EmailMessageContext emailMessageContext = new EmailMessageContext();
    private Thread thread;

    public MessageReceiver(EmailBCEndpoint endpoint) {
        this.mEmailEndpoint = endpoint;
        try {
            parseConfiguration();
        } catch (ApplicationVariableNotDefinedException e) {
            I18n.severe(log(), "EMAILBC-7032: Error parsing Application Variables/Configuration", e);
        }
    }

    public void startReceiving() {
        mReceivingEnabled.set(true);
        I18n.fine(log(), "EMAILBC-1005: Started polling for new messages");
    }

    public void onReceiving(Message emailMessage) {
        //Receive mail, normalize and post to NMR
        try {
            //ComponentContext context = this.mEmailEndpoint.getContext().getComponentContext();
            ManagerContext context = mEmailEndpoint.getContext();
            MessagingChannel channel = context.getMessagingChannel();
            EndpointInfo info = this.mEmailEndpoint.getInfo();
            QName serviceName = info.getServiceName();
            String endpointName = info.getEndpointName();
            ServiceEndpoint endpoint = context.getComponentContext().getEndpoint(serviceName, endpointName);
            final QName operationName = lookupOperation();
            final MessageExchangeFactory mef = channel.createExchangeFactory();

            EmailNormalizer emailNormalizer = new EmailNormalizer(emailMessage, operationName, this.mEmailEndpoint);

            Source source = emailNormalizer.getSource();
            String messageId = ((MimeMessage) emailMessage).getMessageID();
            EmailBCExchangeTemplates emailBCExchangeTemplate =
                    new EmailBCExchangeTemplates(endpoint, operationName, true, null, messageId, source, mef);
            Map<String, String> NMPropMap = new HashMap<String, String>();
            if (mEmailEndpoint instanceof IMAPEndpoint) {
                NMPropMap = getIMAPNMProperties();
            } else if (mEmailEndpoint instanceof POP3Endpoint) {
                NMPropMap = getPOP3NMProperties();
            }
            Properties nmProperties = new Properties();
            nmProperties.putAll(NMPropMap);
            nmProperties.putAll(emailNormalizer.getNMProperties());
            emailBCExchangeTemplate.setPropNM(nmProperties);
            emailBCExchangeTemplate.setNMAttachments(emailNormalizer.getNMAttachments());

            // Store the message information for response later to our
            // External Service
            //Store to correlation map to acknowledge the message later.
            if (supportsAcknowledgement()) {
                emailMessageContext.addPendingMessage(messageId);

                context.getCorrelationMap().put(messageId, emailMessageContext);
            }

            channel.send(emailBCExchangeTemplate);
            I18n.fine(log(), "EMAILBC-1011: Sent message {0} to NMR", messageId);

        } catch (Exception e) {
            I18n.severe(log(), "EMAILBC-7004: Error sending the message to the channel", e);
        }

    }

    public void stopReceiving() {
        mReceivingEnabled.set(false);
        I18n.fine(log(), "EMAILBC-1006: Stopped polling for new messages");
    }

    public void run() {
        this.thread = Thread.currentThread();
        execute();
    }

    public EmailBCEndpoint getEmailEndpoint() {
        return mEmailEndpoint;
    }

    private QName lookupOperation() {
        final QName[] operationNames =
                (QName[]) this.mEmailEndpoint.getEmailOperations().keySet().toArray(new QName[0]);
        return operationNames[0];

    }

    protected void execute() {
        Logger logger = log();
        // polling interval is in seconds
        try {
            long sleepInterval = getPollingInterval() * 1000;
            emailMessageContext.setReceiver(this);
            while (emailMessageContext.hasAcknowledgeableMessages() || mReceivingEnabled.get()) {

                Session session = this.getSession();
                if (logger.isLoggable(Level.FINEST)) {
                    session.setDebug(true);
                }
                Folder folder = null;
                Store store = null;
                try {
                    // connect the store
                    store = session.getStore();
                    store.connect();

                    I18n.finer(logger, "EMAILBC-2032: Connected to the email server");

                    // determine the folder name
                    String folderName = this.getFolderName();

                    // open the folder
                    folder = store.getDefaultFolder().getFolder(folderName);
                    if (folder.exists()) {
                        folder.open(Folder.READ_WRITE);

                        Message[] messages = this.getMessages(folder);
                        if (mReceivingEnabled.get()) {
                            if (messages != null && messages.length > 0) {
                                int i = 1;
                                I18n.fine(logger, "EMAILBC-1009: Got {0} new messages", messages.length);
                                FetchProfile fp = new FetchProfile();
                                fp.add(FetchProfile.Item.ENVELOPE);
                                fp.add(FetchProfile.Item.CONTENT_INFO);
                                for (Message message : messages) {
                                    MimeMessage msg = (MimeMessage) message;
                                    // prefetch the message
                                    folder.fetch(new Message[]{msg}, fp);
                                    I18n.fine(logger, "EMAILBC-1013: Preparing message {0} of {1} to send to NMR", i++, messages.length);
                                    this.onReceiving(msg);
                                    this.afterReceiving(msg);

                                    if (!mReceivingEnabled.get()) {
                                        break;
                                    }
                                }
                            } else {
                                I18n.fine(logger, "EMAILBC-1010: No new messages");
                            }
                        }
                    } else {
                        I18n.severe(logger, "EMAILBC-7005: No folder with name \"{0}\" exists on the server", null, folderName);
                    }
                } catch (ApplicationVariableNotDefinedException e) {
                    I18n.severe(logger, "EMAILBC-7032: Error parsing Application Variables/Configuration", e);
                } catch (MessagingException ex) {
                    I18n.severe(logger, "EMAILBC-7006: Error retreiving emails", ex);
                } finally {
                    try {
                        // clean up
                        if (folder != null && folder.isOpen()) {
                            folder.close(true);
                        }
                        if (store != null) {
                            store.close();
                        }
                    } catch (MessagingException e) {
                        I18n.severe(logger, "EMAILBC-7007: Error while closing connection to the email folder/store", e);
                    }
                }

                if (!mReceivingEnabled.get()) {
                    // avoid sleeping when there is a pending stop request
                    break;
                }

                try {
                    if (!emailMessageContext.hasAcknowledgeableMessages()) {
                        I18n.finer(logger, "EMAILBC-2033: Waiting for next poll cycle to check new messages..");
                        Thread.sleep(sleepInterval);
                    }
                } catch (InterruptedException ex) {
                    I18n.finer(logger, "EMAILBC-2034: Awakened from sleep, to acknowledge DONE messages");
                }
            } // end of while
        } catch (ApplicationVariableNotDefinedException e) {
            I18n.severe(logger, "EMAILBC-7032: Error parsing Application Variables/Configuration", e);
        }
        I18n.finer(logger, "EMAILBC-2035: Stopped retreiving new messages");
        return;
    }

    protected List<Message> processMessages(Message[] messages, int messageCount, String ackMode, String ackOperation) throws MessagingException {
        Logger logger = log();
        Set<String> doneMessages = emailMessageContext.getDoneMessages();
        Set<String> pendingMessages = emailMessageContext.getPendingMessages();
        Set<String> errorMessages = emailMessageContext.getErrorMessages();
        List<Message> newMessages = new ArrayList();
        for (Message message : messages) {
            MimeMessage msg = (MimeMessage) message;
            String messageId = msg.getMessageID();
            if (messageId != null) {
                if (doneMessages.contains(messageId)) {
                    try {
                        acknowledgeMessage(msg);
                        emailMessageContext.removeMessageDone(messageId);
                        I18n.fine(logger, "EMAILBC-1012: DONE Ack: Message acknowledged: {0},Pending done acks: {1}", messageId, emailMessageContext.getDoneMessages().size());
                    } catch (MessagingException ex) {
                        I18n.severe(logger, "EMAILBC-7008: Error marking message \"{0}\" as DONE", ex, messageId);
                    }

                    continue;
                }

                if (errorMessages.contains(messageId)) {
                    try {
                        acknowledgeErrorMessage(message);
                        emailMessageContext.removeErrorMessage(messageId);
                        I18n.finer(logger, "EMAILBC-2037: ERROR Ack: Message acknowledged: {0},Pending error acks: {1}", messageId, emailMessageContext.getErrorMessages().size());
                    } catch (MessagingException ex) {
                        I18n.severe(logger, "EMAILBC-7009: Error marking message \"{0}\"as ERROR", ex, messageId);
                    }
                    continue;
                }

                if (pendingMessages.contains(messageId)) {
                    I18n.finer(logger, "EMAILBC-2038: Message still pending: {0}", messageId);
                    continue;
                }

                if (mReceivingEnabled.get()) {
                    newMessages.add(msg);
                    if (newMessages.size() >= messageCount) {
                        break;
                    }
                } else if (!emailMessageContext.hasAcknowledgeableMessages()) {
                    break;
                }
            }
        }
        return newMessages;
    }

    protected void acknowledgeMessage(Message msg) throws MessagingException {
        Flags.Flag flag = null;
        try {
            String ackMode = getMessageAckMode();
            String ackOperation = getMessageAckOperation();
            // perform the ack operation if in automatic mode
            if (EmailBCConstants.ACK_MODE_AUTOMATIC.equals(ackMode)) {
                if (EmailBCConstants.ACK_OP_DELETE.equals(ackOperation)) {
                    flag = Flags.Flag.DELETED;
                } else if (EmailBCConstants.ACK_OP_MARK_AS_READ.equals(ackOperation)) {
                    flag = Flags.Flag.SEEN;
                }
            }
            if (flag != null) {
                I18n.finer(log(), "EMAILBC-2039: Marking message as {0}", getStatus(flag));
                msg.setFlag(flag, true);
            }
        } catch (ApplicationVariableNotDefinedException e) {
            I18n.severe(log(), "EMAILBC-7032: Error parsing Application Variables/Configuration", e);
        }
    }

    private String getStatus(Flag flag) {
        if (flag == Flag.DELETED) {
            return "deleted";
        }
        if (flag == Flag.SEEN) {
            return "read";
        }
        return null;
    }

    protected abstract Logger log();

    protected abstract void parseConfiguration() throws ApplicationVariableNotDefinedException;

    protected abstract int getPollingInterval() throws ApplicationVariableNotDefinedException;

    protected abstract int getMaxMessageCount() throws ApplicationVariableNotDefinedException;

    protected abstract String getMessageAckMode() throws ApplicationVariableNotDefinedException;

    protected abstract String getMessageAckOperation() throws ApplicationVariableNotDefinedException;

    protected abstract Session getSession();

    protected abstract String getFolderName() throws ApplicationVariableNotDefinedException;

    /**
     * Gets new messages from the email server.
     * The implementation should call processMessages, so that all messages can be acknowledged properly.
     * @param folder
     * @return
     * @throws MessagingException
     */
    protected abstract Message[] getMessages(Folder folder) throws MessagingException, ApplicationVariableNotDefinedException;

    protected abstract void afterReceiving(Message msg) throws MessagingException;

    protected void acknowledgeErrorMessage(Message message) throws MessagingException {
        //Do no delete the error message, in future see, if it can be moved to another folder.
        message.setFlag(Flag.SEEN, true);
    }

    protected final int getPendingMessagesCount() {
        return emailMessageContext.getPendingMessages().size();
    }

    protected abstract boolean supportsAcknowledgement();

    public void doAcknowledge() {
        if (thread.getState().equals(State.TIMED_WAITING)) {
            I18n.finer(log(), "EMAILBC-2046: Interrupting the receiver thread, so that messages can be acknowledged");
            thread.interrupt();
        }
    }

    public Map getIMAPNMProperties() {
        Map<String, String> nmProps = new HashMap<String, String>();
        IMAPAddress imapAddress = ((IMAPEndpoint) this.getEmailEndpoint()).getIMAPAddress();
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_IMAP_SERVER_NAME, imapAddress.getHostName());
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_IMAP_PORT, String.valueOf(imapAddress.getPort()));
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_IMAP_USERNAME, imapAddress.getUserName());
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_IMAP_PASSWORD, imapAddress.getPassword());
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_IMAP_USESSL, String.valueOf(imapAddress.getUseSSL()));
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_IMAP_MAX_MESSAGE_COUNT, String.valueOf(imapAddress.getMaxMessageCount()));
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_IMAP_MESSAGE_ACK_MODE, imapAddress.getMessageAckMode());
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_IMAP_POLLING_INTERVAL, String.valueOf(imapAddress.getPollingInterval()));
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_IMAP_MAIL_FOLDER, imapAddress.getMailFolder());
        return nmProps;

    }

    public Map getPOP3NMProperties() {
        Map<String, String> nmProps = new HashMap<String, String>();
        POP3Address pop3Address = ((POP3Endpoint) this.getEmailEndpoint()).getPOP3Address();
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_POP3_SERVER_NAME, pop3Address.getHostName());
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_POP3_PORT, String.valueOf(pop3Address.getPort()));
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_POP3_USERNAME, pop3Address.getUserName());
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_POP3_PASSWORD, pop3Address.getPassword());
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_POP3_USESSL, String.valueOf(pop3Address.getUseSSL()));
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_POP3_MAX_MESSAGE_COUNT, String.valueOf(pop3Address.getMaxMessageCount()));
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_POP3_MESSAGE_ACK_MODE, pop3Address.getMessageAckMode());
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_POP3_POLLING_INTERVAL, String.valueOf(pop3Address.getPollingInterval()));
        nmProps.put(EmailBCConstants.NM_PROPERTY_EMAIL_POP3_MAIL_FOLDER, "INBOX");
        return nmProps;

    }
}
