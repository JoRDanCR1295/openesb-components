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
 * @(#)IMAPMessageReceiver.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.binding.email.protocol.receive.imap;

import com.sun.jbi.binding.email.protocol.receive.MessageReceiver;
import com.sun.jbi.binding.email.protocol.*;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import java.util.List;
import java.util.Properties;

import javax.mail.Authenticator;

import com.sun.jbi.binding.email.EmailBCEndpoint;
import com.sun.jbi.binding.email.protocol.wsdl.IMAPAddress;
import java.util.logging.Logger;
import javax.mail.Flags;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.internet.MimeMessage;
import javax.mail.search.AndTerm;
import javax.mail.search.FlagTerm;
import javax.mail.search.SearchTerm;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 *
 */
public class IMAPMessageReceiver extends MessageReceiver {

    private static final Logger logger = Logger.getLogger(IMAPMessageReceiver.class.getName());
    private static final String INBOX_FOLDER = "INBOX";
    private IMAPAddress imapAddress;
    private Properties sessionProperties;
    private Authenticator sessionAuthenticator;
    private SearchTerm userDefinedSearchTerm;

    public IMAPMessageReceiver(EmailBCEndpoint endpoint) {
        super(endpoint);
    }

    /**
     * @see com.sun.jbi.binding.email.protocol.MessageReceiver#parseConfiguration()
     */
    protected void parseConfiguration() throws ApplicationVariableNotDefinedException {
        imapAddress = ((IMAPEndpoint) this.getEmailEndpoint()).getIMAPAddress();

        // get the session properties
        Properties props = new Properties();

        //Resolving Application Configuration/Variables values if specified
        boolean useSSL = Boolean.parseBoolean(resolveValue("" + imapAddress.getUseSSL(), IMAPAddress.ATTR_EMAIL_USESSL));
        String imapHost = resolveValue(imapAddress.getHostName(), IMAPAddress.ATTR_EMAIL_HOSTNAME);
        int port = Integer.parseInt(resolveValue("" + imapAddress.getPort(), IMAPAddress.ATTR_EMAIL_PORT));
        String userName = resolveValue(imapAddress.getUserName(), IMAPAddress.ATTR_EMAIL_USERNAME); 
        String password = resolveValue(imapAddress.getPassword(), IMAPAddress.ATTR_EMAIL_PASSWORD);
        if (useSSL) {
            props.put(EmailBCConstants.SESSION_MAIL_STORE_PROTOCOL, EmailBCConstants.PROTOCOL_IMAPS);
            props.put(EmailBCConstants.SESSION_MAIL_IMAPS_HOST, imapHost);
            if (port != 0) {
                props.put(EmailBCConstants.SESSION_MAIL_IMAPS_PORT, Integer.toString(port));
            }
            if (null != userName) {
                props.put(EmailBCConstants.SESSION_MAIL_IMAPS_USER, userName);
            }
        } else {
            props.put(EmailBCConstants.SESSION_MAIL_STORE_PROTOCOL, EmailBCConstants.PROTOCOL_IMAP);
            props.put(EmailBCConstants.SESSION_MAIL_IMAP_HOST, imapHost);
            if (port != 0) {
                props.put(EmailBCConstants.SESSION_MAIL_IMAP_PORT, Integer.toString(port));
            }
            if (null != userName) {
                props.put(EmailBCConstants.SESSION_MAIL_IMAP_USER, userName);
            }
        }

        sessionProperties = props;

        // get the authenticator information
        sessionAuthenticator =
                new EmailSessionAuthenticator(userName, password);
    }

    @Override
    protected int getPollingInterval() throws ApplicationVariableNotDefinedException {
    	int pollInterval = Integer.parseInt(resolveValue("" + imapAddress.getPollingInterval(), IMAPAddress.ATTR_IMAP_POLLING_INTERVAL));
        return pollInterval;
    }

    @Override
    protected int getMaxMessageCount() throws ApplicationVariableNotDefinedException {
    	int maxMessageCount = Integer.parseInt(resolveValue("" + imapAddress.getMaxMessageCount(), IMAPAddress.ATTR_IMAP_MAX_MESSAGE_COUNT));
        return maxMessageCount;
    }

    @Override
    protected String getMessageAckMode() throws ApplicationVariableNotDefinedException {
    	String messageAckMode = resolveValue(imapAddress.getMessageAckMode(), IMAPAddress.ATTR_IMAP_MESSAGE_ACK_MODE);
        return messageAckMode;
    }

    @Override
    protected String getMessageAckOperation() throws ApplicationVariableNotDefinedException {
    	String messageAckOperation = resolveValue(imapAddress.getMessageAckOperation(), IMAPAddress.ATTR_IMAP_MESSAGE_ACK_OPERATION);
        return messageAckOperation;
    }

    @Override
    protected Session getSession() {
        return Session.getInstance(sessionProperties, sessionAuthenticator);
    }

    @Override
    protected String getFolderName() throws ApplicationVariableNotDefinedException {
        // determine the folder name
        String folderName = imapAddress.getMailFolder();
        if (folderName == null || "".equals(folderName.trim())) {
            // use the predefined default INBOX folder name
            folderName = INBOX_FOLDER;
        } else {
        	folderName = resolveValue(imapAddress.getMailFolder(), IMAPAddress.ATTR_IMAP_MAIL_FOLDER);
        }
        return folderName;
    }

    @Override
    protected Message[] getMessages(Folder folder) throws MessagingException, ApplicationVariableNotDefinedException {
        int maxMessageCount = getMaxMessageCount();
        int maxCount = maxMessageCount;

        //Get throttle count, and pending messages
        ThrottlingConfig throttlingConfig = getEmailEndpoint().getThrottlingConfig();
        if (throttlingConfig != null) {
            int throttleCount = throttlingConfig.getMaxConcurrencyLimit();
            if (throttleCount > 0) {
                //get pending message count
                throttleCount = throttleCount - getPendingMessagesCount();
                if (throttleCount >= 0) {
                    maxCount = Math.min(maxCount, throttleCount);
                } else {
                    maxCount = 0;
                }
            }
        }

        // search for messages NOT SEEN and NOT DELETED
        Flags flags = new Flags();
        flags.add(Flags.Flag.DELETED);
        flags.add(Flags.Flag.SEEN);

        SearchTerm searchTerm = new FlagTerm(flags, false);

        if (userDefinedSearchTerm != null) {
            searchTerm = new AndTerm(searchTerm, userDefinedSearchTerm);
        }
        
        Message[] messages = folder.search(searchTerm);
        if (messages != null && messages.length > 0) {
            List<Message> newMessages = processMessages(messages, maxCount, getMessageAckMode(), getMessageAckOperation());
            if (newMessages != null && newMessages.size() > 0) {
                return newMessages.toArray(new Message[newMessages.size()]);
            }
        }
        return null;

    }

    @Override
    protected void afterReceiving(Message msg) throws MessagingException {
        if (EmailBCConstants.ACK_MODE_MANUAL.equals(imapAddress.getMessageAckMode())) {
            acknowledgeMessage(msg);
        }
        
        if (msg instanceof MimeMessage && ((MimeMessage) msg).getMessageID() != null) {
            //IMAP supports acknowledgement later, so mark as unread, till the message is processed.
            msg.setFlag(Flags.Flag.SEEN, false);
        } else {
            //If message does not have message id, there is no easy way to identify
            // the message later, so acknowledge the message at this point
            acknowledgeMessage(msg);
        }

    }

    @Override
    protected boolean supportsAcknowledgement() {
        return true;
    }

    @Override
    protected Logger log() {
        return logger;
    }
    private String resolveValue(String oldValue, String wsdlFieldName) throws ApplicationVariableNotDefinedException {
        String newValue = oldValue;
        IMAPEndpoint imapEndpoint = (IMAPEndpoint) this.getEmailEndpoint();
        newValue = EmailUtil.resolveAppConfigValue(newValue, wsdlFieldName, this.getEmailEndpoint().getApplicationConfiguration());
        newValue = EmailUtil.resolveAppVarsValue(newValue, imapEndpoint.getContext().getConfiguration());
        return newValue;
    }
}
