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
 * @(#)POP3MessageReceiver.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.binding.email.protocol.receive.pop3;

import com.sun.jbi.binding.email.protocol.receive.MessageReceiver;
import com.sun.jbi.binding.email.protocol.receive.imap.IMAPEndpoint;
import com.sun.jbi.binding.email.protocol.ApplicationVariableNotDefinedException;
import com.sun.jbi.binding.email.protocol.*;
import java.util.Properties;

import javax.mail.Authenticator;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;

import com.sun.jbi.binding.email.EmailBCEndpoint;
import com.sun.jbi.binding.email.protocol.wsdl.POP3Address;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import java.util.logging.Logger;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public class POP3MessageReceiver extends MessageReceiver {

    private static final Logger logger = Logger.getLogger(POP3MessageReceiver.class.getName());

    private static final String INBOX_FOLDER = "INBOX";

    private POP3Address pop3Address;
    private Properties sessionProperties;
    private Authenticator sessionAuthenticator;

    public POP3MessageReceiver(EmailBCEndpoint endpoint) {
        super(endpoint);
    }

    /**
     * @see com.sun.jbi.binding.email.protocol.MessageReceiver#parseConfiguration()
     */
    protected void parseConfiguration() throws ApplicationVariableNotDefinedException {
        pop3Address = ((POP3Endpoint) this.getEmailEndpoint()).getPOP3Address();

        // get the session properties
        Properties props = new Properties();

        //Resolving Application Configuration/Variables values if specified
        boolean useSSL = Boolean.parseBoolean(resolveValue("" + pop3Address.getUseSSL(), POP3Address.ATTR_EMAIL_USESSL));
        String pop3Host = resolveValue(pop3Address.getHostName(), POP3Address.ATTR_EMAIL_HOSTNAME);
        int port = Integer.parseInt(resolveValue("" + pop3Address.getPort(), POP3Address.ATTR_EMAIL_PORT));
        String userName = resolveValue(pop3Address.getUserName(), POP3Address.ATTR_EMAIL_USERNAME); 
        String password = resolveValue(pop3Address.getPassword(), POP3Address.ATTR_EMAIL_PASSWORD);
        if (useSSL) {
            props.put(EmailBCConstants.SESSION_MAIL_STORE_PROTOCOL, EmailBCConstants.PROTOCOL_POP3S);
            props.put(EmailBCConstants.SESSION_MAIL_POP3S_HOST, pop3Host);
            if (port != 0) {
                props.put(EmailBCConstants.SESSION_MAIL_POP3S_PORT, Integer.toString(port));
            }
            if (null != userName) {
                props.put(EmailBCConstants.SESSION_MAIL_POP3S_USER, userName);
            }
        } else {
            props.put(EmailBCConstants.SESSION_MAIL_STORE_PROTOCOL, EmailBCConstants.PROTOCOL_POP3);
            props.put(EmailBCConstants.SESSION_MAIL_POP3_HOST, pop3Host);
            if (port != 0) {
                props.put(EmailBCConstants.SESSION_MAIL_POP3_PORT, Integer.toString(port));
            }
            if (null != userName) {
                props.put(EmailBCConstants.SESSION_MAIL_POP3_USER, userName);
            }
        }

        sessionProperties = props;

        // get the authenticator information
        sessionAuthenticator =
            new EmailSessionAuthenticator(userName, password);
    }

    @Override
    protected int getPollingInterval() throws ApplicationVariableNotDefinedException {
    	int pollInterval = Integer.parseInt(resolveValue("" + pop3Address.getPollingInterval(), POP3Address.ATTR_POP3_POLLING_INTERVAL));
        return pollInterval;
    }

    @Override
    protected int getMaxMessageCount() throws ApplicationVariableNotDefinedException {
        //POP3 does not marking messages, hence cannot postponing the retreival later.
        //So message count is ignored.
    	int maxMessageCount = Integer.parseInt(resolveValue("" + pop3Address.getMaxMessageCount(), POP3Address.ATTR_POP3_MAX_MESSAGE_COUNT));
        return maxMessageCount;
    }

    @Override
    protected String getMessageAckMode() throws ApplicationVariableNotDefinedException {
    	String messageAckMode = resolveValue(pop3Address.getMessageAckMode(), POP3Address.ATTR_POP3_MESSAGE_ACK_MODE);
        return messageAckMode;
    }

    @Override
    protected String getMessageAckOperation() throws ApplicationVariableNotDefinedException {
    	String messageAckOperation = resolveValue(pop3Address.getMessageAckOperation(), POP3Address.ATTR_POP3_MESSAGE_ACK_OPERATION);
        return messageAckOperation;
    }

    @Override
    protected Session getSession() {
        return Session.getInstance(sessionProperties, sessionAuthenticator);
    }

    @Override
    protected String getFolderName() throws ApplicationVariableNotDefinedException {
        return INBOX_FOLDER;
    }

    @Override
    protected Message[] getMessages(Folder folder) throws MessagingException, ApplicationVariableNotDefinedException {
        if (!mReceivingEnabled.get()) return null;
        
        int messageCount = folder.getMessageCount();
        int maxMessageCount = getMaxMessageCount();
        int maxCount = maxMessageCount;

        ThrottlingConfig throttlingConfig = getEmailEndpoint().getThrottlingConfig();
        if (throttlingConfig != null) {
            int throttleCount = throttlingConfig.getMaxConcurrencyLimit();
            if (throttleCount > 0) {
                maxCount = Math.min(maxCount, throttleCount);
            }
        }

        //No messages
        if (messageCount == 0) return null;

        //Get all available messages
        if (maxCount == 0) {
            return folder.getMessages();
        }

        //get subset of messages
        return folder.getMessages(1, Math.min(messageCount, maxCount));
    }

    @Override
    protected void afterReceiving(Message msg) throws MessagingException {
        //POP3 does not support postponing acknowledgement. so acknowledge now.
        acknowledgeMessage(msg);
    }

    @Override
    protected boolean supportsAcknowledgement() {
        return false;
    }

    @Override
    protected Logger log() {
        return logger;
    }
    private String resolveValue(String oldValue, String wsdlFieldName) throws ApplicationVariableNotDefinedException {
        String newValue = oldValue;
        POP3Endpoint pop3Endpoint = (POP3Endpoint) this.getEmailEndpoint();
        newValue = EmailUtil.resolveAppConfigValue(newValue, wsdlFieldName, this.getEmailEndpoint().getApplicationConfiguration());
        newValue = EmailUtil.resolveAppVarsValue(newValue, pop3Endpoint.getContext().getConfiguration());
        return newValue;
    }    

}
