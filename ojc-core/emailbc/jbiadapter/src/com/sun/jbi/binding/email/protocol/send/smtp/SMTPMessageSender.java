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
 * @(#)SMTPMessageSender.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.binding.email.protocol.send.smtp;

import com.sun.jbi.binding.email.protocol.*;
import java.security.Security;
import java.util.Enumeration;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.NormalizedMessage;
import javax.mail.MessagingException;
import javax.mail.NoSuchProviderException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.MimeMessage;
import javax.xml.namespace.QName;

import com.sun.jbi.binding.email.I18n;
import com.sun.jbi.binding.email.protocol.wsdl.SMTPAddress;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public class SMTPMessageSender {

    private static final Logger logger = Logger.getLogger(SMTPMessageSender.class.getName());
    private QName mOperationName;
    private SMTPEndpoint mEndpoint;
    private NormalizedMessage mNormalizedMessage;
    // internal usage
    private Session mSession;

    /**
     * @param meOperation
     * @param endpoint
     * @param normalizedMessage
     */
    public SMTPMessageSender(QName meOperation, SMTPEndpoint endpoint, NormalizedMessage normalizedMessage) {
        this.mOperationName = meOperation;
        this.mEndpoint = endpoint;
        this.mNormalizedMessage = normalizedMessage;
    }

    /**
     * @param normalizedMessage
     * @throws Exception
     */
    public void sendMail() throws ApplicationVariableNotDefinedException, SMTPConfigurationException, EmailDenormalizerException, NoSuchProviderException, MessagingException {
        this.mSession = this.buildSession();

        MimeMessage mimeMsg = new EmailDenormalizer(this.mSession, this.mNormalizedMessage, this.mOperationName, this.mEndpoint).denormalize();

        if (logger.isLoggable(Level.FINEST)) {
            StringBuffer headers = new StringBuffer("Email headers:");
            try {
                Enumeration enu = mimeMsg.getAllHeaderLines();
                while (enu.hasMoreElements()) {
                    headers.append("\r\n" + enu.nextElement().toString());
                }
            } catch (Exception e) {
                //ignore
            }
            logger.finest(headers.toString());
        }

        Transport transport = this.mSession.getTransport();
        boolean success;
        try {
            transport.connect();
            transport.sendMessage(mimeMsg, mimeMsg.getAllRecipients());
            success = true;
        } finally {
            transport.close();
        }

        I18n.fine(logger, "EMAILBC-1008: Email message sent : {0}", success);
    }

    /**
     * @return
     * @throws Exception
     */
    private Session buildSession() throws ApplicationVariableNotDefinedException, SMTPConfigurationException {

        boolean useSSL = this.mEndpoint.getSMTPAddress().getUseSSL();
        useSSL = Boolean.parseBoolean(resolveValue("" + useSSL, SMTPAddress.ATTR_EMAIL_USESSL, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_USESSL));
        I18n.finer(logger, "EMAILBC-2001: UseSSL is {0}", useSSL);

        String smtpServer = this.mEndpoint.getSMTPAddress().getHostName();
        smtpServer = resolveValue(smtpServer, SMTPAddress.ATTR_EMAIL_HOSTNAME, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_SERVER_NAME);

        if (null == smtpServer || 0 == smtpServer.length()) {
            throw new SMTPConfigurationException(I18n.loc("EMAILBC-7014: Required field \"{0}\" is not specified, please specify value", SMTPAddress.ATTR_EMAIL_HOSTNAME));
        }
        I18n.finer(logger, "EMAILBC-2002: SMTP Server name is {0}", smtpServer);
        int smtpPort = this.mEndpoint.getSMTPAddress().getPort();
        smtpPort = Integer.parseInt(this.resolveValue("" + smtpPort, SMTPAddress.ATTR_EMAIL_PORT, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_PORT));

        String userName = this.mEndpoint.getSMTPAddress().getUserName();
        userName = this.resolveValue(userName, SMTPAddress.ATTR_EMAIL_USERNAME, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_USERNAME);

        String password = this.mEndpoint.getSMTPAddress().getPassword();
        password = this.resolveValue(password, SMTPAddress.ATTR_EMAIL_PASSWORD, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_PASSWORD);

        Properties props = new Properties();
        if (useSSL) {
            props.setProperty(EmailBCConstants.SESSION_MAIL_SMTP_SSL_ENABLE, String.valueOf(true));
            Security.addProvider(new com.sun.net.ssl.internal.ssl.Provider());
            System.setProperty("java.protocol.handler.pkgs", "com.sun.net.ssl.internal.www.protocol");
            props.setProperty(EmailBCConstants.SESSION_MAIL_TRANSPORT_PROTOCOL, EmailBCConstants.PROTOCOL_SMTPS);
            props.setProperty(EmailBCConstants.SESSION_MAIL_SMTPS_HOST, smtpServer);
            props.setProperty(EmailBCConstants.SESSION_MAIL_SMTPS_PORT, String.valueOf(smtpPort));
            if (null == userName) {
                props.setProperty(EmailBCConstants.SESSION_MAIL_SMTPS_AUTH, String.valueOf(false));
            } else {
                props.setProperty(EmailBCConstants.SESSION_MAIL_SMTPS_AUTH, String.valueOf(true));
                props.setProperty(EmailBCConstants.SESSION_MAIL_SMTPS_USER, userName);
            }
        } else {
            props.setProperty(EmailBCConstants.SESSION_MAIL_TRANSPORT_PROTOCOL, EmailBCConstants.PROTOCOL_SMTP);
            props.setProperty(EmailBCConstants.SESSION_MAIL_SMTP_HOST, smtpServer);
            props.setProperty(EmailBCConstants.SESSION_MAIL_SMTP_PORT, String.valueOf(smtpPort));
            if (null == userName) {
                props.setProperty(EmailBCConstants.SESSION_MAIL_SMTP_AUTH, String.valueOf(false));
            } else {
                props.setProperty(EmailBCConstants.SESSION_MAIL_SMTP_AUTH, String.valueOf(true));
                props.setProperty(EmailBCConstants.SESSION_MAIL_SMTP_USER, userName);
            }
        }

        Session session = Session.getInstance(props, new EmailSessionAuthenticator(userName, password));

        return session;
    }

    private String resolveValue(String oldValue,
            String wsdlFieldName,
            String nmPropertyName) throws ApplicationVariableNotDefinedException {

        return EmailUtil.resolveValue(oldValue,
                wsdlFieldName,
                this.mEndpoint.getApplicationConfiguration(),
                nmPropertyName,
                this.mNormalizedMessage, mEndpoint.getContext().getConfiguration());
    }
}
