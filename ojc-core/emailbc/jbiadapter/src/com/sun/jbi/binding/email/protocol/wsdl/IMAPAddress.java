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
 * @(#)IMAPAddress.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.binding.email.protocol.wsdl;

import javax.xml.namespace.QName;
import com.sun.jbi.binding.email.protocol.EmailBCConstants;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public class IMAPAddress extends EmailAddress {
    private static final long serialVersionUID = 1L;

    public static final String ELEM_IMAP_ADDRESS = "IMAPaddress";
    public static final QName QNAME_IMAP_ADDRESS =
        new QName(EmailBCConstants.NS_URI_EMAILBC, ELEM_IMAP_ADDRESS);

    public static final String ATTR_IMAP_MAIL_FOLDER = "mailFolder";
    public static final String ATTR_IMAP_MESSAGE_ACK_MODE = "messageAckMode";
    public static final String ATTR_IMAP_MESSAGE_ACK_OPERATION = "messageAckOperation";
    public static final String ATTR_IMAP_POLLING_INTERVAL = "pollingInterval";
    public static final String ATTR_IMAP_MAX_MESSAGE_COUNT = "maxMessageCount";

    private String mailFolder = "INBOX";
    private String messageAckMode = EmailBCConstants.ACK_MODE_AUTOMATIC;
    private String messageAckOperation = EmailBCConstants.ACK_OP_MARK_AS_READ;
    private int maxMessageCount = 10;
    private int pollingInterval = 1; 

    public IMAPAddress() {
        // apply defaults
        this.setPort(993);
        this.setUseSSL(true);
    }
    
    /**
     * @see com.sun.jbi.binding.email.protocol.wsdl.EmailAddress#getElementType()
     */
    @Override
    public QName getElementType() {
        return QNAME_IMAP_ADDRESS;
    }

    public String getMailFolder() {
        return mailFolder;
    }

    public void setMailFolder(String mailFolder) {
        this.mailFolder = mailFolder;
    }

    public String getMessageAckMode() {
        return messageAckMode;
    }

    public void setMessageAckMode(String messageAckMode) {
        this.messageAckMode = messageAckMode;
    }

    public String getMessageAckOperation() {
        return messageAckOperation;
    }

    public void setMessageAckOperation(String messageAckOperation) {
        this.messageAckOperation = messageAckOperation;
    }

    public int getMaxMessageCount() {
        return maxMessageCount;
    }

    public void setMaxMessageCount(int maxMessageCount) {
        this.maxMessageCount = maxMessageCount;
    }

    public int getPollingInterval() {
        return pollingInterval;
    }

    public void setPollingInterval(int pollingInterval) {
        this.pollingInterval = pollingInterval;
    }

    /**
     * @see com.sun.jbi.binding.email.protocol.wsdl.EmailAddress#toString()
     */
    public String toString() {
        final StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\n Mailfolder = " + mailFolder);
        strBuf.append("\n MessageAckMode = " + messageAckMode);
        strBuf.append("\n MessageAckOperation = " + messageAckOperation);
        strBuf.append("\n MaxMessageCount = " + maxMessageCount);
        strBuf.append("\n PollingInterval = " + pollingInterval);
        return strBuf.toString();
    }
}
