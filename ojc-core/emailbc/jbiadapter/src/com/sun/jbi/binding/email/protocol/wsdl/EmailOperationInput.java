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
 * @(#)EmailOperationInput.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.binding.email.protocol.wsdl;

import java.io.Serializable;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public abstract class EmailOperationInput implements ExtensibilityElement, Serializable {
    private static final long serialVersionUID = 1L;
    public static final String ATTR_EMAIL_BODY = "message";
    public static final String ATTR_EMAIL_SUBJECT = "subject";
    public static final String ATTR_EMAIL_FROM = "from";
    public static final String ATTR_EMAIL_TO = "to";
    public static final String ATTR_EMAIL_CC = "cc";
    public static final String ATTR_EMAIL_BCC = "bcc";
    public static final String ATTR_EMAIL_NEWSGROUPS = "newsgroups";
    public static final String ATTR_EMAIL_HANDLE_NM_ATTACHMENTS = "handleNMAttachments";

    private Boolean isRequired = Boolean.TRUE;
    private String message = null;
    private String subject = null;
    private String from = null;
    private String to = null;
    private String cc = null;
    private String bcc = null;
    private String newsgroups = null;
    private boolean handleNMAttachments = true;

    public abstract EmailAttachment[] getAttachments();
    
    /**
     * @see javax.wsdl.extensions.ExtensibilityElement#setElementType(javax.xml.namespace.QName)
     */
    public void setElementType(QName qName) {
        // No op
    }

    /**
     * @see javax.wsdl.extensions.ExtensibilityElement#getElementType()
     */
    public abstract QName getElementType() ;

    /**
     * @see javax.wsdl.extensions.ExtensibilityElement#setRequired(java.lang.Boolean)
     */
    public void setRequired(Boolean required) {
        this.isRequired = required;
    }
    /**
     * @see javax.wsdl.extensions.ExtensibilityElement#getRequired()
     */
    public Boolean getRequired() {
        return isRequired;
    }

    public String getBcc() {
        return bcc;
    }

    public void setBcc(String bcc) {
        this.bcc = bcc;
    }

    public String getCc() {
        return cc;
    }

    public void setCc(String cc) {
        this.cc = cc;
    }

    public String getFrom() {
        return from;
    }

    public void setFrom(String from) {
        this.from = from;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String getSubject() {
        return subject;
    }

    public void setSubject(String subject) {
        this.subject = subject;
    }

    public String getTo() {
        return to;
    }

    public void setTo(String to) {
        this.to = to;
    }

    public String getNewsgroups() {
        return newsgroups;
    }

    public void setNewsgroups(String newsgroups) {
        this.newsgroups = newsgroups;
    }

    public boolean getHandleNMAttachments() {
        return handleNMAttachments;
    }

    public void setHandleNMAttachments(boolean handleNMAttachments) {
        this.handleNMAttachments = handleNMAttachments;
    }

    /**
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        final StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\n EmailOperationInput " + getElementType() + ":");
        strBuf.append("\n Required = " + isRequired);
        strBuf.append("\n Message = " + message);
        strBuf.append("\n Subject = " + subject);
        strBuf.append("\n From = " + from);
        strBuf.append("\n To = " + to);
        strBuf.append("\n Cc = " + cc);
        strBuf.append("\n Bcc = " + bcc);
        strBuf.append("\n Newsgroup = " + newsgroups);
        strBuf.append("\n HandleNMAttachments = " + handleNMAttachments);

        return strBuf.toString();
    }

    public String getAttachmentsPart() {
        return "message";
    }
}
