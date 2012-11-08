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
 * @(#)SMTPOperationInput.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;
import com.ibm.wsdl.Constants;

/**
 *
 * @author aegloff
 */
public class SMTPOperationInput implements ExtensibilityElement, Serializable {
    
    // Attribute names
    public static final String ATTR_MESSAGE = "message";
    public static final String ATTR_SUBJECT = "subject";
    public static final String ATTR_FROM = "from";
    public static final String ATTR_CHARSET = "charset";
    public static final String ATTR_ENCODING_STYLE = "encodingStyle";
    public static final String ATTR_USE_TYPE = "use";
    public static final String ATTR_TO="to";
    public static final String ATTR_CC="cc";
    public static final String ATTR_BCC="bcc";


    // Qualified element names
    public static final QName QNAME_OPERATION_INPUT =
        new QName(SMTPConstants.NS_URI_SMTP, Constants.ELEM_INPUT);

    private static final long serialVersionUID = 1L;
    
    private Boolean mFieldRequired = null;
    private String mMessage = null;
    private String mSubject = null;
    private String mFrom = null;
    private String mCharset = null;
    private String mEncodingStyle = null;
    private SMTPAttachment[] mSMTPAttachments = null;
    private String smtpUseType = SMTPConstants.SMTP_USE_TYPE_LITERAL; //default
    private String mTo = null;
    private String mCc = null;
    private String mBcc = null;

    /** 
     * Get the extensibility element type
     * @return the extensibility element's type 
     */
    public QName getElementType() {
        return SMTPOperationInput.QNAME_OPERATION_INPUT;
    }

    /** 
     * Set the extensibility element type
     * @param elementType the type 
     */
    public void setElementType(final QName elementType) {
    }
    
    /** 
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return mFieldRequired;
    }

    /** 
     * Set whether required (for wsdl:required) 
     */
    public void setRequired(final Boolean required) {
        mFieldRequired = required;
    }

    public String getMessage() {
        return mMessage;
    }

    public void setMessage(final String message) {
        mMessage = message;
    }

    public String getSubject() {
        return mSubject;
    }

    public void setSubject(final String subject) {
        mSubject = subject;
    }

    public String getFrom() {
        return mFrom;
    }

    public void setFrom(final String from) {
        mFrom = from;
    }

    public String getCharset() {
        return mCharset;
    }

    public void setCharset(final String charset) {
        mCharset = charset;
    }

    public SMTPAttachment[] getSMTPAttachment() {
        return mSMTPAttachments;
    }

    public void setSMTPAttachment(final SMTPAttachment[] attachments) {
        mSMTPAttachments = attachments;
    }

	public String toString() {
        final StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nSMTP operation Input (" + SMTPOperationInput.QNAME_OPERATION_INPUT + "):");
        strBuf.append("\nRequired=" + mFieldRequired);
        strBuf.append("\nMessage=" + mMessage);
        strBuf.append("\nSubject=" + mSubject);
        strBuf.append("\nCharset=" + mCharset);

        return strBuf.toString();
    }

	public String getEncodingStyle() {
		return mEncodingStyle;
	}

	public void setEncodingStyle(final String encodingStyle) {
		mEncodingStyle = encodingStyle;
	}

	public String getSmtpUseType() {
		return smtpUseType;
	}

	public void setSmtpUseType(final String smtpUseType) {
		this.smtpUseType = smtpUseType;
	}
	
	public String getTo(){
		return mTo;
	}
	public void setTo(final String to){
		mTo = to;
	}

	public String getCc(){
		return mCc;
	}
	public void setCc(final String cc){
		mCc = cc;
	}
	public String getBcc(){
		return mBcc;
	}
	public void setBcc(final String bcc){
		mBcc = bcc;
	}
	
	
}
