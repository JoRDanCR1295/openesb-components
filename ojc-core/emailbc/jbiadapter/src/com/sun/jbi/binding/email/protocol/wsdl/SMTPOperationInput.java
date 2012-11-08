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
public class SMTPOperationInput extends EmailOperationInput {
    private static final long serialVersionUID = 1L;
    public static final String ELEM_SMTP_INPUT = "SMTPinput";
    public static final String ATTR_SMTP_CHARSET = "charset";
    public static final String ATTR_SMTP_USE_TYPE = "use";
    public static final String ATTR_SMTP_ENCODING_STYLE = "encodingStyle";
    public static final String ATTR_SMTP_SEND_OPTION = "sendOption";
    public static final String ATTR_SMTP_EMBED_IMAGES_IN_HTML = "embedImagesInHtml";
    public static final QName QNAME_SMTP_OPERATION_INPUT =
        new QName(EmailBCConstants.NS_URI_EMAILBC, ELEM_SMTP_INPUT);

    private String mCharset = null;
    private String smtpUseType = EmailBCConstants.SMTP_USE_TYPE_LITERAL;
    private String mEncodingStyle = null;
    private String mSendOption = EmailBCConstants.SMTP_SEND_OPTION_TEXT_ONLY;
    private boolean mEmbedImagesInHtml = false;
    private SMTPAttachment[] mAttachments = null;

    /**
     * @see com.sun.jbi.binding.email.protocol.wsdl.EmailOperationInput#getElementType()
     */
    @Override
    public QName getElementType() {
        return QNAME_SMTP_OPERATION_INPUT;
    }

    public String getCharset() {
        return mCharset;
    }

    public void setCharset(final String charset) {
        mCharset = charset;
    }

    public String getSmtpUseType() {
        return smtpUseType;
    }

    public void setSmtpUseType(final String smtpUseType) {
        this.smtpUseType = smtpUseType;
    }

    public String getEncodingStyle() {
        return mEncodingStyle;
    }

    public void setEncodingStyle(final String encodingStyle) {
        mEncodingStyle = encodingStyle;
    }

    public String getSendOption() {
        return this.mSendOption;
    }

    public void setSendOption(String sendOption) {
        this.mSendOption = sendOption;
    }

    public boolean getEmbedImagesInHtml() {
        return this.mEmbedImagesInHtml;
    }

    public void setEmbedImagesInHtml(boolean embedImagesInHtml) {
        this.mEmbedImagesInHtml = embedImagesInHtml;
    }

    /**
     * @see com.sun.jbi.binding.email.protocol.wsdl.EmailOperationInput#getAttachments()
     */
    public SMTPAttachment[] getAttachments() {
        return this.mAttachments;
    }
    
    /**
     * @param attachments
     */
    public void setAttachments(SMTPAttachment[] attachments) {
        this.mAttachments = attachments;
    }
    
    /**
     * @see com.sun.jbi.binding.email.protocol.wsdl.EmailOperationInput#toString()
     */
    public String toString() {
        final StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\n EncodingStyle = " + mEncodingStyle);
        strBuf.append("\n Charset = " + mCharset);
        strBuf.append("\n SmtpUseType = " + smtpUseType);

        return strBuf.toString();
    }

}
