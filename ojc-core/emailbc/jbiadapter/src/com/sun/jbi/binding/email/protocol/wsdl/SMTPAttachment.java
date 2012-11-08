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
 * @(#)EmailAttachment.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.binding.email.protocol.wsdl;

import javax.mail.Part;
import javax.xml.namespace.QName;

import com.sun.jbi.binding.email.protocol.EmailBCConstants;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public class SMTPAttachment extends EmailAttachment {

    private static final long serialVersionUID = 1L;
    public static final String ELEM_SMTP_ATTACHMENT = "SMTPattachment";
    public static final QName QNAME_SMTP_ATTACHMENT =
            new QName(EmailBCConstants.NS_URI_EMAILBC, ELEM_SMTP_ATTACHMENT);
    public static final String ATTR_SMTP_ATTACHMENT_CONTENT_PART = "attachmentContentPart";
    public static final String ATTR_SMTP_ATTACHMENT_FILE_NAME_PART = "attachmentFileNamePart";
    public static final String ATTR_SMTP_ATTACHMENT_READ_FROM_FILE = "readFromFile";
    public static final String ATTR_SMTP_ATTACHMENT_CONTENT_TYPE = "contentType";
    public static final String ATTR_SMTP_ATTACHMENT_DISPOSITION = "disposition"; // INLINE | ATTACHMENT
    public static final String ATTR_SMTP_ATTACHMENT_TRANSFER_ENCODING = "transferEncoding";
    private String attachmentContentPart = null;
    private String attachmentFileNamePart = null;
    private String readFromFile = null;
    private String contentType = null;
    private String disposition = Part.ATTACHMENT; // or Part.INLINE
    private String transferEncoding = null;

    /**
     * @see javax.wsdl.extensions.ExtensibilityElement#getElementType()
     */
    public QName getElementType() {
        return QNAME_SMTP_ATTACHMENT;
    }

    public String getContentType() {
        return this.contentType;
    }

    public void setContentType(String contentType) {
        this.contentType = contentType;
    }

    public String getDisposition() {
        return this.disposition;
    }

    public void setDisposition(String disposition) {
        this.disposition = disposition;
    }

    public String getTransferEncoding() {
        return this.transferEncoding;
    }

    public void setTransferEncoding(String transferEncoding) {
        this.transferEncoding = transferEncoding;
    }

    public String getAttachmentContentPart() {
        return this.attachmentContentPart;
    }

    public void setAttachmentContentPart(String partName) {
        this.attachmentContentPart = partName;
    }

    public String getReadFromFile() {
        return this.readFromFile;
    }

    public void setReadFromFile(String fileName) {
        this.readFromFile = fileName;
    }

    /**
     * @see com.sun.jbi.binding.email.protocol.wsdl.EmailAttachment#toString()
     */
    public String toString() {
        final StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\n ContentType = " + contentType);
        strBuf.append("\n Disposition = " + disposition);
        strBuf.append("\n TransferEncoding = " + transferEncoding);
        strBuf.append("\n AttachmentContentPart = " + attachmentContentPart);
        strBuf.append("\n AttachmentFileNamePart = " + attachmentFileNamePart);
        strBuf.append("\n ReadFromFile = " + readFromFile);
        return strBuf.toString();
    }

    /**
     * @return the attachmentFileNamePart
     */
    public String getAttachmentFileNamePart() {
        return attachmentFileNamePart;
    }

    /**
     * @param attachmentFileNamePart the attachmentFileNamePart to set
     */
    public void setAttachmentFileNamePart(String attachmentFileNamePart) {
        this.attachmentFileNamePart = attachmentFileNamePart;
    }
}
