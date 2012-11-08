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
 * @(#)SMTPAttachment.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.extensions;

import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author aegloff
 */
public class SMTPAttachment implements ExtensibilityElement, Serializable {
    
    // Element name
    public static final String ELEM_ATTACHMENT = "attachment";

    // Attribute names
    public static final String ATTR_CONTENT_TYPE = "contentType";
    public static final String ATTR_NAME = "name";
    public static final String ATTR_CONTENT = "content";

    // Qualified element names
    public static final QName QNAME_ATTACHMENT =
        new QName(SMTPConstants.NS_URI_SMTP, SMTPAttachment.ELEM_ATTACHMENT);

    private static final long serialVersionUID = 1L;
    
    private Boolean mFieldRequired = null;
    private String mContentType = null;
    private String mName = null;
    private String mContent = null;

    /** 
     * Get the extensibility element type
     * @return the extensibility element's type 
     */
    public QName getElementType() {
        return SMTPAttachment.QNAME_ATTACHMENT;
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

    public String getContentType() {
        return mContentType;
    }

    public void setContentType(final String contentType) {
        mContentType = contentType;
    }

    public String getName() {
        return mName;
    }

    public void setName(final String name) {
        mName = name;
    }

    public String getContent() {
        return mContent;
    }

    public void setContent(final String content) {
        mContent = content;
    }

    @Override
	public String toString() {
        final StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nSMTP operation (" + SMTPAttachment.QNAME_ATTACHMENT + "):");
        strBuf.append("\nRequired=" + mFieldRequired);

        return strBuf.toString();
    }
}
