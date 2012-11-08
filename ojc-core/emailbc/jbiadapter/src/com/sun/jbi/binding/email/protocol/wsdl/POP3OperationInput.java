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
 * @(#)POP3OperationInput.java 
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
public class POP3OperationInput extends EmailOperationInput {
    private static final long serialVersionUID = 1L;
    public static final String ATTR_POP3_SAVE_ATTACHMENTS_TO_DIR = "saveAttachmentsToDir";
    public static final String ELEM_POP3_INPUT = "POP3input";
    public static final QName QNAME_POP3_OPERATION_INPUT =
        new QName(EmailBCConstants.NS_URI_EMAILBC, ELEM_POP3_INPUT);
    
    private POP3Attachment[] mAttachments = new POP3Attachment[0];
    private String saveAttachmentsToDir = null;

    /**
     * @see com.sun.jbi.binding.email.protocol.wsdl.EmailOperationInput#getAttachments()
     */
    public POP3Attachment[] getAttachments() {
        return this.mAttachments;
    }
    
    /**
     * @param attachments
     */
    public void setAttachments(POP3Attachment[] attachments) {
        this.mAttachments = attachments;
    }
    
    /**
     * @see com.sun.jbi.binding.email.protocol.wsdl.EmailOperationInput#getElementType()
     */
    @Override
    public QName getElementType() {
        return QNAME_POP3_OPERATION_INPUT;
    }
    
    public String getSaveAttachmentsToDir() {
        return saveAttachmentsToDir;
    }

    public void setSaveAttachmentsToDir(String saveAttachmentsToDir) {
        this.saveAttachmentsToDir = saveAttachmentsToDir;
    }

}
