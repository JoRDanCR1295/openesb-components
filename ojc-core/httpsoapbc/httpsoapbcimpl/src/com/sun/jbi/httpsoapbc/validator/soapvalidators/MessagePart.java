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
 * @(#)HttpSoapHeaderFaultValidator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validator.soapvalidators;

import javax.xml.namespace.QName;

public class MessagePart {
    
    private final String mName;
    private final String mPartName;
    private final int mHashCode;

    public MessagePart(QName messageName, String partName) {
        if (partName == null) {
            throw new NullPointerException("null partName");
        }
        if ("".equals(partName)) {
            throw new IllegalArgumentException("blank partName");
        }
        
        StringBuffer nameBuffer = new StringBuffer();
        if (messageName != null) {
            nameBuffer.append(messageName.toString()).append(':');
        }
        nameBuffer.append(partName);
        mName = nameBuffer.toString();
        mPartName = partName;
        mHashCode = mName.hashCode();
    }
    
    public String getPartName() {
        return mPartName;
    }

    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }
        if (other == null || !(other instanceof MessagePart)) {
            return false;
        }
        
        MessagePart that = (MessagePart) other;
        if (!that.mName.equals(mName)) {
            return false;
        }
        
        return true;
    }
    
    public int hashCode() {
        return mHashCode;
    }
    
    public String toString() {
        return mName;
    }
}
