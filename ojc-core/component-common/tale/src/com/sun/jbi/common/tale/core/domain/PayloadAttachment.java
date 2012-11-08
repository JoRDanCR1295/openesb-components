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
 * @(#)PayloadAttachment.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain;

/**
 * Domain Objects used for storing payload attachments in the
 * PAYLOAD_ATTACH table
 * @author Sun Microsystems
 */
public class PayloadAttachment {
    private String mSequenceID;
    private Object mAttachment;
    
    public static PayloadAttachment copy(PayloadAttachment info) {
        PayloadAttachment copy = null;
        if (info != null) {
            copy = new PayloadAttachment();
            copy.setSequenceID(info.getSequenceID());
            copy.setAttachment(info.getAttachment());
        }
        return copy;
    }

    /**
     * @return the mAttachment
     */
    public Object getAttachment() {
        return mAttachment;
    }

    /**
     * @param attachment the mAttachment to set
     */
    public void setAttachment(Object attachment) {
        mAttachment = attachment;
    }

    /**
     * @return the mSequenceID
     */
    public String getSequenceID() {
        return mSequenceID;
    }

    /**
     * @param sequenceID the mSequenceID to set
     */
    public void setSequenceID(String sequenceID) {
        mSequenceID = sequenceID;
    }
    
}
