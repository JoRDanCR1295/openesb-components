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
 * @(#)ExternalizedPart.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain;

/**
 * Domain Object used for storing externalized parts in the MESG_EXTERNALIZE table
 * @author Sun Microsystems
 */
public class ExternalizedPart {
    private String mPartID;
    private String mPartContent;
    
    public static ExternalizedPart copy(ExternalizedPart info) {
        ExternalizedPart copy = null;
        if (info != null) {
            copy = new ExternalizedPart();
            copy.setPartID(info.getPartID());
            copy.setPartContent(info.getPartContent());
        }
        return copy;
    }

    /**
     * @return the mPartContent
     */
    public String getPartContent() {
        return mPartContent;
    }

    /**
     * @param partContent the mPartContent to set
     */
    public void setPartContent(String partContent) {
        mPartContent = partContent;
    }

    /**
     * @return the mPartID
     */
    public String getPartID() {
        return mPartID;
    }

    /**
     * @param partID the mPartID to set
     */
    public void setPartID(String partID) {
        mPartID = partID;
    }
    
}
