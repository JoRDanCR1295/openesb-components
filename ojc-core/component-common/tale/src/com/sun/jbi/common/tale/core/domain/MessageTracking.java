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
 * @(#)MessageTracking.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.domain;

/**
 * Domain object used for storing message tracking ids in the
 * MSG_TRACKING table
 * @author Sun Microsystems
 */
public class MessageTracking {    
    private String mTrackID;
    
    public MessageTracking() {
    }
 
    public MessageTracking(String trackId) {
        mTrackID = trackId;
    }
    
    public static MessageTracking copy(MessageTracking info) {
        MessageTracking copy = null;
        if (info != null) {
            copy = new MessageTracking();            
            copy.setTrackID(info.getTrackID());
        }
        return copy;
    }

    /**
     * @return the mTrackID
     */
    public String getTrackID() {
        return mTrackID;
    }

    /**
     * @param trackID the mTrackID to set
     */
    public void setTrackID(String trackID) {
        mTrackID = trackID;
    }   

}
