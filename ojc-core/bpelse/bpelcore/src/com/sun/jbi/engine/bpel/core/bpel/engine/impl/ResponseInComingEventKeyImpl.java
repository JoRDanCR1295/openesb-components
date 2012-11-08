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
 * @(#)ResponseInComingEventKeyImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import com.sun.bpel.model.meta.RBPELProcess;


/**
 * Response incoming event key
 *
 * @author Sun Microsystems
 */
public class ResponseInComingEventKeyImpl extends InComingEventKeyImpl {
    private int hashCode = Integer.MIN_VALUE;
    private int mType = Integer.MIN_VALUE;
    private Object meId;
    /** runtime bpel process */
    RBPELProcess mProcess;

    /**
     * Creates a new ResponseInComingEventKeyImpl object.
     *
     * @param process runtime bpel process
     * @param type event type
     * @param meId message exchange ID
     */
    public ResponseInComingEventKeyImpl(RBPELProcess process, int type,
        Object meId) {
        this.meId = meId;
        hashCode = meId.hashCode();
        mType = type;
        mProcess = process;
    }

    /**
     * gets event type
     *
     * @return int event type
     */
    public int getType() {
        return mType;
    }

    /**
     * gets runtime bpel process
     *
     * @return RBPELProcess runtime bpel process
     */
    public RBPELProcess getBPELProcess() {
        // TODO REvisit this Key class. see parent class API
        return mProcess;
    }

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        if (!(obj instanceof ResponseInComingEventKeyImpl)) {
            return false;
        }

        if (obj == this) {
            return true;
        }
        ResponseInComingEventKeyImpl key = (ResponseInComingEventKeyImpl)obj;
        if(meId.equals(key.meId)){
            return true;
        }
        return false;
    }
    
    public Object getMsgExId() {
        return meId;
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return hashCode;
    }
    public String toString() {
        return "MessageExchangeId: " + meId + "\n Event Type" + mType;
    }
}
