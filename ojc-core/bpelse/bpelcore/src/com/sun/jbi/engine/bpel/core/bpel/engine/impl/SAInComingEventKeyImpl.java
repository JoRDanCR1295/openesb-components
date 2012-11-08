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
 * @(#)SAInComingEventKeyImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import com.sun.bpel.model.util.Utility;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;


/**
 * Start activity incoming event key implementation. This key represents incoming  messages that
 * create the bpel instance. Events that correspond to the following cases.
 * 
 * Rec - CreateInstance="yes"
 * (Or) 
 * 
 * Rec - CreateInstance="yes", Correlation="yes"
 * (Or)
 * 
 * Flow
 *  Rec - CreateInstance="yes", Correlation="yes"
 * </Flow>
 * (or)
 *  
 * Flow
 *  Rec - CreateInstance="yes" 
 * </Flow>
 *
 * @author Sun Microsystems
 *
 * @see CorrelatingSAInComingEventKeyImpl
 */
public class SAInComingEventKeyImpl extends InComingEventKeyImpl {
    /** hash code */
    int hashCode = Integer.MIN_VALUE;
    private String mBPID = ""; //$NON-NLS-1$

    /**
     * Creates a new SAInComingEventKeyImpl object.
     *
     * @param model incoming event model
     * @param type event type
     * @param corrIDs DOCUMENT ME!
     * @param cfId callframe ID
     */
    public SAInComingEventKeyImpl(InComingEventModel model, int type, String bpId) {
        super(model, type);
        hashCode = model.getStartElement().hashCode();
        mBPID = bpId;
    }

    /**
     * Creates a new SAInComingEventKeyImpl object.
     *
     * @param key incoming event key
     * @param corrIDs DOCUMENT ME!
     * @param cfId callframe ID
     */
    public SAInComingEventKeyImpl(InComingEventKeyImpl key, String bpId) {
        this(key.getEventModel(), key.getType(), bpId);
    }

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        if ((obj == null) || !(obj instanceof SAInComingEventKeyImpl)) {
            return false;
        }
        if (obj == this) {
            return true;
        }
        SAInComingEventKeyImpl key = (SAInComingEventKeyImpl) obj;
        boolean retVal;
        retVal = Utility.areEqual(mBPID, key.mBPID);

        return retVal;
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return hashCode;
    }
}
