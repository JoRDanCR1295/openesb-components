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
 * @(#)CorrelatingSAInComingEventKeyImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.util.List;

import com.sun.bpel.model.util.Utility;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;


/**
 * Start activity incoming event key implementation. This key represents the  correlating messages
 * and createOrCorrelate (flow based) incoming messages.
 *
 * @author Sun Microsystems
 *
 * @see SAInComingEventKeyImpl
 */
public class CorrelatingSAInComingEventKeyImpl extends InComingEventKeyImpl {
    private static long nextId = 0;
    
    /** hash code */
    int hashCode = Integer.MIN_VALUE;

    /**
     * DOCUMENT ME!
     */
    List mCorrIDs = null;
    
    private long id;
    
    /**
     * Creates a new SAInComingEventKeyImpl object.
     *
     * @param model incoming event model
     * @param type event type
     * @param corrIDs callframe ID
     */
    public CorrelatingSAInComingEventKeyImpl(InComingEventModel model, int type, List corrIDs) {
        super(model, type);
        hashCode = model.getStartElement().hashCode();
        mCorrIDs = corrIDs;
        
        this.id = nextId++;
    }

    /**
     * Creates a new SAInComingEventKeyImpl object.
     *
     * @param key incoming event key
     * @param corrIDs callframe ID
     */
    public CorrelatingSAInComingEventKeyImpl(InComingEventKeyImpl key, List corrIDs) {
        this(key.getEventModel(), key.getType(), corrIDs);
    }
    
    public Long getId() {
        return new Long(id);
    }

    /**
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj) {
        if ((obj == null) || !(obj instanceof CorrelatingSAInComingEventKeyImpl)) {
            return false;
        }

        if (obj == this) {
            return true;
        }

        CorrelatingSAInComingEventKeyImpl key = (CorrelatingSAInComingEventKeyImpl) obj;
        boolean retVal;

        retVal = Utility.areEqual(mCorrIDs, key.mCorrIDs);

        return retVal;
    }

    /**
     * @see java.lang.Object#hashCode()
     */
    public int hashCode() {
        return hashCode;
    }
    
    public List getCorrIds() {
        return mCorrIDs;
    }
}
