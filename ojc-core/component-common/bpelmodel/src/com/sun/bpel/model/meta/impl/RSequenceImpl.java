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
 * @(#)RSequenceImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import com.sun.bpel.model.impl.SequenceImpl;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;


/**
 * Runtime Sequence implementation
 *
 * @author Sun Microsystems
 */
public class RSequenceImpl extends SequenceImpl implements RActivity, RActivityHolder {
    private RActivity mChildAct = null;
    private long mUniqueID;
    private RActivity mNextAct;

    /**
     * Creates a new RSequenceImpl object.
     *
     * @param bpeldoc runtime BPEL document
     * @param id unique ID
     */
    public RSequenceImpl(RBPELDocumentImpl bpeldoc, long id) {
        super(bpeldoc);
        mUniqueID = id;
    }

    /**
     * @see RActivityHolder#setChildActivity(com.sun.bpel.model.meta.RActivity)
     */
    public void setChildActivity(RActivity act) {
        mChildAct = act;
    }

    /**
     * @see com.sun.bpel.model.meta.RActivity#getChildActivity()
     */
    public RActivity getChildActivity() {
        return mChildAct;
    }

    /**
     * @see com.sun.bpel.model.meta.RActivity#getNextActivity()
     */
    public RActivity getNextActivity() {
        return mNextAct;
    }

    /**
     * @see com.sun.bpel.model.meta.RActivity#getUniqueId()
     */
    public long getUniqueId() {
        return mUniqueID;
    }

    /**
     * @see RActivity#setNextActivity(com.sun.bpel.model.meta.RActivity)
     */
    public void setNextActivity(RActivity act) {
        mNextAct = act;
    }
}
