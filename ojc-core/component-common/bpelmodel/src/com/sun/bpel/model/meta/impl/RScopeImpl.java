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
 * @(#)RScopeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import com.sun.bpel.model.impl.ScopeImpl;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RPersistable;
import com.sun.bpel.model.meta.ScopingElement;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;


/**
 * Runtime Scope implementation
 *
 * @author Sun Microsystems
 */
public class RScopeImpl extends ScopeImpl implements RActivity, 
    RActivityHolder, ScopingElement, RPersistable {
    
    private long mUniqueID;
    private RActivity mNextAct = null;

    /**
     * Creates a new instance of RScopeImpl
     *
     * @param bpeldoc runtime BPEL document
     * @param id unique ID
     */
    public RScopeImpl(RBPELDocumentImpl bpeldoc, long id) {
        super(bpeldoc);
        mUniqueID = id;
    }

    /**
     * gtes child activity
     *
     * @return RActivity child activity
     */
    public RActivity getChildActivity() {
        return (RActivity) super.getActivity();
    }

    /**
     * gets next activity
     *
     * @return RActivity next activity
     */
    public RActivity getNextActivity() {
        return mNextAct;
    }

    /**
     * gets unique ID
     *
     * @return long unique ID
     */
    public long getUniqueId() {
        return mUniqueID;
    }

    /**
     * sets child activity
     *
     * @param act child activity
     *
     * @throws UnsupportedOperationException DOCUMENT ME!
     */
    public void setChildActivity(RActivity act) {
        throw new UnsupportedOperationException();
    }

    /**
     * sets next activity
     *
     * @param act next activity
     */
    public void setNextActivity(RActivity act) {
        mNextAct = act;
    }

    /**
     * @see com.sun.bpel.model.meta.ScopingElement#getScopeId()
     */
    public long getScopeId() {
        return mUniqueID;
    }
}
