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
 * @(#)RTerminateImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import com.sun.bpel.model.impl.TerminateImpl;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;


/**
 * Runtime Terminate implementation
 *
 * @author Sun Microsystems
 */
public class RTerminateImpl extends TerminateImpl implements RActivity {
    private long mUniqueID;
    private RActivity mNextAct;

    /**
     * Creates a new instance of RTerminateImpl
     *
     * @param bpeldoc runtime BPEL document
     * @param id unique ID
     */
    public RTerminateImpl(RBPELDocumentImpl bpeldoc, long id) {
        super(bpeldoc);
        mUniqueID = id;
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
     * sets next activity
     *
     * @param act next activity
     */
    public void setNextActivity(RActivity act) {
        mNextAct = act;
    }
}
