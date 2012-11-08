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
 * @(#)RReplyImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import javax.xml.namespace.QName;

import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.impl.ReplyImpl;
import com.sun.bpel.model.meta.CorrelationDefnWrapper;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RReply;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;


/**
 * Runtime Reply implementation
 *
 * @author Sun Microsystems
 */
public class RReplyImpl extends ReplyImpl implements RReply {
    private long mUniqueID;
    private RActivity mNextAct;
    private RVariable mVariable;
    private CorrelationDefnWrapper mCorrDefnWrapper = null;

    /**
     * Creates a new instance of RReplyImpl
     *
     * @param bpeldoc runtime BPEL document
     * @param id unique ID
     */
    public RReplyImpl(RBPELDocumentImpl bpeldoc, long id) {
        super(bpeldoc);
        mUniqueID = id;
    }

    /**
     * @see com.sun.bpel.model.meta.RReply#getNextActivity()
     */
    public RActivity getNextActivity() {
        return mNextAct;
    }

    /**
     * @see com.sun.bpel.model.meta.RReply#getUniqueId()
     */
    public long getUniqueId() {
        return mUniqueID;
    }

    /**
     * @see RReply#setNextActivity(com.sun.bpel.model.meta.RActivity)
     */
    public void setNextActivity(RActivity act) {
        mNextAct = act;
    }

    /**
     * @see com.sun.bpel.model.meta.RReply#getRPortType()
     */
    public QName getRPortType() {
    	return super.getPortType();
    }

    /**
     * @see com.sun.bpel.model.meta.RReply#getRPartner()
     */
    public PartnerLink getRPartner() {
        return super.getBPELPartnerLink();
    }

    /**
     * @see com.sun.bpel.model.meta.RReply#getRVariable()
     */
    public RVariable getRVariable() {
        return mVariable;
    }

    /**
     * @see RReply#setRVariable(com.sun.bpel.model.meta.RVariable)
     */
    public void setRVariable(RVariable var) {
        mVariable = var;
    }

    /**
     * @see com.sun.bpel.model.meta.RReply#getCorrelationDefnWrapper()
     */
    public CorrelationDefnWrapper getCorrelationDefnWrapper() {
        return mCorrDefnWrapper;
    }

    /**
     * @see RReply#setCorrelationDefnWrapper(com.sun.bpel.model.meta.CorrelationDefnWrapper)
     */
    public void setCorrelationDefnWrapper(CorrelationDefnWrapper corrDefn) {
        mCorrDefnWrapper = corrDefn;
    }
}
