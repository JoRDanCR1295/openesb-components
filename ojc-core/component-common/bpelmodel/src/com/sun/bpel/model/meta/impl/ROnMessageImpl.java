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
 * @(#)ROnMessageImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import javax.xml.namespace.QName;

import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.impl.OnMessageImpl;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.ROnMessage;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RStartElementCorrelationDefnWrapper;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.RVariableElement;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;
import com.sun.bpel.model.common.MessageManager;



/**
 * Runtime OnMessage implementation
 *
 * @author Sun Microsystems
 */
public class ROnMessageImpl extends OnMessageImpl implements ROnMessage {

    /** MessageManager for localized strings. */    
    private static MessageManager MESSAGES = MessageManager.getManager(ROnMessageImpl.class);
    
    private boolean mCreateInstance;
    private RVariable mVariable;
    private long mUniqueID;
    private RStartElementCorrelationDefnWrapper mCorrDefnWrapper = null;
    private int mStartType = -1;

    /**
     * Creates a new ROnMessageImpl object.
     *
     * @param bpeldoc runtime BPEL document
     * @param id unique IDf
     */
    public ROnMessageImpl(RBPELDocumentImpl bpeldoc, long id) {
        super(bpeldoc);
        mUniqueID = id;
    }

    /**
     * @see com.sun.bpel.model.meta.ROnMessage#setCreateInstance(boolean)
     */
    public void setCreateInstance(boolean flag) {
        mCreateInstance = flag;
    }

    /**
     * @see com.sun.bpel.model.meta.RStartElement#getRCreateInstance()
     */
    public boolean getRCreateInstance() {
        return mCreateInstance;
    }

    /**
     * @see com.sun.bpel.model.meta.RVariableElement#getRVariable()
     */
    public RVariable getRVariable() {
        return mVariable;
    }

    /**
     * @see RVariableElement#setRVariable(com.sun.bpel.model.meta.RVariable)
     */
    public void setRVariable(RVariable var) {
        mVariable = var;
    }

    /**
     * @see com.sun.bpel.model.meta.RMessagingElement#getRPartner()
     */
    public PartnerLink getRPartner() {
        return super.getBPELPartnerLink();
    }

    /**
     * @see com.sun.bpel.model.meta.RMessagingElement#getRPortType()
     */
    public QName getRPortType() {
    	return super.getPortType();
    }

    /**
     * @see RActivityHolder#setChildActivity(com.sun.bpel.model.meta.RActivity)
     */
    public void setChildActivity(RActivity act) {
        throw new UnsupportedOperationException(MESSAGES.getString("ROnMessageImpl_NOT_SUPPORTED")); //$NON-NLS-1$
    }

    /**
     * @see com.sun.bpel.model.meta.RActivity#getChildActivity()
     */
    public RActivity getChildActivity() {
        return (RActivity) getActivity();
    }

    /**
     * @see com.sun.bpel.model.meta.Common#getUniqueId()
     */
    public long getUniqueId() {
        return mUniqueID;
    }

    /**
     * @see com.sun.bpel.model.meta.RStartElement#getCorrelationDefnWrapper()
     */
    public RStartElementCorrelationDefnWrapper getCorrelationDefnWrapper() {
        return mCorrDefnWrapper;
    }

    /**
     * @see RStartElement#setCorrelationDefnWrapper(RStartElementCorrelationDefnWrapper)
     */
    public void setCorrelationDefnWrapper(RStartElementCorrelationDefnWrapper corrDefn) {
        mCorrDefnWrapper = corrDefn;
    }

    /**
     * @see com.sun.bpel.model.meta.RStartElement#getStartType()
     */
    public int getStartType() {
        return mStartType;
    }

    /**
     * @see com.sun.bpel.model.meta.RStartElement#setStartType(int)
     */
    public void setStartType(int startType) {
        mStartType = startType;
    }
}
