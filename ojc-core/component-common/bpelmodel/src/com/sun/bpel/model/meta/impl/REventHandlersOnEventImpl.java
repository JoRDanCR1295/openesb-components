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
 * @(#)REventHandlersOnEventImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.meta.impl;

import javax.xml.namespace.QName;

import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.impl.EventHandlersOnEventImpl;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RStartElementCorrelationDefnWrapper;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.ScopingElement;
import com.sun.bpel.model.parser.impl.RBPELDocumentImpl;


/**
 * Runtime Receive implementation
 *
 * @author Sun Microsystems
 */
public class REventHandlersOnEventImpl extends EventHandlersOnEventImpl implements RStartElement, RActivity, RActivityHolder, ScopingElement {
    private long mUniqueID;
    private RVariable mVariable;
    private RStartElementCorrelationDefnWrapper mCorrDefnWrapper = null;
    private int mStartType = -1;

    /**
     * Creates a new instance of RReceiveImpl
     *
     * @param bpeldoc runtime BPEL document
     * @param id unique ID
     */
    public REventHandlersOnEventImpl(RBPELDocumentImpl bpeldoc, long id) {
        super(bpeldoc);
        mUniqueID = id;
    }

    /**
     * @see com.sun.bpel.model.meta.RActivity#getNextActivity()
     */
    public RActivity getNextActivity() {
        return null;
    }

    /**
     * @see com.sun.bpel.model.meta.RStartElement#getRVariable()
     */
    public RVariable getRVariable() {
        return mVariable;
    }

    /**
     * sets variable
     *
     * @param var variable
     */
    public void setRVariable(RVariable var) {
        mVariable = var;
    }

    /**
     * @see com.sun.bpel.model.meta.RStartElement#getRCreateInstance()
     */
    public boolean getRCreateInstance() {
        return false;
    }

    /**
     * @see com.sun.bpel.model.meta.RMessagingElement#getRPartner()
     */
    public PartnerLink getRPartner() {
        return super.getBPELPartnerLink();
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
    }

    /**
     * gets PortType
     *
     * @return QName PortType name
     */
    public QName getRPortType() {
    	return super.getPortType();
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

    public RActivity getChildActivity() {
        // TODO Auto-generated method stub
        return (RActivity) getActivity();
    }

    public void setChildActivity(RActivity act) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException();  
    }

    /*
     * @see com.sun.bpel.model.meta.ScopingElement#getScopeId()
     */
    public long getScopeId() {
        // TODO Auto-generated method stub
        return mUniqueID;
    }
}
