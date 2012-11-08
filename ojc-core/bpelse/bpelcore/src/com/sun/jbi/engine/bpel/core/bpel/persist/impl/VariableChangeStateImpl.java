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
 * @(#)$Id: VariableChangeStateImpl.java,v 1.7 2008/04/18 07:51:29 mpottlapelli Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist.impl;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;

/**
 * The State class used to persist changed variable by BPELSEManagementMBean
 * @author Sun Microsystems
 *
 */
public class VariableChangeStateImpl implements State {
	
    private String mEngId;
    //private String mBPELId;
    private QName mBPELId;
    private String mId;
    private List<StateVariableWrapper> mChangedVars = new ArrayList<StateVariableWrapper> ();
    
    /**
     * constructor
     * @param engId engine ID
     * @param bpelId BPEL ID
     * @param id State ID
     */
    public VariableChangeStateImpl(String engId, QName bpelId, String id, RuntimeVariable changedVar) {
        mEngId = engId;
        mBPELId = bpelId;
        mId = id;
        mChangedVars.add(new StateVariableWrapper(getId(), changedVar));
    }

	public void clearPC() {
		// TODO Auto-generated method stub

	}

	public QName getBPELId() {
		// TODO Auto-generated method stub
		return mBPELId;
	}

	public Long getBranchInvokeCounter() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException(); 
	}

	public CRMPState getCRMPState() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException(); 
	}

	public Set getCorrelations() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException(); 
	}

	public String getEngineId() {
		// TODO Auto-generated method stub
		return mEngId;
	}

	public WSMessage getFaultData() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException(); 
	}

	public QName getFaultName() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException(); 
	}

	public String getFaultScopeId() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException(); 
	}

	public Collection getForEach() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException(); 
	}

	public String getId() {
		// TODO Auto-generated method stub
		return mId;
	}

	public Long[] getInvalidPCs() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException(); 
	}

	public Long[] getPC() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException(); 
	}

	public Long getPickCompositeActId() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException(); 
	}

	public Timestamp getTimerValue() {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException(); 
	}

	public State.Mode getUpdateMode() {
		// TODO Auto-generated method stub
		return State.Mode.UPDATE;
	}

	public Collection<StateVariableWrapper> getVariables(boolean for_update) {
		// TODO Auto-generated method stub
		return mChangedVars;
	}

	public boolean hasScopeFaulted(String scopeId) {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException(); 
	}

	public void markInserted() {
		// TODO Auto-generated method stub

	}

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.State#getPartnerLinks(boolean)
     */
    public Collection<StatePartnerLinkWrapper> getPartnerLinks(boolean for_update) {
        throw new UnsupportedOperationException();
    }

	public Collection<FaultHandlingContext> getScopeStates(boolean for_update) {
		// TODO: Is the scope state change is required for monitoring ??
		return null;
	}

}
