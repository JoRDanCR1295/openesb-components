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
 * @(#)CatchUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import com.sun.bpel.model.Catch;
import com.sun.bpel.model.SingleActivityHolder;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.meta.ScopingElement;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;

/**
 * Catch activity unit implementation
 * 
 * @author Sun Microsystems
 */
public class CatchUnitImpl extends CatchAllUnitImpl {

	// private RuntimeVariable mRuntimeVariable;
	private Catch mCatch;

	//private long mIterationCount;
	private String mContextScopeId;

	/**
	 * Creates a new CatchUnitImpl object.
	 * 
	 * @param catchModel
	 *            catch
	 * @param fault
	 *            TODO
	 */
	public CatchUnitImpl(Context context, Catch catchModel, Fault fault) {
    	super(context, (SingleActivityHolder)catchModel, fault);
    	mCatch = catchModel;
        mStaticModel = catchModel;
        Long catchScopeId = ((ScopingElement)mCatch).getScopeId();
        //mIterationCount = mContext.getEnclosedScopesIterCount(catchScopeId);
        // the catch scope id need not have an iteration as the last catch 
        //of the associated scope invoked would be the one that does the 
        // fault handling and hence the fault variable if exists would not
        // need to be tied to an iteration of the fault handlers catch.
        mContextScopeId = new Long(catchScopeId).toString();
        
        if (fault.getData() != null && mCatch.getBPELFaultVariable() != null) {
            RVariable variable = (RVariable) mCatch.getBPELFaultVariable();
            mRuntimeVariable = new RuntimeVariableImpl(variable, context.getProcessInstance(), mContextScopeId);
            WSMessage faultMessage = fault.getData();
            mRuntimeVariable.setWSMessage(faultMessage);
            faultMessage.addInternalReference(variable);
        }
    }

	public RuntimeVariable getRuntimeVariable(RVariable variable) {
		if (mCatch.getBPELFaultVariable() != null
				&& mCatch.getBPELFaultVariable().equals(variable)) {
			return mRuntimeVariable;
		} else {
			return mContext.getRuntimeVariable(variable);
		}
	}
	
    public RuntimeVariable createRuntimeVariable(RVariable variable) {
		if (mCatch.getBPELFaultVariable() != null
				&& mCatch.getBPELFaultVariable().equals(variable)) {
			return mRuntimeVariable;
		} else {
			return mContext.createRuntimeVariable(variable);
		}
    }

    public String getContextScopeId() {
    	return mContextScopeId;
    }
	
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#setRuntimeVariable(com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable)
	 */
	public void setRuntimeVariable(RuntimeVariable runtimeVariable) {

		if (mCatch.getBPELFaultVariable() != null
				&& mCatch.getBPELFaultVariable().equals(
						runtimeVariable.getVariableDef())) {
			mRuntimeVariable = runtimeVariable;
		} else {
			mContext.setRuntimeVariable(runtimeVariable);
		}
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#getRuntimeVariables()
	 */
	public Map getRuntimeVariables() {

		Map visibleRuntimeVariables = mContext.getRuntimeVariables();

		// Replace the variable if the catch defines a variable with same name
		if (mCatch.getBPELFaultVariable() != null) {
			boolean found = false;
			Iterator iter = visibleRuntimeVariables.keySet().iterator();
			for (; iter.hasNext();) {
				RVariable variable = (RVariable) iter.next();
				if (variable.getName().equals(
						mCatch.getBPELFaultVariable().getName())) {
					visibleRuntimeVariables.remove(variable);
					visibleRuntimeVariables.put(mCatch.getBPELFaultVariable(),
							this.mRuntimeVariable);
					found = true;
					break;
				}
			}
			if (!found) {
				visibleRuntimeVariables.put(mCatch.getBPELFaultVariable(),
						this.mRuntimeVariable);
			}
		}
		return visibleRuntimeVariables;
	}
	
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#initUponRecovery(Collection, Collection)
     */
    public void initUponRecovery(Collection<RuntimeVariable> runtimeVariables, Collection<RuntimePartnerLink> runtimePLinks) {
    	mRuntimeVariable = (RuntimeVariable)runtimeVariables.iterator().next();
        // There is no relation with partnerLinks and hence ignore the second 
        // method parameter
    }

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getContext()
	 */
	public Context getContext() {
		return this;
	}
}
