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
 * @(#)CatchAllUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import javax.xml.namespace.QName;

import com.sun.bpel.model.BPELElement;
import com.sun.bpel.model.Catch;
import com.sun.bpel.model.CatchAll;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.SingleActivityHolder;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RReply;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityContainerUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;


/**
 * CatchAll activity unit implementation
 *
 * @author Sun Microsystems
 */
public class CatchAllUnitImpl implements ActivityContainerUnit, Context  {
    
    // the runtimevariable is moved to CatchAllUnitImpl as we need this for 
    // dereferencing the variable from the ws message, once this activity completes
    protected RuntimeVariable mRuntimeVariable;
    protected BPELElement mStaticModel;
    protected boolean mIsProcessLevelFaultHandling;
    protected Context mContext;
    protected SingleActivityHolder mActHolder;
    protected Fault mFault; 
    
    /**
     * Default constructor
     */
    public CatchAllUnitImpl(){
    	//Empty
    }
    /**
     * Creates a new CatchAllUnitImpl object.
     * @param catchAll catchall
     * @param fault TODO
     * @param scopeUnit scope activity unit
     */
    public CatchAllUnitImpl(Context context, CatchAll catchAll, Fault fault) {
    	this(context, (SingleActivityHolder) catchAll, fault);
        mStaticModel = catchAll;
    }
    
    protected CatchAllUnitImpl(Context context, SingleActivityHolder singleActHolder, Fault fault) {
        mContext = context;
        mActHolder = singleActHolder;
        mFault = fault;
        mIsProcessLevelFaultHandling = (mContext.getParentContext() == null);
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doAction(ICallFrame,
     *      BusinessProcessInstanceThread, RequiredObjects)
     */
    public boolean doAction(
        ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {

        long branchId = 0L;

        if (mIsProcessLevelFaultHandling) {
            branchId = RBPELProcess.DEFAULT_PROCESS_BRANCH_ID.longValue();
        } else {
            branchId = ((ActivityUnit) mContext).getBranchId();
        }

        ActivityUnit childActUnit = ActivityUnitFactory.getInstance().createActivityUnit(
        		this, (Unit) this, (RActivity) mActHolder.getActivity(), branchId);

        if(CodeReUseHelper.executeChildActivities(frame, bpit, rObjs, childActUnit)) {
            
            if (mStaticModel instanceof Catch) {
                if (mRuntimeVariable != null) {
                    WSMessage message = mRuntimeVariable.getWSMessage();
                    if (message != null) {
                        message.removeInternalReference(mRuntimeVariable.getVariableDef());
                    }
                }
            } 
            return true;
            
        } else {
            return false;
        }
    }
    
    public boolean doResumeAction(ActivityUnit childActUnit, ICallFrame frame,
            BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {
        return ((StructuredActivityUnitImpl) mContext).doResumeAction(null, frame, bpit, rObjs);
    }

	/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getEnclosingUnit()
     */
    public Unit getEnclosingUnit() {
        return (ActivityUnit) mContext;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getEnclosingScopeUnit()
     */
    public ActivityUnit getEnclosingScopeUnit() {
        return (ActivityUnit) mContext;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getContext()
     */
    public Context getContext() {
        return mContext;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityContainerUnit#getStaticModel()
     */
    public BPELElement getStaticModel() {
        return mStaticModel;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getProcessInstance()
     */
    public BPELProcessInstance getProcessInstance() {
        return mContext.getProcessInstance();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#initUponRecovery(Collection, Collection)
     */
    public void initUponRecovery(Collection<RuntimeVariable> runtimeVariables, Collection<RuntimePartnerLink> runtimePLinks) {
        // TODO: ? 
        throw new UnsupportedOperationException();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#initEHUponRecovery(com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers)
     */
    public void initEHUponRecovery(RuntimeEventHandlers rEH) {
        throw new UnsupportedOperationException();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#addRequest(com.sun.bpel.model.meta.RStartElement, com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
     */
    public void addRequest(RStartElement rcv, MessageContainer req) throws BPELRuntimeException {
        mContext.addRequest(rcv, req);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#removeRequest(com.sun.bpel.model.meta.RStartElement, 
     * com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
     */
    public void removeRequest(RStartElement rcv, MessageContainer req) throws BPELRuntimeException {
         mContext.removeRequest(rcv, req);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#addToCRMPUpdateList(String updateValueKey)
     */
    public void addToCRMPUpdateList(String updateValueKey) {
    	mContext.addToCRMPUpdateList(updateValueKey);
	}

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#crmpUpdateListContains(String updateValueKey)
     */
	public boolean crmpUpdateListContains(String updateValueKey) {
		return mContext.crmpUpdateListContains(updateValueKey);
	}
	
	/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#removeRequest(com.sun.bpel.model.meta.RReply)
     */
    public MessageContainer removeRequest(RReply reply) {
        return mContext.removeRequest(reply);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#sendErrorsForPendingRequests(Exception error)
     */
    public void sendErrorsForPendingRequests(Exception error) {
    	throw new UnsupportedOperationException();
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#completePendingInOnlyRequests()
     */
    public void completePendingInOnlyRequests() {
    	throw new UnsupportedOperationException();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#getRuntimeVariable(com.sun.bpel.model.meta.RVariable)
     */
    public RuntimeVariable getRuntimeVariable(RVariable variable) {
        return mContext.getRuntimeVariable(variable);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#getRuntimeVariables()
     */
    public Map getRuntimeVariables() {
        return mContext.getRuntimeVariables();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#setRuntimeVariable(com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable)
     */
    public void setRuntimeVariable(RuntimeVariable runtimeVariable) {
        mContext.setRuntimeVariable(runtimeVariable);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getParentContext()
     */
    public Context getParentContext() {
        return mContext.getParentContext();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#declareDefaultMessageExchange()
     */
    public void declareDefaultMessageExchange() {
        throw new UnsupportedOperationException();        
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#getRuntimePartnerLink(com.sun.bpel.model.PartnerLink)
     */
    public RuntimePartnerLink getRuntimePartnerLink(PartnerLink pLink) {
        return mContext.getRuntimePartnerLink(pLink);      
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#setRuntimePartnerLink(com.sun.bpel.model.PartnerLink, com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink)
     */
    public void setRuntimePartnerLink(PartnerLink pLink, RuntimePartnerLink runtimePLink) {
        mContext.setRuntimePartnerLink(pLink, runtimePLink);      
    }
    
    public Map getRuntimePartnerLinks() {
        return mContext.getRuntimePartnerLinks();
    }

	public FaultHandlingContext getFaultHandlingContext() {
		return mContext.getFaultHandlingContext();
	}
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getStateContext()
     */
    public StateContext getStateContext() {
        return mContext.getStateContext();
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#createRuntimeVariable(com.sun.bpel.model.meta.RVariable)
     */
    public RuntimeVariable createRuntimeVariable(RVariable variable) {
    	return mContext.createRuntimeVariable(variable);
	}
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#createRuntimePartnerLink(PartnerLink)
     */
    public RuntimePartnerLink createRuntimePartnerLink(PartnerLink partnerlink) {
        return mContext.createRuntimePartnerLink(partnerlink);
    }
}
