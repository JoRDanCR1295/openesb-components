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
 * @(#)BPELProcessInstanceImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EmptyStackException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.EventHandlers;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.Variables;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RReply;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationManagerImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationVal;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.InvalidStatusException;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;
import com.sun.jbi.engine.bpel.core.bpel.management.BPELSEManagement.ActionType;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ScopeOrProcessUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateFactory;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.BPELHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.PropagationContext;
import com.sun.jbi.engine.bpel.core.bpel.util.TxPropagationObject;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * BPEL process instance implementation
 *
 * @author Sun Microsystems
 */
public class BPELProcessInstanceImpl extends StructuredActivityUnitImpl implements 
    BPELProcessInstance, Context, FaultHandlingContext, StateContext {

    private static final Logger LOGGER = Logger.getLogger(BPELProcessInstanceImpl.class.getName());
    
    /** runtime BPELProcessManager */
    BPELProcessManager mProcessManager;

    /** callframe list */
    List mCFs = new ArrayList();
    
    /** The call frame that the process is initiated on.*/
    ICallFrame mInitiatingCallFrame = null;

    /** BPEL process instance ID */
    String mId = null;

    /** BPEL process state */
    MutableState mState = null;
    private PersistenceManager mPerMgr;
    private ClusterManager mClustMgr;
    private MonitorManager mMonitorMgr;
    
    /* The transaction propagation object associated with this instance of the business process. A business process
     * will be associated with a transaction propagation object only if it is marked atomic.
     */
    private TxPropagationObject mTxPropObj;
    
    /* The propagation context associated with this instance of the business process.
     * 
     */
    private PropagationContext mPropContext;
    
    private boolean mTerminated = false;

    private Map<Long, CorrelationVal> mCorrSetIDToVal = new HashMap<Long, CorrelationVal>();
    
    private Map mRuntimeVariables = Collections.synchronizedMap(new HashMap());
    
    private RuntimeEventHandlers mEventHandlersUnit;
    
    private Collection mWaitingEventHandlers = Collections.synchronizedCollection(new ArrayList ());
    
    private boolean mStartActivityDone = false;
    
    //A lock to set/get the 
    private Object mSetGetStartActLock = new Object();    
    
    private Engine mEng;
    
    private IMAScopeBridge mIMAScope;
    
    private Set<IMAScopeBridge> mIMAScopeSet = new HashSet<IMAScopeBridge>();
    
    private Map<PartnerLink, RuntimePartnerLink> mRuntimePartnerLinks = 
        Collections.synchronizedMap(new HashMap<PartnerLink, RuntimePartnerLink>());

    private enum ProcessState {Running, WaitingForEventsToComplete, Faulted, 
    	ExecutingFaultHandlers, FaultedInFH, Terminated, Suspended}

    private ProcessState currentProcessState = ProcessState.Running;
    
    private Set <ICallFrame> ownedCallFrames = Collections.synchronizedSet(new HashSet <ICallFrame>());
    private Set <FaultHandlingContext> enclosedScopes = Collections.synchronizedSet(new HashSet <FaultHandlingContext>());
    private Fault mFaultObj;
    
    private Stack<Context> mCompletedScopes;
    private long lastActivityTime = -1;
    private boolean areVarsPassivated = false;
    
    protected Map<Long, Long>mEnclosedScopesIterationCount = 
    	Collections.synchronizedMap(new HashMap<Long, Long>()); 
    
    private boolean mIsInserted;
    private long mFaultActivityId;
    
    private boolean isExiting;
    private TerminationReason terminationReason;
    
    Object mScalabilityLock = new Object();

    boolean mIsBeingPassivated;
    boolean mIsBeingPersisted;
    
    /**
     * Used by phase 2 scalability solution (instance passivation)
     */
    int mActiveBranchCount = 0;
    
    private List<InactiveCallframeInfo> mInactiveCallFramesList = new ArrayList<InactiveCallframeInfo>();
    
    /**
     * Creates a new BPELProcessInstanceImpl object or recreates one during recovery.
     *
     * @param model runtime bpel process
     */
    public BPELProcessInstanceImpl(BPELProcessManager processManager, Engine eng, String bpInstanceId) {
    	mProcessManager = processManager;
        mEng = eng;
        if (bpInstanceId == null) {
            mId = BPELHelper.getGUID();
            BPELHelper.addUID (mId);
            Variables variables = mProcessManager.getBPELProcess().getVariables();
            if (variables != null) {
                Iterator iterator = variables.getVariables().iterator();
                for(;iterator.hasNext();) {
                    RVariable variable = (RVariable)iterator.next();
                    mRuntimeVariables.put(variable, null);
                }
            }
        } else {
            mId = bpInstanceId;
            BPELHelper.addUID (mId);
        }
        mState = StateFactory.getStateFactory().createState(mEng, processManager.getBPELProcess().getBPELId(), mId);
        // mState can be null if the persistence is not enabled and that is fine.
        mPerMgr = new PersistenceManager(mEng, this);
        mClustMgr = new ClusterManager(mEng);
        mMonitorMgr = new MonitorManager(mEng, this);
        
        mIMAScope = new IMAScopeBridge(this, null); // added by KPS
        mIMAScope.declareDefaultMessageExchange();
        
        mIMAScopeSet.add(mIMAScope);
        
        mCompletedScopes = new Stack<Context>();
        
        if (processManager.isBPAtomic()) {
        	mTxPropObj = new TxPropagationObject(this);
        }
    }

    
    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#getId()
     */
    public String getId() {
        return mId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#getStaticModel()
     */
    public BPELProcessManager getBPELProcessManager() {
    	return mProcessManager;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#addCallFrame(com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
     */
    public void addCallFrame(ICallFrame frame) {
    	incrementActiveBranchCount(1);
        mCFs.add(frame);
        if (mInitiatingCallFrame == null) {
        	// The initiating call frame has not been set yet. Since this is the first call frame to be added to this
        	// process instance, this must be the initiating call frame. Another probably redundant check would be to 
        	// check that the initiating call frame does not have a parent call frame, but is unecessary at this point.
        	// The reason that we need to keep a reference to the Initial callframe is that if the process has an 
        	// event handler on it, we need to create the callframe that the event handlers unit runs on with the 
        	// initiating call frame as its parent. [see method instantiateEventHandler()].
        	mInitiatingCallFrame = frame;
        }
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.StateContext#getState()
     */
    public MutableState getState() {
        return mState;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#checkAndAddCorrelationValues(java.util.List, boolean)
     */
    public void checkAndAddCorrelationValues(List correlationValues) {
        CorrelationVal val = null;

        if ((correlationValues == null) || correlationValues.isEmpty()) {
            return;
        }
        boolean isPersistenceEnabled = false;
        if (getBPELProcessManager().isPersistenceEnabled()) {
            isPersistenceEnabled = true;
        }
        synchronized (mCorrSetIDToVal) {
            for (int i = 0, size = correlationValues.size(); i < size; i++) {
                val = (CorrelationVal) correlationValues.get(i);

                if (mCorrSetIDToVal.get(new Long(val.getSetID())) != null) {
                	throw new StandardException(StandardException.Fault.CorrelationViolation);
                }
            }

            for (int i = 0, size = correlationValues.size(); i < size; i++) {
                val = (CorrelationVal) correlationValues.get(i);
                mCorrSetIDToVal.put(new Long(val.getSetID()), val);
                if (isPersistenceEnabled) {
                    // TODO for now we only support BPELProcess level correlations
                    mState.addCorrelation(val);
                }
            }
        }
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#checkCorrelationValues(java.util.List)
     */
    public void checkCorrelationValues(List correlationValues) {
        CorrelationVal val = null;

        if ((correlationValues == null) || correlationValues.isEmpty()) {
            return;
        }

        synchronized (mCorrSetIDToVal) {
            for (int i = 0, size = correlationValues.size(); i < size; i++) {
                val = (CorrelationVal) correlationValues.get(i);

                if (mCorrSetIDToVal.get(new Long(val.getSetID())) != null) {
                	throw new StandardException(StandardException.Fault.CorrelationViolation);
                }
            }
        }
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#addCorrelationValues(java.util.List)
     */
    public void addCorrelationValues(List<CorrelationVal> correlationValues) {
        CorrelationVal val = null;

        if ((correlationValues == null) || correlationValues.isEmpty()) {
            return;
        }
        
        synchronized (mCorrSetIDToVal) {
            for (int i = 0, size = correlationValues.size(); i < size; i++) {
                val = (CorrelationVal) correlationValues.get(i);
                mCorrSetIDToVal.put(new Long(val.getSetID()), val);
            }
        }
    }    
    
    /**
     * @see BPELProcessInstance#getInitiatedCorrelations(com.sun.jbi.engine.bpel.core.bpel.model.meta.RStartElement)
     */
    public List getInitiatedCorrelations(RStartElement elem) {
        List setIds = CorrelationManagerImpl.getCorrelateOnlySetIds(elem);
        List retVals = new ArrayList();
        Object val = null;

        for (int i = 0, size = setIds.size(); i < size; i++) {
            val = mCorrSetIDToVal.get(setIds.get(i));

            if (val == null) {
            	throw new StandardException(StandardException.Fault.CorrelationViolation);
            }

            retVals.add(val);
        }

        return retVals;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#getInstanceAssociatedCorrVals()
     */
    public Collection<CorrelationVal> getInstanceAssociatedCorrVals() {
        return mCorrSetIDToVal.values();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#getCommonJoinInitiatedCorrelations(
     * com.sun.jbi.engine.bpel.core.bpel.model.meta.RStartElement)
     */
    public List getCommonJoinInitiatedCorrelations(RStartElement elem) {
        List setIds = CorrelationManagerImpl.getCommonJoinOnlySetIds(elem);
        List retVals = new ArrayList();
        Object val = null;

        for (int i = 0, size = setIds.size(); i < size; i++) {
            val = mCorrSetIDToVal.get(setIds.get(i));

            if (val == null) {
                throw new BPELRuntimeException(
                    BPELRuntimeException.CORRELATION_ERROR,
                    I18n.loc("BPCOR-6046: BPELProcessInstanceImpl_invalid_instance_state")
                );
            }

            retVals.add(val);
        }

        return retVals;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#instanceComplete()
     */
    public void instanceComplete() {
//        terminate();
        for (Iterator it = mCFs.iterator(); it.hasNext();) {
            ICallFrame cf = (ICallFrame) it.next();
            cf.onExit();
        }
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#startActivityCompletes()
     */
    public void startActivityCompletes() throws Exception {
        synchronized (mSetGetStartActLock) {
            instantiateEventHandlers();
            mStartActivityDone = true;   
        }
    }

   /**
    *  (non-Javadoc)
    * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#addTobeInstantiatedEventHandlers(
    * com.sun.jbi.engine.bpel.core.bpel.model.runtime.ScopeOrProcessUnit)
    */
    public boolean addTobeInstantiatedEventHandlers(ScopeOrProcessUnit scopeOrProcess) {
        synchronized (mSetGetStartActLock) {
            if (!mStartActivityDone) {
                mWaitingEventHandlers.add(scopeOrProcess);
            }
            return mStartActivityDone;
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#isStartActivityDone()
     */
    public boolean isStartActivityDone() {
    	return mStartActivityDone;
    }

	/** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.ScopeOrProcessUnit#instantiateEventHandlers()
     */
    public void instantiateEventHandlers() throws Exception {
    	if (mEventHandlersUnit != null) {
    		RBPELProcess mModel = mProcessManager.getBPELProcess();
    		//ICallFrame childFrame = mEng.getInterpreter().createCallFrame(mModel, this, mEventHandlersUnit);
    		if (mInitiatingCallFrame == null) {
    			// Throw a runtime exception. This should never happen, but since this is subject to an external
    			// component setting the initiating call frame it is probably a good idea to do this check if there
    			// are any changes there.
    			throw new BPELRuntimeException(
    					BPELRuntimeException.OTHER_ERROR,
    					I18n.loc("BPCOR-6047: BPELProcessInstanceImpl_INITIATING_CALLFRAME_NULL")
    			);
    		}
    		mEventHandlersUnit.startEventHandlers();
    	}
        if (mWaitingEventHandlers.size() > 0) {
            //Avoid synchronize block, make it into array and iterate there
            ScopeOrProcessUnit [] allWaitingScopes = new ScopeOrProcessUnit [mWaitingEventHandlers.size()];
            allWaitingScopes = (ScopeOrProcessUnit []) mWaitingEventHandlers.toArray(allWaitingScopes);
            for (int i = 0; i < allWaitingScopes.length; i ++) {
                allWaitingScopes [i].instantiateEventHandlers();
            }
        }        
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.ScopeOrProcessUnit#eventHandlersComplete()
     */
    public void eventHandlersComplete() {
        mInitiatingCallFrame.setProgramCounter(this);
        mProcessManager.addToReadyToRunQueue(mInitiatingCallFrame);
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#getRuntimeVariable(
     * com.sun.jbi.engine.bpel.core.bpel.model.meta.RVariable)
     */
    public RuntimeVariable getRuntimeVariable(RVariable variable) {
        //TODO: Should throw a fault or exception if the variable
        //is undefined at the process level
        return (RuntimeVariable) mRuntimeVariables.get(variable);
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#setRuntimeVariable(
     * com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable)
     */
    public void setRuntimeVariable(RuntimeVariable runtimeVariable) {
        //TODO: Should throw a fault or exception if the variable
        //is undefined at the process level
        
        mRuntimeVariables.put(runtimeVariable.getVariableDef(), runtimeVariable);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#createRuntimeVariable(com.sun.bpel.model.meta.RVariable)
     */
    public RuntimeVariable createRuntimeVariable(RVariable variable) {
    	return new RuntimeVariableImpl(variable, getProcessInstance(), getScopeGuid());
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#createRuntimePartnerLink(PartnerLink)
     */
    public RuntimePartnerLink createRuntimePartnerLink(PartnerLink partnerlink) {
        return new RuntimePartnerLinkImpl(partnerlink, getScopeGuid());
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope#getRuntimeVariables()
     */
    public Map getRuntimeVariables() {
        Map visibleRuntimeVariables = new HashMap();
        visibleRuntimeVariables.putAll(this.mRuntimeVariables);
        return visibleRuntimeVariables;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getProcessInstance()
     */
    public BPELProcessInstance getProcessInstance() {
        return this;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#initUponRecovery(Collection, Collection)
     */
    public void initUponRecovery(Collection<RuntimeVariable> runtimeVariables, Collection<RuntimePartnerLink> runtimePLinks) {
    	for (Iterator itr = runtimeVariables.iterator(); itr.hasNext(); ) {
    		RuntimeVariable rVar = (RuntimeVariable) itr.next();
    		//((VariableScope) processInstance).setRuntimeVariable(rVar);
    		mRuntimeVariables.put(rVar.getVariableDef(), rVar);
    		getMonitorMgr().addVariableScope(rVar.getVariableDef().getUniqueId(), this);
    	}
        for (RuntimePartnerLink rPLink : runtimePLinks) {
            mRuntimePartnerLinks.put(rPLink.getStaticModel(), rPLink);
        }
    	mStartActivityDone = true;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#initEHUponRecovery(com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeEventHandlers)
     */
    public void initEHUponRecovery(RuntimeEventHandlers rEH) {
        mEventHandlersUnit = rEH;
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getParentContext()
     */
    public Context getParentContext() {
        return null;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#removeRequest(com.sun.bpel.model.meta.RReply)
     */
    public MessageContainer removeRequest(RReply reply) {
        return mIMAScope.removeRequest(reply);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#addRequest(com.sun.bpel.model.meta.RStartElement, 
     * com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
     */
    public void addRequest(RStartElement rcv, MessageContainer req) throws BPELRuntimeException {
         mIMAScope.addRequest(rcv, req);
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#removeRequest(com.sun.bpel.model.meta.RStartElement, 
     * com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
     */
    public void removeRequest(RStartElement rcv, MessageContainer req) throws BPELRuntimeException {
         mIMAScope.removeRequest(rcv, req);
    }

    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#addToCRMPUpdateList(java.lang.String)
     */
    public void addToCRMPUpdateList(String updateValueKey) {
    	mIMAScope.addToCRMPUpdateList(updateValueKey);
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#crmpUpdateListContains(java.lang.String)
     */
    public boolean crmpUpdateListContains(String updateValueKey) {
    	return mIMAScope.crmpUpdateListContains(updateValueKey);
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#sendErrorsForPendingRequests(java.lang.Exception)
     */
    public void sendErrorsForPendingRequests(Exception error) {
        mIMAScope.sendErrorsForPendingRequests(error);
    }
    
    public void completePendingInOnlyRequests() {
    	mIMAScope.completePendingInOnlyRequests();
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#declareDefaultMessageExchange()
     */
    public void declareDefaultMessageExchange() {
        /*
         * Note that the process instance defines a default message exchange, so
         * there is no need for any other class to do so. This is unlike a scope
         * which needs to be explicitly marked as supporting a default message
         * exchange if it was created by an onEvent or a parallel form of
         * foreach.
         */
        throw new UnsupportedOperationException();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#getRuntimePartnerLink(com.sun.bpel.model.PartnerLink)
     */
    public RuntimePartnerLink getRuntimePartnerLink(PartnerLink pLink) {
        return mRuntimePartnerLinks.get(pLink);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.PartnerLinkScope#setRuntimePartnerLink(
     * com.sun.bpel.model.PartnerLink, RuntimePartnerLink)
     */
    public void setRuntimePartnerLink(PartnerLink pLink, RuntimePartnerLink runtimePLink) {
        mRuntimePartnerLinks.put(pLink, runtimePLink);
    }
    
    public Map getRuntimePartnerLinks() {
        final Map visibleRuntimePLs = new HashMap();
        visibleRuntimePLs.putAll(mRuntimePartnerLinks);
        
        return visibleRuntimePLs;
    }
    
    private boolean waitingForEventsComplete = false;
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#doAction(
     * com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame, 
     * com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread, 
     * com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    public boolean doAction(ICallFrame frame,
    		BusinessProcessInstanceThread bpit, RequiredObjects rObjs)
    throws Exception {

		if(isExiting) {
    		doProcesCompletionTasks(frame, bpit, rObjs, false);
    		return true;    		
        }
    	
    	if (currentProcessState == ProcessState.Running) {
    		ActivityUnit childActUnit;
    		try {
                ownedCallFrames.add(frame);
    			getPersistenctMgr().updateState(this, false, getStateContext().getState());
    			if (rObjs.getBPELProcessManager().isPersistenceEnabled()) {
    				getStateContext().getState().updateScope(getBranchId(), this);
    			}
    			EventHandlers rEvhandlers = mProcessManager.getBPELProcess().getEventHandlers();
    			if (rEvhandlers != null) {
    				long bid = RBPELProcess.DEFAULT_PROCESS_BRANCH_ID;
    				mEventHandlersUnit = new RuntimeEventHandlersImpl (this, this, this, this,
    						rEvhandlers, bid, rObjs.getEngine(), rObjs.getInterp(), 
    						rObjs.getBPELProcessManager(), FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID);
    			}

    			childActUnit = ActivityUnitFactory.getInstance().createActivityUnit(this, this, 
    					getChildActivity(), getBranchId());
    		} catch (Exception e) {
    			StandardException scopeIniExp = 
    				new StandardException(StandardException.Fault.ScopeInitializationFailure, e);
    			throw scopeIniExp;
    		}

    		boolean childActCompleted = executeChildActivities(frame, bpit, rObjs, childActUnit);

    		/* Note that when the control comes back here the scope activty would
    		 * either be completed or was terminated. If it was terminated it would
    		 * return false. Note that if the child activity would have caused a
    		 * fault, the child activity would return control using doResumeAction()
    		 */

    		//Child activity was completed successfully
    		if (childActCompleted) {
                frame.setProgramCounter(this);
    			/* The main activity in the process scope has completed, so if EH are present 
    			 * the process scope needs to wait for the EH to complete, before it can complete 
    			 * the process instance. Change the process scope state to indicate that it is 
    			 * waiting for EH to complete and inform EH that the process scope has completed.
    			 */
    			if (mEventHandlersUnit != null) {
    				currentProcessState = ProcessState.WaitingForEventsToComplete;
    				mEventHandlersUnit.associatedScopeCompletes();
    				return false;
    			} else {
    	    		if (rObjs.getBPELProcessManager().isPersistenceEnabled()) {
    	    			getStateContext().getState().updateScope(getBranchId(), this);
    	    		}
    	    		
    	    		if(isExiting) {
    	        		doProcesCompletionTasks(frame, bpit, rObjs, false);
    	            } else {
        				doProcesCompletionTasks(frame, bpit, rObjs, true);
    	            }

    				return true;
    			}
    		} else {
    			//The activity did not yet complete
    			return false;
    		}

    	} else if(currentProcessState == ProcessState.WaitingForEventsToComplete) {
    		if (rObjs.getBPELProcessManager().isPersistenceEnabled()) {
    			getStateContext().getState().updateScope(getBranchId(), this);
    		}

    		doProcesCompletionTasks(frame, bpit, rObjs, true);
    		return true;

    	} else if(currentProcessState == ProcessState.Faulted) {
    		return doFaultHandlingIfPossible(frame, bpit, rObjs, true);

    	} else {
			LOGGER.warning(I18n.loc("BPCOR-6048: Illegal state - method {0} called when the " + 
					"value of currentProcessState is {1}", "doAction", currentProcessState));
    		return false;
    	}
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.StructuredActivityUnitImpl#doResumeAction(
     * com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit,
     * com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame,
     * com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     * com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    protected boolean doResumeAction(ActivityUnit childActUnit, ICallFrame frame, 
    		BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {

        frame.setProgramCounter(this);
        
        if(isExiting) {
    		doProcesCompletionTasks(frame, bpit, rObjs, false);
    		if (this.terminationReason == TerminationReason.ExitActivityEncountered) {
        		LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6156: The process instance has been terminated because an exit activity was encountered."));
    		} else {
        		LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6158: The process instance has been terminated because a command to terminate the process instance was received."));    			
    		}
            return true;
        }
        
    	if(currentProcessState == ProcessState.Running) {
    	
            /* The main activity in the process scope has completed, so if EH are present 
             * the process scope needs to wait for the EH to complete, before it can complete 
             * the process instance. Change the process scope state to indicate that it is 
             * waiting for EH to complete and inform EH that the process scope has completed.
             */
            EventHandlers rEvhandlers = mProcessManager.getBPELProcess().getEventHandlers();
            if (rEvhandlers != null) {
            	currentProcessState = ProcessState.WaitingForEventsToComplete;
                mEventHandlersUnit.associatedScopeCompletes();
                return false;
            } else {
        		if (rObjs.getBPELProcessManager().isPersistenceEnabled()) {
        			getStateContext().getState().updateScope(getBranchId(), this);
        		}

            	doProcesCompletionTasks(frame, bpit, rObjs, true);
                return true;
            }
    	} else if (currentProcessState == ProcessState.Faulted) {
    		//If we do not have an EventHandlers then we can do FH.
    		return doFaultHandlingIfPossible(frame, bpit, rObjs, false);
    		
    	} else if (currentProcessState == ProcessState.ExecutingFaultHandlers) {
    		doProcesCompletionTasks(frame, bpit, rObjs, true);
            return true;
            
    	} else if (currentProcessState == ProcessState.FaultedInFH) {
    		doProcesCompletionTasks(frame, bpit, rObjs, false);
    		
            
    		LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6151: The process instance has been terminated because a fault was not handled; Fault Name is {0}; Fault Data is {1}", 
    		        mFaultObj.getName(), mFaultObj.getData()), mFaultObj.getException());
            return true;
            
    	} else {
    		LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6048: Illegal state - method {0} called when the " + 
    				"value of currentProcessState is {1}", "doResumeAction()", currentProcessState.toString()), mFaultObj.getException());
    		return false;
    	}
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getContext()
     */
    public Context getContext() {
        return this;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getEnclosingScopeUnit()
     */
    public ActivityUnit getEnclosingScopeUnit() {
        return null;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getEnclosingUnit()
     */
    public Unit getEnclosingUnit() {
        return null;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit#doActionOnRecovery(com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame, com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread, com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    public boolean doActionOnRecovery(RecoveredCallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs) throws Exception {
        throw new UnsupportedOperationException();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit#getBranchId()
     */
    public long getBranchId() {
        return RBPELProcess.DEFAULT_PROCESS_BRANCH_ID;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit#getEnclosingActivityUnit()
     */
    public ActivityUnit getEnclosingActivityUnit() {
        return null;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit#getNextActivityUnit()
     */
    public ActivityUnit getNextActivityUnit() {
        return null;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit#getPrevActivityUnit()
     */
    public ActivityUnit getPrevActivityUnit() {
        return null;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit#getStaticModelActivity()
     */
    public RActivity getStaticModelActivity() {
    	// return null;
    	// TODO: This is a temporary fix. Similar code exists in CallFrameFactory.getCallFrames()
        // needs to be fixed there also.
    	return (RActivity)mProcessManager.getBPELProcess().getActivity();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.StructuredActivityUnitImpl#getChildActivity()
     */
    protected RActivity getChildActivity() {
        return mProcessManager.getBPELProcess().getChildActivity();
    }

    
    //**************************** Methods related to FCT Handlers **************************//    
    
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#registerCallFrame(
	 * com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
	 */
	public void registerCallFrame(ICallFrame callframe) {
		ownedCallFrames.add(callframe);
	}
	
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#registerEnclosedScope(
	 * com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext)
	 */
	public void registerEnclosedScope(FaultHandlingContext enclosedScope) {
		enclosedScopes.add(enclosedScope);
	}

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#unregisterEnclosedScope(
     * com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext)
     */
    public void unregisterEnclosedScope(FaultHandlingContext enclosedScope) {
        enclosedScopes.remove(enclosedScope);
    }
	
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#notifyFault(
     * com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault, 
     * com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame, 
     * com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread, 
     * com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    public synchronized void notifyFault(Fault fault, ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs) {
    	
    	if (currentProcessState == ProcessState.Running) {
        	currentProcessState = ProcessState.Faulted;
        	this.mFaultObj = fault;
    		if (rObjs.getBPELProcessManager().isPersistenceEnabled()) {
    			getStateContext().getState().updateScope(getBranchId(), this);
    		}
        	activateWaitingCallFrames();
        	
        	//If the fault was from an EH, then the callFrame associated
        	//with this scope could be waiting; so we activate it. 
        	ArrayList tempList = new ArrayList();
        	tempList.add(mInitiatingCallFrame);
        	mInitiatingCallFrame.getProcessInstance().getBPELProcessManager()
        							.activateWaitingCallFrames(tempList);
    	} else if (currentProcessState == ProcessState.Faulted) {
    		//Already been notified of a fault by another branch, so return without
    		//doing anything
    		return;
    	}  else if(currentProcessState == ProcessState.ExecutingFaultHandlers) {
    		currentProcessState = ProcessState.FaultedInFH;
    		mFaultObj = fault;
    		if (rObjs.getBPELProcessManager().isPersistenceEnabled()) {
    			getStateContext().getState().updateScope(getBranchId(), this);
    		}
        	activateWaitingCallFrames();
    	} else if(currentProcessState == ProcessState.FaultedInFH) {
    		return;
    	} else {
    		LOGGER.warning(I18n.loc("BPCOR-6048: Illegal state - method {0} called when the " + 
    				"value of currentProcessState is {1}", "notifyFault()", currentProcessState.toString()));
    	}
	}	
	
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#activateWaitingCallFrames()
     */
    public void activateWaitingCallFrames() {

    	/*Call the activateWaitingCallFrames() method on the immediately
    	 *enclosed scopes so that they can put the waiting callframes 
    	 *belonging to them in the ReadyToRun queue
    	 */
    	Iterator iter = enclosedScopes.iterator();
    	while (iter.hasNext()){
    		Context enclosedScope = (Context) iter.next();
    		enclosedScope.getFaultHandlingContext().activateWaitingCallFrames();
    	}
        //BEGIN: temporary add logging statement for debugging
//    	if (mInitiatingCallFrame == null) {
//            LOGGER.warning(">>>>mInitiatingCallFrame == null");
//        } 
//        if (mInitiatingCallFrame.getProcessInstance() == null) {
//            LOGGER.warning(">>>>mInitiatingCallFrame.getProcessInstance() == null");
//        }        
//        if (mInitiatingCallFrame.getProcessInstance().getBPELProcessManager()== null) {
//            LOGGER.warning(">>>>mInitiatingCallFrame.getProcessInstance().getBPELProcessManager() == null");
//        }        
//        if (ownedCallFrames== null || ownedCallFrames.size() == 0) {
//            LOGGER.warning(">>>>ownedCallFrames == null");
//        }        
        
    	//Activate the waiting CallFrames
    	mInitiatingCallFrame.getProcessInstance().getBPELProcessManager()
    							.activateWaitingCallFrames(ownedCallFrames);
	}    
    
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#isBeingTerminated()
	 */
	public boolean isBeingTerminated() {
		
		if(isExiting){
			return true;
		}
		
		if(currentProcessState == ProcessState.Faulted 
				|| currentProcessState == ProcessState.FaultedInFH) {
			return true;
		} else {
			return false;
		}
	}
	
    /**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getStateId()
	 */
	public String getStateId() {
		return getStateContext().getState().getId();
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getScopeId()
	 */
	public long getScopeId() {
		return RBPELProcess.DEFAULT_PROCESS_SCOPE_ID;
	}
	
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getScopeGuid()
     */
    public String getScopeGuid() {
    	return FaultHandlingContext.PROCESS_INSTANCE_SCOPE_ID;
    }
    
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getParentScopeGuid()
	 */
	public String getParentScopeGuid() {
		return null;
	}
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getScopeState()
     */
    public String getScopeState() {
    	if (currentProcessState == ProcessState.Running	||     			
    			currentProcessState == ProcessState.WaitingForEventsToComplete ||
    			currentProcessState == ProcessState.Suspended) {
    		return RUNNING;
    	} else if (currentProcessState == ProcessState.ExecutingFaultHandlers) {
    		return EXE_FAULT_HANDLER;
    	} else if (currentProcessState == ProcessState.FaultedInFH) {
    		return FAULTED_IN_FH;
    	} else if (currentProcessState == ProcessState.Faulted) {
    		return FAULTED;
    	} else {
    		// only other state left is the ScopeState.Terminated.
    		//TODO: have to see if this state is needed.
    		return TERMINATED;
    	}
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getFault()
     */
    public Fault getFault() {
		return mFaultObj;
	}

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#setFault(com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault, long)
     */
    public void setFault(Fault faultObj, long faultActivityId) {
    	mFaultObj = faultObj;
    	mFaultActivityId = faultActivityId;
    }
	
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getFaultActivityId()
	 */
	public long getFaultActivityId() {
		return mFaultActivityId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getCompensateId()
     */
    public long getCompensateId() {
    	return 0;
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getCompletionOrder()
     */
    public long getCompletionOrder() {
    	// the process instance does not have a completion order.
    	return 0;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getEnclosedScopesIterCount(long)
     */
    public long getEnclosedScopesIterCount(long scopeId) {
    	Long iterVal = mEnclosedScopesIterationCount.get(scopeId);
    	long val = -1;
    	if (iterVal == null) {
    		val = 1;
    	} else {
    		val = iterVal.longValue();
    		val += 1;
    	}
    	mEnclosedScopesIterationCount.put(scopeId, new Long(val));
    	return val;
    }    
    
    /**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#addCompletedScope(com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext)
	 */
	public void addCompletedScope(FaultHandlingContext completedScope) {
		mCompletedScopes.push((Context)completedScope);
		Utility.sortScopesByCompletionOrder(mCompletedScopes);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#setCompletionOrder(long)
	 */
	public void setCompletionOrder(long completionOrder) {
		throw new UnsupportedOperationException();
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#setCompensateId(long)
	 */
	public void setCompensateId(long compensateId) {
		throw new UnsupportedOperationException();
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#setScopeState(java.lang.String)
	 */
	public void setScopeState(String scopeState) {
        if (currentProcessState == ProcessState.Suspended) {
            return;
        }
		if (scopeState.equals(RUNNING)) {
			currentProcessState = ProcessState.Running;
		} else if (scopeState.equals(EXE_FAULT_HANDLER)) {
			currentProcessState = ProcessState.ExecutingFaultHandlers;			
		} else if (scopeState.equals(FAULTED_IN_FH)) {
			currentProcessState = ProcessState.FaultedInFH;			
		} else if (scopeState.equals(FAULTED)) {
			currentProcessState = ProcessState.Faulted;			
		} else {
			currentProcessState = ProcessState.Terminated;
		}
	}

	/**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#isInserted()
     */
    public boolean isInserted() {
    	return mIsInserted;
    }
    
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#markInserted()
	 */
	public void markInserted() {
		mIsInserted = true;
	}
    
    private boolean doFaultHandlingIfPossible (ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects rObjs, boolean ehCompleted) throws Exception {
    	
    	if (ehCompleted == false) {
    		/* Inform the EH that it should wind up. Note that we use the same API
    		 * associatedScopeCompletes(). This is because the EH, onEvents, onAlarms, 
    		 * their associated scopes and the activities contained in those scopes 
    		 * will be terminated by checking Context.isBeing Terminated(). All the onEvents
    		 * and onAlarms would return control to the EH Unit which will inform this scope
    		 * that the EH has finished winding up.
    		 */
    		if (mEventHandlersUnit != null) {
    			/* Note that we dont change the state here to WaitingForEventsToComplete, since
    			 * the state is Faulted. When the doAction() of scope is called, the scope 
    			 * implicitly knows that it is waiting for EH to wind up. 
    			 */
    			mEventHandlersUnit.associatedScopeCompletes();
    			return false;
    		}
    	}
    	
		//We do not have EH or they were completed
		currentProcessState = ProcessState.ExecutingFaultHandlers;
		if (rObjs.getBPELProcessManager().isPersistenceEnabled()) {
			getStateContext().getState().updateScope(getBranchId(), this);
		}
		
		Unit faultHandlingUnit = ActivityUnitFactory.getInstance().createFaultHandlingUnit(this,
				mFaultObj, mProcessManager.getBPELProcess().getFaultHandlers());

		if (faultHandlingUnit == null) {
			faultHandlingUnit = new VirtualCatchAllUnit(this, mAct, mFaultObj);
		}

		//The clearFHRelatedData() had removed all the callframes, so we need to add back
		//the callframe that is executing the scope
		ownedCallFrames.add(frame);
		
		//Call the fault handler to execute its activity
		boolean fhCompleted = faultHandlingUnit.doAction(frame, bpit, rObjs);

		//If the fault handling has completed, the scope itself has completed
		if (fhCompleted) {
			doProcesCompletionTasks(frame, bpit, rObjs, true);
			return true;
		} else {
			return false;
		}
    }
	
    private void doProcesCompletionTasks(ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects rObjs, boolean orderlyCompletion) throws Exception {
        
    	// If the process completion is orderly then complete the pending inOnly requests. This will happen
    	// only if the BP is atomic.
    	if (orderlyCompletion && getBPELProcessManager().isBPAtomic()) {
    		completePendingInOnlyRequests();
    	}
    	
    	//Note that there is no need to append the line number and activity name for the process 
    	//scope, since there is only one process scope
    	if(isExiting) {
    		//This means the process instance is exiting
    		String message = I18n.loc("BPCOR-6054: Sending errors for the pending requests in the process scope, since the process instance is being terminated");
    		sendErrorsForPendingRequests(new Exception(message, null));
    		
    	} else if (currentProcessState == ProcessState.FaultedInFH) {
    		
    		//This means we are exiting the process scope because there is an unhandled fault 
    		String message = I18n.loc("BPCOR-6135: A fault was not handled in the process scope; Fault Name is {0}; Fault Data is {1}. Sending errors for the pending requests in the process scope before terminating the process instance", 
    						mFaultObj.getName(), mFaultObj.getData());
    		sendErrorsForPendingRequests(new Exception(message, mFaultObj.getException()));
    		getMonitorMgr().postEventForFault(this, mFaultObj);
    		getMonitorMgr().postInstanceFaultedEvent();  
    		BPELTraceManager.getInstance().alertBPInstanceTerminatedOnUnHandledFault(rObjs.getEngine().getId(), getBPELProcessManager().getBPELProcess().getBPELId().toString()
                    , getId(), mFaultObj.getName() + " " + (mFaultObj.getData() == null ? "" : ("Details: " + mFaultObj.getData().toString())))  ;
    		
    	} else {
    		//This means the process scope is completing either normally of after completing FCT Handler
    		String message = I18n.loc("BPCOR-6136: Sending errors for the pending requests in the process scope since, the process instance has completed");
    		sendErrorsForPendingRequests(new Exception(message, null));
    	}

        getPersistenctMgr().updateState(this, true, getStateContext().getState());
        if(orderlyCompletion) {
            rObjs.getBPELProcessManager().instanceComplete(this);
        } else {
            rObjs.getBPELProcessManager().terminate(this);
        }
    }
    
    /* ** METHODS for compensation handling*/
	/*
	 * When a contained scope of this instance completes successfully(without faults) 
	 * it would be added to this instance completed scopes stack in the order of 
	 * completion  
	 * (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#
	 * 	pushCompletedScope(com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context)
	 */
    public int pushCompletedScope(Context completedScope) {
		synchronized (mCompletedScopes) {
			if (currentProcessState == ProcessState.Running 
					|| currentProcessState == ProcessState.WaitingForEventsToComplete) {
				mCompletedScopes.push(completedScope);
				return mCompletedScopes.size();
			} else {
				return 0;
			}
		}
    }
    
    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#getCompletedScopes()
     */
    public Stack<Context> getCompletedScopes() {
		return mCompletedScopes;
	}
    
	private Context popCompletedScope() {
		try {
			return mCompletedScopes.pop();
		} catch (EmptyStackException ex) {
			return null;
		}
	}
	
	/*
	 * (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.
	 * 	FaultHandlingContext#getScopeForCompensation()
	 */
	public Context getScopeForCompensation() {
		return popCompletedScope();
	}
	
	/*
	 * (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext
	 * #getTargetScopeForCompensation(java.lang.String)
	 */
	public Context getTargetScopeForCompensation(String targetScopeName) {
		Context selectedScope = null;
		String selectedScopeName = null;
		Stack<Context> tempStack = new Stack<Context>();
		synchronized (mCompletedScopes) {
			try {
				while ((selectedScope = popCompletedScope()) != null) {
					selectedScopeName = ((ActivityUnit) selectedScope)
							.getStaticModelActivity().getName();
					if (selectedScopeName.equals(targetScopeName)) {
						return selectedScope;
					} else {
						tempStack.push(selectedScope);
					}
				}
				return null;
			} finally {
				Context tempContext = null;
				try {
					while ((tempContext = tempStack.pop()) != null) {
						pushCompletedScope(tempContext);
					}
				} catch (EmptyStackException empExcep) {
					// ignore the exception. means that the tempStack is empty
				}
				tempContext = null;
				tempStack = null;
			}
		}
	}
	    
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#
	 * setCompensateContinuation(com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit)
	 */
    public void setCompensateContinuation(ActivityUnit unit) {
    	throw new UnsupportedOperationException();
	}
    
    
    //**************************** End of methods related to FCT Handlers *******************//        	
    
    private LinkedList<ActivityUnit> faultUnits = new LinkedList<ActivityUnit>();
    
    /** This API helps identify if the instance is in an infinite loop of throwing faults
     * @param actUnit
     * @return
     */
    public boolean isInfiniteFaultLoop(ActivityUnit actUnit) {
        if (actUnit.getStaticModelActivity().getXPath().contains(ForEach.TAG)) {
            // foreach has a completion condition with "successfulBranchesOnly" that
            // makes it unpredictable  even if the exception happens multiple times
            return false;
        }
        /*        fix for infinite loop due to errors in engine.
        ****************************************************************************
        ****************************************************************************
        Engine takes at it's best stab to control this infinite loop, but it can be be
        never 100% guaranteed. Engine will identify the instance and terminate it. this
        logic doesn't apply for FOREACH because foreach has a completion condition with
        "successfulBranchesOnly" that makes it unpredictable even if the exception
        happens multiple times

        This has one interesting side effect. The intention of the above code is to come
        out of infinite loops because of engine errors. This also helps the end user in
        the situation where the error happens in business logic in a repeating
        construct. (while, repeatuntil).

        loop (100 times)
          Scope
               FH
            Assign (always throws fault which is handled by FH)

        after 3 loops, engine terminates the instance. Perhaps the 4th iteration might
        be OK, what then? I tend to support infinite loop case so that engine doesn't
        bring the machine down to trying to support such a silly BPEL user as mentioned
        here. If still the user prefers to do this kindof thing, he can use FOREACH and
        engine ignores the loop.
        ****************************************************************************
        ****************************************************************************
        */
        faultUnits.addFirst(actUnit);
        if (faultUnits.size() < 3) {
            return false;
        }
        ActivityUnit faultUnit;
        Activity oldFaultAct = null;
        Activity faultAct;
        for (int i = 0; i < 3; i++) {
            faultUnit = faultUnits.get(i);
            if (!(faultUnit instanceof VirtualThrowUnitImpl)) {
                return false;
            }
            faultAct = faultUnit.getStaticModelActivity();
            if (oldFaultAct == null) {
                oldFaultAct = faultAct;
            } else {
                if (oldFaultAct != faultAct) {
                    return false;
                } 
            }
        }
        return true;
    }
    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#getPersistenctMgr()
     */
    public PersistenceManager getPersistenctMgr() {
        return mPerMgr;
    }
    
	/** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#getClusterMgr()
     */
    public ClusterManager getClusterMgr() {
        return mClustMgr;
    }

	/** @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#getMonitorMgr()
     */
    public MonitorManager getMonitorMgr() {
        return mMonitorMgr;
    }    
    
    /**
	 * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getFaultHandlingContext()
	 */
	public FaultHandlingContext getFaultHandlingContext() {
		return this;
	}

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context#getStateContext()
     */
    public StateContext getStateContext() {
        return this;
    }

    /**
     * @see BPELProcessInstance#isSuspended()
     */
	public boolean isSuspended() {
		//Concurrency consideration see comments in suspend()
		return currentProcessState == ProcessState.Suspended;
	}

    /**
     * @see BPELProcessInstance#resume()
     */	
	public void resume() throws Exception {
		//Concurrency consideration see comments in suspend()
		currentProcessState = ProcessState.Running;
		try {
			  getPersistenctMgr().updateState(this, false, true);
		}catch (Exception e ) {
			currentProcessState = ProcessState.Suspended;
			throw e;
		}		
	}
	
    /**
     * @see BPELProcessInstance#suspend()
     */		
	public void suspend() throws Exception {
		//It is considered that suspend () call is done by the user
		//and not frequently, no concurrent suspend calls are expected.
		//Suspend and resume are not supposed to happen concurrently,
		//using synchronize block for get/set currentProcessState to prevent the race condition introduced by
		//suspend() and resume is not necessary		
		if (currentProcessState != ProcessState.Running) {
			throw new InvalidStatusException(I18n.loc("BPCOR-3021: The instance is not in RUNNING state, " + 
					"it is in state {0}", currentProcessState));
		}
		currentProcessState = ProcessState.Suspended;
		try {
			  getPersistenctMgr().updateState(this, true, false);
		}catch (Exception e ) {
			currentProcessState = ProcessState.Running;
			throw e;
		}
		// notifiy the alerter that the instance has been suspended. 
		// refer to bug https://open-esb.dev.java.net/issues/show_bug.cgi?id=2175
		QName bpName = getBPELProcessManager().getBPELProcess().getBPELId();
		BPELTraceManager.getInstance().alertBPInstanceChangeByAPI(mEng.getId(), bpName.toString(), 
		        getId(), null, ActionType.Suspended);
		
	}

	public void setSuspended() {
		currentProcessState = ProcessState.Suspended;
	}
	
	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#getTransactionPropagationObject()
	 */
	public TxPropagationObject getTxPropagationObject() {
		return this.mTxPropObj;
	}
    /*public long getLastActivityTime() {
        return lastActivityTime;
    }*/

    public void addToInactiveList(ICallFrame callframe, InactivityReason reason) {
    	
    	InactiveCallframeInfo callframeInfo = new InactiveCallframeInfo(callframe, reason);
    	
    	synchronized (mScalabilityLock) {
    		this.mInactiveCallFramesList.add(callframeInfo);
    		
    		/*
    		 * NOTE: The on-alarm callframe does not create any new branch, 
    		 * hence we don't need to decrement the branch count. Also, note
    		 * that for active on-alarm there will be an in active on-message.
    		 * We need to add the on-alarm into inactive list as when the scalability
    		 * passivation happen, we need to dereference the enclosing callframe 
    		 * reference held by this (on-alarm) bpit.
    		 */
    		if (reason != InactivityReason.ONALARM_FOR_PICK) {
    			decrementActiveBranchCount(1);
    		}
    	}
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#removeFromInactiveList(com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame)
     */
    public void removeFromInactiveList(ICallFrame callframe, boolean isOnAlarmForPick) {
    	InactiveCallframeInfo callframeInfo = new InactiveCallframeInfo(callframe);
    	
    	synchronized (mScalabilityLock) {
    		if (mInactiveCallFramesList.remove(callframeInfo)) {
    			
    			/*
    			 * See note for addToInactiveList
    			 */
        		if (!isOnAlarmForPick) {
        			incrementActiveBranchCount(1);
        		}
    		}
    	}
    }
    
    public void markVarsPassivated(boolean flag) {
        this.areVarsPassivated = flag;
    }
    
    public boolean areVarsPassivated() {
        return areVarsPassivated;
    }

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#isExiting()
	 */
	public boolean isExiting() {
		return isExiting;
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#doTerminate(TerminationReason)
	 */
	public void doTerminate(TerminationReason terminationReason) {
		this.terminationReason = terminationReason;
		isExiting = true;
        currentProcessState = ProcessState.Running;
		activateWaitingCallFrames();
	}    

    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#getPropagationContext()
     */
	public PropagationContext getPropagationContext() {
		return mPropContext;
	}

	/*
	 * (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance#setPropagationContext(com.sun.jbi.engine.bpel.core.bpel.util.PropagationContext)
	 */
	public void setParentMExArtifacts(MessageContainer msgContainer) 
	throws Exception
	{
		mPropContext = msgContainer.getPropagationContext();
		if (mProcessManager.isBPAtomic()) {
			TxPropagationObject txPropObj = getTxPropagationObject();
			if (txPropObj.extractTransactionFromRequest(msgContainer, mProcessManager)) {
				txPropObj.setPropagationStatus(TxPropagationObject.PropagationStatus.active);
			} else {
				txPropObj.setPropagationStatus(TxPropagationObject.PropagationStatus.unavailable);
			}
		}

	}
    public void addIMAScope(IMAScopeBridge scope) {
        synchronized (mIMAScopeSet) {
            mIMAScopeSet.add(scope);
        }
    }

    public Object[] qualifiesForInstancePassivation(long idlenessCriterion) {
		Object [] returnValues = new Object[2];
		
		if (hasOutstandingRequests()) {
			returnValues[0] = false;
			return returnValues;
		}
		
		if (passesIdlenssCriterion(idlenessCriterion)) {
			returnValues[0] = true;
			returnValues[1] = mInactiveCallFramesList;
		} else {
			returnValues[0] = false;
		}
		
		return returnValues;
	}

        
    public boolean passesIdlenssCriterion(long idlenessCriterion) {
    	
    	if((mActiveBranchCount > 0) 
    			|| (mActiveBranchCount > mInactiveCallFramesList.size())) {
    		return false;
    	}
    	
    	Iterator<InactiveCallframeInfo> iter = mInactiveCallFramesList.iterator();
    	
    	InactiveCallframeInfo callframeInfo = null;
    	
    	while (iter.hasNext()) {
    		callframeInfo = (InactiveCallframeInfo) iter.next();
    	
    		if(!callframeInfo.passesIdlenssCriterion(idlenessCriterion)) {
    			return false;
    		}
    	}
    	
    	return true;
    }
    
    private boolean hasOutstandingRequests() {
        Iterator<IMAScopeBridge> iter = null;
        IMAScopeBridge scope = null;
        boolean returnFlag = false;

        synchronized (mIMAScopeSet) {
            iter = mIMAScopeSet.iterator();
            while (iter.hasNext()) {
                scope = (IMAScopeBridge) iter.next();
                returnFlag = scope.hasOutstandingRequests();
                if (returnFlag) {
                    returnFlag = true; 
                }
            }
        }
        return returnFlag;
    }
    
    public boolean acquirePersistenceLock(boolean isPassivation) {
		synchronized (mScalabilityLock) {
			if (isPassivation) {
				if (mIsBeingPersisted) {
					return false;
				}
				mIsBeingPassivated = true;
				return true;
			}
			if (mIsBeingPassivated) {
				try {
					mScalabilityLock.wait();
				} catch (InterruptedException ex) {
					throw new RuntimeException(ex);
				}
			} 
			mIsBeingPersisted = true;
		}
		return false;
	}
    
    
    public void releasePersistenceLock(boolean isPassivation) {
		synchronized (mScalabilityLock) {
			if (isPassivation) {
				mIsBeingPassivated = false;
			} else {
				mIsBeingPersisted = false;
			}
			mScalabilityLock.notify();
		}
	}
    
    public void incrementActiveBranchCount(int count) {
    	synchronized (mScalabilityLock) {
    		this.mActiveBranchCount += count;
		}
    }
    
    public void decrementActiveBranchCount(int count) {
    	synchronized (mScalabilityLock) {
    		this.mActiveBranchCount-= count;
		}
    }

	@Override
	protected boolean doPassControlToParent(ICallFrame frame,
			BusinessProcessInstanceThread bpit, RequiredObjects objs)
			throws Exception {
		// Do passControlToParent should not be called on a BPELProcessInstanceImpl instance.
		throw new UnsupportedOperationException();
	}


    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext#hasFaulted()
     */
	public boolean hasFaulted() {
        if(currentProcessState == ProcessState.Faulted 
                || currentProcessState == ProcessState.FaultedInFH) {
            return true;
        } else {
            return false;
        }
    }
}
