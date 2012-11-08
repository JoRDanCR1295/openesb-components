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
 * @(#)$Id: MonitorManager.java,v 1.15 2010/02/04 02:51:39 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.event.ActivityEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.EventsFilter;
import com.sun.jbi.engine.bpel.core.bpel.event.Variable;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.VariableEvent.VariableType;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.BPELEventFactory;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.VariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.VariableScope;

/**
 * Manages sending bpel events and provide required states
 * for bpel monitor 
 * 
 * @author Sun Microsystems
 *
 */
public class MonitorManager {
	
	/** */
	public static long USER_INITIATED_CHANGE_ACTIVITY_ID = -5;
	
	/** */
	public static boolean needSynch = false;
	
	/* */
	private static long FAULT_VAR_ID = -1;
	
	/* */
	private Engine mEngine;
	
	/* */
	private BPELProcessInstance mInstance;
	
	/* */
	private RBPELProcess mProcess;
	
	/* */
	private EventsFilter mEventsFilter;
	
	/* */
	private boolean mGenerateEvents;
	
	/* Map<varId, VariableScope>> */
	private Map<Long, VariableScope> mVariableScopeMap = Collections.synchronizedMap(new HashMap<Long,VariableScope> ());
	
	/**
	 * 
	 * @param engine
	 * @param instance
	 */
	public MonitorManager(Engine engine, BPELProcessInstance instance) {
		mEngine = engine;
		mInstance = instance;
		mProcess = instance.getBPELProcessManager().getBPELProcess();
		mEventsFilter = instance.getBPELProcessManager().getEventsFilter();
		mGenerateEvents = mInstance.getBPELProcessManager().getGenerateEventsFlag();
	}
	
	/**
	 * 
	 * @param engine
	 */
	public synchronized static void synchronizeBPStatus(Engine engine) {
		if (needSynch) {
			BPELEvent event = BPELEventFactory.createBPELSynchronizeEvent(engine.getId());
			engine.postEvent(event);
			needSynch = false;
		}
	}
	
	/**
	 * 
	 */
	public synchronized static void needSynch () {
        	needSynch = true;
	}

	/**
	 * 
	 * @param varId
	 * @param scope
	 */
	public void addVariableScope (long varId, VariableScope scope) {
		mVariableScopeMap.put(varId, scope);
	}

	/**
	 * 
	 */
	public void postInstanceStartEvent () {
	    BPELEvent event = BPELEventFactory.createBPELInstanceEvent(mEngine.getId(), 
	            mInstance.getBPELProcessManager().getBPELProcess().getBPELId(), 
	            mInstance.getId(), 
	            BPELEvent.EventType.BP_START);
	    mInstance.getBPELProcessManager().getPerformanceManger().instanceCreated(event);
	    //Post Event
	    if (generateEventsForProcess()) {
	        mEngine.postEvent(event);
	    }
	}
	
    /**
     * Event indicating the creation of the recovered instance. There is no need
     * to persist the event, it is assumed that entry would be persisted in the
     * monitoring instance table.
     */
    public void postInstanceRecoveredEvent() {
        BPELEvent event = BPELEventFactory.createBPELInstanceEvent(mEngine.getId(), mInstance.getBPELProcessManager()
                .getBPELProcess().getBPELId(), mInstance.getId(), BPELEvent.EventType.BP_RECOVERED);
        mInstance.getBPELProcessManager().getPerformanceManger().instanceRecovered(event);
        /*
         * TODO: In the event that the instance is not present in the monitoring
         * table(the case where the engine was not running with monitoring
         * enabled before it crashed and is now monitoring enabled after
         * restart), here the next activity executed after recovery will persist
         * the instance too, hence we can avoid a persistence point. If this is
         * not sufficient then we have to persist the instance to the monitoring
         * table at this point.
         */

    }
	
	/**
	 * 
	 */
	public void postInstanceSuspendEvent () {
	    BPELEvent event = BPELEventFactory.createBPELInstanceEvent(mEngine.getId(), 
	            mInstance.getBPELProcessManager().getBPELProcess().getBPELId(), 
	            mInstance.getId(), 
	            BPELEvent.EventType.BP_SUSPEND);
	    mInstance.getBPELProcessManager().getPerformanceManger().instanceSuspended(event);
	    //Post Event
	    if (generateEventsForProcess()) {
	        mEngine.postEvent(event);
	    }
	}	
	
	/**
	 * 
	 */
	public void postInstanceResumeEvent () {
	    BPELEvent event = BPELEventFactory.createBPELInstanceEvent(mEngine.getId(), 
	            mInstance.getBPELProcessManager().getBPELProcess().getBPELId(), 
	            mInstance.getId(), 
	            BPELEvent.EventType.BP_RESUME);
	    mInstance.getBPELProcessManager().getPerformanceManger().instanceResumed(event);
	    //Post Event
	    if (generateEventsForProcess()) {
	        mEngine.postEvent(event);
	    }
	}	

	/**
	 * 
	 */
	public void postInstanceCompleteEvent () {
	    BPELEvent event = BPELEventFactory.createBPELInstanceEvent(mEngine.getId(), 
	            mInstance.getBPELProcessManager().getBPELProcess().getBPELId(), 
	            mInstance.getId(), 
	            BPELEvent.EventType.BP_COMPLETE);
	    mInstance.getBPELProcessManager().getPerformanceManger().instanceCompleted(event);
	    //Post Event
	    if (generateEventsForProcess()) {
	        mEngine.postEvent(event);
	    }
	}	

	/**
	 * 
	 */
	public void postInstanceTerminatEvent () {
	    BPELEvent event = BPELEventFactory.createBPELInstanceEvent(mEngine.getId(), 
	            mInstance.getBPELProcessManager().getBPELProcess().getBPELId(), 
	            mInstance.getId(), 
	            BPELEvent.EventType.BP_TERMINATE);
	    mInstance.getBPELProcessManager().getPerformanceManger().instanceTerminated(event);
	    //Post Event
	    if (generateEventsForProcess()) {
	        mEngine.postEvent(event);
	    }
	}	
	
	/**
	 * 
	 */
	public void postInstanceFaultedEvent () {
	    BPELEvent event = BPELEventFactory.createBPELInstanceEvent(mEngine.getId(), 
	            mInstance.getBPELProcessManager().getBPELProcess().getBPELId(), 
	            mInstance.getId(), 
	            BPELEvent.EventType.BP_FAULT);
	    mInstance.getBPELProcessManager().getPerformanceManger().instanceFaulted(event);
	    //Post Event
	    if (generateEventsForProcess()) {
	        mEngine.postEvent(event);
	    }
	}		

	/**
	 * 
	 * @param unit
	 */
	public void postActivityStartEvent(ActivityUnit unit) {
	    if (generateEventsForActivity(unit)) {
		    ActivityEvent event = BPELEventFactory.createActivityEvent(mEngine.getId(), 
		            mInstance.getBPELProcessManager().getBPELProcess().getBPELId(), 
		            mInstance.getId(), BPELEvent.EventType.ACTIVITY_START,
		            unit.getStaticModelActivity().getUniqueId(), 
		            unit.getStaticModelActivity().getLocalName(), 
		            unit.getStaticModelActivity().getXPath());	    	
	        mEngine.postEvent(event);
	    }
	}
    
    /**
	 * Post activity complete event
	 * 
	 */
    public void postActivityCompleteEvent(ActivityUnit unit) {
    	if (generateEventsForActivity(unit)) {
			BPELEvent event = BPELEventFactory.createActivityEvent(mEngine.getId(), 
					mProcess.getBPELId(),
					mInstance.getId(), 
					BPELEvent.EventType.ACTIVITY_COMPLETED,
					unit.getStaticModelActivity().getUniqueId(), 
					unit.getStaticModelActivity().getLocalName(), 
					unit.getStaticModelActivity().getXPath());
			mEngine.postEvent(event);
		}
    }    
    
    /**
	 * Post activity terminate event
	 * 
	 */
    public void postActivityTerminateEvent(ActivityUnit unit) {
    	if (generateEventsForActivity(unit)) {
			BPELEvent event = BPELEventFactory.createActivityEvent(mEngine.getId(), 
					mProcess.getBPELId(),
					mInstance.getId(), 
					BPELEvent.EventType.ACTIVITY_TERMINATE,
					unit.getStaticModelActivity().getUniqueId(), 
					unit.getStaticModelActivity().getLocalName(), 
					unit.getStaticModelActivity().getXPath());
			mEngine.postEvent(event);
		}
    }    
        
    /**
	 * Post activity start event
	 * 
	 */
    public void postActivityFaultedEvent(ActivityUnit unit) {
    	if (generateEventsForActivity(unit)) {
    		long actId = 0;
    		String actLocalName = null;
    		String actXpath = null;
    		if (unit.getStaticModelActivity() != null) {
    			actId = unit.getStaticModelActivity().getUniqueId();
    			actLocalName = unit.getStaticModelActivity().getLocalName();
    			actXpath = unit.getStaticModelActivity().getXPath();
    		} else {
    			actId = RBPELProcess.DEFAULT_PROCESS_SCOPE_ID;
    			actLocalName = mProcess.getLocalName();
    			actXpath = mProcess.getXPath();
    		}
			BPELEvent event = BPELEventFactory.createActivityEvent(mEngine.getId(), 
					mProcess.getBPELId(),
					mInstance.getId(), 
					BPELEvent.EventType.ACTIVITY_FAULTED,
					actId, 
					actLocalName, actXpath);
			mEngine.postEvent(event);
		}
    }    
   
    /**
     * 
     * @param unit
     * @param rObjs
     * @param varMap
     * @param hasFault
     * @param receiveCRMPId
     * @param invokeCRMPId
     */
    public void  postVariableEvent(ActivityUnit unit, Map<VariableEvent.VariableType, 
    		List<Variable>> varMap, boolean hasFault, String receiveCRMPId, String invokeCRMPId )  {
    	// DEVNOTE: For variable events we do not take into account if KPI is enabled or not. This is because today
		// KPI events ignore variable events anyway. If this changes (not sure why it would) this logic will need to 
		// be changed to take KPI into account. See <code>generateEventsForVariable(long uid)</code> also.
		if (mGenerateEvents && mEngine.isMonitorEnabled() && (mEngine.isVariableMonitorEnabled() || hasFault) ) {
			processAndFilterVariables(varMap);
			if (!varMap.isEmpty()) {
				long actId = 0;
				String actXpath = null;
				if (unit.getStaticModelActivity() != null) {
					actId = unit.getStaticModelActivity().getUniqueId();
					actXpath = unit.getStaticModelActivity().getXPath();
				} else {
					actId = RBPELProcess.DEFAULT_PROCESS_SCOPE_ID;
					actXpath = mProcess.getXPath();
				}    		

				VariableEvent event = BPELEventFactory.createVariableEvent(mEngine.getId(), 
						mProcess.getBPELId(), mInstance.getId(), BPELEvent.EventType.VARIABLE_CHANGED,
						actId, varMap, actXpath);

				if (receiveCRMPId != null) {
					event.setReceiveCRMPID(receiveCRMPId);
				} 
				if (invokeCRMPId != null) {
					event.setInvokeCRMPID(invokeCRMPId);
				}		
				mEngine.postEvent(event);
			}
		}  	
    }
    
    /**
     * 
     * @param unit
     * @param rObjs
     * @param bpId
     * @param fault
     */
    public void postEventForFault(ActivityUnit unit, Fault fault) {
		if (generateEventsForActivity(unit)) {
			postActivityFaultedEvent(unit);
			if (mEngine.isMonitorEnabled()) {
				String varStr = null;
				if (fault.getData() != null) {
					varStr = fault.getName() + " Details: " + fault.getData().toString();
				} else {
					varStr = fault.getName().toString();
				}
	    		long actId = 0;
	    		if (unit.getStaticModelActivity() != null) {
	    			actId = unit.getStaticModelActivity().getUniqueId();
	    		} else {
	    			actId = RBPELProcess.DEFAULT_PROCESS_SCOPE_ID;
	    		}    					
				Variable variable = new VariableImpl(actId, 
						Variable.DataType.Message, varStr, FAULT_VAR_ID, fault.getName());
				Map<VariableEvent.VariableType, List<Variable>> variableMap = new HashMap<VariableType, List<Variable>>();
				List<Variable> vars = new ArrayList<Variable>();
				vars.add(variable);
				variableMap.put(VariableEvent.VariableType.FAULT, vars);
				postVariableEvent(unit, variableMap, true, null, null);
			}
		}
	}
    
    /**
     * 
     * @param varId
     * @return
     */
    public VariableScope getVariableScope (long varId) {
    	return mVariableScopeMap.get(varId);
    }
	
    /**
     * 
     * @return
     */
    public BPELProcessInstance getInstance () {
    	return mInstance;
    }
    
    /**
     * 
     * @param bpit
     * @return
     */
    public boolean checkInstanceSuspended (BusinessProcessInstanceThread bpit) {
    	if (mInstance.isSuspended())  {
    		mInstance.getBPELProcessManager().addToReadyToRunQueue(bpit);
    		return true;
    	}
    	return false;
    }
    
    /**
     * 
     * @throws Exception
     */
    public void suspendInstance () throws Exception {
    	if (mInstance.isSuspended()) {
    		return;
    	}
    	mInstance.suspend();
    	postInstanceSuspendEvent ();
    }
    
    /**
     * 
     * @throws Exception
     */
    public void resumeInstance () throws Exception {
    	if (!mInstance.isSuspended()) {
    		return;
    	}
        postInstanceResumeEvent();
    	mInstance.resume();    	
//    	mEngine.process();
    }
    
    /**
     * User terminates the instance (Forced Termination).
     * Assumption: The user can terminate an instance which is either running
     * or suspended.  
     * @throws Exception
     */
    public void terminateInstance () throws Exception {
        // see if the current state of instance is suspended before
        // issuing terminate call. The actual resetting of the count
        // is done after issuing the terminate command.
        boolean isSuspended = mInstance.isSuspended();
    	mInstance.doTerminate(BPELProcessInstance.TerminationReason.ForcedTermination);
    	// if the previous state is not suspended then it is assumed that it
    	// is in running state. 
    	// NOTE: bug fix: 2597 this has to be revisited since the management of the instance
    	// state's count was not addressed properly in the initial event notification design
    	if (isSuspended) {
    	    mInstance.getBPELProcessManager().getPerformanceManger()
    	    .updateSuspendedCountOnTerminate();
    	} else {
    	    mInstance.getBPELProcessManager().getPerformanceManger()
    	    .updateRunningCountOnTerminate();
    	}
    }
    
	/**
	 * 
	 * @param uid
	 * @return
	 */
	public boolean generateEventsForVariable(long uid) {
		// DEVNOTE: For variable events we do not take into account if KPI is enabled or not. This is because today
		// KPI events ignore variable events anyway. If this changes (not sure why it would) this logic will need to 
		// be changed to take KPI into account. See <code>postVariableEvent()</code> also.
		return  (mGenerateEvents && mEngine.isMonitorEnabled() && mEngine.isVariableMonitorEnabled() &&
				(mEventsFilter == null || mEventsFilter.generateEventsForVariable(uid)));
	}
	
	/**
	 * 
	 * @param uid
	 * @return
	 */
	public boolean generateEventsForActivity(ActivityUnit unit) {
		long uid = unit.getStaticModelActivity().getUniqueId();
		return (generateEventsForProcess() && (mEventsFilter == null || mEventsFilter.generateEventsForActivity(uid)));
	}

	
	/**
	 * 
	 * @return
	 */
	public boolean generateEventsForProcess() {
		return (mGenerateEvents && (mEngine.isMonitorEnabled() || mEngine.isKPIEnabled()));
	}

        public boolean generateMonitorEventsForProcess() {
		return (mGenerateEvents && mEngine.isMonitorEnabled());
	}
    
	/*
	 * 
	 */
	private void processAndFilterVariables(Map<VariableEvent.VariableType, List<Variable>> varMap) 
	{
		List<VariableEvent.VariableType> varTypeToRemoveList = new ArrayList<VariableEvent.VariableType>();
		List<Variable> varToRemoveList = new ArrayList<Variable>();
		for (Map.Entry<VariableEvent.VariableType,  List<Variable>> entry : varMap.entrySet()) {
			List<Variable> vars = entry.getValue();
			for (Variable var : vars) {
				long uid = var.getVarId();
				if (mEventsFilter == null || mEventsFilter.generateEventsForVariable(uid) || uid == FAULT_VAR_ID) {
					if (var.getVariableScope() != null) {
						mVariableScopeMap.put(uid, var.getVariableScope());
					}
				} else {
					varToRemoveList.add(var);
				}
			}
			if (!varToRemoveList.isEmpty()) {
				vars.removeAll(varToRemoveList);
				varToRemoveList.clear();
				if (vars.isEmpty()) {
					varTypeToRemoveList.add(entry.getKey());
				}
			}
		}
		if (!varTypeToRemoveList.isEmpty()) {
			for (VariableEvent.VariableType varType : varTypeToRemoveList) {
				varMap.remove(varType);
			}
		}
	}
}
