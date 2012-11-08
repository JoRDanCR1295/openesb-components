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
 * @(#)RecoveryStateInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.dbo.EventHandlerDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.LastCheckPointDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.PartnerLinkDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.ScopeDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.StateDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.VariableDBO;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.BPELInterpreter;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState;

/**
 * Data structure to hold necessary information to construct 
 * <code>ICallFrame</code> instances during recovery.
 * 
 * @author Kevan Simpson
 */
public class RecoveryStateInfo implements RecoveredState {
	
    private BPELProcessManager mProcessManager = null;
    private Engine mEngine = null;
    private String mBpelId = null;
    private List<LastCheckPointDBO> mLastCheckpointDBOs = null;
    private List<ScopeDBO> mScopeDBOs = null;
    private List<ForEachDBO> mForEachDBOs = null;
    private List<VariableDBO> mVariableDBOs = null;
    private List<EventHandlerDBO> mEvntDBOs = null;
    private List<PartnerLinkDBO> mPLinkDBOs = null;
    
    private Map<ICallFrame, List<ICallFrame>> mParentChildMap = 
            new HashMap<ICallFrame, List<ICallFrame>>();
    
    private long[] mCheckpointIds = null;
    private long[] mForEachIds = null;
    private long[] mPartnerLinkIds = null;
    private String mStatus = StateDBO.RUNNING_STATUS;
    private BPELProcessInstance mProcessInstance = null;
    private Map<String, List<EventHandlerDBO>> mScopeToEventDBOMap = new HashMap<String, List<EventHandlerDBO>>();
    
    private ICallFrame mRootCF = null;
    private List<ICallFrame> mCFList = new ArrayList<ICallFrame>();
    private List<ICallFrame> parentCFsWithCheckPointedChildCFs = new ArrayList<ICallFrame>();
    private int countRecoveredCF = 0;
    /** call frames associated with uninitialized onEvents and onAlarms  */
    private List<ICallFrame> mUninitializedEHCallFrames = new ArrayList<ICallFrame>();
    
    //Map of scope activity unique id (of the form 1000002) to the list of matching scope DBOs. 
    private Map<String, List<ScopeDBO>> mScopes = new HashMap <String, List <ScopeDBO>> ();
    
    //Map of the scopeId to the associated VariablesDBOs. ScopeId is of the form 1000002,3
    private Map<String, VariableDBO> mVariables = new HashMap <String, VariableDBO> ();
    private Map<String, PartnerLinkDBO> mPartnerLinks = new HashMap <String, PartnerLinkDBO> ();
    
    private static final LCPDBOComparator LCPDBO_COMPARATOR = new LCPDBOComparator(); 
    private static final ForEachComparator FE_COMPARATOR = new ForEachComparator(); 
    
    public RecoveryStateInfo(BPELProcessManager processManager,
                             Engine eng,
                             String bpelId,
                             List lcpDBOs,
                             List scopeDBOs,
                             List foreachDBOs,
                             List varDBOs, List<EventHandlerDBO> evntDBOs,
                             List<PartnerLinkDBO> pLinkDBOs,
                             String status) {
        mProcessManager = processManager;
        mEngine = eng;
        mBpelId = bpelId;
        mLastCheckpointDBOs = lcpDBOs;
        mScopeDBOs = scopeDBOs;
        mForEachDBOs = foreachDBOs;
        mVariableDBOs = varDBOs;
        mEvntDBOs = evntDBOs;
        mPLinkDBOs = pLinkDBOs;
        mStatus = status;
        init();
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#addCallFrame(com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame) */
    public void addCallFrame(ICallFrame frame) {
        if (frame.getParent() == null) mRootCF = frame;

        if (findCheckpoint(frame.getProgramCounter().getStaticModelActivity()
				.getUniqueId()) != null) {
			countRecoveredCF++;
		}

        ICallFrame parentCF = frame.getParent();
        if (parentCF != null) {
        	List<ICallFrame> childCfs = mParentChildMap.get(parentCF);
        	if (childCfs == null) {
        		//only add parentCF if it was not added before, so it needs to be added if check passes
        		//or need to use Set
        		parentCFsWithCheckPointedChildCFs.add(parentCF);
        		childCfs = new ArrayList<ICallFrame>();
        		mParentChildMap.put(parentCF, childCfs);
        	}
            childCfs.add(frame);
        }

        mCFList.add(frame);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#containsCheckpoint(long) */
    public boolean containsCheckpoint(long actId) {
        return (Arrays.binarySearch(mCheckpointIds, actId) >= 0);
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#continueTraversing() */
    public boolean continueTraversing() {
		return (countRecoveredCF < mCheckpointIds.length);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#findCheckpoint(long) */
    public LastCheckPointDBO findCheckpoint(long actId) {
        int pos = Arrays.binarySearch(mCheckpointIds, actId);
        return (pos < 0) ? null : mLastCheckpointDBOs.get(pos);
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#findEventhHandlers(String)
     */
    public List<EventHandlerDBO> findEventhHandlers(String scopeGuid) {
        return mScopeToEventDBOMap.get(scopeGuid);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#findForEach(long) */
    public ForEachDBO findForEach(long actId) {
        int pos = Arrays.binarySearch(mForEachIds, actId);
        return (pos < 0) ? null : mForEachDBOs.get(pos);
    }
    
    private PartnerLinkDBO findPartnerLink(long pLinkId) {
        int pos = Arrays.binarySearch(mPartnerLinkIds, pLinkId);
        return (pos < 0) ? null : mPLinkDBOs.get(pos);
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#getCallFrames() */
    public List<ICallFrame> getCallFrames() { 
        return mCFList; 
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#getEngine() */
    public Engine getEngine() { return mEngine; }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#getParentCFsWithCheckPointedChildCFs() */
    public List<ICallFrame> getParentCFsWithCheckPointedChildCFs() { return parentCFsWithCheckPointedChildCFs; }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#getForEaches() */
    public List<ForEachDBO> getForEaches() {
        return new ArrayList<ForEachDBO>(mForEachDBOs);
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#getInitiatedChildBranches(com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame) */
    public List<ICallFrame> getInitiatedChildBranches(ICallFrame frame) {
        return mParentChildMap.get(frame);
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#getInterpreter() */
    public BPELInterpreter getInterpreter() { return mEngine.getInterpreter(); }
    
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#getProcessDef() */
    public RBPELProcess getProcessDef() { 
        return mProcessManager.getBPELProcess(); 
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#getProcessInstance() */
    public BPELProcessInstance getProcessInstance() { return mProcessInstance; }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#getRootCallFrame() */
    public ICallFrame getRootCallFrame() { return mRootCF; }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#hasCheckpoints() */
    public boolean hasCheckpoints() {
        return (mLastCheckpointDBOs != null && mLastCheckpointDBOs.size() > 0);
    }
    
    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#getUninitializedEHCallFrames()
     */
    public List<ICallFrame> getUninitializedEHCallFrames() {
        return mUninitializedEHCallFrames;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#addUninitializedEHCallFrames(ICallFrame)
     */
    public void addUninitializedEHCallFrames(ICallFrame callFrame) {
        mUninitializedEHCallFrames.add(callFrame);
    }

    private void init() {
        // sort checkpoints and foreach
        Collections.sort(mLastCheckpointDBOs, LCPDBO_COMPARATOR);
        Collections.sort(mForEachDBOs, FE_COMPARATOR);
        
        // initialize id arrays
        mCheckpointIds = new long[mLastCheckpointDBOs.size()];
        for (int i = 0, size = mLastCheckpointDBOs.size(); i < size; i++) {
            mCheckpointIds[i] = mLastCheckpointDBOs.get(i).getActivityId();
        }
        
        for (ScopeDBO scopeDBO: mScopeDBOs){
        	
        	if(scopeDBO.getScopeState().equals(FaultHandlingContext.DONE)){
        		continue;
        	}
        	
        	String scopeInstancesKey = null;
        	
        	if(scopeDBO.getScopeId() == RBPELProcess.DEFAULT_PROCESS_SCOPE_ID) {
        		scopeInstancesKey = String.valueOf(scopeDBO.getScopeId());  		
        	} else {
        		scopeInstancesKey = scopeDBO.getScopeId() + IDS_SEPARATOR + scopeDBO.getParentScopeGuid();
        	}

    		List <ScopeDBO> scopesList = mScopes.get(scopeInstancesKey);
    		if (scopesList == null) {
    			scopesList = new ArrayList <ScopeDBO> ();
    			mScopes.put(scopeInstancesKey, scopesList);
    		}
    		scopesList.add(scopeDBO);
        }
        
        for (VariableDBO variableDBO: mVariableDBOs) {
        	mVariables.put(variableDBO.getId() + IDS_SEPARATOR + variableDBO.getScopeGuid(), variableDBO);
        }
        
        for (PartnerLinkDBO pLinkDBO: mPLinkDBOs) {
            mPartnerLinks.put(pLinkDBO.getId() + IDS_SEPARATOR + pLinkDBO.getScopeGuid(), pLinkDBO);
        }
        
        mForEachIds = new long[mForEachDBOs.size()];
        for (int i = 0, size = mForEachDBOs.size(); i < size; i++) {
            mForEachIds[i] = mForEachDBOs.get(i).getForEachId();
        }
        
        mPartnerLinkIds = new long[mPLinkDBOs.size()];
        for (int i = 0, size = mPLinkDBOs.size(); i < size; i++) {
            mPartnerLinkIds[i] = mPLinkDBOs.get(i).getId();
        }
        
        List<EventHandlerDBO> tempList;
        EventHandlerDBO eveDBO; 
        for (int i = 0, size = mEvntDBOs.size(); i < size; i++) {
            eveDBO = mEvntDBOs.get(i);
            String scopeGuid = eveDBO.getScopeGuid();
            tempList = mScopeToEventDBOMap.get(scopeGuid);
            if (tempList == null) {
                tempList = new ArrayList<EventHandlerDBO>();
                mScopeToEventDBOMap.put(scopeGuid, tempList);
            }
            tempList.add(eveDBO);
        }
        
        // create process instance
        mProcessInstance = mProcessManager.createBPInstance(mBpelId);
        if (mStatus.equals(StateDBO.SUSPENDED_STATUS)) {
        	mProcessInstance.setSuspended();
        }
    }
    
    public boolean canScheduleRecoveredEventFrame(String onEventEHId) {
        int count = 0;
        for (LastCheckPointDBO lcp : mLastCheckpointDBOs) {
            if (onEventEHId.equals(lcp.getBPId())) {
                count++;
                if (count == 2) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
	 * @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#getScopeDBOs(String)
	 */
	public List<ScopeDBO> getScopeDBOs(String scopeInstancesKey) {
		List <ScopeDBO> scopeDBOs = mScopes.get(scopeInstancesKey);
		if(scopeDBOs != null) {
			return scopeDBOs;
		} else {
			return null;
		}
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#getVariables()
	 */
	public Map<String, VariableDBO> getVariables() {
		return mVariables;
	}
    
	/** @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#getPartnerLinks()
     */
    public Map<String, PartnerLinkDBO> getPartnerLinks() {
        return mPartnerLinks;
    }


    /**
	 * @see com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState#isEHAssociatedScopeComplete(String)
	 */
	public boolean isEHAssociatedScopeComplete(String ehId) {
		for(ScopeDBO scopeDBO: mScopeDBOs){
			if(scopeDBO.getBPId().equals(ehId) 
					&& scopeDBO.getScopeState().equals(FaultHandlingContext.COMPLETED)){
				return true;
			}
		}
		return false;
	}

	private static class LCPDBOComparator implements Comparator {
        /** @see java.util.Comparator#compare(java.lang.Object, java.lang.Object) */
        public int compare(Object o1, Object o2) {
            LastCheckPointDBO obj1 = (LastCheckPointDBO) o1;
            LastCheckPointDBO obj2 = (LastCheckPointDBO) o2;

            return (new Long(obj1.getActivityId()).compareTo(new Long(obj2.getActivityId())));
        }
    }
    
    private static class ForEachComparator implements Comparator {
        /** @see java.util.Comparator#compare(java.lang.Object, java.lang.Object) */
        public int compare(Object o1, Object o2) {
            ForEachDBO obj1 = (ForEachDBO) o1;
            ForEachDBO obj2 = (ForEachDBO) o2;

            return (new Long(obj1.getForEachId()).compareTo(new Long(obj2.getForEachId())));
        }
    }

    private static class PartnerLinkComparator implements Comparator {
        /** @see java.util.Comparator#compare(java.lang.Object, java.lang.Object) */
        public int compare(Object o1, Object o2) {
            PartnerLinkDBO obj1 = (PartnerLinkDBO) o1;
            PartnerLinkDBO obj2 = (PartnerLinkDBO) o2;

            return (new Long(obj1.getId()).compareTo(new Long(obj2.getId())));
        }
    }
}
