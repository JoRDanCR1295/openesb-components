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
 * @(#)FlowUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.ArrayList;
import java.util.List;

import com.sun.bpel.model.Flow;
import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FlowUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;


/**
 * Flow activity unit implementation
 *
 * @author Sun Microsystems
 */
public class FlowUnitImpl extends StructuredActivityUnitImpl implements FlowUnit {
    private static final int INIT_COUNT_VAL = 0;
    private boolean mWaitingForBranchesToJoin = false;
    private int mCount = INIT_COUNT_VAL;
    private int mMergedFramesCount = INIT_COUNT_VAL;
    private List mFlowFrames;
    private boolean mIsRecoveredUnit = false;
    /**
     * Creates a new FlowUnitImpl object.
     *
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId branch ID
     */
    public FlowUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
        super(context, parentActUnit, act, branchId);
        mCount = ((Flow) mAct).getActivities().size();
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.impl.ActivityUnitImpl#doAction(
     *      com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     *      com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects)
     */
    public boolean doAction(ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects rObjs) throws Exception {
    	
        if (mWaitingForBranchesToJoin) {
        	
        	if(mContext.getProcessInstance().isExiting()){
            	frame.onActivityTerminate(this);
        		return doPassControlToParent(frame, bpit, rObjs);
        	}
        	
        	//Do persistence state management since all child branches have
        	// joined back.
            exitFlowUpdateState(rObjs);

            //All the branches of the flow have joined back. 
        	//Check if the flow if being terminated.
        	if (mContext.getFaultHandlingContext().isBeingTerminated()) {
            	frame.onActivityTerminate(this);
            	return super.doPassControlToParent(frame, bpit, rObjs);
            	
            } else {
            	 //The flow has completed successfully. Pass control to parent
                frame.onActivityComplete(this);
                frame.getProcessInstance().getMonitorMgr().postActivityCompleteEvent(this);
            	
                return super.doPassControlToParent(frame, bpit, rObjs);
            }
            
        } else {
            frame.setProgramCounter(this);
            frame.onLineChange(this);
            BPELTraceManager.getInstance().doTraceOnStart(mAct, mContext, frame.getProcessInstance());
            frame.getProcessInstance().getMonitorMgr().postActivityStartEvent(this);    	
            
            mFlowFrames = new ArrayList();
            List branchIds = new ArrayList();
            RActivity childAct = ((RActivityHolder) mAct).getChildActivity();

            while (childAct != null) {
            	long branchId = childAct.getUniqueId();
            	branchIds.add(new Long(branchId));
            	
            	ActivityUnit childActUnit = ActivityUnitFactory.getInstance().
            								createActivityUnit(mContext, this, childAct, branchId);
            	ICallFrame childFrame = rObjs.getInterp().createCallFrame(frame, childActUnit, mContext);
            	mFlowFrames.add(childFrame);
            	childAct = childAct.getNextActivity();
            }

            mWaitingForBranchesToJoin = true;
            frame.getProcessInstance().getPersistenctMgr().updateState(
            		this, branchIds, mContext.getStateContext().getState(), mIsRecoveredUnit, 
            		frame.getBranchInvokeCounter());
            
    		/* Scalability related -
    		 * since we added one less branch, we will decrement one less. The
			 * logic here is that, say if a flow branches off into two branches,
			 * effectively, there are two total branches now, not 3 hence one
			 * less of total flow branches are added and subtracted.
			 */
            frame.getProcessInstance().incrementActiveBranchCount(mCount - 1);

            if (rObjs.getBPELProcessManager().isBPAtomic() && (!frame.isEventHandlerCF())) {
            	// If the BP is atomic, put only the one of the flow frames on the ready to run queue.
            	// the remaining will be added one after the other as the previous completes.
            	rObjs.getBPELProcessManager().addToReadyToRunQueue((ICallFrame) mFlowFrames.get(mMergedFramesCount)); 
            } else {
            	rObjs.getBPELProcessManager().addToReadyToRunQueueForFlow(mFlowFrames);
            }
            
            return false;
        }
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.StructuredActivityUnitImpl#doResumeAction(com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit, com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame, com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread, com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects)
     */
    protected boolean doResumeAction(ActivityUnit childActUnit, ICallFrame frame, BusinessProcessInstanceThread bpit, 
    		RequiredObjects rObjs) throws Exception {
    	
   	   /* A child branch has either completed successfully, or one of the activities 
   	 	* has either faulted or terminated. 
    	* 
    	* Note that not all branches will end by calling the doResumeAction(). Some 
    	* directly end in the BPELInterpreter's execute() method. Consider a branch 
    	* that has only assign or the empty activity. Those activies, once they finish, 
    	* will return true and the method call will end in the BPELInterpreter's execute() 
    	* method. So we return true from here, so that these kind of branches (that call 
    	* doResumeAction()) can also use the same mechanism to join the parent Flow.    	
    	*
    	* Return true to indicate to the BPELInterpreter that the branch has completed 
    	* and so that it can make the branch join with the parent Flow.
    	*/
    	return true;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.FlowUnit#join(ICallFrame,
     *      com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects)
     */
    public synchronized void join(ICallFrame frame, RequiredObjects rObjs, ICallFrame joiningChildFrame) {
    	//If all the branches have already joined, throw an exception
    	if(mMergedFramesCount == mCount) {
    		throw new IllegalStateException();
    	}
    	
    	// Check the value of the branchInvokeCounter with the joiningChildFrame, replace the value 
    	// with the child's branchInvokeCounter value if its greater than self. This way the value 
    	// will be set to the highest value among all the child branch CallFrames. This path remains
    	// same for the RecoveredCallFrame's.
    	long childCounterValue = joiningChildFrame.getBranchInvokeCounter();
    	if (childCounterValue > frame.getBranchInvokeCounter()) {
    		frame.setBranchInvokeCounter(joiningChildFrame.getBranchInvokeCounter());
    	}
    	
    	//Update the branch count
    	mMergedFramesCount++;

    	if (mMergedFramesCount < mCount) {
    		/* Scalability related -
    		 * since we added one less branch, we will decrement one less. The
    		 * logic here is that, say if a flow branches off into two branches,
    		 * effectively, there are two total branches now, not 3 hence one
    		 * less of total flow branches are added and subtracted.
    		 */
    		frame.getProcessInstance().decrementActiveBranchCount(1);
    	}

    	if(mMergedFramesCount == mCount) {
    		//All the flow branches have joined. Resume the execution of the 
    		//parent flow (which is this unit).
    		rObjs.getBPELProcessManager().addToReadyToRunQueue((ICallFrame) frame);
    	} else if (rObjs.getBPELProcessManager().isBPAtomic() && (!frame.isEventHandlerCF())) {
    		rObjs.getBPELProcessManager().addToReadyToRunQueue((ICallFrame) mFlowFrames.get(mMergedFramesCount));
    	}
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.impl.StructuredActivityUnitImpl#getChildActivity()
     */
    protected RActivity getChildActivity() {
        throw new RuntimeException(I18n.loc("BPCOR-3024: Invalid method call {0}", getClass().getName()));
    }

    private void exitFlowUpdateState(RequiredObjects rObjs) {
        if (rObjs.getBPELProcessManager().isPersistenceEnabled()) {
            MutableState state = mContext.getStateContext().getState();
            state.exitFlow(mAct.getUniqueId(), getBranchId());
        }
    }
    
    public boolean doActionOnRecovery(RecoveredCallFrame frame,
                                      BusinessProcessInstanceThread bpit,
                                      RequiredObjects rObjs) throws Exception {
        if (frame.hasPersistedChildCF()) {
        	frame.convert();
        	//TODO: This is same as doAction's block where mWaitingForBranchesToJoin is true
        	//We could avoid this by simply doing a convert for a reconstructed call frame 
        	//when it is created, or one of the child call frames could do that
        	mWaitingForBranchesToJoin = true;
        	return doAction(frame, bpit, rObjs);
        } else {
            frame.convert();
            
        	//Check if the flow if being terminated.
        	if (mContext.getFaultHandlingContext().isBeingTerminated()) {
                //TODO: Do we need to call exitFlowUpdateState here? 
        		//exitFlowUpdateState(rObjs);
            	frame.onActivityTerminate(this);
            	return doPassControlToParent(frame, bpit, rObjs);
            }
        	mIsRecoveredUnit = true;
            return doAction(frame, bpit, rObjs);
        }
    }
    
}
