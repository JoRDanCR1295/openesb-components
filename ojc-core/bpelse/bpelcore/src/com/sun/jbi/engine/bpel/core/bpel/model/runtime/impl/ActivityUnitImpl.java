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
 * @(#)ActivityUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.bpel.model.Scope;
import com.sun.bpel.model.meta.RActivity;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityContainerUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.CompensateContinuation;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;


/**
 * Activity unit implementation
 *
 * @author Sun Microsystems
 */
public abstract class ActivityUnitImpl implements ActivityUnit {
    /** runtime activity */
    private static final Logger LOGGER = Logger.getLogger(ActivityUnitImpl.class.getName());
    
    private static final DummyActivityUnit DUMMY_ACT_UNIT = new DummyActivityUnit();
    protected RActivity mAct = null;
    private ActivityUnit mNextActUnit = null;
    private ActivityUnit mEncActUnit = null;
    
    /* 
     * mContext refers to the parent context of this activity
     * NOTE: for activities that are context by itself(ex: invoke, scope), use the
     * the getContext() api for cases where the activity own 
     * context is required.
    */
    protected Context mContext;
    private Unit mEncUnit = null;

    /** End. Unit Interface related API */
    /** Persistence related functionality. */
    protected long mBranchId;
    protected Fault mFault;
    
    //protected long mIterationCount = -1;
    
    /**
     * Creates a new ActivityUnitImpl object.
     */
    protected ActivityUnitImpl() {
    }

    /**
     * constructor
     *
     * @param parentActUnit parent activity unit
     * @param act activity
     * @param branchId used in persistence. Even though it is for persistence, it is always  set.
     *        This option even though wastes some space is opted under the assumption that it
     *        would be better performance compared to the code where it needs  to check if the
     *        persistence is enabled and only then set /assign the value.
     */
    public ActivityUnitImpl(Context context, Unit parentActUnit, RActivity act, long branchId) {
        this.mContext = context;
        mAct = act;
        mEncUnit = parentActUnit;

        if (parentActUnit instanceof ActivityUnit) {
            mEncActUnit = (ActivityUnit) parentActUnit;
        }

        mBranchId = branchId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#getStaticModelActivity()
     */
    public RActivity getStaticModelActivity() {
        return mAct;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#getEnclosingActivityUnit()
     */
    public ActivityUnit getEnclosingActivityUnit() {
        return mEncActUnit;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#getNextActivityUnit()
     */
    public ActivityUnit getNextActivityUnit() {
        if (mNextActUnit instanceof DummyActivityUnit) {
            return null;
        }

        if (mNextActUnit != null) {
            return mNextActUnit;
        }

        RActivity act = mAct.getNextActivity();

        if (act == null) {
            mNextActUnit = createDummyActivityUnit();

            return null;
        } else {
            ActivityUnit actUnit = ActivityUnitFactory.getInstance().createActivityUnit(this.mContext,
                    getEnclosingActivityUnit(), act, getBranchId()
                );
            mNextActUnit = actUnit;
        }

        return mNextActUnit;
    }
    
    public void setCompletedActivityState() {
    	mNextActUnit = createDummyActivityUnit();
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#getPrevActivityUnit()
     */
    public ActivityUnit getPrevActivityUnit() {
        /*
           if (mPrevActUnit instanceof DummyActivityUnit) {
               return null;
           }
           if (mPrevActUnit != null) {
               return mPrevActUnit;
           }
           Activity act = getActivity(mAct.getPrevSiblingTree());
           if (act == null) {
               mPrevActUnit = createDummyActivityUnit();
               return null;
           } else {
               ActivityUnit actUnit = ActivityUnitFactory.getInstance().createActivityUnit(null, act);
               mPrevActUnit = actUnit;
           }
           return mPrevActUnit;
         */
        return null;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doActionOnRecovery(RecoveredCallFrame,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     *      com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects)
     */
    public boolean doActionOnRecovery(
        RecoveredCallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {
        
    	frame.convert();
    	
        if (frame.getPC() != null) {
        	if (LOGGER.isLoggable(Level.FINE)) {
        		LOGGER.log(Level.FINE, 
        				I18n.loc("BPCOR-3070: Recovery started from activity : {0}", frame.getPC().getName()));
        	}
        }
        
		if(mContext.getFaultHandlingContext().isBeingTerminated()) {
			frame.onActivityTerminate(this);
		}

        return doPassControlToParent(frame, bpit, rObjs);
    }

//    protected void doFaultProcessingAfterRecovery(ICallFrame frame, 
//            BusinessProcessInstanceThread bpit, RequiredObjects rObjs) {
//        QName faultName = mFault.getName();
//        WSMessage faultData = mFault.getData();
// 
//        // TODO a temporary fixed.
//        QName wsdlMsgTypeAttrQName = null;
//        if (faultData != null) {
//            Element docElement = faultData.getElement();
//            String wsdlMsgTypeAttr = docElement.getAttribute("type");
//            wsdlMsgTypeAttrQName = QName.valueOf(wsdlMsgTypeAttr);
//        }
//        
//        frame.onFault(this, faultName, wsdlMsgTypeAttrQName, faultData);
//        frame.getProcessInstance().getMonitorMgr().postActivityFaultedEvent(this, rObjs, frame.getBPId());    
//        frame.getProcessInstance().getMonitorMgr().postEventForFault(this, rObjs, frame.getBPId(), mFault);
//
//        mContext.getFaultHandlingContext().notifyFault(mFault, frame, bpit, rObjs);
//    }
    
    /**
     * continue execution with the enclosed activityunit. This is called when an event is consumed
     * by an activity waiting for that. In other words, it is used to resume the execution from
     * where it has stopped. This compliments and supplements doAction() API.
     *
     * @param frame callframe
     * @param bpit bpel process instance thread
     * @param rObjs required objects
     *
     * @return boolean whether enclosed activity unit is completed
     *
     * @throws Exception DOCUMENT ME!
     *
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#doAction(ICallFrame,
     *      BusinessProcessInstanceThread, RequiredObjects)
     */
    protected boolean doPassControlToParent(
        ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception {
    	
    	if(!mContext.getFaultHandlingContext().isBeingTerminated()){
    		BPELTraceManager.getInstance().doTraceOnComplete(mAct, mContext, frame.getProcessInstance());
		}
    	
        Unit enclosingUnit = this.getEnclosingUnit();
        if (enclosingUnit != null) {
            if (enclosingUnit instanceof StructuredActivityUnitImpl) {
                return ((StructuredActivityUnitImpl) enclosingUnit).doResumeAction(
                    this, frame, bpit, rObjs
                );
            } else if (enclosingUnit instanceof ActivityContainerUnit) {
                return ((ActivityContainerUnit) enclosingUnit).doResumeAction(this, frame, bpit, rObjs);
            } else if (enclosingUnit instanceof CompensateContinuation) {
            	return ((CompensateContinuation) enclosingUnit).doResumeAction(this, frame, bpit, rObjs);
            }
        }

        //We have reached the process level. Simply return true
        //since the process instance is now complete
        //TODO: Some cleanup may be required. This code
        //will be similar to that in the  handleFault in the 
        //process instance
        return true;
    }

    private DummyActivityUnit createDummyActivityUnit() {
        return DUMMY_ACT_UNIT;
    }

    /**
     * Start. Unit Interface related API
     *
     * @return ActivityUnit enclosing scope activity unit
     */
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.Unit#getEnclosingScopeUnit()
     */
    public ActivityUnit getEnclosingScopeUnit() {
        Unit unit = getEnclosingUnit();

        if (unit == null) {
            return null;
        }

        if (
            unit instanceof ActivityUnit && unit instanceof ActivityUnitImpl &&
                ((ActivityUnitImpl) unit).mAct instanceof Scope
        ) {
            return (ActivityUnit) unit;
        } else {
            return unit.getEnclosingScopeUnit();
        }
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.Unit#getEnclosingUnit()
     */
    public Unit getEnclosingUnit() {
        return mEncUnit;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.Unit#doAction(
     *      com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame,
     *      com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread,
     *      com.sun.jbi.engine.bpel.core.bpel.model.RequiredObjects)
     */
    public abstract boolean doAction(
        ICallFrame frame, BusinessProcessInstanceThread bpit, RequiredObjects rObjs
    ) throws Exception;

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.ActivityUnit#getBranchId()
     */
    public long getBranchId() {
        return mBranchId;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit#getContext()
     */
    public Context getContext() {
        return mContext;
    }
  
}
