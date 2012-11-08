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
 * @(#)BPELInterpreter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Message;
import javax.xml.namespace.QName;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.SystemFault;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.exception.POJOException;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;
import com.sun.jbi.engine.bpel.core.bpel.exception.SystemException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnitFactory;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FlowUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Unit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.BPELProcessInstanceImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.FaultImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.RequiredObjectsImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;

/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class BPELInterpreter {

    /** Log handle */
    private static final Logger LOGGER = Logger.getLogger(BPELInterpreter.class.getName());

    /** Engine */
    Engine mEng;

    /**
     * Creates a new BPELInterpreter object.
     *
     * @param eng DOCUMENT ME!
     */
    public BPELInterpreter(Engine eng) {
        mEng = eng;
    }

    /**
     * DOCUMENT ME!
     *
     * @param bp business process
     * @param actUnit TODO
     * @return the newly created callFrame
     */
    public ICallFrame createCallFrame(RBPELProcess bp, BPELProcessInstance instance,
            ActivityUnit actUnit, Context context) {

    	// last argument "branchInvokeCounter" for a new callframe is 0
    	ICallFrame frame = new CallFrame(bp, this, instance, actUnit, 0);
    	if(context != null) {
        	context.getFaultHandlingContext().registerCallFrame(frame);
    	}
        return frame;
    }

    /**
     * This is called only for creating the child branches of the Flow activity.
     * In the forward path this is called from the FlowUnitImpl.doAction() while creating 
     * the child branches CallFrame's.
     * In the recovery logic this is called from the CallFrameFactory.createUninitiatedCFs(Flow flow..)
     * to created only the uninitialized branches of the Flow activity that does not have an entry 
     * in the LastCheckPoint table. 
     * In both these cases we have to set the value of the branchInvokeCounter for the newly created
     * CallFrame to its parent branchInvokeCounter value. 
     * In the recovery path the logic does not call this method to create the CallFrames for the 
     * child branches of the Flow activity that have entries in the LastCheckPoint table (ie these 
     * branches had persistence points).
     *
     * @param parent parent CallFrame.
     * @param actUnit the program counter value.
     *
     * @return CallFrame for the child branch.
     */
    public ICallFrame createCallFrame(ICallFrame parent, ActivityUnit actUnit, 
    		Context context) {
        // since this a Flow branch CallFrame, set the parent's branchInvokeCounter value
        // to the child CallFrame.
        long parentCounter = parent.getBranchInvokeCounter();

    	ICallFrame frame = new CallFrame((ICallFrame) parent, this, actUnit, parentCounter);
    	context.getFaultHandlingContext().registerCallFrame(frame);
    	return frame;
    }

    /**
     * @param bpit
     * @param processManager
     */
    public void execute(BusinessProcessInstanceThread bpit, BPELProcessManager processManager) {
    	ICallFrame frame = bpit.getCallFrame();

    	//TODO: When is this true? Should never be. Need to review and remove if required
    	//or fix the original problem
    	if (frame == null) {
    		return;
    	}

    	ActivityUnit actUnit = frame.getProgramCounter();
    	RequiredObjects rObjs = new RequiredObjectsImpl(mEng, this, processManager);

    	try {
    		boolean actionCompleted = false;

    		if (actUnit != null) {
    			if (frame.isRecovered()) {
    				if (frame.getProcessInstance().getMonitorMgr().checkInstanceSuspended(bpit)) {
    					return;
    				}
    				actionCompleted = actUnit.doActionOnRecovery((RecoveredCallFrame) frame, bpit, rObjs);
    			} else {
    				if (frame.getProcessInstance().getMonitorMgr().checkInstanceSuspended(bpit)) {
    					return;
    				}        			
    				actionCompleted = actUnit.doAction(frame, bpit, rObjs);
    			}

    			if (actionCompleted) {
    				if (frame.isFlowBranchCF()) {
    					ICallFrame parentFrame = frame.getParent();
    					FlowUnit flowUnit = (FlowUnit) parentFrame.getProgramCounter();
    					//TODO: DEVNOTE: The flowUnit should not be null. However, in the current fault 
    					//handling logic, in certain cases the flow unit shows up null here. This is a 
    					//known issue and will be addressed in the updated fault handling code. 
    					if (null == flowUnit) {
    						//The instance is passivated and cleaned up from memory. Hence return from here
    						if (frame.getProcessInstance().getClusterMgr().isPassivated()) {
    							return;
    						}	
    					} else {
    						flowUnit.join(parentFrame, rObjs, frame);
    					}
    				}
    				frame.onExit();
    			}
    		}
    	} catch (Exception e) {

    		// Check if this is a JUnit thread which is interrupted to simulate a crash.
    		if (!(e instanceof StandardException) && Thread.currentThread().isInterrupted() 
    				&& Thread.currentThread().getName().startsWith("BPELSE_JUNIT_THREAD")) {
    			throw new RuntimeException("JUNIT_RELATED_CRASH_SIMULATION");
    		}

    		// No further processing required since engine is stopping. This currently means that 
    		// all processing/threads should be suspended ASAP. Note that we do not need to worry 
    		// about the engine being in shutdown state since the engine needs to be stopped before 
    		// being shutdown. Stopping the engine is a blocking call since the engine waits for all 
    		// threads to return before completing the stop cycle. In short, we should never encounter 
    		// the Engine.SHUTDOWN if we are here.
    		if (mEng.getState() == Engine.STOPPED) {
    			return;
    		}

    		try {

    			// Check if the process instance is in an infinite loop and throwing exceptions
    			// TODO: Do we need to do this for StandardException also?
    			if (!(e instanceof StandardException) && 
    					((BPELProcessInstanceImpl) frame.getProcessInstance()).isInfiniteFaultLoop(frame.getProgramCounter())) {
                    throw new Exception(I18n.loc("BPCOR-6015: An infinite loop was detected in a process instance; terminating the associated instance"));

    			}

    			// This will ALWAYS return false. Before returning false, the VirtualThrowUnit will put
    			// itself in the ReadyToRunQueue. So VirtualThrowUnit is scheduled again from the
    			// ReadyToRunQueue and its doAction is called from the BPELInterpreter's execute method,
    			// as is done for any other scheduled activity unit. Therefore, there is no need to do
    			// anything here.
    			RBPELProcess processDef = frame.getProcess();
    			Unit unit = createVirtualFaultUnit(e, frame.getProgramCounter(), processDef);
    			unit.doAction(frame, null, rObjs);

    		} catch (Exception e1) {

    			if (mEng.getState() == Engine.STOPPED) {
    				return;
    			}

    			// TODO: Need to decide what to do if the VirtualThrowUnit throws an exception when called the 
    			// first time. Perhaps we should terminate the process instance at this time since something 
    			// fatal has happened. That would mean calling notifyExit() or its equivalent.
    			// For now we will go with the original logic. Once we finalize the approach we should change this.
    			// Note that sendErrorsForPendingRequests at process instance level will not send errors for
    			// outstanding requests at the nested scope levels.
    			LOGGER.log(Level.SEVERE, I18n.loc("BPCOR-7129: A fatal exception occurred while trying to convert an exception to a fault and handling it. Attempting to terminate the process instance."), e1);
    			LOGGER.log(Level.SEVERE, I18n.loc("BPCOR-7130: The original exception was: "), e);    			
    			((IMAScope) frame.getProcessInstance()).sendErrorsForPendingRequests(e);
    			processManager.terminate(frame.getProcessInstance());
    		}        	
    	}
    }
        
    private Unit createVirtualFaultUnit(Exception e, ActivityUnit activityUnit, RBPELProcess processDef){

    	QName faultName = null;
    	WSMessage faultData = null;
        Exception newE = null;
        Message msgDef = processDef.getWSDLMessage(SystemFault.MESSAGE_QNAME);
    	if (e instanceof StandardException) {
    		
    		//Create a new StandardException to enrich the original message that was passed 
    		//in (adding line number and activity name).
    		newE = new StandardException(((StandardException)e).getFault(), Utility.appendLineNumberAndActName(e.getMessage(), activityUnit), e.getCause());
    		faultName = new QName(BPELDocument.BPEL_NAMESPACE, ((StandardException)e).getFault().toString());
    		faultData = Utility.createGenericFaultMsg(msgDef, e.getMessage());
    	} else if (e instanceof SystemException) {
    		faultName = SystemFault.FAULT_QNAME;
    		faultData = Utility.createGenericFaultMsg(msgDef, e.getCause().getMessage());
    	} else if (e instanceof POJOException) {
                POJOException pojoe = (POJOException)e;
                faultName = new QName(SystemFault.NAMESPACE, escape$(pojoe.getCause().getClass().getName()));
                String activityName = activityUnit.getStaticModelActivity().getName();
                int activityLineNumber = activityUnit.getStaticModelActivity().getLocator().getLineNumber();
    		faultData = Utility.createPOJOFaultMsg(msgDef, pojoe, activityName, activityLineNumber);
        } else {
    		faultName = SystemFault.FAULT_QNAME;
    		
    		//Create a new SystemException to enrich the original exception (adding line number 
    		//and activity name).    		
    		String message = Utility.appendLineNumberAndActName(I18n.loc("BPCOR-7131: A fatal exception has occurred"), activityUnit);
    		newE = new SystemException(message, e);
    		faultData = Utility.createGenericFaultMsg(msgDef, e.getMessage());
    	}
    	
        // The following two log statements are at fine level and not at warning level, for the reason that there
        // may be a fault handler used to handle this fault then, we don't need to log this.
    	if(LOGGER.isLoggable(Level.FINE)) {
        	//Print the message from the exception
        	String message1 = I18n.loc("BPCOR-3055: An exception was caught. Exception message: {0}", e.getMessage());
    		LOGGER.log(Level.FINE, message1 + e.getMessage());
    	}
    	if(LOGGER.isLoggable(Level.FINEST)) {
    		//Print the stacktrace of the exception
    		LOGGER.log(Level.FINEST, I18n.loc("BPCOR-3067: Exception details: "), e);
    	}
    	   	
		return ActivityUnitFactory.getInstance().createVirtualThrowUnit(activityUnit.getContext(), 
				activityUnit.getEnclosingActivityUnit(), activityUnit.getStaticModelActivity(), 
				activityUnit.getBranchId(), activityUnit, true, new FaultImpl(faultName, faultData, newE != null ? newE : e));
    }
    private String escape$(String name){
        return name.replace("$", "..");
    }
}

