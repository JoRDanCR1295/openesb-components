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
 * @(#)CallFrame.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import javax.xml.namespace.QName;

import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELDebugger;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.BPELVariable;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebugFrame;
import org.netbeans.modules.bpel.debuggerbdi.rmi.api.DebuggableEngine;

import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.jbi.engine.bpel.core.bpel.debug.BPELVariableImpl;
import com.sun.jbi.engine.bpel.core.bpel.debug.DefaultDebuggableEngine;
import com.sun.jbi.engine.bpel.core.bpel.debug.DefaultDebugger;
import com.sun.jbi.engine.bpel.core.bpel.debug.WSDLMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELEngine;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersChildAct;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersOnAlarmUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.EventHandlersOnEventUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FlowUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ActivityUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.util.BPELHelper;


/**
 * DOCUMENT ME! *
 * 
 * @author Sun Microsystems
 */
public class CallFrame implements ICallFrame {

    /** Serial number counter for CallFrame instances */
    private static int callFrameId = 0;
    /** process instance with which this call frame is associated */
    public BPELProcessInstance mProcessInstance;

    /** BPEL4WS process definition */
    RBPELProcess mBPELprocess;

    /** Parent thread */
    private CallFrame mParentCF;

    /** Instruction Count */
    int ic;

    /** Thread id */
    public String cfId;

    private BPELInterpreter mIntrerpreter;

    /** program counter */
    ActivityUnit mActUnit;

    /** Debugger's representation of this thread */
    transient DebugFrame mDebugFrame;
    
    /** DOCUMENT ME! */
    private String mBpelFileName;

    private String mProcessURI;

    private long mBranchInvokeCounter = 0L;
    
    private boolean isFlowBranchCF;
    
    private boolean isEventHandlerCF;
    
    /** The CallFrame CRMP id, refer to ICallFrame.getCallFrameCRMPId */
    private String mCallFrameCRMPId;
    
    /**
     * Creates a new CallFrame object.
     */
    protected CallFrame() {
    }

    /**
     * Creates a new CallFrame object. *
     * 
     * @param process
     *            BPEL4WS process definition
     * @param interp
     *            DOCUMENT ME!
     * @param instance
     *            DOCUMENT ME!
     * @param actUnit
     *            DOCUMENT ME!
     */
    public CallFrame(RBPELProcess process, BPELInterpreter interp, BPELProcessInstance instance,
            ActivityUnit actUnit, long branchInvokeCounter) {
        mProcessInstance = instance;
        init(interp);
        this.mBPELprocess = process;
        mActUnit = actUnit;
        cfId = "CF-" + callFrameId++;
        mBranchInvokeCounter = branchInvokeCounter;
        if (getDebugger() != null) {
            enterFrame();
        }
        mCallFrameCRMPId = mProcessInstance.getId();
        if ((actUnit instanceof EventHandlersOnEventUnit) || (actUnit instanceof EventHandlersOnAlarmUnit)) {
        	isEventHandlerCF = true;
        	mCallFrameCRMPId = ((EventHandlersChildAct) actUnit).getUID();
        }
    }

    /**
     * Creates a new CallFrame object. *
     * 
     * @param parent
     *            Parent thread
     * @param interp
     *            DOCUMENT ME!
     * @param actUnit
     *            DOCUMENT ME!
     */
    public CallFrame(ICallFrame parent, BPELInterpreter interp, ActivityUnit actUnit, 
    				 long branchInvokeCounter) {
        init(interp);
        this.mParentCF = (CallFrame) parent;
        this.mBPELprocess = mParentCF.mBPELprocess;
        mProcessInstance = mParentCF.mProcessInstance;
        // correlation objs are shared
        mActUnit = actUnit;
        cfId = "CF-" + callFrameId++;
        mBranchInvokeCounter = branchInvokeCounter;
        if (getDebugger() != null) {
            enterFrame();
        }
        mCallFrameCRMPId = mProcessInstance.getId();
        if (null != parent && parent.getProgramCounter() instanceof FlowUnit) {
            isFlowBranchCF = true;
        } else if ((actUnit instanceof EventHandlersOnEventUnit) || (actUnit instanceof EventHandlersOnAlarmUnit)) {
        	isEventHandlerCF = true;
        	mCallFrameCRMPId = ((EventHandlersChildAct) actUnit).getUID();
        }
    }

    /**
     * Helper method to notify the debugger about this thread *
     * 
     * @param frame
     */
    private void enterFrame() {
        mBpelFileName = BPELHelper.getNameInPath(mBPELprocess.getOwnerDocument().getBaseURI(), "/");
        mProcessURI = BPELHelper.getProcessURI(mBPELprocess);
        String rootFrameId = BPELHelper.getSeq(getBPId()).toString();
        String  parentFrameId = null;
        if (mParentCF != null) {
             parentFrameId = mParentCF.cfId;
         }
        DefaultDebugger debugger = (DefaultDebugger) getDebugger();
        if (debugger.isDebugFrameValid(cfId)) {            
            mDebugFrame = debugger.enterFrame(cfId, rootFrameId, parentFrameId, mBpelFileName, mProcessURI);
        }
    }

    private void init(BPELInterpreter interp) {
        mIntrerpreter = interp;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#setProgramCounter(
     *      com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit)
     */
    public void setProgramCounter(ActivityUnit actUnit) {
        mActUnit = actUnit;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#getProgramCounter()
     */
    public ActivityUnit getProgramCounter() {
        return mActUnit;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#isRecovered()
     */
    public boolean isRecovered() {
        return false;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#getProcessInstance()
     */
    public BPELProcessInstance getProcessInstance() {
        return mProcessInstance;
    }

    /**
     * converts the recoveredCallFrame or any variant instance of ICallFrame to CallFrame instance. *
     * 
     * @return DOCUMENT ME!
     */
    protected ICallFrame duplicate() {
        CallFrame dup = new CallFrame();
        dup.init(mIntrerpreter);
        dup.mBPELprocess = this.mBPELprocess;
        dup.mParentCF = null;
        if (this.mParentCF != null) {
            dup.mParentCF = this.mParentCF;
        }
        dup.mActUnit = mActUnit;
        dup.mProcessInstance = mProcessInstance;
        dup.ic = this.ic;
        dup.mBranchInvokeCounter = this.mBranchInvokeCounter;
        return dup;
    }

    /**
     * Get the source line number of the current program counter *
     * 
     * @param pc
     *            The current programe counter *
     * @return Its line number
     */
    int getLineNumber(ActivityUnit pc) {
        return pc.getStaticModelActivity().getLocator().getLineNumber();
    }

    /**
     * Get the Xpath of the current program counter
     * 
     * @param pc
     *            The current program counter
     * @return Its xpath string
     */
    String getXpath(ActivityUnit pc) {
        return pc.getStaticModelActivity().getXPath();
    }

    /**
     * Return the business process instance id associated with this thread *
     * 
     * @return The business process instanced id
     */
    public String getBPId() {
        return mProcessInstance.getId();
    }

    /**
     * Return a string-ified version of this object *
     * 
     * @return The string-ified version of this object
     */
    public String toString() {
        return " pc = " + getPC() + " cfid = " + cfId;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#getPC()
     */
    public RActivity getPC() {
        return mActUnit == null ? null : mActUnit.getStaticModelActivity();
    }

    /**
     * creates RuntimeVariable for the given variable name. *
     * 
     * @param variable
     * @return
     */
    public RuntimeVariable createRuntimeVariable(String varName) {
        return null;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#getProcess()
     */
    public RBPELProcess getProcess() {
        return mBPELprocess;
    }

    /**
     * DOCUMENT ME! *
     * 
     * @return DOCUMENT ME!
     */
    public String getId() {
        return cfId;
    }

    /**
     * DOCUMENT ME! *
     * 
     * @return DOCUMENT ME!
     */
    public ICallFrame getParent() {
        return mParentCF;
    }

    /**
     * Get debugger from Engine *
     * 
     * @return DOCUMENT ME!
     */
    private BPELDebugger getDebugger() {
        return ((BPELEngine) mIntrerpreter.mEng).getDebugger();
    }

    public void onLineChange(ActivityUnit pc){
        DefaultDebugger debugger = (DefaultDebugger) getDebugger();
        if (debugger != null && !(debugger.isDetached ()) && mDebugFrame != null && pc != null) {
            DebuggableEngine debuggableEngine = new DefaultDebuggableEngine(pc);
            mDebugFrame.onLineChange(mBpelFileName, mProcessURI, getLineNumber(pc), getXpath(pc), debuggableEngine);
        }
    }
    
    public void onLineChange(ActivityUnit pc, Object object) {
        final DefaultDebugger debugger = (DefaultDebugger) getDebugger();
        
        if ((debugger != null) && 
                !debugger.isDetached() && 
                (mDebugFrame != null) && 
                (pc != null) &&
                (object != null)) {
            
            final DebuggableEngine debuggableEngine = 
                    new DefaultDebuggableEngine(pc);
            
            if (object instanceof XMLElement) {
                final XMLElement element = (XMLElement) object;
                
                mDebugFrame.onLineChange(
                        mBpelFileName, 
                        mProcessURI, 
                        element.getLocator().getLineNumber(), 
                        element.getXPath(), 
                        debuggableEngine);
            }
        }
    }
    
    /**
     * Notifies debugger an fault being thrown
     * 
     * @param pc   activity where to throw fault
     * @param faultQName     fault QName
     * @param faultMessageType     fault message QName if faultData is a WSDL message. 
     * @param faultData    fault data, can be RVariable, WSMessage or Element string .
     */
    public void onFault(ActivityUnit pc, QName faultQName, QName faultMessageType, Object faultData) {
        DefaultDebugger debugger = (DefaultDebugger) getDebugger();
        if (debugger != null && !(debugger.isDetached ()) 
        		&& mDebugFrame != null && pc != null) {
            ActivityUnitImpl act = (ActivityUnitImpl) pc;
            DebuggableEngine debuggableEngine = new DefaultDebuggableEngine(pc);
            BPELVariable faultDataMir = null;
            if (faultData != null) {
                if (faultData instanceof RVariable) {
                    RVariable model = (RVariable) faultData;
                    RuntimeVariable  data = (RuntimeVariable) act.getContext()
                	    .getRuntimeVariable(model);
                    faultDataMir = new BPELVariableImpl(model, data);                    
                } else if (faultData instanceof WSMessage) {
                    org.netbeans.modules.bpel.debuggerbdi.rmi.api.WSDLMessage msgMirror = 
                        new WSDLMessageImpl(((WSMessage) faultData).getWSDLMessage(), (WSMessage) faultData);
                    faultDataMir = new BPELVariableImpl(msgMirror);
                } else if (faultData instanceof String) {
                    faultDataMir = new BPELVariableImpl((String) faultData);
                }
            }      
            String faultQNameToString = "";
            if (faultQName != null) {
                faultQNameToString = faultQName.getNamespaceURI() + "\n" + 
                            faultQName.getPrefix() + "\n" + 
                            faultQName.getLocalPart();
            }
            mDebugFrame.onFault(mBpelFileName, mProcessURI, 
                getLineNumber(pc), getXpath(pc), faultQNameToString, faultDataMir, debuggableEngine);
        }
    }

    /**
     * Running to the end of the process without exception
     */
    public void onExit() {
        setProgramCounter(null);
        if (getDebugger() != null && mDebugFrame != null) {
            mDebugFrame.onExit(mBpelFileName, mProcessURI);
            ((DefaultDebugger) getDebugger()).frameExit(mDebugFrame);
        }
     }

    /**
     * Notify the debugger when an activity completes
     * @param pc The activity which has completed
     */
    public void onActivityComplete(ActivityUnit pc) {
        if (getDebugger() != null && mDebugFrame != null) {
            mDebugFrame.onActivityComplete(mBpelFileName, mProcessURI, getLineNumber(pc), getXpath(pc));
         }        
    }
    
    public void onSubActivityComplete(ActivityUnit pc, Object object) {
        final DefaultDebugger debugger = (DefaultDebugger) getDebugger();
        
        if ((debugger != null) && 
                !debugger.isDetached() && 
                (mDebugFrame != null) && 
                (pc != null) &&
                (object != null)) {
            
            if (object instanceof XMLElement) {
                final XMLElement element = (XMLElement) object;
                
                mDebugFrame.onSubActivityComplete(
                        mBpelFileName, 
                        mProcessURI, 
                        element.getLocator().getLineNumber(), 
                        element.getXPath());
            }
        }
    }
    
    /**
     * Notify the debugger when an activity has been terminated because of a 
     * fault in an enclosing scope.
     * @param pc The activity which was terminated
     */
    public void onActivityTerminate(ActivityUnit pc) {
        if (getDebugger() != null && mDebugFrame != null) {
            mDebugFrame.onTerminate(mBpelFileName, mProcessURI, getLineNumber(pc), getXpath(pc));
        }
		getProcessInstance().getMonitorMgr().postActivityTerminateEvent(pc);	        
    }
    
    public Long getBranchInvokeCounter() {
    	return mBranchInvokeCounter;
    }
    
    public void incrementBranchInvokeCounter() {
    	mBranchInvokeCounter++;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#setBranchInvokeCounter(long)
     */
    public void setBranchInvokeCounter(long recoveredValue) {
    	mBranchInvokeCounter = recoveredValue;
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#isFlowBranchCF()
     */
    public boolean isFlowBranchCF() {
        return isFlowBranchCF;
    }
    
    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#isEventHandlerCF()
     */
    public boolean isEventHandlerCF() {
    	return isEventHandlerCF;
    }

    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#getCallFrameCRMPId()
     */    
	public String getCallFrameCRMPId() {
		return mCallFrameCRMPId;
	}
    
}
