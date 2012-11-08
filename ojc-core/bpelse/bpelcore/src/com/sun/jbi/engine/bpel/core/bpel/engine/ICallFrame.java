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
 * @(#)ICallFrame.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;

import javax.xml.namespace.QName;

import com.sun.bpel.model.meta.RActivity;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public interface ICallFrame {
    /**
     * DOCUMENT ME!
     *
     * @param actUnit DOCUMENT ME!
     */
    public void setProgramCounter(ActivityUnit actUnit);

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#getProgramCounter()
     */
    public ActivityUnit getProgramCounter();

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#isRecovered()
     */
    public boolean isRecovered();

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#getProcessInstance()
     */
    public BPELProcessInstance getProcessInstance();

    /**
     * Return the business process instance id associated with this thread
     *
     * @return The business process instanced id
     */
    public String getBPId();

    /**
     * Get the current program counter
     *
     * @return The activity that represents the current program location
     */
    public RActivity getPC();

    /**
     * Get the BPEL4WS process definition for process instance this thread is part of
     *
     * @return The BPEL4WS process definition
     */
    public RBPELProcess getProcess();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String getId();

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public ICallFrame getParent();

    /**
     * Running to the end of the process without exception
     */
    public void onExit();
    
    /**
     * Notify the debugger of a line change for an activity.
     *
     * @param pc The new program counter
     */
    public void onLineChange(ActivityUnit pc);
    
    /** 
     * Notify the debugger of a line change for a subactivity. Whether the 
     * concrete subactivity is supported is defined by the implementation --
     * it may or may not send events to the remote debugger.
     * 
     * @param pc The current program counter.
     * @param object The subactivity being executed.
     */
    public void onLineChange(ActivityUnit pc, Object object);    
    
    /**
     * Notifies the debugger an fault being thrown
     * 
     * @param pc   activity where to throw fault
     * @param faultQName     fault QName
     * @param faultMessageType     fault message QName if faultData is a WSDL message. 
     * @param faultData    fault data, can be RVariable, WSMessage or Element string representation.
     */
    public void onFault(ActivityUnit pc, QName faultQName, QName messageType, Object faultData);
    
    /**
     * Notify the debugger when an activity completes
     * @param pc The activity which has completed
     */
    public void onActivityComplete(ActivityUnit pc);
    
    /**
     * Notifies the debugger when a subactivity completes. Whether a concrete
     * subactivity is supported is defined by the implementation -- it may or 
     * may not send events to the remote debbugger.
     * 
     * @param pc The current program counter.
     * @param object The subactivity which has finished executing.
     */
    public void onSubActivityComplete(ActivityUnit pc, Object object);
    
    /**
     * Notify the debugger when an activity has been terminated because of a 
     * fault in an enclosing scope.
     * @param pc The activity which was terminated
     */
    public void onActivityTerminate(ActivityUnit pc);
    
    public Long getBranchInvokeCounter();
    
    public void incrementBranchInvokeCounter();
    
    /**
     * Used by clustered engine to determine if the callframe is a flow callframe.
     * 
     * @return
     */
    public boolean isFlowBranchCF();
    
    /**
     * To determine if the callframe is for an OnEvent of an OnAlarm
     * @return
     */
    public boolean isEventHandlerCF();

    /**
     * Used by the Flow unit, when child branches join back to the parent Flow
     * activity the branchInvokeCounter of the child branch CallFrame is set 
     * to the CallFrame of the parent Flow activity if it is greater.
     */
    public void setBranchInvokeCounter(long recoveredValue);
    
    /**
     * The CallFrame CRMP ID that is part of the Unique composite CRMP Inovke id that is 
     * set on every invoke. 
     * The CRMP inovke id is "CallFrame CRMP ID + Activity ID + branch Invoke counter.
     * In the case of a EventHandler CallFrame this will be the EventHanlderChild Activity 
     * GUID and in all other cases it defualts to the process instance id.
     * @return String - value of the Id.
     */
    public String getCallFrameCRMPId();

}
