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
 * @(#)RecoveredCallFrameImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;


/**
 * Recovered Callframe implementation
 *
 * @author Sun Microsystems
 */
public class RecoveredCallFrameImpl extends CallFrame implements RecoveredCallFrame {
    private long mTimerVal = Long.MIN_VALUE;
    private boolean hasCheckPointedChildCFs = false;
    private boolean mIsRecovered = true;
    
    /**
     * Creates a new RecoveredCallFrameImpl object.
     *
     * @param parent DOCUMENT ME!
     * @param interp DOCUMENT ME!
     * @param actUnit DOCUMENT ME!
     */
    public RecoveredCallFrameImpl(ICallFrame parent, BPELInterpreter interp, ActivityUnit actUnit, 
    						      long branchInvokeCounter) {
        this(parent, interp, Long.MIN_VALUE, actUnit, branchInvokeCounter);
    }

    /**
     * Creates a new RecoveredCallFrameImpl object.
     *
     * @param process DOCUMENT ME!
     * @param interp DOCUMENT ME!
     * @param instance DOCUMENT ME!
     * @param actUnit DOCUMENT ME!
     */
    public RecoveredCallFrameImpl(
        RBPELProcess process, BPELInterpreter interp, BPELProcessInstance instance,
        ActivityUnit actUnit, long branchInvokeCounter) {
        this(process, interp, Long.MIN_VALUE, instance, actUnit, branchInvokeCounter);
    }

    /**
     * Creates a new RecoveredCallFrameImpl object.
     *
     * @param parent DOCUMENT ME!
     * @param interp DOCUMENT ME!
     * @param timerVal DOCUMENT ME!
     * @param actUnit DOCUMENT ME!
     */
    public RecoveredCallFrameImpl(
        ICallFrame parent, BPELInterpreter interp, long timerVal, ActivityUnit actUnit, 
       	long branchInvokeCounter) {
        super(parent, interp, actUnit, branchInvokeCounter);
        mTimerVal = timerVal;
    }

    /**
     * Creates a new RecoveredCallFrameImpl object.
     *
     * @param process DOCUMENT ME!
     * @param interp DOCUMENT ME!
     * @param timerVal DOCUMENT ME!
     * @param instance DOCUMENT ME!
     * @param actUnit DOCUMENT ME!
     */
    public RecoveredCallFrameImpl(
        RBPELProcess process, BPELInterpreter interp, long timerVal,
        BPELProcessInstance instance, ActivityUnit actUnit, long branchInvokeCounter) {
        super(process, interp, instance, actUnit, branchInvokeCounter);
        mTimerVal = timerVal;
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#getTimerVal()
     */
    public long getTimerVal() {
        return mTimerVal;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame#isRecovered()
     */
    public boolean isRecovered() {
        return mIsRecovered;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame#convert()
     */
    public ICallFrame convert() {
    	// Devnote: Lets see, long long ago.... Jokes apart some history as concisely as I can put it..
    	// The original intention of extending the CallFrame class to RecoveredCallFrame was for the obvious of 
    	// better code organization, reduction of method clutter and seperation of functionality. So far so good.
    	// Then the reason for this method: Once recovery is complete, there is a chance that the recovered call 
    	// frame can be put on the ready to run queue and encounter the execute method of the bpel interpreter. 
    	// The bpel interpreter checks to see if it a recovered call frame and takes action accordingly. Since
    	// recovery is complete we do not want this to happen, we need to set some flag to tell the bpel interpreter 
    	// that recovery is not required (as it is done)... or we could have a method called convert that an activity
    	// unit calls when recovery is complete, which returns a new call frame (not recovered callframe) and is a
    	// clone of the recovered call frame in all other aspects (other than it not being a recovered callframe). 
    	// Hence if and when the new callframe is put on the ready to run queue, the bpel interpreter will determine
    	// that the recovery actions do not need to be initiated. PHEW! If you kept up so far, this is the home stretch.
    	// However, doing this (creating a duplicate) means that the callframe the bpel interpreter is keeping a 
    	// reference to is out of sync with the newly constructed (duplicated) callframe. This creates another set
    	// of problems which I will not eloborate on. So as a temporary and quick fix (i.e. no changes to api), guess
    	// what.. we now keep a flag to indicate that if a call frame is recovered and convert just notifies the 
    	// call frame that recovery is complete. So back to square one with a lot of excess baggage :) And everyone
    	// lived happily ever after... till we revisit this in a year or so.
    	mIsRecovered = false;
        return this; //super.duplicate();
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame#hasPersistedChildCF()
     */
    public boolean hasPersistedChildCF() {
        return hasCheckPointedChildCFs;
    }

    /** @see com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame#setHasPersistedChildCF(boolean)
     */
    public void setHasPersistedChildCF(boolean reconstructedFlow) {
        hasCheckPointedChildCFs = reconstructedFlow;
    }
}
