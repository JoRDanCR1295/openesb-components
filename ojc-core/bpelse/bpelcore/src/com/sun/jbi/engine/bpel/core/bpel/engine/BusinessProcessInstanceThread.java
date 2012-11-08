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
 * @(#)BusinessProcessInstanceThread.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;



/**
 * BP instance thread
 *
 * @author Sun Microsystems
 */
public class BusinessProcessInstanceThread {
    /** timeout */
    public static final int TIMEOUT = 0;

    /** without timeout */
    public static final int NON_TIMEOUT = 1;

    /** waiting for reply */
    public static final int WAITING_FOR_REPLY = 2;

    /** waiting for receive */
    public static final int WAITING_FOR_RECEIVE = 3;

    /** waiting for DONE */
    public static final int WAITING_FOR_DONE = 4;

    public static final int SCALABILITY_PASSIVATED = 5;
    
    private int mType;
    private long mTimeout = 0;
    
    protected ICallFrame mFrame;
    
    /** the following field will be populated when the waiting bpit is scalability passivated by
     * phase 2 passivation. When that happen, the callframe is dereferenced (so as to release all the
     * memory related objects for this instance. But the instance id is kept so that the recovery
     * for this instance can be called when the timeout for the wait expires.
     */
    private String mProcessInstanceId;
    
    /**
     * The following field is used when the instance is scalability passivated the suspended status 
     * is saved locally in the bpit which will retain the status when the instance is removed from memory.
     * Also, when the i
     */
    private boolean mIsSuspended = false;
    
    private Object mMessageExchangeKey;

    protected BPELProcessManager mProcessManager;
    protected Engine mEngine;
    
    /**
     * Creates a new instance of BusinessProcessInstance
     */
    public BusinessProcessInstanceThread(BPELProcessManager processManager, Engine engine, ICallFrame mFrame) {
        this.mProcessManager = processManager;
        this.mEngine = engine;
        this.mFrame = mFrame;
        if (mFrame != null) {
        	this.mProcessInstanceId = mFrame.getProcessInstance().getId();
        }
    }

    /**
     * This is default behavior, override this method to custom behavior
     */
    public void execute() {
        mEngine.getInterpreter().execute(this, mProcessManager);
    }

    /**
     * gets type
     *
     * @return int type
     */
    public int getType() {
        return mType;
    }

    /**
     * sets type
     *
     * @param type type
     */
    public void setType(int type) {
        mType = type;
    }

    /**
     * gets timeout
     *
     * @return long timeout
     */
    public long getTimeout() {
        return mTimeout;
    }

    /**
     * sets timeout
     *
     * @param timeout timeout
     */
    public void setTimeout(long timeout) {
        mTimeout = timeout;
    }

    /**
     * checks if thread is ready
     *
     * @return boolean: if thread is ready, returns true; otherwise, returns false
     */
    public boolean isReady() {
        //Terminating, make it always ready
        if (mFrame != null && mFrame.getProcessInstance().isExiting()) {
            return true;
        }
        if (isSuspended ()) {
            return false;
        }
        return true;
    }

    public boolean hasExpired() {
        if (getType() == TIMEOUT || getType() == SCALABILITY_PASSIVATED) {
            long now = System.currentTimeMillis();
            return (mTimeout <= now) ? true : false;
        }
        return false;
    }
    

    public final boolean isSuspended () {
        if (mFrame != null  && mFrame.getProcessInstance().isSuspended()) {
            return true;
        } else if (mType == SCALABILITY_PASSIVATED) {           
            return mIsSuspended;
        }
        return false;        
    }
    
    /**
     * gets callframe
     *
     * @return ICallFrame callframe
     */
    public ICallFrame getCallFrame() {
        return mFrame;
    }

//    /**
//     * sets callframe
//     *
//     * @param object callframe
//     */
//    public void setCallFrame(ICallFrame object) {
//        mFrame = object;
//        if (object != null) {
//        	this.mProcessInstanceId = object.getProcessInstance().getId();
//        }
//    }

    /**
     * gets message exchange key
     *
     * @return Object message exchange key
     */
    public Object getMessageExchangeKey() {
        return mMessageExchangeKey;
    }

    /**
     * sets message exchange key
     *
     * @param messageExchangeKey message exchange key
     */
    public void setMessageExchangeKey(Object messageExchangeKey) {
        mMessageExchangeKey = messageExchangeKey;
    }

    public String getProcessInstanceId() {
        return mProcessInstanceId;
    }

//    public void setProcessInstanceId(String processInstanceId) {
//        this.mProcessInstanceId = processInstanceId;
//    }

    public void setSuspended(boolean isSuspended) {
        this.mIsSuspended = isSuspended;
    }
    
    /**
     * Scalability passivate the bpit. The instance id is saved so that 
     * the instance can be recovered when needed.
     * 
     * @param bpit
     */
    public void markScalabilityPassivated() {
        setType(BusinessProcessInstanceThread.SCALABILITY_PASSIVATED);
        if (mFrame != null) {
        	setSuspended(mFrame.getProcessInstance().isSuspended());
        }
        this.mFrame = null;
    }
}
