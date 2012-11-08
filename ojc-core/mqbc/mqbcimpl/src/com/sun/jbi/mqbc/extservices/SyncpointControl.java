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
 * @(#)SyncpointControl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;


import com.ibm.mq.MQQueueManager;

/**
 * Facade-Proxy for a MQ Queue Manager that restricts use to its
 * syncpoint control aspect.
 * 
 */
public class SyncpointControl  {
    
    /**
     * Proxy subject
     */ 
    MQQueueManager mSubject;
    
    /**
     * Create a SyncpointControl.
     * 
     * @param qm MQ Queue Manager represented by this object
     * 
     * @throws NullPointerException  if <code>qm</code> is <code>null</code>
     */
    public SyncpointControl(MQQueueManager qm) {
        if (qm == null) {
            throw new NullPointerException("Null queue manager"); // no i18n
        }
        mSubject = qm;
    }

    /**
     * Command the associated queue manager to make permanent all gets and puts
     * that have occured since the last syncpoint, and to establish a new
     * syncpoint.  This method delegates to {@link com.ibm.mq.MQQueueManager#commit}.
     * 
     * <p>Note that this should NOT be called in XA compliant mode. (In XA mode,
     * eGate is acting as the transaction coordinator). If this is called in XA
     * mode it will throw an exception.</p>
     * 
     * @throws MQException if thrown by {@link com.ibm.mq.MQQueueManager#commit}
     */
    public void commit()
            throws MQBCExtServiceException {
        try {
            mSubject.commit();
        }
        catch (com.ibm.mq.MQException e) {
            throw new MQBCExtServiceException(e);
        }
    }
    
    /**
     * Command the associated queue manager to repudiate all gets and puts
     * that have occured since the last syncpoint.  This method delegates to
     * {@link com.ibm.mq.MQQueueManager#backout}.
     * 
     * <p>Note that this should NOT be called in XA compliant mode. (In XA mode,
     * eGate is acting as the transaction coordinator). If this is called in XA
     * mode it will throw an exception.</p>
     * 
     * @throws MQException if thrown by {@link com.ibm.mq.MQQueueManager#backout}
     */ 
    public void backout()
            throws MQBCExtServiceException {
        try {
            mSubject.backout();
        }
        catch (com.ibm.mq.MQException e) {
            throw new MQBCExtServiceException(e);
        }
    }
    
    
}
