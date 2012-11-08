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
 * @(#)MessageContainerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.util.Map;
import javax.transaction.Transaction;
import javax.xml.namespace.QName;

import net.sf.hulp.measure.Measurement;

import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.util.PropagationContext;


/**
 * Message Container Implementation
 *
 * @author Sun Microsystems
 */
public class MessageContainerImpl implements MessageContainer {

	private int mFlag;
    private static final String CLASSIDENTIFIER_FORMEASUREMENT = "BPEL-SE.MessageContainerImpl";
    private Object mContent;
    private String mMessageExchangeId;
    private String mCRMPInvokeId;
    private Transaction mTransaction;
    private PropagationContext mPropContext;
    private MessageContainer request;
    private RedeliveryStatus mRedeliveryStatus = null;
    private boolean mTxAtomicStarted = false;
    /**
     * Service provider while processing requests might add/update NM properties
     * on the request, for example JMS message-id. To make these NMProperties
     * available to business process. bpel-engine reads them while processing
     * response/status and set it on the bpel input variable associated with
     * Invoke.
     */
    private Map<String, Object> mNMProperties;

    /**
     * Creates a new MessageContainerImpl object.
     *
     * @param flag create flag
     * @param id messasge ID
     * @param content message content
     * @param reliableMesgId reliable messaging ID
     */
    public MessageContainerImpl(int flag, String id, Object content,
        Object crmpInvokeId, Transaction transaction) {
        mFlag = flag;
        mMessageExchangeId = id;
        mContent = content;
        // the crmpInvokeId that is sent on the message exchange has to be a valid string.
        if (crmpInvokeId != null && (crmpInvokeId instanceof String)) {
        	String tmp = (String) crmpInvokeId;
        	if (tmp.trim().length() > 0) {
        		mCRMPInvokeId = tmp;
        	}
        }
        mTransaction = transaction;
    }

    public MessageContainerImpl(int flag, String id, Object content, MessageContainer request) {
        this(flag, id, content, null, null);
        this.request = request;
    }

    /**
     * gets message ID
     *
     * @return Object message ID
     */
    public String getId() {
        return mMessageExchangeId;
    }

    /**
     * gets Content
     *
     * @return Object content
     */
    public Object getContent() {
        return mContent;
    }

    /**
     * check if message is a status
     *
     * @return boolean: if message is status, returns true; otherwise, returns false
     */
    public boolean isStatus() {
        return ((mFlag == MessageContainer.STATUS_DONE) || (mFlag == MessageContainer.STATUS_ERROR));
    }

    /**
     * check if message is data
     *
     * @return boolean: if message is data, returns true; otherwise, returns false
     */
    public boolean isMessage() {
        return (mFlag == MessageContainer.MESSAGE);
    }

    /**
     * check if message is fault
     *
     * @return boolean: if message is fault, returns true; otherwise, returns false
     */
    public boolean isFault() {
        return (mFlag == MessageContainer.FAULT);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer#getCRMPInvokeId()
     */
    public String getCRMPInvokeId() {
    	return mCRMPInvokeId;
    }
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer#getTransaction()
     */
    public Transaction getTransaction() {
        return mTransaction;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer#setTransaction(javax.transaction.Transaction)
     */
    public void setTransaction(Transaction transaction) {
        mTransaction = transaction;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer#isStatusError()
     */
    public boolean isStatusError() {
        return (mFlag == MessageContainer.STATUS_ERROR);
    }
    
    private Measurement m1 = null;

    public void startMeasurement(QName serviceName, String operation) {
        if (Measurement.isInstalled()) {
            String measure = "Processing InOut request : [partner-> "
                    + serviceName + ", opeation-> " + operation + "]";
            m1 = Measurement.begin(CLASSIDENTIFIER_FORMEASUREMENT, measure);
        }
    }
    public void endMeasurement() {
        if(request!=null){
            request.endMeasurement();
        } else {
            if (Measurement.isInstalled()) {
                m1.end();
            }
        }
    }

    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer#getPropagationContext()
     */
	public PropagationContext getPropagationContext() {
		return mPropContext;
	}

	/*
	 * (non-Javadoc)
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer#setPropagationContext(com.sun.jbi.engine.bpel.core.bpel.util.PropagationContext)
	 */
	public void setPropagationContext(PropagationContext propContext) {
		mPropContext = propContext;
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer#getRedeliveryStatus()
	 */
	public RedeliveryStatus getRedeliveryStatus() {
		return mRedeliveryStatus;
	}

	/**
	 * @see com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer
	 * 	#setRedeliveryStatus(com.sun.jbi.common.qos.redelivery.RedeliveryStatus)
	 */
	public void setRedeliveryStatus(RedeliveryStatus status) {
		mRedeliveryStatus = status;
	}

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer#isTxAtomicStarted()
     */
    public boolean isTxAtomicStarted() {
        return mTxAtomicStarted;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer#setTxAtomicStarted()
     */
    public void setTxAtomicStarted() {
        mTxAtomicStarted = true;
    }
    /*
     * @see
     * com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer#getNMProperties
     * ()
     */
	public Map<String, Object> getNMProperties() {
        return mNMProperties;
    }

    /*
     * @see
     * com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer#setNMProperties
     * (java.util.Map)
     */
    public void setNMProperties(Map<String, Object> mNMProperties) {
        this.mNMProperties = mNMProperties;
    }
}
