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
 * @(#)IMAScopeBridge.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.transaction.Transaction;
import javax.transaction.TransactionManager;

import com.sun.bpel.model.meta.RReply;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.RequestReplyKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Context;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.TxPropagationObject;

public class IMAScopeBridge  implements IMAScope {
    
    /** Log handle */
    private static final Logger LOGGER = Logger.getLogger(IMAScopeBridge.class.getName());    
    
    /** Outstanding requests */
    private Map mRequests;
    
    /* Keeps track of pending in only requests when the BP is marked atomic. 
     * Using a list so that order can be preserved.  */
    private List mInOnlyRequests;
    private boolean hasDefaultMessageExchange = false;
    
    /* list of entries in the CRMP table that do not have a response object set yet
     * This is required to check if persistence at the end of the reply activity need 
     * to include an update statment for CRMP table. Our connection API's throws RuntimeException
     * if update statement returns 0 rows.
     * The value insterted is a string concatenation of "bpid + partnerlink + operation"
     * TODO: we may need to check for duplicates when the engine allows two Inbound activities with the 
     * same partnerlink and operation in the same process. 
     */
    private List mCRMPUpdateList;
    
    private BPELProcessInstance mInstance;
    private Context mParentContext;

    public IMAScopeBridge(BPELProcessInstance instance, Context parentContext) {
        super();
        mInstance = instance;
        mRequests = new HashMap();
        mInOnlyRequests = new ArrayList();
        mCRMPUpdateList = new ArrayList();
        mParentContext = parentContext;
    }

    public MessageContainer removeRequest(RReply reply) {
        // Use frame's service references instead of WSRequest/WSReply
        RequestReplyKeyImpl key = new RequestReplyKeyImpl(reply.getRPartner(),
                reply.getRPortType(), reply.getWSDLOperation().getName(), null, reply
                        .getMessageExchange(), mInstance.getId());
        synchronized (mRequests) {
            MessageContainer container = (MessageContainer) mRequests.remove(key);
            if (container != null)
                return container;
        }
        if (mParentContext != null) {
            return  mParentContext.removeRequest(reply);
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#receive(com.sun.jbi.engine.bpel.core.bpel.model.meta.RStartElement, com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
     */
    public void addRequest(RStartElement rcv, MessageContainer req) throws BPELRuntimeException {
        
        if(this.hasDefaultMessageExchange) {
        	String operPattern = mInstance.getBPELProcessManager().getOperationPattern(rcv);
        	if (operPattern.equals(Engine.IN_OUT)) {
        		// Use frame's service references instead of WSRequest/WSReply
                RequestReplyKeyImpl key = new RequestReplyKeyImpl(rcv.getRPartner(), rcv.getRPortType(),
                        rcv.getWSDLOperation().getName(), null, rcv.getMessageExchange(), mInstance.getId());
        		synchronized (mRequests) {
        			mRequests.put(key, req);
        		}   
        	} else {
        		synchronized (mInOnlyRequests) {
        			mInOnlyRequests.add(req);
        		}
        	}
        } else {
            mParentContext.addRequest(rcv, req);
        }
    }
    
    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#removeRequest(com.sun.jbi.engine.bpel.core.bpel.model.meta.RStartElement, com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
     */
    public void removeRequest(RStartElement rcv, MessageContainer associatedReq) throws BPELRuntimeException {
        if(this.hasDefaultMessageExchange) {
        	String operPattern = mInstance.getBPELProcessManager().getOperationPattern(rcv);
        	if (operPattern.equals(Engine.IN_OUT)) {
        		synchronized (mRequests) {
                    Iterator reqIter = mRequests.keySet().iterator();
                    while (reqIter.hasNext()) {
                        RequestReplyKeyImpl key = (RequestReplyKeyImpl) reqIter.next();
                        MessageContainer req = (MessageContainer) mRequests.get(key);
                        if (req.equals(associatedReq)) {
                        	reqIter.remove();
                        	return;
                        }
                    }
        		}   
        	} else {
        		synchronized (mInOnlyRequests) {
        			mInOnlyRequests.remove(associatedReq);
        		}
        	}
        } else if (mParentContext != null){
            mParentContext.removeRequest(rcv, associatedReq);
        }
    }
    /*
     * (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#addToCRMPUpdateList(String updateValueKey)
     */
    public void addToCRMPUpdateList(String updateValueKey) {
    	synchronized (mCRMPUpdateList) {
    		mCRMPUpdateList.add(updateValueKey);
    	}
    }
    
    /*
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#crmpUpdateListContains(String updateValueKey)
     */
    public boolean crmpUpdateListContains(String updateValueKey) {
    	synchronized (mCRMPUpdateList) {
    		return mCRMPUpdateList.contains(updateValueKey);
    	}
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#completePendingInOnlyRequests()
     */
    public void completePendingInOnlyRequests() {
        if (mInstance.getBPELProcessManager().isBPAtomic()) {
            TxPropagationObject txPropObj = mInstance.getTxPropagationObject();
            MessageContainer associatedReq = txPropObj.getParentContainer();
            boolean processTxReq = false;
            if ((txPropObj.getPropagationStatus().equals(TxPropagationObject.PropagationStatus.active))
                    || (txPropObj.getPropagationStatus().equals(TxPropagationObject.PropagationStatus.unavailable))) {
                processTxReq = true;
                if (txPropObj.getAtomicTransactionType().equals(TxPropagationObject.TransactionType.started)) {
                    TransactionManager tm = (TransactionManager) BPELSERegistry.getInstance().lookup(
                            TransactionManager.class.getName());
                    // Tx was started by the bpel-se so we have to commit since
                    // the atomic instance has
                    // completed without any errors.
                    Transaction tx = txPropObj.getTransaction();
                    try {
                        tm.resume(tx);
                        tx.commit();
                    } catch (Exception e) {
                        // attempt to commit resulted in error so we have to do
                        // error handling
                        throw new RuntimeException(I18n.loc("BPCOR-7137: Atomic mode, encountered an error while trying to commit "
                                + "a Transaction that was started by the bpel-se."), e);

                    }
                }
            }

            synchronized (mInOnlyRequests) {
                // Process the requests in reverse order so that the last
                // request in is completed first.
                for (int i = (mInOnlyRequests.size() - 1); i >= 0; i--) {
                    MessageContainer req = (MessageContainer) mInOnlyRequests.get(i);
                    mInstance.getBPELProcessManager().sendInOnlyRequestDoneStatus(req.getId());
                    if ((processTxReq) && (req.equals(associatedReq))) {
                        txPropObj.setPropagationStatus(TxPropagationObject.PropagationStatus.complete);
                        processTxReq = false;
                    }
                }
                mInOnlyRequests.clear();
            }
        }
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#sendErrorsForPendingRequests(java.lang.Exception)
     */
    public void sendErrorsForPendingRequests(Exception error) {

        boolean bpAtomic = mInstance.getBPELProcessManager().isBPAtomic();
        boolean processTxReq = false;
        MessageContainer associatedReq = null;
        TxPropagationObject txPropObj = null;

        if (bpAtomic) {
            txPropObj = mInstance.getTxPropagationObject();
            associatedReq = txPropObj.getParentContainer();
            if ((txPropObj.getPropagationStatus().equals(TxPropagationObject.PropagationStatus.active))
                    || (txPropObj.getPropagationStatus().equals(TxPropagationObject.PropagationStatus.unavailable))) {

                processTxReq = true;

                if (txPropObj.getAtomicTransactionType().equals(TxPropagationObject.TransactionType.started)) {
                    TransactionManager tm = (TransactionManager) BPELSERegistry.getInstance().lookup(
                            TransactionManager.class.getName());
                    Transaction tx = txPropObj.getTransaction();

                    try {
                        tm.resume(tx);
                        tx.rollback();
                    } catch (Exception e) {
                        throw new RuntimeException(I18n
                                .loc("BPCOR-7138: Atomic mode, encountered an error while trying to rollback "
                                        + "a Transaction that was started by the bpel-se."), e);
                    }

                }
            }

            synchronized (mInOnlyRequests) {
                // Process the requests in reverse order so that the last
                // request in is completed first.
                for (int i = (mInOnlyRequests.size() - 1); i >= 0; i--) {
                    MessageContainer req = (MessageContainer) mInOnlyRequests.get(i);
                    if ((processTxReq) && (req.equals(associatedReq))) {
                        txPropObj.setPropagationStatus(TxPropagationObject.PropagationStatus.error);
                        processTxReq = false;
                    }

                    mInstance.getBPELProcessManager().sendRequestError((String) req.getId(), error);
                }
                mInOnlyRequests.clear();
            }
        }

        synchronized (mRequests) {
            Iterator reqIter = mRequests.keySet().iterator();
            while (reqIter.hasNext()) {
                RequestReplyKeyImpl key = (RequestReplyKeyImpl) reqIter.next();
                MessageContainer req = (MessageContainer) mRequests.get(key);
                // processTxReq might be true only if BP is atomic
                if ((processTxReq) && (req.equals(associatedReq))) {
                    txPropObj.setPropagationStatus(TxPropagationObject.PropagationStatus.error);
                    processTxReq = false;
                }
                mInstance.getBPELProcessManager().sendRequestError((String) req.getId(), error);
                reqIter.remove();
            }
        }
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.model.runtime.IMAScope#declareDefaultMessageExchange()
     */
    public void declareDefaultMessageExchange() {
        hasDefaultMessageExchange = true;
    }

    public boolean hasOutstandingRequests() {
        synchronized (mRequests) {
            if (mRequests!= null && mRequests.size() > 0) {
                return true;
            }
            return false;
        }
    }
}
