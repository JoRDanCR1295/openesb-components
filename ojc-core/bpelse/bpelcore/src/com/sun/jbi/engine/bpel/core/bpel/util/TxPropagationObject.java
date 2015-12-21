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
 * @(#)TransactionPropagationObject.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.core.bpel.util;

import javax.transaction.Transaction;
import javax.transaction.TransactionManager;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;

/**
 *
 *
 * @author Sun Microsystems
 */
public class TxPropagationObject {
	
	public enum PropagationStatus {
		uninitialized,
		unavailable,
		active, 
		waitingOnReply,
		replyComplete, 
		complete,
		error
	}
	
	/**
	 * Provides the type of the Transaction that this Object encapsulates. Used for the instance
	 * that is marked as in atomic mode.  
	 * inbound : specifies that the Tx is associated with the inbound message. 
	 * started : specifies that the Tx is started by the bpel-se as there was no Tx associated 
	 * with the inbound message. 
	 */
	public enum TransactionType {
		nosupport,
		participates,
		started
	}
	
	private MessageContainer mRequest;
	
	private Transaction mTransaction;
	
	private ICallFrame mReplyFrame;
	
	private PropagationStatus mPropStatus;
	
	private BPELProcessInstance mBpelProcInstance;
	
	private TransactionType mAtomicTxType;
	
	public static final String TX_TYPE_REQUIRED = RBPELProcess.ATOMIC_TX_TYPES[1];

	public TxPropagationObject(BPELProcessInstance bpelProcInstance) {
		mPropStatus = PropagationStatus.uninitialized;
		mBpelProcInstance = bpelProcInstance;
	}
	
	/**
	 * Method that either propagates or begins a transaction context. 
	 * If the inbound message (request) container does not have an associated transaction
	 * context then a new one is associated. 
	 * Addition: If the 'atomicTxType=Required' and the there is no Tx associated with the
	 * inbound message exchange then start a new transaction. Refer to 
	 * <code>BPELProcessManager.mAtomicTxType</code> for more information on the attribute
	 * values.
	 * Registers a javax.transaction.Synchronization object with the transaction for callback's.
	 * @param request
	 * @return
	 */
    public boolean extractTransactionFromRequest(MessageContainer request, BPELProcessManager processMgr) {
        mAtomicTxType = TransactionType.nosupport;
        return false;
/*        mRequest = request;
        mTransaction = request.getTransaction();
        String atomicTxType = processMgr.getAtomicTxType();
        if (mTransaction == null && atomicTxType != null && atomicTxType.equals(TX_TYPE_REQUIRED)) {
            TransactionManager tm = (TransactionManager) BPELSERegistry.getInstance().lookup(TransactionManager.class.getName());
            try {
                tm.begin();
                mTransaction = tm.getTransaction();
                mRequest.setTransaction(mTransaction);
                tm.suspend();
            } catch (Exception e) {
                throw new RuntimeException(I18n.loc("BPCOR-7136: Atomic mode, encountered an error while trying to begin "
                        + "and associate a transaction."), e);
            }
            mAtomicTxType = TransactionType.started;

        } else if (mTransaction == null) {
            mAtomicTxType = TransactionType.nosupport;
            return false;
        } else {
            mAtomicTxType = TransactionType.participates;
        }

        TxPropagationSynchronization txPropSync = new TxPropagationSynchronization(this);
        try {
            mTransaction.registerSynchronization(txPropSync);
            return true;
        } catch (Exception e) {
            throw new RuntimeException(I18n.loc("BPCOR-6105: Encountered an error while trying to register "
                    + "synchronization object with the transaction to be propagated."), e);
        }*/
    }
	
	public Transaction getTransaction() {
		return mTransaction;
	}
	
	public MessageContainer getParentContainer() {
		return mRequest;
	}
	
	public PropagationStatus getPropagationStatus() {
		return mPropStatus;
	}
	
	public void setPropagationStatus(PropagationStatus propStatus) {
		mPropStatus = propStatus;
	}
	
	public void setReplyFrame(ICallFrame callFrame) {
		mReplyFrame = callFrame;
	}
	
	public ICallFrame getReplyFrame() {
		return mReplyFrame;
	}

	public BPELProcessInstance getBPELProcessInstance() {
		return mBpelProcInstance;
	}

	public TransactionType getAtomicTransactionType() {
		return mAtomicTxType;
	}

	public void setAtomicTransactionType(TransactionType type) {
		this.mAtomicTxType = type;
	}

}
