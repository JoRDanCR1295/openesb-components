package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.io.File;
import java.io.FileInputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.transaction.Status;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;
import com.sun.jbi.engine.bpel.core.bpel.util.TxPropagationObject;

public class BPITForTransactionSync extends
		BusinessProcessInstanceThread {

	private static final Logger LOGGER = Logger.getLogger(BPITForTransactionSync.class.getName());
	private int transactionstatus;
	private TxPropagationObject mTxPropObj;
	
	public BPITForTransactionSync(
			BPELProcessManager processManager, Engine engine, ICallFrame frame, TxPropagationObject txPropObj,int transactionstatus) {
		super(processManager, engine, frame);
		this.transactionstatus = transactionstatus;
		this.mTxPropObj = txPropObj;
	}
	
	 /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread#execute()
     */
    public void execute() {
    	if (Status.STATUS_COMMITTED == this.transactionstatus) {
    		synchronized (mTxPropObj) {
    			if (mTxPropObj.getPropagationStatus().equals(TxPropagationObject.PropagationStatus.replyComplete)) {
    				ICallFrame callFrame = mTxPropObj.getReplyFrame();
    				this.mProcessManager.addToReadyToRunQueue(callFrame);
    				mTxPropObj.setPropagationStatus(TxPropagationObject.PropagationStatus.complete);
    			}
    		}
    	} else {
			// Terminate the process since the transaction is rolled back. No point in continuing.
			this.mProcessManager.terminate(mTxPropObj.getBPELProcessInstance());
		}
    }    

}
