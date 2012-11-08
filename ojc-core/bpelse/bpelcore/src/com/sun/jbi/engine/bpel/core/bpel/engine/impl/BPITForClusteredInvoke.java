package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import java.util.Map;

import javax.transaction.Transaction;

import com.sun.jbi.engine.bpel.core.bpel.dbo.CRMPDBO;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RequiredObjects;


public class BPITForClusteredInvoke extends BusinessProcessInstanceThread {

    ResponseInComingEventKeyImpl mEvent;
    RequiredObjects mRObjs;
    String mCrmpInvokeId;
    String meId;
    Transaction transaction;

    public BPITForClusteredInvoke(RequiredObjects rObjs, ResponseInComingEventKeyImpl event, String crmpInvokeId, 
    		Object meId, Transaction transaction, ICallFrame callFrame) {
        super(rObjs.getBPELProcessManager(), rObjs.getEngine(), callFrame);
        this.mRObjs = rObjs;
        this.mEvent = event;
        this.mCrmpInvokeId = crmpInvokeId;
        this.meId = meId.toString();
        this.transaction = transaction;
    }

    public boolean getInvokeCRMPResponse() {
        MessageContainer msgC = getResponseMessage();
        
        if (msgC != null) {
            msgC.setTransaction(transaction);
            Map eventResponseMap = mRObjs.getBPELProcessManager().getEventResponseMap();
            eventResponseMap.put(mEvent, msgC);
            return true;
        }
        return false;
    }

    private MessageContainer getResponseMessage() {
        CRMPDBO crmpDbo = null;
        Engine engine = mRObjs.getEngine();
        try {
            crmpDbo = engine.getEngStateMgr().getCRMPDBOForCRMPInvokeID(mCrmpInvokeId, true);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        if (crmpDbo == null) {
            return null; // Response not created yet
        }

        MessageContainer con = mRObjs.getBPELProcessManager().getMessageContainerForCRMP(mCrmpInvokeId,
                meId, crmpDbo.getReplyVariableId());
        return con;
    }
}
