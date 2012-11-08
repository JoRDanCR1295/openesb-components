package com.sun.jbi.engine.bpel.core.bpel.engine.impl;

import com.sun.bpel.model.Pick;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.ReceiveUnitImpl;

public class BPITForPendingInstancePassivation extends BusinessProcessInstanceThread {

    private BPELProcessManager mProcessManager = null;
   
    private Pick mPick;
    
    private long mDeadline;
    
    InComingEventKeyImpl[] mEvents;

    public BPITForPendingInstancePassivation(InComingEventKeyImpl[] events, BPELProcessManager processManager,
            Engine engine, ICallFrame callframe, Pick pick, long deadline) {
        super(processManager, engine, callframe);
        this.mEvents = events;
        this.mProcessManager = processManager;
        this.mPick = pick;
        this.mDeadline = deadline;
    }

    public boolean isReady() {
        int activeMsgCount = mFrame.getProcessInstance().getClusterMgr().getActiveMessagingActivitiesCount();
        return activeMsgCount == 0 ? true : false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.engine.bpel.core.bpel.engine.BusinessProcessInstanceThread#execute()
     */
    public void execute() {
        if (mProcessManager.checkRequest(mEvents)) {
            // do one more check to see of the message has arrived for this activity unit
            mEngine.getInterpreter().execute(this, mProcessManager);
        } else {
            // passivate the instance and clean up from memory
            mFrame.getProcessInstance().getClusterMgr().passivateForFlowBranchCase(mEvents, mFrame, 
                    mEngine, mPick, mDeadline);
        }
    }
}
