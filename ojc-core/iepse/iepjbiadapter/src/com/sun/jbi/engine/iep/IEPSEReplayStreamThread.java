package com.sun.jbi.engine.iep;

import java.sql.Connection;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.engine.iep.core.runtime.operator.impl.ExternalTablePollingStream;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ReplayStream;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.internationalization.Messages;

/**
 * Replay Stream
 * @author radval
 *
 */
public class IEPSEReplayStreamThread implements Runnable {

    private static final Messages mMessages = Messages.getMessages(IEPSEReplayStreamThread.class);
    private Logger mLogger = Logger.getLogger(IEPSEReplayStreamThread.class.getName());
    private ReplayStream mOperator;
    /**
     * connections are kept open and prepared stmt is created once for efficiency till the lifetime of this
     * thread since it is run periodically. 
     */
    private Connection mSource;
    private Connection mTarget;

    public IEPSEReplayStreamThread(Connection source, Connection target, ReplayStream operator) throws Exception {
        this.mSource = source;
        this.mTarget = target;
        this.mOperator = operator;
        this.mOperator.createStatements(mSource, mTarget);
    }

    public void run() {
        try {
            if (this.mOperator != null) {
//                this.mSource.setAutoCommit(false);
//                this.mTarget.setAutoCommit(false);
                //we do not set autocommit since this one executes at fix interval
                //not like external table polling where interval is configurable
                this.mOperator.executeStatements();
//                this.mSource.commit();
//                this.mTarget.commit();
            }
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("IEPSEReplayStreamThread.Problem_replaying_stream"),  ex);
//          throw an unchecked exception so that this thread does not run again
            throw new RuntimeException(ex);
        } 

    }
}
