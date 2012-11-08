package com.sun.jbi.engine.iep;

import java.sql.Connection;
import java.util.Date;
import java.util.concurrent.Future;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.engine.iep.core.runtime.operator.impl.ExternalTablePollingStream;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.internationalization.Messages;

/**
 * 
 * @author radval
 *
 */
public class IEPSEExternalTablePollingThread implements Runnable {

    private static final Messages mMessages = Messages.getMessages(IEPSEExternalTablePollingThread.class);
    private Logger mLogger = Logger.getLogger(IEPSEExternalTablePollingThread.class.getName());
    private ExternalTablePollingStream mOperator;
    
    /**
     * connections are kept open and prepared stmt is created once for efficiency till the lifetime of this
     * thread since it is run periodically. 
     */
    private Connection mSource;
    private Connection mTarget;
    
    public IEPSEExternalTablePollingThread(Connection source, Connection target, ExternalTablePollingStream operator) throws Exception {
        this.mSource = source;
        this.mTarget = target;
        this.mOperator = operator;
        this.mOperator.createStatements(mSource, mTarget);
    }

    public void run() {
        try {
                if (this.mOperator != null) {
                    this.mSource.setAutoCommit(false);
                    this.mTarget.setAutoCommit(false);
                    this.mOperator.executeStatements();
                    this.mSource.commit();
                    this.mTarget.commit();
                }
            
        }  catch (Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("IEPSEExternalTablePollingThread.Problem_polling_external_table"),  ex);
            //throw an unchecked exception so that this thread does not run again
            throw new RuntimeException(ex);
        }

    }
    
    
}
