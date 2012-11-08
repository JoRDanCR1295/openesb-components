package com.sun.jbi.engine.iep;

import java.sql.Connection;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledFuture;

/**
 * 
 * @author radval
 *
 */
public class FutureInfo {

    private Connection mSource;
    private Connection mTarget;
    private Future mFuture;

    public FutureInfo(Connection source,
            Connection target,
            Future future) {
        this.mSource = source;
        this.mTarget = target;
        this.mFuture = future;
    }

    /**
     * @return the mFuture
     */
    public Future getFuture() {
        return mFuture;
    }

    /**
     * @return the mSource
     */
    public Connection getSource() {
        return mSource;
    }

    /**
     * @return the target
     */
    public Connection getTarget() {
        return mTarget;
    }
}
