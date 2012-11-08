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
 * @(#)WorkManagerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jca;

import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;

import java.util.logging.Logger;
import java.util.logging.Level;

import javax.resource.spi.work.ExecutionContext;
import javax.resource.spi.work.Work;
import javax.resource.spi.work.WorkException;
import javax.resource.spi.work.WorkListener;
import javax.resource.spi.work.WorkManager;

import com.sun.jbi.jmsbc.LogSupport;

import com.sun.jbi.internationalization.Messages;

/**
 *
 * Implementation of WorkManager
 */
public class WorkManagerImpl implements WorkManager {

    private static final Messages mMessages =
        Messages.getMessages(WorkManagerImpl.class);
    private static final Logger mLogger =
        Messages.getLogger(WorkManagerImpl.class);    
    
    private ExecutorService threadPool;
    
    public WorkManagerImpl() {
        threadPool = Executors.newCachedThreadPool();
    }

    /**
     * @see javax.resource.spi.work.WorkManager#doWork(javax.resource.spi.work.Work)
     */
    public void doWork(Work arg0) throws WorkException {
        String errMsg = mMessages.getString("JMSBC-E0102.WorkManagerMethodNotImplemented",
                    new Object [] {"doWork(Ljavax.resource.spi.work.Work;)V"});
        throw new IllegalStateException(errMsg);
    }

    /**
     * @see javax.resource.spi.work.WorkManager#doWork(javax.resource.spi.work.Work, long, javax.resource.spi.work.ExecutionContext, javax.resource.spi.work.WorkListener)
     */
    public void doWork(Work arg0, long arg1, ExecutionContext arg2, WorkListener arg3) throws WorkException {
        String errMsg = mMessages.getString("JMSBC-E0102.WorkManagerMethodNotImplemented",
                    new Object [] {"doWork(Ljavax.resource.spi.work.Work;JLjavax.resource.spi.work.ExecutionContext;Ljavax.resource.spi.work.WorkListener;)V"});
        throw new IllegalStateException(errMsg);
    }

    /**
     * @see javax.resource.spi.work.WorkManager#startWork(javax.resource.spi.work.Work)
     */
    public long startWork(Work arg0) throws WorkException {
        String errMsg = mMessages.getString("JMSBC-E0102.WorkManagerMethodNotImplemented",
                    new Object [] {"startWork(Ljavax.resource.spi.work.Work;)J"});
        throw new IllegalStateException(errMsg);
    }

    /**
     * @see javax.resource.spi.work.WorkManager#startWork(javax.resource.spi.work.Work, long, javax.resource.spi.work.ExecutionContext, javax.resource.spi.work.WorkListener)
     */
    public long startWork(Work arg0, long arg1, ExecutionContext arg2, WorkListener arg3) throws WorkException {
        String errMsg = mMessages.getString("JMSBC-E0102.WorkManagerMethodNotImplemented",
                    new Object [] {"startWork(Ljavax.resource.spi.work.Work;JLjavax.resource.spi.work.ExecutionContext;Ljavax.resource.spi.work.WorkListener;)J"});
        throw new IllegalStateException(errMsg);
    }

    /**
     * @see javax.resource.spi.work.WorkManager#scheduleWork(javax.resource.spi.work.Work)
     */
    public void scheduleWork(Work work) throws WorkException {
        try {
            threadPool.execute(work);            
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG,
                            "WorkManagerImpl_WORK_SCHEDULED",
                            new Object[]{work});                    
            }
        } catch (RejectedExecutionException e) {
            throw new WorkException(e);
        } catch (NullPointerException e) {
            throw new WorkException(e);
        }
    }

    /**
     * @see javax.resource.spi.work.WorkManager#scheduleWork(javax.resource.spi.work.Work, long, javax.resource.spi.work.ExecutionContext, javax.resource.spi.work.WorkListener)
     */
    public void scheduleWork(Work arg0, long arg1, ExecutionContext arg2, WorkListener arg3) throws WorkException {
        String errMsg = mMessages.getString("JMSBC-E0102.WorkManagerMethodNotImplemented",
                    new Object [] {"scheduleWork(Ljavax.resource.spi.work.Work;JLjavax.resource.spi.work.ExecutionContext;Ljavax.resource.spi.work.WorkListener;)V"});
        throw new IllegalStateException(errMsg);
    }
 
    /**
     * Disallow further work requests to be submitted and waits until all executing Worker threads to complete 
     * or until wait timeout has expired
     * 
     * @param waitSeconds Wait timeout for Worker threads to complete.
     * @return If all tasks had completed, then true will be returned, otherwise false
     *         will be returned.
     * @throws ManagerException upon error
     */    
    public boolean shutdownAndWaitForWorkersToComplete (long waitSeconds) throws ManagerException {
        try {
            boolean retVal = false;
            threadPool.shutdown();
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG,
                            "WorkManagerImpl_SHUTDOWN_THREADPOOL_SHUTDOWN_SUCCESS");
            }
            retVal=threadPool.awaitTermination(waitSeconds, TimeUnit.SECONDS);
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG,
                            "WorkManagerImpl_SHUTDOWN_AWAIT_WORK_THREADS_TERMINATION",
                            new Object [] {Boolean.valueOf(retVal)});
            }
            return retVal;
        } catch (SecurityException ex) {
            throw new ManagerException(ex);
        } catch (InterruptedException ex) {
            throw new ManagerException (ex);
        }
    }
}
