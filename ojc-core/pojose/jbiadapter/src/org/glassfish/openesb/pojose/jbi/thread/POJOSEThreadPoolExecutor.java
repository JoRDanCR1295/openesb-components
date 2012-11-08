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
 * @(#)POJOSEThreadPoolExecutor.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.pojose.jbi.thread;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.glassfish.openesb.pojose.jbi.I18n;
import org.glassfish.openesb.pojose.jbi.su.MasterTracker;

/**
 *
 * @author gpatil
 */
public class POJOSEThreadPoolExecutor extends ThreadPoolExecutor{
    private static final Logger logger = Logger.getLogger(
            org.glassfish.openesb.pojose.jbi.nmr.BasePojoExecutor.class.getName()); //PojoSE Executor

    private final ThreadLocal<Long> taskStartTime = new ThreadLocal<Long>() ;

    private final AtomicLong executorTasksActive = new AtomicLong() ;
    private final AtomicLong executorTaskCompleted = new AtomicLong() ;
    private final AtomicLong totalTaskExecTime = new AtomicLong() ;

    public POJOSEThreadPoolExecutor(int corePool, int maxPoolSize,
            long defKeepAliveTime, TimeUnit secs, BlockingQueue bq,
            ThreadFactory tf, RejectedExecutionHandler rh){
        super(corePool, maxPoolSize, defKeepAliveTime, secs, bq, tf, rh);
    }

    @Override
    protected void beforeExecute(Thread currThread, Runnable task) {
        super.beforeExecute(currThread, task) ;
        taskStartTime.set(System.nanoTime());
        if (logger.isLoggable(Level.FINEST)) {
            logger.finest(String.format("Executor starting thread %s with task %s.", //NOI18N
                    currThread.getName(), task.toString()));
        }
        if (task instanceof InMsgTask){
            InMsgTask it = (InMsgTask) task;
            String pojoClsName = it.getPojoServiceClassName();
            MasterTracker.getInstance().updateActiveInRequestForPojo(pojoClsName, true);
        }

        if (currThread instanceof TaskThread){
            TaskThread tt = (TaskThread) currThread;
            tt.setTarget(task);
        }
        executorTasksActive.incrementAndGet();
    }

    @Override
    protected void afterExecute(Runnable task, Throwable exp) {
        try {
            long endTime = System.nanoTime() ;
            long taskTime = endTime - taskStartTime.get() ;
            taskStartTime.remove();

            totalTaskExecTime.addAndGet(taskTime) ;
            if (logger.isLoggable(Level.FINEST)) {
                logger.finest( String.format("Executor finished running thread %s with task %s taking time %dns.", //NOI18N
                        Thread.currentThread(), task.toString(), taskTime));
            }

            if (task instanceof InMsgTask){
                InMsgTask it = (InMsgTask) task;
                String pojoClsName = it.getPojoServiceClassName();
                MasterTracker.getInstance().updateActiveInRequestForPojo(pojoClsName, false);
            }

            executorTaskCompleted.incrementAndGet();
            executorTasksActive.decrementAndGet();
        } finally {
            super.afterExecute(task, exp) ;
        }
    }

    @Override
    protected void terminated() {
        try {
            long completed = executorTaskCompleted.get();
            if (completed > 0){
                String m = I18n.loc("POJOSE-5512: Executor terminating. Average time per completed task is {0} ms.",
                        totalTaskExecTime.get() / (completed * 1000)); // nano secs to ms. 10^9 to 10^6
                logger.log(Level.INFO, m);
            }
        } finally {
            super.terminated() ;
        }
    }

    //

    public long getActualActiveExecutorTaskCount(){
        return this.executorTasksActive.get();
    }
}
