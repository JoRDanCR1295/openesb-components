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
 * @(#)BPELSEThreadExecutor.java
 *
 * Copyright 2004-2011 Open ESB Community
 *
 * END_HEADER - DO NOT EDIT
 */


/**
 *
 * @author Alexander Lomov
 */

package com.sun.jbi.engine.bpel.core.bpel.engine.execution;


import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class BPELSEThreadExecutor {

    private ThreadPoolExecutor executor;
    private static final ReentrantReadWriteLock rwl = new ReentrantReadWriteLock();
    private static final Lock rl = rwl.readLock();
    private static final Lock wl = rwl.writeLock();

    private static BPELSEThreadExecutor me;
    


    private BPELSEThreadExecutor(int maxPoolSize){
        LinkedBlockingQueue<Runnable> queue = new LinkedBlockingQueue<Runnable>();
        executor = new ThreadPoolExecutor(4, maxPoolSize, 600, TimeUnit.SECONDS, queue, new ExecutorThreadFactory());
    }


    public static BPELSEThreadExecutor instance(){
        try {
            rl.lock();
            if (me == null)
                throw new RuntimeException("Call BPELSEThreadExecutor.start first");
            return me;
        } finally{
            rl.unlock();
        }
    }


    public static void start(int threadPoolSize){
        try{
            wl.lock();
            if (me == null)
                me = new BPELSEThreadExecutor(threadPoolSize);
            else
                me.setThreadPoolSize(threadPoolSize);
        } finally {
            wl.unlock();
        }
    }

    public void stop(){
        try{
            wl.lock();
            executor.shutdown();
            me = null;
        } finally {
            wl.unlock();
        }
    }


    public void execute(Runnable task){
        try{
            wl.lock();
            executor.execute(task);
        } finally{
            wl.unlock();
        }
        
    }

    private void setThreadPoolSize(int threadPoolSize) {        
        try{
            wl.lock();
            executor.setMaximumPoolSize(threadPoolSize);
        } finally{
            wl.unlock();
        }
    }



    private static class ExecutorThreadFactory implements ThreadFactory {

        private AtomicLong threadNumber = new AtomicLong(0);
        private final String name = "sun-bpel-engine-execution-thread";

        public ExecutorThreadFactory() {
            super();
        }

        public Thread newThread(Runnable r) {
            Thread daemonThread = new Thread(r, name + getThreadNumber());
            daemonThread.setDaemon(true);
            return daemonThread;
        }

        private long getThreadNumber() {
            return threadNumber.getAndIncrement();
        }
    }


}
