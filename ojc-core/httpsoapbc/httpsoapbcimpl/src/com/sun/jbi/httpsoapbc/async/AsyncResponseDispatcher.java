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
 *
 * @author Alexander Lomov
 *
 * Copyright 2011 Open-ESB Community.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.httpsoapbc.async;

import com.sun.jbi.httpsoapbc.*;
import com.sun.jbi.httpsoapbc.configuration.RuntimeConfiguration;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.concurrent.locks.Lock;

public class AsyncResponseDispatcher {

    private ThreadPoolExecutor responsePool;
    private ThreadPoolQueue queue;
    private static AsyncResponseDispatcher me;
    private final static ReentrantReadWriteLock lock = new ReentrantReadWriteLock(false);
    private final static Lock rl = lock.readLock();
    private final static Lock wl = lock.writeLock();
    private NotificationListener notificationListener = new NotificationListener() {

        public void handleNotification(Notification notification, Object handback) {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                if (attrName.equals(RuntimeConfiguration.CONFIG_OUTBOUND_THREADS)) {
                    int newVal = (Integer) attrNotif.getNewValue();
                    setThreadPoolSize(newVal);
                }

            }
        }
    };

    public static void intitalize(RuntimeConfiguration config) {
        wl.lock();
        try {
            if (me == null) {
                me = new AsyncResponseDispatcher(config);
            } else {
                me.setThreadPoolSize(config.getOutboundThreads());
            }
        }
        finally {
            wl.unlock();
        }

    }

    public static void uninitialize(){
        wl.lock();
        try{            
            me = null;
        }
        finally {
            wl.unlock();
        }

    }

    public static AsyncResponseDispatcher instance() throws RuntimeException {
        rl.lock();
        try {
            if (me == null) {
                throw new RuntimeException("Call AsyncRequestDispatcher.inititlize first");
            }
            return me;
        } finally {
            rl.unlock();
        }
    }

    private AsyncResponseDispatcher(RuntimeConfiguration runtimeConfig) {
        runtimeConfig.addNotificationListener(notificationListener, null, null);

        queue = new ThreadPoolQueue();
        responsePool = new ThreadPoolExecutor(5, runtimeConfig.getOutboundThreads(), 600, TimeUnit.SECONDS, queue, new DaemonThreadFactory());
        queue.setThreadPoolExecutor(responsePool);
    }

    public <T> AsyncResponseHandler<T> getHandler(AsyncRequestContext context) {
        return new AsyncResponseHandler<T>(this, context);
    }

    private void setThreadPoolSize(int poolSize) {
        responsePool.setMaximumPoolSize(poolSize);
    }

    void dispatch(AsyncResponseProcessor processor) {
        responsePool.execute(processor);
    }

    private static class DaemonThreadFactory implements ThreadFactory {

        private AtomicLong threadNumber = new AtomicLong(0);
        private final String name = "HTTPBC-OutboundAsyncReceiver-";

        public DaemonThreadFactory() {
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

