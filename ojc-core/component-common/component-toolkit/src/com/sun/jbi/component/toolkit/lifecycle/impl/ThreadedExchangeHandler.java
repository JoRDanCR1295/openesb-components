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
 * @(#)ThreadedExchangeHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.lifecycle.impl;

import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchange.Role;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.ObjectName;
import com.sun.jbi.common.util.ThreadPoolQueue;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.util.I18n;

/**
 * An {@link ExchangeHandler} decorator that manages as many as three thread
 * pools to handle message exchanges accepted from the NMR.
 * 
 * @author Kevan Simpson
 */
public class ThreadedExchangeHandler implements ExchangeHandler {
    public static final String EXCHANGE_THREADING_PROPERTY = "ExchangeThreading";
    public static final String DEFAULT_EXCHANGE_THREADING = "-1";

    private ExchangeHandler mHandler;
    private Logger mLogger;
    private int mPoolCount; // helps during shutDown
    // Pool settings
    private int mCorePoolSize;
    private int mKeepAliveTime;
    private TimeUnit mTimeUnit;
    // By using an unbounded queue the max pool size becomes irrelevant
//    private int mRequestMaxPoolSize = Integer.MAX_VALUE;
//    private int mStatusMaxPoolSize = Integer.MAX_VALUE;
//    private int mReplyMaxPoolSize = Integer.MAX_VALUE;
    // thread pools
    private ThreadPoolExecutor mRequestPool, mStatusPool, mReplyPool;
    
    public ThreadedExchangeHandler(ExchangeHandler handler) {
        this(handler, -1, -1, null);    // default values
    }

    public ThreadedExchangeHandler(ExchangeHandler handler, int corePoolSize,
                                   int keepAliveTime, TimeUnit timeUnit) {
        mHandler = handler;
        mCorePoolSize = (corePoolSize < 1) ? 5 : corePoolSize;
        mKeepAliveTime = (keepAliveTime < 1) ? 60 * 10 : keepAliveTime;
        mTimeUnit = (timeUnit == null) ? TimeUnit.SECONDS : timeUnit;
        mLogger = Util.getLogger(handler.getContext().getComponentContext(), 
                                 mHandler.getClass().getName());
    }

    /**
     * Fetches the decorated {@link ExchangeHandler}.
     * @return the decorated <code>ExchangeHandler</code>.
     */
    public ExchangeHandler getDecoratedHandler() {
        return mHandler;
    }
    
    /** @see com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler#handleExchange(javax.jbi.messaging.MessageExchange) */
    public void handleExchange(MessageExchange mex) throws JBIException {
        HandleAction axn = new HandleAction(
                getDecoratedHandler(), mex, mLogger);
        switch (mPoolCount) {
            case 0: {
                axn.run();
                break;
            }
            case 1: {
                mRequestPool.execute(axn);
                break;
            }
            case 2:
            case 3: {
                if (mex.getRole().equals(Role.PROVIDER)) {
                    if (mex.getStatus().equals(ExchangeStatus.ACTIVE)) {
                        mRequestPool.execute(axn);
                    }
                    else {
                        mStatusPool.execute(axn);
                    }
                }
                else {
                    mReplyPool.execute(axn);// same as Status pool if count == 2
                }
            }
        }
    }

    /** @see javax.jbi.component.ComponentLifeCycle#init(javax.jbi.component.ComponentContext) */
    public void init(ComponentContext ctx) throws JBIException {
        getDecoratedHandler().init(ctx);
        // won't init pools if decorated handler fails init()
        String cfg = getContext().getConfiguration()
            .getProperty(EXCHANGE_THREADING_PROPERTY).getValue();
        try {
            initPools(cfg);
            getContext().getConfiguration().addNotificationListener(this);
        }
        catch (Exception ex) {
            shutDownPools();
            throw new JBIException(ex);
        }
    }

    /** @see javax.jbi.component.ComponentLifeCycle#shutDown() */
    public void shutDown() throws JBIException {
        JBIException jbi = null;
        try {
            getDecoratedHandler().shutDown();
        }
        catch (JBIException ex) {
            jbi = ex;   // decorated should've logged, will throw in just a sec...
        }
        
        shutDownPools();
        
        if (jbi != null) {
            throw jbi;
        }
    }
    
    /** @see javax.jbi.component.ComponentLifeCycle#start() */
    public void start() throws JBIException {
        getDecoratedHandler().start();
    }

    /** @see javax.jbi.component.ComponentLifeCycle#stop() */
    public void stop() throws JBIException {
        getDecoratedHandler().stop();
    }

    /** @see javax.jbi.component.ComponentLifeCycle#getExtensionMBeanName() */
    public ObjectName getExtensionMBeanName() {
        return null;    // no op
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ContextAware#getContext() */
    public ManagerContext getContext() {
        return getDecoratedHandler().getContext();
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ContextAware#setContext(com.sun.jbi.component.toolkit.lifecycle.ManagerContext) */
    public void setContext(ManagerContext ctx) {
        getDecoratedHandler().setContext(ctx);
    }

    /** 
     * @see javax.management.NotificationListener#handleNotification(javax.management.Notification, java.lang.Object)
     * @see PollerConfigMBean#setExchangeThreading(String) 
     */
    public void handleNotification(Notification note, Object handback) {
        if (note instanceof AttributeChangeNotification) {
            AttributeChangeNotification chg = (AttributeChangeNotification) note;
            if (chg.getAttributeName().equals(EXCHANGE_THREADING_PROPERTY)) {
                // avoiding complicated logic to determine resize vs. new pool
                shutDownPools();
                initPools(String.valueOf(chg.getNewValue()));
            }
        }
    }

    /**
     * Initializes the exchange handling thread pools.
     * @param cfg The raw threading configuration.
     * @see PollerConfig#getExchangeThreading()
     */
    protected void initPools(String cfg) {
        String name = getDecoratedHandler().getClass().getSimpleName();
        if (Util.isEmpty(cfg)) {
            mRequestPool = mStatusPool = mReplyPool = null;
            mPoolCount = 0;
        }
        else {
            String[] tkns = Util.tokenize(cfg, ",");
            mPoolCount = tkns.length;
            switch (mPoolCount) {
                case 3: {
                    mReplyPool = newPool(tkns[2], name +"-Reply");
                    mStatusPool = newPool(tkns[1], name +"-Status");
                    mRequestPool = newPool(tkns[0], name +"-Request");
                    break;
                }
                case 2: {   // reply + status share the same pool
                    mReplyPool = mStatusPool = newPool(tkns[1], name +"-Status+Reply");
                    mRequestPool = newPool(tkns[0], name +"-Request");
                    break;
                }
                case 1: {   // one pool for all MEx, unless non-positive or non-numeric
                    if (!Util.isEmpty(tkns[0]) && Util.parseInt(tkns[0], -1) > 0) {
                        mReplyPool = mStatusPool = mRequestPool = newPool(tkns[0], name +"-MEx");
                        break;
                    }
                    // else fall through and have no pool
                }
                default: {  // no pool, totally synchronous
                    mReplyPool = mStatusPool = mRequestPool = null;
                    mPoolCount = 0;
                    break;
                }
            }
        }
        
        logInitPools();
    }
    
    /**
     * Utility method to log the initialized thread pools.
     */
    protected void logInitPools() {
        if (mLogger.isLoggable(Level.CONFIG)) {
            if (mRequestPool == null || mPoolCount == 0) {
                mLogger.config(I18n.loc(
                        "COMPTK-4001: No exchange thread pools created, {0} is synchronous.", 
                        getContext().getComponentContext().getComponentName()));
            }
            else {
                StringBuffer buff = new StringBuffer();
                switch (mPoolCount) {
                    case 3: {
                        buff.append(",")
                            .append(mReplyPool.getThreadFactory().toString())
                            .append("(").append(mReplyPool.getMaximumPoolSize())
                            .append(")");
                    }
                    case 2: {   // reply + status share the same pool
                        buff.append(",")
                            .append(mStatusPool.getThreadFactory().toString())
                            .append("(").append(mStatusPool.getMaximumPoolSize())
                            .append(")");
                    }
                    case 1: {   // one pool for all MEx
                        buff.append(mReplyPool.getThreadFactory().toString())
                            .append("(").append(mReplyPool.getMaximumPoolSize())
                            .append(")");
                        break;
                    }
                }

                mLogger.config(I18n.loc(
                        "COMPTK-4002: Created {0} exchange thread pool(s) - {1}", 
                        String.valueOf(mPoolCount), buff.toString()));
            }
        }
    }

    /**
     * Creates a new thread pool, which uses a {@link DaemonThreadFactory}
     * with the specified name.
     * <p>
     * If the specified <code>newSize</code> is non-numeric or less than 1,
     * a synchronous thread pool (i.e. zero threads) will be returned.
     * 
     * @param newSize The maxiumum pool size.
     * @param name The name of the thread factory.
     * @return a thread pool for handling exchanges.
     */
    protected ThreadPoolExecutor newPool(String newSize, String name) {
        int size = Util.parseInt(newSize, -1);
        ThreadPoolExecutor pool = null;
        ThreadPoolQueue queue = new ThreadPoolQueue();

        if (size >= 1) {
            pool = new ThreadPoolExecutor(
                    Math.min(mCorePoolSize, size), size, mKeepAliveTime, mTimeUnit,
                    queue, new DaemonThreadFactory(name));
        }
        
        if (pool == null) { // return synchronous thread pool
            pool = (ThreadPoolExecutor) 
                    Executors.newFixedThreadPool(1, new DaemonThreadFactory(name));
        }

        queue.setThreadPoolExecutor(pool);
        pool.prestartAllCoreThreads();
        
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.config(I18n.loc(
                    "COMPTK-4003: Started {0} core threads in {1} pool.", 
                    String.valueOf(pool.getCorePoolSize()), name));
        }
        
        return pool;
    }

    /**
     * Starts a thread for each pool and calls {@link ThreadPoolExecutor#shutdown()}.
     */
    protected void shutDownPools() {
        switch (mPoolCount) {
            case 3:
                (new Thread(new CloseAction(mReplyPool, mLogger))).start();
            case 2: 
                (new Thread(new CloseAction(mStatusPool, mLogger))).start();
            case 1:
                (new Thread(new CloseAction(mRequestPool, mLogger))).start();
        }
    }

    /**
     * Simple {@link Runnable} to handle message exchanges.
     */
    private static class HandleAction implements Runnable {
        private final ExchangeHandler mHandler;
        private final MessageExchange mExchange;
        private final Logger mLog;
        
        /** Creates a new instance of HandleAction */
        public HandleAction(ExchangeHandler handler, MessageExchange exchange, Logger log) {
            mHandler = handler;
            mExchange = exchange;
            mLog = log;
        }

        /** @see java.lang.Runnable#run() */
        public void run() {
            try {
                // the decorated handler is expected to make a good faith
                // effort to complete the MEx, because this class will not
                mHandler.handleExchange(mExchange);
            } 
            catch (Exception ex) {
                mLog.log(Level.SEVERE, I18n.loc(
                        "COMPTK-7001: Failed handling exchange-{0}: {1}", mExchange.getExchangeId(), ex.getMessage()), ex);
            }
        }
    }
    
    /**
     * Simple {@link Runnable} to shutdown exchange handling thread pools.
     */
    private static class CloseAction implements Runnable {
        private ThreadPoolExecutor mPool;
        private Logger mLog;
        
        public CloseAction(ThreadPoolExecutor pool, Logger log) {
            mPool = pool;
            mLog = log;
        }
        
        /** @see java.lang.Runnable#run() */
        public void run() {
            try {
                if (mLog.isLoggable(Level.FINER)) {
                    mLog.finer("COMPTK-2008: Shutting down exchange thread pool: "+ 
                            mPool.getThreadFactory().toString());
                }
                
                mPool.shutdown();

                if (mLog.isLoggable(Level.FINER)) {
                    mLog.finer("COMPTK-2009: Exchange thread pool shut down: "+ 
                            mPool.getThreadFactory().toString());
                }
            } 
            catch (Exception ex) {
                mLog.log(Level.WARNING, I18n.loc(
                        "COMPTK-6031: Error occurred shutting down exchange handling thread pool: {0}", 
                        ex.getMessage()), ex);
            }
        }
    }
    
    /**
     * Creates new threads for the pools.
     * @author Sujit Biswas
     */
    private static class DaemonThreadFactory implements ThreadFactory {
        private AtomicLong mThreadNumber = new AtomicLong(1);
        private String mName;
        
        /**
         * @param name
         */
        public DaemonThreadFactory(String name) {
            mName = name;
        }
    
        public Thread newThread(Runnable r) {
            Thread daemonThread = new Thread(r, mName + getThreadNumber());
            daemonThread.setDaemon(Boolean.TRUE);
            return daemonThread;
        }
        
        private long getThreadNumber(){
            //TODO what happens if l reaches Long.MAX_VALUE
            return mThreadNumber.getAndIncrement();
        }

        /** @see java.lang.Object#toString() */
        @Override
        public String toString() {
            return mName +" ThreadFactory";
        }
    }
}
