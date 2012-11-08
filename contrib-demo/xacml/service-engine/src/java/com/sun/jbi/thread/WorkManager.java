/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)WorkManager.java - ver 1.EA7 - 12/10/2005
 *
 * Copyright 2004-2005 Sun Microsystems, Inc. All Rights Reserved.
 */

package com.sun.jbi.thread;

import java.util.Hashtable;

import java.util.logging.Logger;

/**
 * This class manages the work requests and dispatches the requests to be
 * executed in a free thread.
 *
 * @author Praveen Patange
 */
public final class WorkManager
{
    /**
     * Handle to the list of Work Manager instances.
     */
    private static Hashtable sWorkManagerBucket = new Hashtable();

    /**
     * Internal handle to the logger instance.
     */
    private static Logger sLog;

    /**
     * Default value for max threads.
     */
    private static final int DEFAULT_MAX_THREADS = 10;

    /**
     * Default value for min threads.
     */
    private static final int DEFAULT_MIN_THREADS = 2;

    /**
     * Handle to store the Work Manager state. Valid values are "INIT", "READY"
     * and "STOP".
     */
    private String mState;

    /**
     * Handle to the WorkThreadPool instance.
     */
    private WorkThreadPool mThreadPool;

    /**
     * Value for max threads.
     */
    private int mMaxNumberOfThreads = DEFAULT_MAX_THREADS;

    /**
     * Value for min threads.
     */
    private int mMinNumberOfThreads = DEFAULT_MIN_THREADS;

    /**
     * Creates a new instance of WorkManager.
     */
    private WorkManager()
    {
        mState = "PARKED";
        init();
    }

    /**
     * Sets the log file.
     *
     * @param logFile log file.
     */
    public void setLogger(String logFile)
    {
        sLog = sLog.getLogger(logFile);
    }

    /**
     * This method can be used to set the maximum threads in the Pool.
     *
     * @param count maximum threads.
     */
    public void setMaxThreads(int count)
    {
        mMaxNumberOfThreads = count;
    }

    /**
     * Sets the minimu threads in the Thread pool.
     *
     * @param count minimum thread.
     */
    public void setMinThreads(int count)
    {
        mMinNumberOfThreads = count;
    }

    /**
     * Returns a handle to the Work Manager instance for a unique service .
     *
     * @param name name of the work manager.
     *
     * @return a work manager instance.
     */
    public static WorkManager getWorkManager(String name)
    {
        WorkManager manager = null;

        String serviceName = name;

        if (serviceName != null)
        {
            manager = (WorkManager) sWorkManagerBucket.get(serviceName);

            if (manager == null)
            {
                manager = new WorkManager();

                sWorkManagerBucket.put(serviceName, manager);
            }
        }
        else
        {
            sLog = Logger.getLogger("com.sun.jbi.binding.jms.framework");
            sLog.severe("service name is null Cannot get work manager");
        }

        return manager;
    }

    /**
     * Returns the number of wokring threads in the pool.
     *
     * @return count of number of busy threads
     */
    public int getBusyThreads()
    {
        return mThreadPool.getBusyThreads();
    }

    /**
     * Cleans up the workmanager. It notifies the workthread pool to shutdown
     * all its threads.
     */
    public void cease()
    {
        // Stop the thread pool.
        if (mState.equals("INIT") || mState.equals("STARTED"))
        {
            try
            {
                mThreadPool.stop();
            }
            catch (Exception e)
            {
                e.printStackTrace();
            }
        }

        setState("STOPPED");
    }

    /**
     * Stop the system when all threads are done.
     */
    public void finishWork()
    {
        mThreadPool.exitWhenBusyThreadsDone();
    }

    /**
     * Initializes the Work Manager.
     */
    public void init()
    {
        if (mState.equals("INIT"))
        {
            return;
        }

        sLog = Logger.getLogger(this.getClass().getPackage().getName());
        sWorkManagerBucket = new Hashtable();
        mThreadPool = new WorkThreadPool();
        mThreadPool.setMinThreads(mMinNumberOfThreads);
        mThreadPool.setMaxThreads(mMaxNumberOfThreads);
        mThreadPool.init();
        setState("INIT");
    }

    /**
     * Process the Command in a different thread. The method places the command
     * in its internal cache and returns control to the invoking thread.
     *
     * @param command - command to be processed.
     *
     * @return true if a free thread is available, false otherwise
     */
    public boolean processCommand(Command command)
    {
        WorkThread workerThread;
        workerThread = mThreadPool.getFreeThread();

        boolean status = false;

        if (workerThread != null)
        {
            sLog.info("WorkManager passing request to worker thread");
            workerThread.setCommand(command);
            status = true;
        }
        else
        {
            // Worker thread pool has been instructed to cleanup
            sLog.info("Could not obtain free thread");
        }

        return status;
    }

    /**
     * Starts the Work Manager.
     */
    public void start()
    {
        try
        {
            mThreadPool.start();
        }
        catch (Exception e)
        {
            sLog.severe("Cannot start thread pool " + e.getMessage());
        }

        setState("STARTED");
    }

    /**
     * Sets the state of the work manager.
     *
     * @param state - state of the work manager.
     */
    protected void setState(String state)
    {
        mState = state;
    }
}
