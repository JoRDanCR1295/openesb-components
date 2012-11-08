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
 * @(#)WorkThreadPool.java - ver 1.EA7 - 12/10/2005
 *
 * Copyright 2004-2005 Sun Microsystems, Inc. All Rights Reserved.
 */

package com.sun.jbi.thread;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.NoSuchElementException;

import java.util.logging.Logger;

/**
 * This class manages a pool of worker threads. The Work Manager uses this
 * class to execute the command in different threads.
 *
 * @author Praveen Patange
 */
class WorkThreadPool
{
    /**
     * Default Maximum Thread count.
     */
    private static final int MAX_THREAD_COUNT = 10;

    /**
     * Default Minimum Thread count.
     */
    private static final int MIN_THREAD_COUNT = 1;

    /**
     * Handle to the busy thread pool.
     */
    private ArrayList mBusyThreadPool;

    /**
     * Handle to the free thread pool.
     */
    private ArrayList mFreeThreadPool;

    /**
     * Contains a list of threads on which we are waiting for response.
     */
    private ArrayList mThreadWaitList;

    /**
     * Internal handle to the logger instance.
     */
    private Logger mLog;

    /**
     * Internal variable which indicates that workethread pool has been
     * instructed to stop all its threads.
     */
    private String mState;

    /**
     * Maximum Thread count.
     */
    private int mMaxThreadCount = MAX_THREAD_COUNT;

    /**
     * Minimum Thread count.
     */
    private int mMinThreadCount = MIN_THREAD_COUNT;

    /**
     * Creates a new instance of WorkThreadPool.
     *
     * @param minThreadCount - minimum thread limit
     * @param maxThreadCount - maximum thread limit
     */
    WorkThreadPool(
        int minThreadCount,
        int maxThreadCount)
    {
        mLog = Logger.getLogger(this.getClass().getPackage().getName());
        mMaxThreadCount = maxThreadCount;
        mMinThreadCount = minThreadCount;
        mThreadWaitList = new ArrayList();
        mState = "NEW";
    }

    /**
     * Creates a new instance of WorkThreadPool.
     */
    WorkThreadPool()
    {
        mLog = Logger.getLogger(this.getClass().getPackage().getName());
        mThreadWaitList = new ArrayList();
        mState = "NEW";
    }

    /**
     * Returns the count of threads in busy thread pool
     *
     * @return int count of number of busy threads
     */
    public int getBusyThreads()
    {
        return mBusyThreadPool.size();
    }

    /**
     * Sets the log file.
     *
     * @param logFile DOCUMENT ME!
     */
    public void setLogger(String logFile)
    {
        mLog = mLog.getLogger(logFile);
    }

    /**
     * DOCUMENT ME!
     *
     * @param count DOCUMENT ME!
     */
    public void setMaxThreads(int count)
    {
        mMaxThreadCount = count;
    }

    /**
     * DOCUMENT ME!
     *
     * @param count DOCUMENT ME!
     */
    public void setMinThreads(int count)
    {
        mMinThreadCount = count;
    }

    /**
     *
     */
    public synchronized void exitWhenBusyThreadsDone()
    {
        while (mBusyThreadPool.size() != 0)
        {
            try
            {
                wait();
            }
            catch (Exception e)
            {
                mLog.info("GOt notification in exitWhenBusyThreadsDone"
                    + mBusyThreadPool.size());

                continue;
            }
        }
    }

    /**
     * Gets a free thread from the free thread pool.
     *
     * @return a worker thread instance if a free thread exists; otherwise
     *         null.
     *
     * @throws IllegalStateException when the thread pool is not running.
     */
    synchronized WorkThread getFreeThread()
    {
        WorkThread workerThread = null;

        if (!mState.equals("START"))
        {
            throw new IllegalStateException("Thread pool is not running");
        }

        if (mFreeThreadPool.size() > 0)
        {
            workerThread = (WorkThread) mFreeThreadPool.get(0);
            mFreeThreadPool.remove(0);
            mBusyThreadPool.add(workerThread);
        }
        else if ((mBusyThreadPool.size()) < mMaxThreadCount)
        {
            workerThread = new WorkThread(this);
            mBusyThreadPool.add(workerThread);
            new Thread(workerThread).start();
        }
        else
        {
            try
            {
                mThreadWaitList.add(Thread.currentThread());
                wait();
                mThreadWaitList.remove(Thread.currentThread());

                // There should always be a free thread now.
                if (mFreeThreadPool.size() > 0)
                {
                    workerThread = (WorkThread) mFreeThreadPool.get(0);
                    mFreeThreadPool.remove(0);
                    mBusyThreadPool.add(workerThread);
                }
                else
                {
                    // this should never happen.
                    mLog.severe("Something is seriously wrong here. fix it");
                }
            }
            catch (InterruptedException interupException)
            {
                // Threadpool has been instructed to cleanup
                // do nothing
                mLog.info("Received interrupt signal to cleanup");
                workerThread = null;
            }
            catch (Exception exception)
            {
                // This should not happen.
                // Log warning
                mLog.warning("Exception thrown when waiting for a free thread");
                mLog.warning("Details : " + exception.toString());
                workerThread = null;
            }
        }

        return workerThread;
    }

    /**
     * Cleans up the free and busy threads.
     *
     * @throws IllegalStateException DOCUMENT ME!
     */
    synchronized void cleanup()
    {
        mLog.info("Cleaning up the worker thread pool");

        if (!(mState.equals("INIT") || mState.equals("STOP")))
        {
            throw new IllegalStateException("Thread Pool is still active");
        }

        mFreeThreadPool.clear();
        mBusyThreadPool.clear();
    }

    /**
     * Initializes the instance.
     *
     * @throws IllegalStateException
     */
    void init()
    {
        if (!mState.equals("NEW"))
        {
            throw new IllegalStateException(
                "Threadpool has already been initialized");
        }

        mFreeThreadPool = new ArrayList(mMaxThreadCount);
        mBusyThreadPool = new ArrayList(mMaxThreadCount);

        for (int i = 0; i < mMinThreadCount; i++)
        {
            mFreeThreadPool.add(new WorkThread(this));
        }

        mState = "INIT";
    }

    /**
     * Release the thread to the free pool. This method is used by worker
     * threads to notify that it has completed processing its command.
     *
     * @param thread - worker thread instance.
     *
     * @throws IllegalStateException
     * @throws NoSuchElementException DOCUMENT ME!
     */
    synchronized void releaseThread(WorkThread thread)
    {
        if (!mState.equals("START"))
        {
            throw new IllegalStateException("Thread pool is not running");
        }

        int threadIndex = mBusyThreadPool.indexOf(thread);

        if (threadIndex != -1)
        {
            mBusyThreadPool.remove(threadIndex);

            if (mFreeThreadPool.size() < mMinThreadCount)
            {
                mFreeThreadPool.add(thread);

                // Notify the manager to indicate that a thread has been released
                // This is ignored if no one are waiting for a free thread.
                try
                {
                    notifyAll();
                }
                catch (IllegalMonitorStateException exp)
                {
                    // This should not happen
                    mLog.severe("Exception while notifying work manager");
                    mLog.severe("Details :" + exp.toString());
                }
            }
            else
            {
                thread.stop();
            }

            if (mBusyThreadPool.size() == 0)
            {
                try
                {
                    notify();
                }
                catch (IllegalMonitorStateException exp)
                {
                    // This should not happen
                    mLog.severe(
                        "Exception while notifying COMPLETION OF BUSY THREADS");
                    mLog.severe("Details :" + exp.toString());
                }
            }
        }
        else
        {
            throw new NoSuchElementException("thread " + thread.getName()
                + " cannot be found");
        }
    }

    /**
     * Start the free threads.
     *
     * @throws IllegalStateException
     */
    void start()
    {
        mLog.info("Starting the thread pool");

        WorkThread workerThread;

        if (!(mState.equals("INIT") || mState.equals("STOP")))
        {
            throw new IllegalStateException(
                "Thread pool has already been started");
        }

        for (Iterator iter = mFreeThreadPool.iterator(); iter.hasNext();)
        {
            workerThread = (WorkThread) iter.next();
            new Thread(workerThread).start();
        }

        for (Iterator iter = mBusyThreadPool.iterator(); iter.hasNext();)
        {
            workerThread = (WorkThread) iter.next();
            new Thread(workerThread).start();
        }

        mState = "START";
    }

    /**
     * Stops the free and busy threads.
     *
     * @throws IllegalStateException DOCUMENT ME!
     */
    synchronized void stop()
    {
        mLog.info("Stopping the worker thread pool");

        WorkThread workerThread;

        if (!mState.equals("START"))
        {
            throw new IllegalStateException("Threadpool has not been started");
        }

        for (Iterator iter = mFreeThreadPool.iterator(); iter.hasNext();)
        {
            workerThread = (WorkThread) iter.next();
            workerThread.stop();
        }

        for (Iterator iter = mBusyThreadPool.iterator(); iter.hasNext();)
        {
            workerThread = (WorkThread) iter.next();
            workerThread.stop();
        }

        // Interrupt all threads in which work manager is waiting for a free 
        // thread.
        Thread waitThread = null;

        for (Iterator iter = mThreadWaitList.iterator(); iter.hasNext();)
        {
            waitThread = (Thread) iter.next();
            waitThread.interrupt();
        }

        mState = "STOP";
    }
}
