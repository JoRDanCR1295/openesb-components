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
 * @(#)WorkThread.java - ver 1.EA7 - 12/10/2005
 *
 * Copyright 2004-2005 Sun Microsystems, Inc. All Rights Reserved.
 */

package com.sun.jbi.thread;

import java.util.logging.Logger;

/**
 * This class executes the command in its thread. The class is always a part of
 * a threadpool and cannot exist on its own.
 *
 * @author Praveen Patange
 */
class WorkThread
    implements Runnable
{
    /**
     * Default Thread sleep time in milliseconds.
     */
    private static final int DEFAULT_THREAD_SLEEP_TIME = 100;

    /**
     * Thread sleep time in milliseconds.
     */
    private static int sThreadSleeptime = DEFAULT_THREAD_SLEEP_TIME;

    /**
     * Container for holding the command to be executed in this thread.
     */
    private Command mCurrentCommand;

    /**
     * Internal handle to the logger instance.
     */
    private Logger mLog;

    /**
     * Flag to hold the state of the Thread.  Valid states are "INIT", "READY",
     * "PROCESS" and "STOP"
     */
    private String mState;

    /**
     * Internal handle to the thread in which this runnable object is running.
     */
    private Thread mThread;

    /**
     * Internal handle to WorkThreadPool.
     */
    private WorkThreadPool mThreadPool;

    /**
     * A flag which indicates whether the thread should continue processing or
     * not.
     */
    private boolean mContinue;

    /**
     * Creates a new instance of WorkThread.
     *
     * @param workThreadPool - worker thread pool parent
     */
    WorkThread(WorkThreadPool workThreadPool)
    {
        mThreadPool = workThreadPool;
        mLog = Logger.getLogger(this.getClass().getPackage().getName());
        setState("INIT");
    }

    /**
     * Sets the log file.
     *
     * @param logFile log file.
     */
    public void setLogger(String logFile)
    {
        mLog = mLog.getLogger(logFile);
    }

    /**
     * Gets the thread name
     *
     * @return thread name.
     */
    public String getName()
    {
        return mThread.getName();
    }

    /**
     * Sets the sleep time for the thread.
     *
     * @param time in millieseconds
     */
    public void setSleepTime(int time)
    {
        sThreadSleeptime = time;
    }

    /**
     * The method polls the container for a new work when the thread is ready
     * to do work. This is indicated by the method doWork(). If there is new
     * work, it processes the command in its thread. Once the command has been
     * processed, it clears its work container and notifies the thread pool
     * that it is now free. The method will clean up the thread and shut
     * itself down when its state is set to "STOP".
     */
    public void run()
    {
        mThread = Thread.currentThread();
        mLog.info("Running thread " + mThread.getName());
        mContinue = true;

        while (mContinue)
        {
            if (isWorkAssigned())
            {
                try
                {
                    processCommand();
                }
                catch (Throwable th)
                {
                    mLog.info("The command failed to execute");
                    th.printStackTrace();
                }

                clearCommand();
                mThreadPool.releaseThread(this);
            }

            try
            {
                Thread.sleep(sThreadSleeptime);
            }
            catch (InterruptedException interruptException)
            {
                // someone must have interrupted this thread
                // do nothing
                mLog.info("Received an interrupt signal in "
                    + mThread.getName());
            }
        }

        try
        {
            mLog.info("Thread " + mThread.getName() + " has been stopped");
        }
        catch (Exception e)
        {
            ;
        }
    }

    /**
     * Assigns the command to this thread.
     *
     * @param command - command instance.
     */
    synchronized void setCommand(Command command)
    {
        if (mCurrentCommand == null)
        {
            mCurrentCommand = command;
        }
    }

    /**
     * Gets the command associated with this thread.
     *
     * @return the command associated with the thread
     */
    synchronized Command getCommand()
    {
        return mCurrentCommand;
    }

    /**
     * Sets the State of the Worker Thread.
     *
     * @param state - worker thread state.
     */
    void setState(String state)
    {
        mState = state;
    }

    /**
     * Returns a boolean indicating if work has been assigned to this thread.
     *
     * @return true if work has been allocated to this thread; otherwise false.
     */
    synchronized boolean isWorkAssigned()
    {
        return (mCurrentCommand != null);
    }

    /**
     * Clears the command associated with the thread.
     */
    void clearCommand()
    {
        mCurrentCommand = null;
    }

    /**
     * Processes the command in this thread.
     */
    void processCommand()
    {
        mCurrentCommand.execute();
    }

    /**
     * Stops the worker thread.
     *
     * @throws IllegalStateException 
     */
    void stop()
    {
        if (mContinue)
        {
            mLog.fine("Shutting down thread " + getName());
            mContinue = false;

            try
            {
                // Interrupt the thread so that it can shutdown quickly
                // This will also enable the command implementation to cleanup quickly.
                mThread.interrupt();
            }
            catch (SecurityException securityException)
            {
                mLog.warning("Could not interrupt thread");
                mLog.warning("Details : " + securityException.toString());
            }

            try
            {
                //Wait for the thread to stop
                mThread.join();
            }
            catch (InterruptedException exp)
            {
                ; // do nothing
            }

            mThread = null;
        }
        else
        {
            throw new IllegalStateException("Thread is not running");
        }
    }
}
