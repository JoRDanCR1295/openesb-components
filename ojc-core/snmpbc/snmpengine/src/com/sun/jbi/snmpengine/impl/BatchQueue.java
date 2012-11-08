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
 * @(#)BatchQueue.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpengine.impl;

import java.util.LinkedList;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 * A task queue with batching functionality. Items to be processed are added to the 
 * queue using the <code>add()</code> method; this item is added to a batch. Once the 
 * batch is full, or if the <code>maxWait</code> time has been exceeded, the batch is 
 * executed through the <code>processBatch</code> method.
 * 
 * The <code>maxWait</code> parameter spans the time from the moment the first item
 * is added to a batch; it therefore acts as a max-latency parameter.
 * 
 * Through batching, the queue is optimized for queuing large numbers of small items.
 * 
 * The concurrency of processing is limited by the number of threads in the threadpool.
 * The concurrency can be constrained even further by specifying the 
 * <code>maxTasks</code> parameter. In this case the number of outstanding tasks in the
 * threadpool can never exceed this parameter.
 * 
 * The queue can also be used for asynchonous processing of batches. In this case the
 * number of asynchronous tasks can be throttled. For asynchronous batch throttling,
 * each time the processing of a batch is completed, <code>taskComplete()</code> 
 * must be called.
 * 
 * @author fkieviet
 * @param <E> Type of object to store
 */
public abstract class BatchQueue < E > {
    private int mBatchSize;
    private int mMaxWait;
    private int mMaxTasks = Integer.MAX_VALUE;
    private ScheduledThreadPoolExecutor mExecutor;
    private LinkedList < Task > mPending = new LinkedList < Task > ();
    
    private E[] mCurrentBatch;
    private int mCurrentSize;
    private int mCurrentBatchId;
    private Task mCurrentTask;
    private int mAliveTasks;
    private boolean mIsAsync;

    private class Task implements Runnable {
        private E[] mBatch;
        private int mBatchId;
        private int mState;
        private boolean mIsDelayed;
        private static final int WAITING = 0;
        private static final int RUNNING = 1;
        private static final int CANCELLED = 2;
        
        public Task(E[] batch, int batchid, boolean isDelayed) {
            mBatch = batch;
            mBatchId = batchid;
            mState = WAITING;
            mIsDelayed = isDelayed;
            
            assert Thread.holdsLock(BatchQueue.this);
            mAliveTasks++;
        }

        public void run() {
            boolean shouldRun = false;
            synchronized (this) {
                if (mState == WAITING) {
                    mState = RUNNING;
                    shouldRun = true;
                }
            }
            if (shouldRun) {
                E[] batch = grabBatch(mBatchId, mBatch);
                try {
                    processBatch(batch, mBatchId);
                } finally {
                    if (!mIsAsync) {
                        doneTask();
                    }
                }
            }
        }
        
        public synchronized boolean cancel() {
            if (mState == WAITING) {
                mState = CANCELLED;
                assert Thread.holdsLock(BatchQueue.this);
                mAliveTasks--;
                return true;
            }
            return false;
        }

        public synchronized boolean post() {
            assert Thread.holdsLock(BatchQueue.this);
            if (mState == CANCELLED) {
                return false;
            } else {
                if (mIsDelayed) {
                    mExecutor.schedule(this, mMaxWait, TimeUnit.MILLISECONDS);
                } else {
                    mExecutor.submit(this);
                }
                return true;
            }
        }
    }
    
    private synchronized void doneTask() {
        mAliveTasks--;
        for (;;) {
            if (!mPending.isEmpty()) {
                Task t = mPending.removeFirst();
                boolean posted = t.post();
                if (!posted) {
                    continue;
                }
            }
            break;
        }
    }
    
    /**
     * Needs to be called for asynchronous processing when a batch is processed
     * 
     * @param batchId for verification, indicates which batch is complete
     */
    public void batchComplete(int batchId) {
        assert mIsAsync;
        doneTask();
    }
    
    /**
     * Factory method to create a new buffer array
     * 
     * @param size size of array
     * @return new array
     */
    public abstract E[] newArray(int size);
    
    
    /**
     * Constructor
     * 
     * @param batchSize max size of batch
     * @param maxWait maximum time to wait until batch is full
     * @param exec thread pool
     */
    public BatchQueue(int batchSize, int maxWait, ScheduledThreadPoolExecutor exec) {
        mBatchSize = batchSize;
        mMaxWait = maxWait;
        mExecutor = exec;
        
        mCurrentBatch = newArray(mBatchSize);
    }
    
    /**
     * Constructor
     * 
     * @param batchSize max size of batch
     * @param maxWait maximum time to wait until batch is full
     * @param exec thread pool
     * @param maxTasks maximum number of outstanding tasks (constrains the number of threads used)
     * @param isAsync set to true to call asynchronous processing
     */
    public BatchQueue(int batchSize, int maxWait, ScheduledThreadPoolExecutor exec, int maxTasks, boolean isAsync) {
        this (batchSize, maxWait, exec);
        mMaxTasks = maxTasks;
        mIsAsync = isAsync;
    }
    
    /**
     * Adds an object to the queue
     * 
     * @param o object to queue
     * @return batchid
     */
    public synchronized int add(E o) {
        mCurrentBatch[mCurrentSize++] = o;
        int ret = mCurrentBatchId;

        // Virgin batch?
        if (mCurrentSize == 1 && mCurrentSize != mBatchSize) {
            mCurrentTask = new Task(mCurrentBatch, mCurrentBatchId, true);
            if (mAliveTasks > mMaxTasks) {
                mPending.add(mCurrentTask);
            } else {
                mCurrentTask.post();
            }
        }
        
        // Full batch?
        if (mCurrentSize == mBatchSize) {
            // If Virgin-batch-task can be cancelled, crate a new one for immediate
            // execution
            if (mCurrentTask == null || mCurrentTask.cancel()) {
                E[] batch = newArray(mCurrentSize);
                System.arraycopy(mCurrentBatch, 0, batch, 0, mCurrentSize);
                Task t = new Task(batch, mCurrentBatchId, false);
                if (mAliveTasks > mMaxTasks) {
                    mPending.add(t);
                } else {
                    t.post();
                }
            }
            
            mCurrentTask = null;
            mCurrentBatchId++;
            mCurrentSize = 0;
        }
        
        return ret;
    }
    
    /**
     * Processes a batch
     * 
     * @param batch objects to process
     * @param batchId identifies this batch
     */
    public abstract void processBatch(E[] batch, int batchId);
    
    private synchronized E[] grabBatch(int batchid, E[] batch) {
        E[] ret = null;
        if (mCurrentBatchId != batchid) {
            ret = batch;
        } else {
            // Called from a virgin batch task; need to resize the batch
            ret = newArray(mCurrentSize);
            System.arraycopy(mCurrentBatch, 0, ret, 0, mCurrentSize);
            mCurrentTask = null;
            mCurrentBatchId++;
            mCurrentSize = 0;
        }
        return ret;
    }
    
}
