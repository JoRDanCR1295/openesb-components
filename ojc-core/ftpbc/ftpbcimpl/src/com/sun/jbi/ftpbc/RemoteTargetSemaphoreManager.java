/*
 * RemoteTargetSemaphoreManager.java
 * 
 * Created on July 24, 2008, 2:44:02 PM
 * 
 */
package com.sun.jbi.ftpbc;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * 
 * A Singleton serving as a registry of Semaphores (permit == 1) per unique remote target directory;
 *   
 * Semaphores are keyed in a HashMap, the key is formed by :
 * 
 * ConnectionPool key (a concatenation of all connection parameters for
 * the connections hashed into that pool ) + target directory.
 * 
 * For File Operations under the target directory,
 * clients (threads on the same JVM) have to synch 
 * on the sema in order to avoid racing conditions;
 *
 * @author jfu (jim.fu@sun.com)
 *
 */
public final class RemoteTargetSemaphoreManager {

    static final Map<String, Semaphore> mSemaphoreMap;
    static final ReentrantReadWriteLock mRWL;
    static final java.util.concurrent.locks.Lock mRL;
    static final java.util.concurrent.locks.Lock mWL;

    static {
        mSemaphoreMap = new HashMap<String, Semaphore>();
        mRWL = new ReentrantReadWriteLock();
        mRL = mRWL.readLock();
        mWL = mRWL.writeLock();
    }

    public static Semaphore get(String key) {
        Semaphore s = null;

        mRL.lock();

        try {
            s = mSemaphoreMap.get(key);
        } finally {
            mRL.unlock();
        }

        if (s == null) {
            // not registered yet
            mWL.lock();
            try {
                s = mSemaphoreMap.get(key);
                if (s == null) {
                    s = new Semaphore(1);
                    mSemaphoreMap.put(key, s);
                }
            } finally {
                mWL.unlock();
            }
        }
        assert s != null : "Can not obtain a semaphore for protecting remote resource with key=" + key;
        return s;
    }

    public static void remove(String key) {
        mWL.lock();
        try {
            mSemaphoreMap.remove(key);
        } finally {
            mWL.unlock();
        }
    }

    /**
     * this method is supposed to be called by the BC lifecycle
     * method ComponentLifeCycle.stop(), it is reasonable to assume 
     * that there is no other threads accessing the registry
     * but still do a timed write lock first to be graceful, then
     * if can not acquire the lock timely, proceed with cleaning up
     * any way.
     */
    public static void cleanup() {
        try {
            if (mWL.tryLock(1000, TimeUnit.MILLISECONDS)) {
                try {
                    releaseAndCleanup();
                } finally {
                    mWL.unlock();
                }
                return;
            }
        } catch (InterruptedException e) {
            // ignore
        }
        // can not obtain lock, clean up any way
        releaseAndCleanup();
    }

    private static void releaseAndCleanup() {
        if (mSemaphoreMap.size() > 0) {
            Set keys = mSemaphoreMap.keySet();
            Iterator it = keys.iterator();
            try {
                while (it.hasNext()) {
                    String key = (String) it.next();
                    Semaphore sema = mSemaphoreMap.remove(key);
                    if (sema != null) {
                        int pending = sema.getQueueLength();
                        if (pending > 0) {
                            // some threads are still waiting to acquire
                            sema.release(pending);
                        }
                    }
                }
            } catch (Exception e) {
                // ignore
            }
            mSemaphoreMap.clear();
        }
    }
}
