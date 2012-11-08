/*
 * LockRegistry.java
 * 
 * Created on May 9, 2007, 2:44:02 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.execbc;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * A registry of object of type <code>java.util.concurrent.locks.Lock</code> per
 * output destination for append mode for outbound file bc endpoints.
 * the lock is used to sync the outbound processors (thread)
 * so that at one time, only one thread can be accessing the destination file
 * to append message to.
 * when a SU is undeployed, check each outbound EP in it
 * and descrease the destination ref count by one of the EP appends to the destination file.
 * 
 * when a ref count is 0, remove the destination file from the registry
 * 
 * @author jfu
 */
public final class AppendDestinationRegistry {
    static final Map mAppendDestRegistry;
    static final Map mEP2AppDest;
    static final Map mAppDest2EP;
    static final ReentrantReadWriteLock mRWL;
    static final java.util.concurrent.locks.Lock mRL;
    static final java.util.concurrent.locks.Lock mWL;

    static {
        mAppendDestRegistry = new HashMap();
        mEP2AppDest = new HashMap();
        mAppDest2EP = new HashMap();
        mRWL = new ReentrantReadWriteLock();
        mRL = mRWL.readLock();
        mWL = mRWL.writeLock();
    }
    
    /**
     * key is the cannonical name of the append destination file
     * 
     **/
    public static ReentrantLock get(String key) {
        mRL.lock(); try { return (ReentrantLock)mAppendDestRegistry.get(key); } finally { mRL.unlock(); }
    }
    
    /**
     * key is the cannonical name of the append destination file
     * 
     **/
    public static void deregister(String epKey) {
        mWL.lock(); 
        try {
            Map dests = (Map)mEP2AppDest.remove(epKey);
            if ( dests != null && dests.size() > 0 ) {
                Set s = dests.keySet();
                Iterator it = s.iterator();
                String dest = null;
                while ( it.hasNext() ) {
                    dest = (String)it.next();
                    if ( dest != null ) {
                        Map eps = (Map)mAppDest2EP.get(dest);
                        if ( eps != null && eps.size() > 0 ) {
                            eps.remove(epKey);
                            if ( eps.size() == 0 ) {
                                // no ep reference the seq any more
                                mAppendDestRegistry.remove(dest);
                            }
                        }
                    }
                }
            }
            else {
                // for an outbound EP that does not reference 
                // a persisted sequence 
            }
        } finally {
            mWL.unlock(); 
        }
    }
    
    /**
     * key is the cannonical name of the append destination file
     * lock is used for sync competing threads for 
     * put a file lock on the append destination file
     **/
    public static ReentrantLock register(String key, String epKey, ReentrantLock lock) {
        ReentrantLock l = null;
        mWL.lock();
        try {
            l = (ReentrantLock)mAppendDestRegistry.get(key);
            if ( l == null ) {
                mAppendDestRegistry.put(key, lock);
                l = lock;
            }
            // also maintain the EP <-> AppDest reference tables
            // so that when an EP is un-deployed
            // the destination' it referenced can be checked 
            // for removal - when there is no EP 
            // referencing a append desination - the append destination is to be removed
            // from the registry

            // EP -> app dest
            Map dests = (Map)mEP2AppDest.get(epKey);
            if ( dests == null ) {
                dests = new HashMap();
                mEP2AppDest.put(epKey, dests);
            }
            dests.put(key, null);
            // seq -> EP
            Map eps = (Map)mAppDest2EP.get(key);
            if ( eps == null ) {
                eps = new HashMap();
                mAppDest2EP.put(key, eps);
            }
            eps.put(epKey, null);
        }
        finally {
            mWL.unlock();
        }
        return l;    
    }
}
