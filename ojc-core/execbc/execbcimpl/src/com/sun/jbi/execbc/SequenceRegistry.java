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
 * named sequence (persisted) for outbound file bc endpoints.
 * the lock is used to sync the outbound processors (thread)
 * so that at one time, only one thread can be accessing the sequence
 * store.
 * when a SU is undeployed, check each outbound EP in it
 * and descrease the sequence ref count by one of the EP referenced 
 * a sequence.
 * 
 * when a ref count is 0, remove the sequence from the registry
 * 
 * @author jfu
 */
public final class SequenceRegistry {
    static final Map mSequenceRegistry;
    static final Map mEP2Seq;
    static final Map mSeq2EP;
    static final ReentrantReadWriteLock mRWL;
    static final java.util.concurrent.locks.Lock mRL;
    static final java.util.concurrent.locks.Lock mWL;

    static {
        mSequenceRegistry = new HashMap();
        mEP2Seq = new HashMap();
        mSeq2EP = new HashMap();
        mRWL = new ReentrantReadWriteLock();
        mRL = mRWL.readLock();
        mWL = mRWL.writeLock();
    }
    
    /**
     * key is the cannonical name of the sequence file
     * 
     **/
    public static ReentrantLock get(String key) {
        mRL.lock(); try { return (ReentrantLock)mSequenceRegistry.get(key); } finally { mRL.unlock(); }
    }
    
    /**
     * key is the cannonical name of the sequence file
     * 
     **/
    public static void deregister(String epKey) {
        mWL.lock(); 
        try {
            Map seqs = (Map)mEP2Seq.remove(epKey);
            if ( seqs != null && seqs.size() > 0 ) {
                Set s = seqs.keySet();
                Iterator it = s.iterator();
                String seq = null;
                while ( it.hasNext() ) {
                    seq = (String)it.next();
                    if ( seq != null ) {
                        Map eps = (Map)mSeq2EP.get(seq);
                        if ( eps != null && eps.size() > 0 ) {
                            eps.remove(epKey);
                            if ( eps.size() == 0 ) {
                                // no ep reference the seq any more
                                mSequenceRegistry.remove(seq);
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
     * key is the cannonical name of the sequence file
     * lock is used for sync competing threads for 
     * put a file lock on the sequence file
     **/
    public static ReentrantLock register(String key, String epKey, ReentrantLock lock) {
        ReentrantLock l = null;
        mWL.lock();
        try {
            l = (ReentrantLock)mSequenceRegistry.get(key);
            if ( l == null ) {
                mSequenceRegistry.put(key, lock);
                l = lock;
            }
            // also maintain the EP <-> Seq reference tables
            // so that when an EP is un-deployed
            // the seqs' it referenced can be checked 
            // for removal - when there is no EP 
            // referencing a seq - the seq is to be removed
            // from the registry

            // EP -> seq
            Map seqs = (Map)mEP2Seq.get(epKey);
            if ( seqs == null ) {
                seqs = new HashMap();
                mEP2Seq.put(epKey, seqs);
            }
            seqs.put(key, null);
            // seq -> EP
            Map eps = (Map)mSeq2EP.get(key);
            if ( eps == null ) {
                eps = new HashMap();
                mSeq2EP.put(key, eps);
            }
            eps.put(epKey, null);
        }
        finally {
            mWL.unlock();
        }
        return l;    
    }
}
