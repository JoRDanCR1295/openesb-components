package com.sun.jbi.bindings.synchronization;

/*
 * LockRegistry.java
 * 
 * Created on May 9, 2007, 2:44:02 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A registry of object of type <code>com.sun.jbi.bindings.sync.registry.CompositeLock</code> per inbound 
 * ftp bc web service operation, the CompositeLock includes :
 * 
 * (1) a <code>java.util.concurrent.locks.ReentrantLock</code> object used to synchronize 
 * inbound processors that polling the same external URI, this lock is effective for threads in same process (JVM)
 * (2) a physical mechanism backed token of type <code>com.sun.jbi.bindings.sync.registry.Sync</code>
 * which can be a file system based (using java.nio.channel.FileLock) or a database based lock (e.g. leveraging a SQL query
 * with UPDATABLE clause) which serves as the physical lock providing synchronization among
 * threads across different JVMs
 * 
 * @author jfu
 */
public final class CompositeLockRegistry {
    public static final Map mLockRegistry;
    public static final ReentrantReadWriteLock mRWL;
    public static final java.util.concurrent.locks.Lock mRL;
    public static final java.util.concurrent.locks.Lock mWL;

    static {
        mLockRegistry = new HashMap();
        mRWL = new ReentrantReadWriteLock();
        mRL = mRWL.readLock();
        mWL = mRWL.writeLock();
    }
    
    public static CompositeLock get(String key) {
        mRL.lock(); try { return (CompositeLock)mLockRegistry.get(key); } finally { mRL.unlock(); }
    }
    
    /**
     * remove the composite lock from the registry and deregister 
     * it.
     * @param key - the UUID key for the WSDL operation;
     */
    public static void remove(String key) {
        mWL.lock(); 
        try { 
            CompositeLock l = (CompositeLock)mLockRegistry.remove(key);
            if ( l != null ) {
                try {
                    l.deregister();
                } catch (SyncException ex) {
                    Logger.getLogger(CompositeLockRegistry.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        } finally {
            mWL.unlock(); 
        }
    }
    
    /**
     * register the composite lock in the registry
     * 
     * @param key - the UUID key for the WSDL operation
     * @param lock - the composite lock object for the operation
     * @return - the composite lock registered;
     */
    public static CompositeLock register(CompositeLock aLock) {
        CompositeLock l = null;
        mWL.lock();
        try {
            l = (CompositeLock)mLockRegistry.get(aLock.getKey());
            if ( l == null ) {
                mLockRegistry.put(aLock.getKey(), aLock);
                l = aLock;
                try {
                    l.register();
                } catch (SyncException ex) {
                    Logger.getLogger(CompositeLockRegistry.class.getName()).log(Level.SEVERE, "SyncException when register composite lock:" + l, ex);
                    l = null;
                }
            }
        }
        finally {
            mWL.unlock();
        }
        return l;    
    }
}
