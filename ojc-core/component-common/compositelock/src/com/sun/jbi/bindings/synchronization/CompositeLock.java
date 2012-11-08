package com.sun.jbi.bindings.synchronization;

/*
 * Locks.java
 * 
 * Created on May 9, 2007, 2:58:55 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
import java.util.concurrent.locks.ReentrantLock;

/**
 * Wrapper of a lock of type <code>java.util.concurrent.locks.ReentrantLock</code> 
 * and a file channel of type <code>java.nio.channels.FileChannel</code>, both are used
 * to synchronized inbound threads (on same JVM) polling the same endpoint (lock of type <code>java.util.concurrent.locks.ReentrantLock</code> )
 * and synchronize inbound threads (on different JVM) polling the same physical file directory;
 * @author jfu
 */
public class CompositeLock {
    // the per operation directory for holding lock files, 
    // sequence files, recovery logs, error message logs
    private PersistStore mStore;
    // the ReentrantLock for sync inbound threads on same JVM (process)
    private ReentrantLock mLock; 
    // the sync object that provide exclusive 
    // locking among callers (usually threads):
    // running on same clustered server instances,
    // running on server instances in different domains
    // and the domains can be distributed on different hosts
    private Sync mSync;
    private String mKey;

    /**
     * instantiate a CompositeLock object - a composite lock which include the following :
     * (1) persistence location for internal data for the operation of a component
     * (2) a in memory lock (for threads on the same jvm / clustered server instance)
     * (3) a physical lock (e.g. based on database updatable query or java.nio.channel.FileLock)
     * 
     * the Lock exposes the following methods:
     * 
     * (1) register() - register the underlying physical lock : e.g. for file based lock, create the lock file, for database based
     * create the table for the token and register it in a control table (registry table)
     * 
     * (2) deregister() - deregister the token from the underlying mechanism.
     * 
     * (3) lock() - which, internally, is a 2 level locking : thread level lock + physical lock (file lock or db lock)
     *              lock() will first call tryLock(), if granted a lock then proceed to acquire the physical lock:
     *              the behavior of acquiring physical lock depends on the underlying mechanism, it can be a try lock 
     *              (as in java.nio.channel.FileLock.tryLock())or a 
     *              blocking lock (as in a lock based on a SQL query with updatable clause).
     * (4) unlock() - which, internally, is a 2 level locking : thread level lock + physical lock (file lock or db lock)
     *              will unlock the thread lock then the physical lock.
     * 
     * (5) getPersistStore() - return an object that represents a component specific persisted store
     * for persisting runtime control data such as recovery log etc.
     * 
     * @param root - the file system directory as the storage of component internal data such as
     *               recovery log, persisted sequence numbers, etc.
     * 
     */
    public CompositeLock(String aKey, PersistStore aStore, Sync sync) {
        mKey = aKey;
        mSync = sync;
        mLock = new ReentrantLock();
        mStore = aStore;
    }

    /** 
     * register the underlying physical token
     * 
     * @throws com.sun.jbi.bindings.sync.registry.SyncException
     */
    public void register() throws SyncException {
        if ( mSync != null )
            mSync.register();
    }

    /**
     * deregister the underlying physical token
     * 
     * @throws com.sun.jbi.bindings.sync.registry.SyncException
     */
    public void deregister() throws SyncException {
        if ( mSync != null )
            mSync.deregister();
    }

    /**
     * acquire the composite lock
     * @return - this ock object if the composite lock granted
     * NULL otherwise.
     * 
     */
    public CompositeLock lock() throws Exception {
        CompositeLock l = null;
        if (mLock.tryLock()) {
            // thread lock granted
            // further attempt physical lock
            if ( mSync != null ) {
                Sync sync = null;
                Exception t = null;
                try {
                    sync = mSync.acquire();
                } catch (SyncException ex) {
                    t = ex;
                    try {
                        if ( sync != null )
                            sync.release();
                    } catch (Exception e) {
                        // ignore
                    }
                    sync = null;
                }
                if ( sync != null ) {
                    // both locks granted
                    l = this;
                }
                else {
                    // either physical lock not granted
                    // or error when acquiring physical lock
                    mLock.unlock();
                    if ( t != null )
                        throw t;
                }
            }
            else {
                l = this;
            }
        }
        return l;
    }

    public void unlock() {
        // note, physical lock mSync can be null
        // if runtime configuration EnableClusterAware = false
        if (mLock.isLocked()) {
            mLock.unlock();
            if ( mSync != null )
                mSync.release();
        }
    }

    
    public String getKey() {
        return mKey;
    }

    /**
     * get the persist store
     * for the component
     * 
     * @return
     */
    public PersistStore getPersistStore() {
        return mStore;
    }
    
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append("operation key::").append(mKey).append("|");
        if ( mLock != null ) {
            sb.append("lock::").append(mLock.toString());
        }
        else {
            sb.append("no transient lock");
        }
        if ( mSync != null ) {
            sb.append("physical lock::").append(mSync.toString());
        }
        else {
            sb.append("no physical lock");
        }
        return sb.toString();
    }
}
