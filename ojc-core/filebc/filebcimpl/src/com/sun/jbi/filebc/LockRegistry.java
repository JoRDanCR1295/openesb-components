/*
 * LockRegistry.java
 * 
 * Created on May 9, 2007, 2:44:02 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.jbi.filebc;

import java.nio.channels.FileChannel;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * A registry of object of type <code>com.sun.jbi.filebc.Locks</code> per inbound 
 * file bc endpoint, used to synchronize inbound processors that polling the endpoint,
 * effective for threads in same process (JVM)
 * @author jfu
 */
public final class LockRegistry {

    static final Map<String, Lock> mLockRegistry;
    static final Map mLock2FileMap;
    //static final Map<String, Map> mLock4OutputDestMap;
    static final ReentrantReadWriteLock mRWL;
    static final java.util.concurrent.locks.Lock mRL;
    static final java.util.concurrent.locks.Lock mWL;

    static {
        mLockRegistry = new HashMap<String, Lock>(); // endpoint UUID, per endpoint Lock
        mLock2FileMap = new HashMap();
        //mLock4OutputDestMap = new HashMap<String, Map>(); // endpoint UUID, dest file Locks associated with the EP
        mRWL = new ReentrantReadWriteLock();
        mRL = mRWL.readLock();
        mWL = mRWL.writeLock();
    }

    private LockRegistry() {
    }

    public static Lock get(String key) {
        mRL.lock();
        try {
            return mLockRegistry.get(key);
        } finally {
            mRL.unlock();
        }
    }

//    public static Lock getPerFileLock(String key, File dest) {
//        mWL.lock();
//        Lock lock = null; // the lock associated with a literal outbound destination
//        try {
//            Lock l = mLockRegistry.get(key);
//            // l must not be null - i.e. - when getPerFileLock) get called, the endpoint has to be registered
//            if (l == null) {
//                throw new IllegalStateException("Endpoint not properly registered: can not find Lock object for endpoint, UUID=" + key);
//            }
//
//            Map<String, Lock> destLocks = mLock4OutputDestMap.get(key);
//
//            if (destLocks == null) {
//                destLocks = new HashMap<String, Lock>();
//            }
//
//            lock = destLocks.get(dest.getPath());
//
//            RandomAccessFile rf = null;
//            Exception err = null;
//            if (lock == null) {
//                File lockFile = new File(l.getLockFilePath());
//                File parent = lockFile.getParentFile();
//                parent.mkdirs();
//                File destLock = new File(parent, dest.getName().concat(".lck"));
//                try {
//                    destLock.createNewFile();
//                    rf = new RandomAccessFile(destLock, "rw");
//                } catch (IOException ex) {
//                    if ( rf != null ) {
//                        try {
//                            rf.close();
//                        }
//                        catch (IOException ioe) {
//                            // ignore ok
//                        }
//                        rf = null;
//                    }
//                    err = ex;
//                    Logger.getLogger(LockRegistry.class.getName()).log(Level.SEVERE, "IOException creating lock file for write destination : " + dest.getPath(), ex);
//                }
//                if (err == null) {
//                    try {
//                        lock = new Lock(rf != null ? rf.getChannel() : null, new ReentrantLock(), destLock.getCanonicalPath());
//                    } catch (IOException ex) {
//                        err = ex;
//                        Logger.getLogger(LockRegistry.class.getName()).log(Level.SEVERE, "OException when creating Lock object for outbound destination :" + dest.getPath() + " lock file: " + destLock.getPath(), ex);
//                    }
//                    if (err == null) {
//                        destLocks.put(dest.getPath(), lock);
//                        mLock4OutputDestMap.put(key, destLocks);
//                    }
//                }
//            }
//            else {
//                try {
//                    rf = new RandomAccessFile(lock.getLockFilePath(), "rw");
//                } catch (IOException ex) {
//                    if ( rf != null ) {
//                        try {
//                            rf.close();
//                        }
//                        catch (IOException ioe) {
//                            // ignore ok
//                        }
//                        rf = null;
//                    }
//                    err = ex;
//                    Logger.getLogger(LockRegistry.class.getName()).log(Level.SEVERE, "IOException re-creating lock file for write destination : " + dest.getPath(), ex);
//                }
//                if (err == null) {
//                    lock.setChannel(rf.getChannel());
//                }
//            }
//            return lock;
//        } finally {
//            mWL.unlock();
//        }
//    }

    public static void remove(String key) {
        mWL.lock();
        try {
            Lock l = mLockRegistry.remove(key);
            if (l != null) {
                FileChannel channel = l.getFileChannel();
                ReentrantLock rl = l.getLock();
                if (channel != null) {
                    if (channel.isOpen()) {
                        try {
                            channel.close();
                        } catch (Exception ex) {
                            // ignore on purpose
                        }
                    }
                }
                if (rl != null) {
                    if (rl.isLocked()) {
                        rl.unlock();
                    }
                }
                mLock2FileMap.remove(l.getLockFilePath());
                // also remove outbound destination files locks
                // associated with the endpoint
//                Map<String, Lock> destLocks = (Map) mLock4OutputDestMap.get(key);
//                if (destLocks != null) {
//                    Iterator it = destLocks.keySet().iterator();
//                    while (it.hasNext()) {
//                        Lock dl = destLocks.get(it.next().toString());
//                        if (dl != null) {
//                            if (dl.getFileChannel() != null && dl.getFileChannel().isOpen() ) {
//                                try {
//                                    dl.getFileChannel().close();
//                                } catch (IOException ex) {
//                                    Logger.getLogger(LockRegistry.class.getName()).log(Level.SEVERE, "IOException closing outbound destination lock file: " + key, ex);
//                                }
//                            }
//                        }
//                    }
//                    destLocks.clear();
//                    mLock4OutputDestMap.remove(key);
//                }
            }
        } finally {
            mWL.unlock();
        }
    }

    public static Lock register(String key, Lock lock) {
        Lock l = null;
        mWL.lock();
        try {
            l = mLockRegistry.get(key);
            if (l == null) {
                // check lock file name conflict
                if (!mLock2FileMap.containsKey(lock.getLockFilePath())) {
                    mLockRegistry.put(key, lock);
                    mLock2FileMap.put(lock.getLockFilePath(), null);
                    l = lock;
                } else {
                    // return null - indicate to the caller that there is conflict
                }
            } else {
                // already exists
            }
        } finally {
            mWL.unlock();
        }
        return l;
    }
}
