/*
 * Locks.java
 * 
 * Created on May 9, 2007, 2:58:55 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.jbi.filebc;

import java.nio.channels.FileChannel;
import java.util.concurrent.locks.ReentrantLock;

/**
 * 
 * Wrapper of a lock of type <code>java.util.concurrent.locks.ReentrantLock</code> 
 * and a file channel of type <code>java.nio.channels.FileChannel</code>, both are used
 * to synchronized inbound threads (on same JVM) polling the same endpoint
 * (lock of type <code>java.util.concurrent.locks.ReentrantLock</code> )
 * and synchronize inbound threads (on different JVM) polling the same 
 * physical file directory;
 * 
 * @author jfu
 */
public class Lock {
    // filebc.lck - the default lock file name, it is created under the target directory indicated by file:address->fileDirectory
    // to synchronize concurrent directory listing operation from threads (input file pollers) 
    // from multiple processes running on multiple machines (clustering)
    // - used only by inbound processors - i.e. - concerns only message consuming;
    // - be overwritten by file:address->lockName

    public static final String DEFAULT_INBOUND_LOCKFILE_NAME = "filebc.lck";
    // filebc-in-processing - the default tmp dir name, the input files matching input file name pattern
    // will be UUID tagged and moved here by the inbound processor currently acquired
    // T-Lock (thread lock) and F-Lock (file lock) (so it won't be consumed again by other concurrent pollers
    // - this is guarantteed by the file lock), the inbound processor also put the 
    // tmp file name into a queue which is consumed by 5 inbound file workers.
    // the inbound file workers will de-Q the tmp files and do the normal message normalization 
    // and NMR routing.
    // - only used by inbound processors - i.e. - only concerns message consuming;
    // - be overwritten by file:address->workArea
    public static final String DEFAULT_INBOUND_TMPDIR_NAME = "filebc-in-processing";
    // filebc.seq - the default sequence file name which is used as the sequence number persistence
    // it is created under the target directory indicated by file:address->fileDirectory
    // - used only by outbound processors - i.e. - concerns only message provisioning;
    // - be overwritten by file:address->seqName
    public static final String DEFAULT_SEQFILE_NAME = "filebc.seq";
    private FileChannel mFileChannel;
    private String mLockFilePath; // the lock file kept for conflict checking
    private ReentrantLock mLock;

    public Lock(FileChannel fileChannel, ReentrantLock lock, String path) {
        mFileChannel = fileChannel;
        mLock = lock;
        mLockFilePath = path;
    }

    public FileChannel getFileChannel() {
        return mFileChannel;
    }

    public void setChannel(FileChannel channel) {
        mFileChannel = channel;
    }

    public ReentrantLock getLock() {
        return mLock;
    }

    public void setLock(ReentrantLock lock) {
        mLock = lock;
    }

    public void setLockFilePath(String s) {
        mLockFilePath = s;
    }

    public String getLockFilePath() {
        return mLockFilePath;
    }
}
