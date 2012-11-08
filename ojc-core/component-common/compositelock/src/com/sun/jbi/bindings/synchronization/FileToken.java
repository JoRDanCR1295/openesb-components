/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.bindings.synchronization;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileLock;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * a file system backed token for synchronizing
 * threads running in different processes.
 * 
 * @author jfu
 */
public class FileToken implements Sync {

    private static Logger mLogger = Logger.getLogger(FileToken.class.getName());
    // the value should be a path pointing to 
    // the actual lock file - unique per WSDL operation
    public static final String TOKEN_PROPERTY_LOCK_BASE_DIR = "org.glassfish.openesb.sync.file.lock.basedir";
    public static final String TOKEN_PROPERTY_LOCKFILE = "org.glassfish.openesb.sync.file.lock";
    private RandomAccessFile mLockAccess;
    private File mLockFileBaseDir; // the value of "org.glassfish.openesb.sync.file.lock"
    private File mLockFile; // the File object of the "org.glassfish.openesb.sync.file.lock"
    private AtomicBoolean bRegistered;
    private FileLock mLock;
    private Map mParams;
    private String mLockFileName;

    /**
     * factory method to create a Sync instance - file based
     * @param params
     * @return
     */
    public static Sync createSync(Map params) {
        return new FileToken(params);
    }

    private FileToken(Map params) {
        mLockFileBaseDir = (File) params.get(TOKEN_PROPERTY_LOCK_BASE_DIR);
        if (mLockFileBaseDir == null) {
            throw new IllegalArgumentException("Missing required token property : ".concat(TOKEN_PROPERTY_LOCK_BASE_DIR));
        }
        mLockFileName = (String) params.get(TOKEN_PROPERTY_LOCKFILE);
        if (mLockFileName != null) {
            mLockFileName = mLockFileName.trim();
        }
        if (mLockFileName == null || mLockFileName.length() == 0) {
            throw new IllegalArgumentException("Missing required token property : ".concat(TOKEN_PROPERTY_LOCKFILE));
        }
        mLockFile = new File(mLockFileBaseDir, mLockFileName);
        mParams = params;
        bRegistered = new AtomicBoolean(false);
    }

    public synchronized void register() throws SyncException {
        if (bRegistered.get()) {
            throw new IllegalStateException("register() ::: File based token at : " + mLockFile.getPath() + " already registered.");
        }

        try {
            if (!mLockFile.exists()) {
                File parent = mLockFile.getParentFile();
                if (!parent.exists()) {
                    boolean ok = parent.mkdirs();
                    if (!ok) {
                        if (mLogger.isLoggable(Level.SEVERE)) {
                            mLogger.log(Level.SEVERE, "register() ::: Failed to create directory for lock file: " + mLockFile.getPath());
                        }
                        throw new SyncException("register() ::: Failed to create directory for lock file: " + mLockFile.getPath());
                    }
                } else if (!parent.isDirectory()) {
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE, "register() ::: Parent for lock file: " + mLockFile.getPath() + " is not a directory.");
                    }
                    throw new SyncException("register() ::: Parent for lock file: " + mLockFile.getPath() + " is not a directory.");
                }
                mLockFile.createNewFile();
            } else if (!mLockFile.isFile()) {
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE, "register() ::: lock file: " + mLockFile.getPath() + " is not a file.");
                }
                throw new SyncException("register() ::: lock file: " + mLockFile.getPath() + " is not a file.");
            }
            mLockAccess = new RandomAccessFile(mLockFile, "rw");
        } catch (FileNotFoundException ex) {
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, "Lock file not found when creating file based token", ex);
            }
            throw new SyncException("Lock file not found when creating file based token", ex);
        } catch (IOException ioe) {
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, "IOException when creating file based token", ioe);
            }
            throw new SyncException("IOException when creating file based token", ioe);
        }

        bRegistered.set(true);
    }

    public synchronized void deregister() throws SyncException {
//        if (!bRegistered.get()) {
//            throw new IllegalStateException("File based token at : " + mLockFile.getPath() + " already deregistered.");
//        }
        bRegistered.set(false);
    }

    /**
     * try to acquire a file lock on the lockfile;
     * @return - this object if lock granted, NULL otherwise;
     * @throws com.sun.jbi.bindings.sync.registry.SyncException
     */
    public Sync acquire() throws SyncException {
        Sync sync = null;
        if (!bRegistered.get()) {
            throw new IllegalStateException("File based token at : " + mLockFile.getPath() + " already registered.");
        }
        try {
            mLock = mLockAccess.getChannel().tryLock();
        } catch (IOException ex) {
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, "acquire() :::: IOException when tryLock() on lock file" + mLockFile.getPath(), ex);
            }
            throw new SyncException("acquire() :::: IOException when tryLock() on lock file: " + mLockFile.getPath());
        }
        if (mLock != null) {
            sync = this;
        }
        return sync;
    }

    public void release() {
        if (!bRegistered.get()) {
            throw new IllegalStateException("release() ::: Token not registered when release() invoked against it.");
        }
        if (mLock == null) {
            throw new IllegalStateException("release() ::: the random file associated with the lock file not available when release() invoked against it.");
        }
        try {
            mLock.release();
        } catch (IOException ex) {
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, "release() ::: IOException when release the lock on the random file associated with the lock file ", ex);
            }
        }
    }

    /**
     * full information about the token
     * @return - the full info as a string
     */
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append(this.getClass().getName()).append(":");
        sb.append("specified base dir :").append(mLockFileBaseDir.getPath());
        sb.append("specified lock file name :").append(mLockFileName);
        String cp = null;
        try {
            mLockFile.getCanonicalPath();
        } catch (IOException ex) {
            // ignore
            cp = "IOException:".concat(ex.getLocalizedMessage());
        }
        sb.append("canonical path:").append(cp);
        return sb.toString();
    }
}
