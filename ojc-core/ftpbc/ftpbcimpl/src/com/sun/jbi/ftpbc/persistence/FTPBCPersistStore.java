/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc.persistence;

import com.sun.jbi.bindings.synchronization.PersistStore;

import java.io.File;

/**
 * this class represents a WSDL operation's access to a
 * file system based persistence store where component specific 
 * operational data is saved.
 * 
 * @author jfu
 */
public class FTPBCPersistStore implements PersistStore {

    /**
     * for malformed message handling
     * both normalization and de-normalization
     */
    public static final String MESSAGE_ERRORS_LOG_DIR = "message_errors";
    public static final String NORMALIZATION_ERRORS_LOG_DIR = "normalization";
    public static final String DENORMALIZATION_ERRORS_LOG_DIR = "denormalization";
    public static final String MESSAGE_ERROR_LOG_SUFFIX = ".err";
    /**
     * for recovery
     */
    public static final String RECOVER_LOG_DIR_NAME = "recoverylogs";
    public static final String RECOVER_LOG_SUFFIX = ".recover";
    public static final String RECOVER_LOG_ENTRY_DIR = "dir=";
    public static final String RECOVER_LOG_ENTRY_FILE = "file=";
    public static final String RECOVER_LOG_ENTRY_PRE_DIR = "preDir=";
    public static final String RECOVER_LOG_ENTRY_PRE_FILE = "preFile=";
    public static final String RECOVER_LOG_ENTRY_POST_DIR = "postDir=";
    public static final String RECOVER_LOG_ENTRY_POST_FILE = "postFile=";
    // ftpbc.lck - the default lock file name, it is created under the 
    // persistence base directory indicated by ftp:address->baseLocation
    // to synchronize concurrent directory listing operation from threads (input file pollers) 
    // from multiple processes running on multiple machines (clustering)
    // - used only by inbound processors - i.e. - concerns only message consuming;
    // - be overwritten by ftp:address->lockName
    public static final String DEFAULT_LOCKFILE_NAME = "ftpbc.lck";
    // ftpbc.seq - the default sequence file name which is used as the sequence number persistence
    // - used only by outbound processors - i.e. - concerns only message provisioning;
    public static final String DEFAULT_SEQFILE_PREFIX = "ftpbc.seq";
    // sub dir under per operation persist root dir
    // where per message exchange recovery log is persisted
    public static final String RECOVERYLOGS_DIR_NAME = "recoverylogs";
    // sub dir under the per operation persist root dir
    // where information about a normalization/de-normalization
    // failure are recorded - malformed message
    public static final String BADMESSAGE_LOG_DIR_NAME = "message_errors";
    // the message errors are categorized into normalization and denormalization
    public static final String BADMESSAGE_LOG_DIR_NAME_4_NORM = "normalization";
    public static final String BADMESSAGE_LOG_DIR_NAME_4_DENORM = "denormalization";
    // the base dir of the file system based persist store
    private File mRoot;
    // for recovery logs
    private File mRecoveryLogsDir;
    // for message errors directory
    private File mMessageErrorsDir;
    private File mMessageErrorsDir4Norm;
    private File mMessageErrorsDir4DeNorm;

    public FTPBCPersistStore(File root) {
        mRoot = root;
    }

    /**
     * get directory for recovery logs - per operation
     * @return
     */
    public File getRecoveryLogDir(boolean create) {
        if (mRecoveryLogsDir == null) {
            mRecoveryLogsDir = new File(mRoot, RECOVERYLOGS_DIR_NAME);
        }
        if (create && !mRecoveryLogsDir.exists()) {
            mRecoveryLogsDir.mkdirs();
        }
        if (create && !mRecoveryLogsDir.exists()) {
            throw new IllegalStateException("FTPBC recovery log directory does not exist, path=" + mRecoveryLogsDir.getPath());
        }
        if (mRecoveryLogsDir.exists() && !mRecoveryLogsDir.isDirectory()) {
            throw new IllegalStateException("FTPBC recovery log directory invalid - not a directory, path=" + mRecoveryLogsDir.getPath());
        }
        return mRecoveryLogsDir;
    }

    /** 
     * get base directory for malformed message error logs
     * @return 
     */
    public File getMessageErrorDir(boolean create) {
        if (mMessageErrorsDir == null) {
            mMessageErrorsDir = new File(mRoot, BADMESSAGE_LOG_DIR_NAME);
        }
        if (create) {
            if (!mMessageErrorsDir.exists()) {
                mMessageErrorsDir.mkdirs();
            }
            if (!mMessageErrorsDir.exists()) {
                throw new IllegalStateException("FTPBC message errors log directory does not exist, path=" + mMessageErrorsDir.getPath());
            }
            if (!mMessageErrorsDir.isDirectory()) {
                throw new IllegalStateException("FTPBC message errors log directory invalid - not a directory, path=" + mMessageErrorsDir.getPath());
            }
        }
        return mMessageErrorsDir;
    }

    /**
     * get the sub dir for logging bad message info for normalization
     * 
     * @return
     */
    public File getMessageErrorDir4Norm(boolean create) {
        if (mMessageErrorsDir4Norm == null) {
            mMessageErrorsDir4Norm = new File(getMessageErrorDir(create), BADMESSAGE_LOG_DIR_NAME_4_NORM);
        }
        if (create) {
            if (!mMessageErrorsDir4Norm.exists()) {
                mMessageErrorsDir4Norm.mkdirs();
            }
            if (!mMessageErrorsDir4Norm.exists()) {
                throw new IllegalStateException("FTPBC Fatal Error: failed to create persistence directory for recording message normalization failure, path=" + mMessageErrorsDir4Norm.getPath());
            }
            if (!mMessageErrorsDir4Norm.isDirectory()) {
                throw new IllegalStateException("FTPBC Fatal Error: invalid persistence directory for recording message normalization failure [not a directory], path=" + mMessageErrorsDir4Norm.getPath());
            }
        }
        return mMessageErrorsDir4Norm;
    }

    /**
     * get the sub dir for logging bad message info from de-normalization
     * 
     * @return
     */
    public File getMessageErrorDir4DeNorm(boolean create) {
        if (mMessageErrorsDir4DeNorm == null) {
            mMessageErrorsDir4DeNorm = new File(getMessageErrorDir(create), BADMESSAGE_LOG_DIR_NAME_4_DENORM);
        }
        if (create) {
            if (!mMessageErrorsDir4DeNorm.exists()) {
                mMessageErrorsDir4DeNorm.mkdirs();
            }
            if (!mMessageErrorsDir4DeNorm.exists()) {
                throw new IllegalStateException("FTPBC Fatal Error: failed to create persistence directory for recording message denormalization failure, path=" + mMessageErrorsDir4DeNorm.getPath());
            }
            if (!mMessageErrorsDir4DeNorm.isDirectory()) {
                throw new IllegalStateException("FTPBC Fatal Error: invalid persistence directory for recording message denormalization failure [not a directory], path=" + mMessageErrorsDir4DeNorm.getPath());
            }
        }
        return mMessageErrorsDir4DeNorm;
    }
}
