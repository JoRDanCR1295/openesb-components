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
 * @(#)InboundMessageProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.filebc.Endpoint.EndpointMessageType;
import com.sun.jbi.filebc.extensions.FileAddress;
import com.sun.jbi.filebc.extensions.FileInput;
import com.sun.jbi.filebc.extensions.FileMessage;
import com.sun.jbi.filebc.extensions.FileOperation;
import com.sun.jbi.filebc.extensions.FileOutput;
import com.sun.jbi.filebc.util.FileNamePatternUtil;
import com.sun.jbi.filebc.util.FileUtil;
import com.sun.jbi.filebc.util.InputFilenameFilter;
import com.sun.jbi.filebc.util.InputDirFilter;
import com.sun.jbi.filebc.util.MessageUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.filebc.util.FileNamePatternType;
import com.sun.jbi.filebc.util.AlertsUtil;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.ReentrantLock;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;

/** This is the thread that checks for inbound messages from the
 *  file system based on end point configuration.
 *
 * @author Sherry Weng
 * @author Qian Fu jim.fu@sun.com
 */
public class InboundMessageProcessor implements Runnable, MessageExchangeReplyListener, RedeliveryListener {

    private static final Messages mMessages =
            Messages.getMessages(InboundMessageProcessor.class);
    private static Logger mLogger = Messages.getLogger(InboundMessageProcessor.class);
    // local constants
    //public static final int NUM_IB_WORKER = 5;
    public static final String SUFFIX_PROCESSED_LOCALE_KEY = "processed";
    public static final String IB_WORKER_THREAD_NAME_PREFIX = "filebc-ib-worker";
    /**
     * Defines the JBI identifier for an 'in' message, for use with exchange.getMessage
     */
    public static final String IN_MSG = mMessages.getString("in");
    private static Map mInboundExchanges = Collections.synchronizedMap(new HashMap());
    private AtomicBoolean mStopRequested;
    private AtomicBoolean mStopped; // set when inbound processor stopped
    private Map<String, FileMeta> mInboundReplys;
    private DeliveryChannel mChannel;
    private ComponentContext mContext;
    private MessageExchangeFactory mMsgExchangeFactory;
    private Endpoint mEndpoint;
    private QName mOperationName;
    private ServiceEndpoint mServiceEndpoint;
    private FileMessage mFileMessage;
    private String mTargetDir;
    // to maximize concurrency of all IB processors 
    // with same target directory, keep a reference
    // to the Lock for the EP;
    private Lock mLock;
    private Map mWorkers; // list of inbound file workers + their thread
    private LinkedBlockingQueue<FileMeta> mFiles; // list of file names to be processed by workers
    private File mWorkAreaBaseDir;
    private MessageExchangeFactory mMessageExchangeFactory;
    
    private int mNumIBWorkers;

    public InboundMessageProcessor(ComponentContext context,
            DeliveryChannel channel,
            Endpoint endpoint,
            QName operationName, int numIBWorkers) throws IBProcCreationException {
        mContext = context;
        mChannel = channel;
        mEndpoint = endpoint;
        mOperationName = new QName(getPortType(endpoint).getQName().getNamespaceURI(),
                operationName.getLocalPart());
        mStopRequested = new AtomicBoolean(false);
        mStopped = new AtomicBoolean(false);
        mInboundReplys = Collections.synchronizedMap(new HashMap<String, FileMeta>());
        mLock = LockRegistry.get(mEndpoint.getEPUUID());
        mWorkAreaBaseDir = new File(mLock.getLockFilePath()).getParentFile();
        mMessageExchangeFactory = mChannel.createExchangeFactory();
        mNumIBWorkers = numIBWorkers;
    }

    public void run() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "IMP_EP_status",
                    new Object[]{mEndpoint.getServiceName(), mEndpoint.getEndpointName()});
        }

        FileOperation operation = locateFileOperation(mOperationName);
        String mep = getMessageExchangePattern(mOperationName);
        try {
            validateInboundMessageExchangeProperties(operation, mep);
        } catch (Exception e) {
            mLogger.log(Level.SEVERE, e.getLocalizedMessage());
            return;
        }

        FileInput fileInput = operation.getFileOperationInput();
        /**
         * We have an one-way or request-response inbound operation.
         * The file "read" properties will be provided in
         * BindingInput extensibility element.
         * The BindingInput and its corresponding required file:read
         * properties are guaranteed or else we won't even reach here.
         */
        mFileMessage = fileInput.getFileMessage();
        FileAddress address = (FileAddress) mEndpoint.getFileAddress();
        Boolean relativePath = (address.getRelativePath() != null) ? address.getRelativePath() : Boolean.FALSE;
        String rootPath = address.getPathRelativeTo();
        mTargetDir = (relativePath.booleanValue() && rootPath != null) ? (rootPath + File.separator + address.getFileDirectory()) : address.getFileDirectory();
        String fileName = mFileMessage.getFileName();
        Boolean isPattern = (mFileMessage.getFileNameIsPattern() != null) ? mFileMessage.getFileNameIsPattern() : Boolean.FALSE;
        long pollingIntervalMillis = (mFileMessage.getPollingInterval() != null) ? mFileMessage.getPollingInterval().longValue() : 1000;

        if (mLogger.isLoggable(Level.FINE)) {
            if (isPattern.booleanValue()) {
                mLogger.log(Level.FINE, "IMP_Input_file_pattern_properties", new Object[]{mTargetDir, fileName, pollingIntervalMillis});
            } else {
                mLogger.log(Level.FINE, "IMP_Input_file_properties", new Object[]{mTargetDir, fileName, pollingIntervalMillis});
            }
        }

        if (getFiles() == null) {
            // may put a cap less than MAX_INTEGER later
            setFiles(new LinkedBlockingQueue());
        }

        try {
            recover(mep, mTargetDir, fileName, fileInput, address);
        } catch (Exception e1) {
            mLogger.log(Level.SEVERE, e1.getLocalizedMessage());
        }

        boolean yield = false;

        AtomicInteger maxFilesPerPoll = new AtomicInteger(0); // initial 0
        InputDirFilter dirFilter = address.getRecursive().booleanValue() ? new InputDirFilter(address.getExcludeRegex()) : null;

        InputFilenameFilter fileFilter = null;
        if (mFileMessage.getFileNameIsRegex()) {
            try {
                fileFilter = new InputFilenameFilter(fileName, address.getExcludeRegex(), maxFilesPerPoll, true);
            } catch (Exception ex) {
                throw new IllegalArgumentException(mMessages.getString("FILEBC-E00772.Invalid_regex_pattern", ex.getMessage()), ex);
            }
        } else if (isPattern) {
            fileFilter = new InputFilenameFilter(fileName, address.getExcludeRegex(), maxFilesPerPoll, false);
        } else {
            // the file name specified is not pattern and it is not regex either - it is a literal
            fileFilter = new InputFilenameFilter(fileName, address.getExcludeRegex(), maxFilesPerPoll);
        }

        do {
            try {
                yield = execute(mep, mTargetDir, fileName, isPattern, fileInput, address, maxFilesPerPoll, fileFilter, dirFilter,mEndpoint.getMaxConcurrencyLimit());
            } catch (Exception e) {
                mLogger.log(Level.SEVERE,
                        mMessages.getString("FILEBC-E00720.Send_message_failed_exception",
                        new Object[]{mep, mOperationName, e.getMessage()}),
                        e);
                AlertsUtil.getAlerter().critical(mMessages.getString("FILEBC-E00720.Send_message_failed_exception",
                        new Object[]{mep, mOperationName, e.getMessage()}),
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        mEndpoint.getServiceUnitID(),
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-E00720");
            }

            // if no yield occurred - sleep for the interval
            // otherwise, sleep a much smaller value
            // give other poller (clustered) chance to compete for
            // the access of the target dir
            try {
                Thread.currentThread().sleep(!yield ? pollingIntervalMillis : (pollingIntervalMillis > 50 ? 50 : pollingIntervalMillis));
            } catch (Exception e) {
                // nothing to do...
            }
        } while (!mStopRequested.get());
        // workers will check this flag 
        // to shutdown accordingly
        mStopped.set(true);
    }

    /**
     * 
     * On the in-bound side file-bc moves the input file to the working area
     * before processing , Processing is done once the files are moved to the
     * work area, once the file processing is complete the files are removed
     * form the work area,
     * 
     * Recovery: what happens if the file-bc or system crashes in the midst of
     * processing input file/files , Once the system re-boot file-bc should be
     * able to gracefully recover , that is it should be able to re-process all
     * the files in the work area whose processing was incomplete
     * 
     * 
     * @param mep
     * @param targetDir
     * @param filename
     * @param fileInput
     * @param fileAddress
     * @throws Exception
     */
    private void recover(String mep, String targetDir, String filename,
            FileInput fileInput, FileAddress fileAddress) throws Exception {
        File inDir = new File(targetDir);

        //File tmpDir = new File(inDir, fileAddress.getWorkArea());
        File tmpDir = new File(mWorkAreaBaseDir, fileAddress.getWorkArea());

        validateInputParams(targetDir, filename);

        if (tmpDir.exists()) {
            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "recovery_started", new Object[]{
                            mTargetDir, filename});
            }

            FileLock fLock = null;
            ReentrantLock tLock = null;

            if (mLogger.isLoggable(Level.FINEST)) {
                mLogger.log(Level.FINEST, "start_worker_thread");
            }
            startWorkerThreads(mep, fileInput, inDir);

            try {
                tLock = mLock.getLock();
                if (tLock != null && tLock.tryLock()) {
                    fLock = acquireLockOnTargetDir(/*fileAddress, inDir*/);
                    if (fLock != null) {
                        File[] files = null;
                        files = tmpDir.listFiles();
                        if (files.length > 0) {
                        	//Modified by Vishnu/SOLDEVILA Fabien
                        	//To process the files sequentially
                            Arrays.sort(files, new FileUtil.FileComparator());
                            mLogger.log(Level.FINEST, "files_being_recovered",
                                    tmpDir.list());
                            for (File f : files) {
                                FileMeta inFileMeta = new FileMeta(f);
                                inFileMeta.setNMProperties(
                                        new String[]{
                                            FileMeta.NMPROP_INBOUND_FILEDIR,
                                            FileMeta.NMPROP_INBOUND_FILENAME,
                                            FileMeta.NMPROP_INBOUND_POLL_RECURSIVE,
                                            FileMeta.NMPROP_INBOUND_POLL_EXCLUDE_REGEX,
                                            FileMeta.NMPROP_INBOUND_RELATIVE_PATH,
                                            FileMeta.NMPROP_INBOUND_PATH_RELATIVE_TO,
                                            FileMeta.NMPROP_INBOUND_PERSIST_BASE_DIR,
                                            FileMeta.NMPROP_INBOUND_LOCK_NAME,
                                            FileMeta.NMPROP_INBOUND_LOCK_FILE,
                                            FileMeta.NMPROP_INBOUND_WORK_AREA,
                                            FileMeta.NMPROP_INBOUND_WORK_AREA_DIR,
                                            FileMeta.NMPROP_INBOUND_FILE_IN_PROCESSING,
                                            FileMeta.NMPROP_INBOUND_ARCHIVE_ENABLED,
                                            FileMeta.NMPROP_INBOUND_ARCHIVE_AREA_RLATIVE,
                                            FileMeta.NMPROP_INBOUND_ARCHIVE_AREA,
                                            FileMeta.NMPROP_INBOUND_ARCHIVED_FILE,
                                            FileMeta.NMPROP_INBOUND_FILE_IN_ERROR,
                                            FileMeta.NMPROP_INBOUND_DEL_ON_READ /* NA, for on demand only */},
                                        new String[]{
                                            tmpDir.getAbsolutePath() != null ? tmpDir.getAbsolutePath() : "",
                                            f.getName() != null ? f.getName() : "",
                                            fileAddress.getRecursive() != null ? fileAddress.getRecursive().toString() : "",
                                            fileAddress.getExcludeRegex() != null ? fileAddress.getExcludeRegex() : "",
                                            fileAddress.getRelativePath() != null ? fileAddress.getRelativePath().toString() : "false",
                                            fileAddress.getPathRelativeTo() != null ? fileAddress.getPathRelativeTo() : "",
                                            fileAddress.getPersistenceBaseLoc() != null ? fileAddress.getPersistenceBaseLoc() : "",
                                            fileAddress.getLockName() != null ? fileAddress.getLockName() : "",
                                            mLock.getLockFilePath() != null ? mLock.getLockFilePath() : "",
                                            fileAddress.getWorkArea() != null ? fileAddress.getWorkArea() : "",
                                            tmpDir.getCanonicalPath() != null ? tmpDir.getCanonicalPath() : "",
                                            f.getCanonicalPath() != null ? f.getCanonicalPath() : "",
                                            fileInput.getFileMessage().getArchive() != null ? fileInput.getFileMessage().getArchive().toString() : "true",
                                            fileInput.getFileMessage().getArchiveDirIsRelative() != null ? fileInput.getFileMessage().getArchiveDirIsRelative().toString() : "false",
                                            fileInput.getFileMessage().getArchiveDirectory() != null ? fileInput.getFileMessage().getArchiveDirectory() : "",
                                            FileUtil.fabricateFilePath(tmpDir,
                                            fileInput.getFileMessage().getArchiveDirectory() != null ? fileInput.getFileMessage().getArchiveDirectory() : "",
                                            f.getName() != null ? f.getName().concat(mMessages.getString(SUFFIX_PROCESSED_LOCALE_KEY)) : ""),
                                            /* for now, what we can do is to provide a fabricated path - pointing to the archived file, if ACK would occur and archive enabled */
                                            FileUtil.fabricateFilePath(tmpDir,
                                            FileUtil.DEFAULT_ERRORS_DIR_NAME,
                                            f.getName() != null ? f.getName() : ""),
                                            /* for now, what we can do is to provide a fabricated path - pointing to the error file, if NACK would occur */
                                            "NA" /* NA for poll */});
                                getFiles().put(inFileMeta);
                            }
                        }
                    }
                }
            } finally {
                if (fLock != null) {
                    fLock.release();
                }
                if (tLock != null) {
                    tLock.unlock();
                }
            }

        }

    }

    /**
     * this ensures that one thread/one process have access to the working
     * directory , if there are two instance of file-bc running in cluster, only
     * one will get access to the work area for processing the files
     * 
     * @param fileAddress
     * @param inDir
     * @return
     * @throws IOException
     * @throws FileNotFoundException
     */
    private FileLock acquireLockOnTargetDir(/*FileAddress fileAddress, File inDir*/)
            throws IOException, FileNotFoundException {
        FileLock fLock = null;
        // we are the only thread on this process entered this area
        // further lock the physical target directory
        //File lockFile = new File(inDir, fileAddress.getLockName());
        File lockFile = new File(mLock.getLockFilePath());

        // make sure the lock file is there
        lockFile.createNewFile();
        FileChannel channel = mLock.getFileChannel();
        if (channel == null) {
            FileOutputStream fos = new FileOutputStream(lockFile);
            mLock.setChannel(channel = fos.getChannel());
        }
        try {
            fLock = channel.tryLock();
        } catch (IOException e) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, "Exception caught when trying to lock file for inbound target: " + lockFile.getName() + ", destination file :" + mLock.getLockFilePath() + ". Trying to recreate. Exception: " + e.getMessage());
            }
            FileOutputStream fos = new FileOutputStream(lockFile);
            mLock.setChannel(channel = fos.getChannel());
            if (fLock == null) fLock = channel.tryLock();
        }
        return fLock;
    }

    public boolean execute(
            String mep,
            String targetDir,
            String fileName,
            Boolean isPattern,
            FileInput fileInput,
            FileAddress fileAddress,
            AtomicInteger maxFilesPerPoll,
            FilenameFilter fileFilter,
            FilenameFilter dirFilter, int maxCC) throws Exception {
        boolean yield = false;

        validateInputParams(targetDir, fileName);

        // lazy init workers and input file queue
        File inDir = new File(targetDir);

        if (inDir.exists()) {
            // start workers first if not yet
            // better for responsiveness
            // when input files flow in
            if (getWorkers() == null || getWorkers().size() == 0) {
                startWorkerThreads(mep, fileInput, inDir);
            } else {
                // there are already workers threads
                Collection threads = getWorkers().values();
                Iterator it = threads.iterator();
                while (it.hasNext()) {
                    Thread t = (Thread) it.next();
                    if (!t.isAlive()) {
                        t.start();
                    }
                }
            }
            // need to protect this area
            FileLock fLock = null;
            ReentrantLock tLock = null;
            AtomicBoolean tLockAcquired = new AtomicBoolean(false);
            try {
                tLock = mLock.getLock();
                if (tLock != null && tLock.tryLock()) {
                    tLockAcquired.set(true);
                    fLock = acquireLockOnTargetDir(/*fileAddress, inDir*/);

                    if (fLock != null) {
                        // the target is locked for exclusive use
                        // filter out the input file(s) - put them into queue
                        // and leave the area quickly
                        int workerCnt = getWorkerCount();
                        maxFilesPerPoll.set(workerCnt + 1);

                        // need to scan the files and filter out
                        // what are expected (by name), then put each of them
                        // into a queue - where workers will fetch and
                        // process (normalize + send into NMR etc.)

                        File[] inputFiles = FileUtil.extractFiles(inDir,
                                fileName,
                                fileFilter,
                                dirFilter,
                                maxFilesPerPoll,maxCC);

                        if (inputFiles != null && inputFiles.length > 0) {
                            File tmpDir = new File(mWorkAreaBaseDir, fileAddress.getWorkArea());
                            tmpDir.mkdirs();
                            for (int ii = 0; ii < inputFiles.length && ii < workerCnt; ii++) {
                                // this loop is likely to keep
                                // the worker threads from
                                // starting to process the input files in the Queue
                                // and other threads from gaining access to the target dir
                                // due to the lock.
                                //
                                // so need to yield
                                File inFile = inputFiles[ii];
                                // Note, in order to avoid super long file name resulted from UUID tagging
                                // if the original input file name has UUID in it, e.g. data_67e9e747-ee91-4669-9ae1-1048785f6f9e.july.2007
                                // the UUID will be parsed and replaced by the new UUID tag
                                // making sure the re-tagged file name is unique but does not become too long
                                String uuidVal = UUID.randomUUID().toString();
                                String tmpFileName = null;
                                int uuidPos = 0;
                                if (isPattern && (uuidPos = fileName.indexOf(FileNamePatternUtil.GUID_MARKER)) >= 0) {
                                    // need to make it unique by replacing the UUID in the file name
                                    tmpFileName = replaceUUID(inFile.getName(), uuidPos, uuidVal);
                                } else {
                                    tmpFileName = inFile.getName().concat(uuidVal);
                                }
                                // move input files to a tmp directory and UUID tagged
                                // for further processing, by doing so, it won't be visible
                                // to other threads and processes - concurrency control achieved
                                // another benifit is keep the file entries under target
                                // small as possible - speed up directory listing when the 
                                // input files expecting are specified as a pattern - such as input_%d.txt
                                //

                                File tmpFile = null;
                                inFile.renameTo(tmpFile = new File(tmpDir, tmpFileName));
                                // Set NM property meta-data 
                                FileMeta inFileMeta = new FileMeta(tmpFile);
                                inFileMeta.setNMProperties(
                                        new String[]{
                                            FileMeta.NMPROP_INBOUND_DATATYPE,
                                            FileMeta.NMPROP_INBOUND_FILEDIR,
                                            FileMeta.NMPROP_INBOUND_FILENAME,
                                            FileMeta.NMPROP_INBOUND_POLL_RECURSIVE,
                                            FileMeta.NMPROP_INBOUND_POLL_EXCLUDE_REGEX,
                                            FileMeta.NMPROP_INBOUND_RELATIVE_PATH,
                                            FileMeta.NMPROP_INBOUND_PATH_RELATIVE_TO,
                                            FileMeta.NMPROP_INBOUND_PERSIST_BASE_DIR,
                                            FileMeta.NMPROP_INBOUND_LOCK_NAME,
                                            FileMeta.NMPROP_INBOUND_LOCK_FILE,
                                            FileMeta.NMPROP_INBOUND_WORK_AREA,
                                            FileMeta.NMPROP_INBOUND_WORK_AREA_DIR,
                                            FileMeta.NMPROP_INBOUND_FILE_IN_PROCESSING,
                                            FileMeta.NMPROP_INBOUND_ARCHIVE_ENABLED,
                                            FileMeta.NMPROP_INBOUND_ARCHIVE_AREA_RLATIVE,
                                            FileMeta.NMPROP_INBOUND_ARCHIVE_AREA,
                                            FileMeta.NMPROP_INBOUND_ARCHIVED_FILE,
                                            FileMeta.NMPROP_INBOUND_FILE_IN_ERROR,
                                            FileMeta.NMPROP_INBOUND_DEL_ON_READ
                                        },
                                        new String[]{
                                            fileInput.getFileMessage().getFileType(),
                                            inDir.getAbsolutePath() != null ? inDir.getAbsolutePath() : "",
                                            inFile.getName() != null ? inFile.getName() : "",
                                            fileAddress.getRecursive() != null ? fileAddress.getRecursive().toString() : "",
                                            fileAddress.getExcludeRegex() != null ? fileAddress.getExcludeRegex() : "",
                                            fileAddress.getRelativePath() != null ? fileAddress.getRelativePath().toString() : "false",
                                            fileAddress.getPathRelativeTo() != null ? fileAddress.getPathRelativeTo() : "",
                                            fileAddress.getPersistenceBaseLoc() != null ? fileAddress.getPersistenceBaseLoc() : "",
                                            fileAddress.getLockName() != null ? fileAddress.getLockName() : "",
                                            mLock.getLockFilePath() != null ? mLock.getLockFilePath() : "",
                                            fileAddress.getWorkArea() != null ? fileAddress.getWorkArea() : "",
                                            tmpDir.getCanonicalPath() != null ? tmpDir.getCanonicalPath() : "",
                                            tmpFile.getCanonicalPath() != null ? tmpFile.getCanonicalPath() : "",
                                            fileInput.getFileMessage().getArchive() != null ? fileInput.getFileMessage().getArchive().toString() : "true",
                                            fileInput.getFileMessage().getArchiveDirIsRelative() != null ? fileInput.getFileMessage().getArchiveDirIsRelative().toString() : "false",
                                            fileInput.getFileMessage().getArchiveDirectory() != null ? fileInput.getFileMessage().getArchiveDirectory() : "",
                                            FileUtil.fabricateFilePath(tmpDir,
                                            fileInput.getFileMessage().getArchiveDirectory() != null ? fileInput.getFileMessage().getArchiveDirectory() : "",
                                            tmpFileName != null ? tmpFileName.concat(mMessages.getString(SUFFIX_PROCESSED_LOCALE_KEY)) : ""),
                                            /* for now, what we can do is to provide a fabricated path - pointing to the archived file, if ACK would occur and archive enabled */
                                            FileUtil.fabricateFilePath(tmpDir,
                                            FileUtil.DEFAULT_ERRORS_DIR_NAME,
                                            tmpFileName != null ? tmpFileName : ""),
                                            /* for now, what we can do is to provide a fabricated path - pointing to the error file, if NACK would occur */
                                            "NA" /* NA for poll */});
                                // this will wake up any worker waiting on the queue
                                getFiles().put(inFileMeta);
                            }
                            // if we see more matched files
                            // than the cap, we need to yield to other inbound processors
                            // polling at the same input dir (due to clustering or multi-domain etc.)
                            yield = inputFiles.length > workerCnt;
                        }
                    }
                }
            } finally {
                if (fLock != null) {
                    fLock.release();
                }
                if (tLockAcquired.get()) {
                    // only when successfully acquired lock - need to unlock
                    if (tLock != null) {
                        tLock.unlock();
                    }
                }
            }
        } else {
            // the target directory does not exists yet - this should be OK - don't throw exception
            // the directory could be created by the application that feeds the input files
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "The target directory [" + targetDir + "] does not yet exist - no need to poll for files");
            }
        }
        return yield;
    }

    private void validateInputParams(String targetDir, String fileName)
            throws Exception, MessagingException {
        if (fileName == null) {
            throw new Exception(mMessages.getString("FILEBC-E00721.IMP_Invalid_filename"));
        }

        if (targetDir == null) {
            throw new Exception(mMessages.getString("FILEBC-E00722.IMP_Invalid_filedir"));
        }

        if (mMsgExchangeFactory == null) {
            mMsgExchangeFactory = mChannel.createExchangeFactory();
        }

        // trying to locate the service endpoint again if it's not yet found
        if (mServiceEndpoint == null) {
            mServiceEndpoint = locateServiceEndpoint();
        }
        if (mServiceEndpoint == null) {
            throw new MessagingException(mMessages.getString("FILEBC-E00723.IMP_Failed_locate_EP",
                    new Object[]{mEndpoint.getServiceName(), mEndpoint.getEndpointName()}));
        }
    }

    private void startWorkerThreads(String mep, FileInput fileInput, File inDir)
            throws Exception {
        // if workers are not initialized yet
        // initialize them now - lazy worker init
        // in case there is no input available - no need
        // to pool the workers;
        if (getWorkers() == null) {
            setWorkers(new HashMap());
        }
        // for now, a fixed number of inbound workers are enough
        int workerThreadCount = getWorkerCount();
        for (int ii = 0; ii < workerThreadCount; ii++) {
            IBFileWorker worker = new IBFileWorker(this, mep, fileInput.getFileMessage(), inDir, mWorkAreaBaseDir);
            addWorker(worker);
            ((Thread) (getWorkers().get(worker))).start();
        }
    }

    public synchronized void processReplyMessage(MessageExchange exchange) throws Exception {
        if (!(exchange instanceof InOnly) &&
                !(exchange instanceof InOut)) {
            mLogger.log(Level.SEVERE, "FILEBC-E00724.Message_pattern_reply_status_unsupported", exchange.getPattern().toString());
            AlertsUtil.getAlerter().critical("FILEBC-E00724.Message_pattern_reply_status_unsupported",
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    mEndpoint.getServiceUnitID(),
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00724");
            throw new Exception(mMessages.getString("FILEBC-E00724.Message_pattern_reply_status_unsupported", exchange.getPattern().toString()));
        }

        String messageId = exchange.getExchangeId();

// open esb issue 930 : refer to the ticket notes for details
// populating inbound NM properties can not be done here - too late
// instead, a predicted values for
// FileMeta.NMPROP_INBOUND_FILE_IN_ERROR
// FileMeta.NMPROP_INBOUND_ARCHIVED_FILE
// are given before NMR send
//
//        NormalizedMessage msg = null;

        if (exchange instanceof InOnly) {
            String lastRecord = (String) ((InOnly) exchange).getInMessage().getProperty(FileMeta.NMPROP_INBOUND_LASTRECORD);
            if (lastRecord == null || !Boolean.parseBoolean(lastRecord)) {
                mInboundExchanges.remove(messageId);
                removeInboundME(exchange);
                return;
            }
// open esb issue 930 : refer to the ticket notes for details
//            msg = ((InOnly)exchange).getInMessage();
        } else {
            // InOut
// open esb issue 930 : refer to the ticket notes for details
//            msg = ((InOut)exchange).getInMessage();
        }

        if (mInboundExchanges.containsKey(messageId)) {
            FileMeta inFileMeta = mInboundReplys.get(messageId);
            File inputFile = inFileMeta.getInProcessFileHandle();
            if (exchange.getStatus() != ExchangeStatus.DONE) {
                // Any status other than 'DONE' is considered an error
                // move input file to error folder and create error details file
                // Note that, every input file is UUID tagged as in input.txt<UUID>
                // it is reasonable to assume that the renameTo will never run into an existing
                // file with the same name
                File inDir = new File(mTargetDir);
                //File errDir = new File(inDir.getAbsolutePath() + File.separator +
                //		FileUtil.DEFAULT_ERRORS_DIR_NAME);
                File errDir = new File(mWorkAreaBaseDir + File.separator +
                        FileUtil.DEFAULT_ERRORS_DIR_NAME);
                if (!errDir.exists()) {
                    errDir.mkdirs();
                }
                // move input file to error dir
                File errInputFile = new File(errDir.getAbsolutePath() + File.separator + inputFile.getName());
                inputFile.renameTo(errInputFile);

                // Write the details of error to a separate file
                String errFilePath = errInputFile.getAbsolutePath() + FileUtil.getErrorFileSuffix();
                FileUtil.createErrorFile(errFilePath, exchange);
// open esb issue 930 : refer to the ticket notes for details
//                msg.setProperty(FileMeta.NMPROP_INBOUND_FILE_IN_ERROR, errInputFile.getCanonicalPath());
//                msg.setProperty(FileMeta.NMPROP_INBOUND_ARCHIVED_FILE, "");
            } else {
                // when the message is sent successfully by the outbound
                // the input file will be UUID tagged and "processed" tagged
                // and moved to archive directory OR deleted if archive is not enabled

                // it is time to archive the processed input file
                // moving from tmp to archive dir - or delete it if archive is not enabled
                if (getFileMessage() != null && getFileMessage().getArchive()) {
                    String archiveDir = getFileMessage().getArchiveDirectory();
                    if (archiveDir != null && archiveDir.trim().length() > 0) {
                        String inputFileName = inputFile.getName();

                        //File archiveDirObj = getFileMessage().getArchiveDirIsRelative().booleanValue() ? new File(getTargetDirectory(), archiveDir) : new File(archiveDir);
                        File archiveDirObj = getFileMessage().getArchiveDirIsRelative().booleanValue() ? new File(mWorkAreaBaseDir.getAbsolutePath(), archiveDir) : new File(archiveDir);

                        if (!archiveDirObj.exists()) {
                            if (!archiveDirObj.mkdirs()) {
                                throw new Exception(mMessages.getString("FILEBC-E00725.IMP_Failed_archiving_file_create_archive_dir", new Object[]{archiveDirObj.getAbsolutePath()}));
                            }
                        } else if (!archiveDirObj.isDirectory()) {
                            throw new Exception(mMessages.getString("FILEBC-E00726.IMP_Archive_dir_not_dir_type", new Object[]{archiveDirObj.getAbsolutePath()}));
                        }

                        if (inputFile.exists()) {
                            File dest = new File(archiveDirObj, inputFileName + mMessages.getString(SUFFIX_PROCESSED_LOCALE_KEY));
                            if (!inputFile.renameTo(dest)) {
                                // move failed
                                mLogger.log(Level.SEVERE, mMessages.getString("FILEBC-E00727.Archive_rename_failed",
                                        new Object[]{inputFile.getAbsolutePath(), dest.getAbsolutePath()}));
                                AlertsUtil.getAlerter().critical(mMessages.getString("FILEBC-E00727.Archive_rename_failed",
                                        new Object[]{inputFile.getAbsolutePath(), dest.getAbsolutePath()}),
                                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                                        mEndpoint.getServiceUnitID(),
                                        AlertsUtil.getServerType(),
                                        AlertsUtil.COMPONENT_TYPE_BINDING,
                                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                        NotificationEvent.EVENT_TYPE_ALERT,
                                        "FILEBC-E00727");
                            }
// open esb issue 930 : refer to the ticket notes for details
//                            else {
//                                msg.setProperty(FileMeta.NMPROP_INBOUND_FILE_IN_ERROR, "");
//                                msg.setProperty(FileMeta.NMPROP_INBOUND_ARCHIVED_FILE, dest.getCanonicalPath());
//                            }
                        }
                    } else {
                        // error - archive directory must not be empty or unspecified
                        throw new Exception(mMessages.getString("FILEBC-E00728.IMP_Failed_archive_file_null_or_empty_archive_dir"));
                    }
                } else {
                    // delete the processed
                    if (inputFile.delete()) {
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, "File [" + inputFile.getAbsolutePath() + "] processed and deleted");
                        }
                    } else {
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, "Failed to delete processed file [" + inputFile.getAbsolutePath() + "]");
                        }
                    }
                }
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "IMP_Remove_exchange_msg_id", messageId);
            }
            mInboundExchanges.remove(messageId);
            removeInboundME(exchange);
        } else {
            mLogger.log(Level.SEVERE, "FILEBC-E00724.Message_pattern_reply_status_unsupported", messageId);
        }
    }

    public void stopReceiving() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "IMP_Inbound_stopped");
        }
        mStopRequested.set(true);
    }

    public void onRedelivery(MessageExchange exchange) throws MessagingException {

        NormalizedMessage inMsg = null;
        String groupId = (String) exchange.getProperty(FileComponentContext.CRMP_GROUP_ID);
        String messageId = (String) exchange.getProperty(FileComponentContext.CRMP_MESSAGE_ID);

        // remove the listener associated with the exchange ID
        MessageExchangeSupport.removeRedeliveryListener(exchange.getExchangeId());

        //get the input file associated with this message, then remove the entry
        FileMeta inFileMeta = mInboundReplys.get(exchange.getExchangeId());
        File inputFile = inFileMeta.getInProcessFileHandle();

        // remove meta-data associated with this exchange instance.
        removeInboundME(exchange);

        MessageExchange me = null;
        switch (ExchangePattern.valueOf(exchange)) {
            case IN_OUT:
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Resending the InOut exchange with group ID '" + groupId + "' and message ID '" + messageId + "'...");
                }
                inMsg = ((InOut) exchange).getInMessage();
                me = mMessageExchangeFactory.createInOutExchange();
                break;
            case IN_ONLY:
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Resending the InOnly exchange with group ID '" + groupId + "' and message ID '" + messageId + "'...");
                }
                inMsg = ((InOnly) exchange).getInMessage();
                me = mMessageExchangeFactory.createInOnlyExchange();
                break;
            default:
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Retry handler received an unsupported exchange pattern: " +
                            ExchangePattern.valueOf(exchange) + ". Ignoring the retry attempt...");
                }
                break;
        }

        if (me != null) {
            // make sure that the message id is the same as for the message for which
            // re-delivery is being attempted.
            me.setProperty(FileComponentContext.CRMP_GROUP_ID, groupId);
            me.setProperty(FileComponentContext.CRMP_MESSAGE_ID, messageId);
            resend(me, inMsg, inFileMeta);
        }
    }

    private void resend(MessageExchange me, NormalizedMessage nm, FileMeta inFileMeta) throws MessagingException {
        send(me, nm, inFileMeta, true);
    }

    /**
     * Sends the givem MessageExchange with the given Normalized Message as payload.
     * 
     * Honors Throttling configuration for the endpoint.
     *  
     * Adds redelivery listner if reTryEnbaled is true.
     * 
     * @param me
     * @param nm
     * @param inputFile
     * @param reTryEnbaled
     * @throws MessagingException
     */
    public void send(MessageExchange me, NormalizedMessage nm, FileMeta inFileMeta, boolean reTryEnbaled) throws MessagingException {

        if (me == null) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "send called with a null ME: aborted");
            }
            return;
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Sending message " + me.getExchangeId());
        }

        // this call might block depending on throttling configurations.
        addInboundME(me, inFileMeta);

        if (reTryEnbaled) {
            MessageExchangeSupport.addRedeliveryListener(me.getExchangeId(), this);
        }

        String exchangeId = me.getExchangeId();
        try {
            MessageUtil.checkAndGenerateInMemoryCRMP(me, mEndpoint);
            me.setEndpoint(getServiceEndpoint());
            me.setOperation(getOperationName());
            me.setMessage(nm, IN_MSG);

            // set NM Properties
            MessageUtil.setNMProperties(nm, inFileMeta);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "About to send the message exchange containing the file payload to the downstream component...");
            }
            mChannel.send(me);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Successfully sent the message exchange containing the file payload to the downstream component...");
            }
        } catch (MessagingException ex) {
            String error = mMessages.getString("FILEBC-E00771.Failed_send_inout");
            mLogger.log(Level.SEVERE, error, ex);
            AlertsUtil.getAlerter().warning(error,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    mEndpoint.getServiceUnitID(), AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, "FILEBC-E00771");

            if (exchangeId != null) {
                removeInboundME(me);
                if (reTryEnbaled) {
                    MessageExchangeSupport.removeRedeliveryListener(exchangeId);
                }
            }
            throw ex;
        } finally {
        }

        // message sent successfully, increment stats
        getEndpoint().getEndpointStatus().incrementSentRequests();

        return;
    }

    /**
     * Adds the given MessageExchange to collection of currently active Inbound messages 
     * (i.e messages for which replys are outstanding), for the endpoint represented by 
     * this {@link InboundMessageProcessor}. 
     * 
     * This method takes into consideration throttling configuration for the endpoint. 
     * It may block if number of pending messages is >= maxConcurrency limit (set in 
     *  the throttling configuration).
     * 
     *  
     * @param me
     * @param inputFile
     */
    public void addInboundME(MessageExchange me, FileMeta inFileMeta) {

        // Get the maxConcurrencyLimit for this endpoint
        int maxCC = getEndpoint().getMaxConcurrencyLimit();

        synchronized (mInboundReplys) {
            int nPending = mInboundReplys.size();

            if ((maxCC > 0) && (nPending >= maxCC)) {
                // Throttle - wait until count of outstanding messages is
                // not less than maxConcurrencyLimit
                while (nPending >= maxCC) {
                    try {
                        mInboundReplys.wait(1000);
                    } catch (InterruptedException ie) {
                    }

                    nPending = mInboundReplys.size();
                }
            }
            mInboundReplys.put(me.getExchangeId(), inFileMeta);
        }

        mInboundExchanges.put(me.getExchangeId(), new ListenerMeta(System.currentTimeMillis(), this));
    }

    /**
     * Remove meta-data associated with this message from mInboundExchanges map and mInboundReplys maps.
     * 
     * Also sends a notification on mInboundReplys after remove, so as to enable resume of any threads 
     * waiting to send messages. Threads could be waiting to send messages due to throttling limits being 
     * maxed (i.e when mInboundReplys.size() >= maxConcurrencyLimit (set by Throttling config) ).
     * 
     * @param me
     */
    public void removeInboundME(MessageExchange me) {
        if (me == null || me.getExchangeId() == null) {
            return;
        }

        mInboundExchanges.remove(me.getExchangeId());

        synchronized (mInboundReplys) {
            mInboundReplys.remove(me.getExchangeId());
            mInboundReplys.notify();
        }
    }

    private FileOperation locateFileOperation(QName opname) {
        return (FileOperation) mEndpoint.getFileOperations().get(opname);
    }

    private String getMessageExchangePattern(QName opname) {
        String mep = null;
        Map opMEPs = mEndpoint.getOperationMsgExchangePattern();
        if (opMEPs.get(opname) != null) {
            mep = (String) opMEPs.get(opname);
        }
        return mep;
    }

    /** Arguable the validations included in this method should already been performed
     * in WSDL validation (either at design time or deployment time).
     * This method can be moved to WSDL validation modules once the framework is ready
     */
    protected void validateInboundMessageExchangeProperties(FileOperation operation, String mep) throws Exception {
        // 1. Check if the Message Exchange Pattern is valid.
        if (mep == null ||
                (!mep.equals(EndpointMessageType.IN_ONLY) &&
                !mep.equals(EndpointMessageType.IN_OUT))) {
            throw new Exception(mMessages.getString("FILEBC-E00729.IMP_Invalid_mep", mOperationName));
        }

        // 2. Check if required file:message properties are present and valid on File binding Input element
        FileInput fileInput = operation.getFileOperationInput();
        if (fileInput == null) {
            throw new Exception(mMessages.getString("FILEBC-E00730.IMP_Invalid_No_FileInput", mOperationName));
        }

        FileMessage fileMessage = fileInput.getFileMessage();
        if (fileMessage == null) {
            throw new Exception(mMessages.getString("FILEBC-E00731.IMP_Invalid_No_FileMessage", mOperationName));
        }

        if (fileMessage.getFileName() == null) {
            throw new Exception(mMessages.getString("FILEBC-E00732.IMP_Invalid_No_FileMessage_FileName", mOperationName));
        }

        if (!fileMessage.getFileType().equals(FileMessage.FILE_TYPE_TEXT) &&
                !fileMessage.getFileType().equals(FileMessage.FILE_TYPE_BINARY)) {
            throw new Exception(mMessages.getString("FILEBC-E00733.IMP_Invalid_File_TYPE", new Object[]{fileMessage.getFileType(), mOperationName}));
        }

        if (fileMessage.getFileUseType().equals(FileMessage.FILE_USE_TYPE_ENCODED) &&
                (fileMessage.getFileEncodingStyle() == null ||
                fileMessage.getFileEncodingStyle().equals(""))) {
            throw new Exception(mMessages.getString("FILEBC-E00734.IMP_Invalid_No_FileMessage_EncodingStyle", mOperationName));
        }

        // 2. validate file name if file name is pattern is "true"
        if (fileMessage.getFileNameIsPattern().booleanValue() && !fileMessage.getFileNameIsRegex().booleanValue()) {
            // invalid file name pattern will prevent the inbound processor from running
            String[] decomposed = new String[3];
            FileNamePatternType type = FileNamePatternUtil.validatePattern(fileMessage.getFileName(), decomposed);
        }

        // validate the regex pattern is fileNameIsRegEx is "true"
        // just trying to catch any problems early
        if (fileMessage.getFileNameIsRegex().booleanValue()) {
            try {
                FileNamePatternType type = FileNamePatternUtil.validateRegExPattern(fileMessage.getFileName());
            } catch (Exception e) {
                throw new Exception(mMessages.getString("FILEBC-E00772.Invalid_regex_pattern", e.getMessage()));
            }
        }

        // 3. validate archive related attributes
        if (fileMessage.getArchive()) {
            if (fileMessage.getArchiveDirectory() == null || fileMessage.getArchiveDirectory().trim().length() == 0) {
                // error - archive directory must not be empty or unspecified
                throw new Exception(mMessages.getString("FILEBC-E00728.IMP_Failed_archive_file_null_or_empty_archive_dir"));
            }
        }

        // 4. Check if required file:message properties are present and valid on File binding Output element
        if (mep.equals(EndpointMessageType.IN_OUT)) {
            FileOutput fileOutput = operation.getFileOperationOutput();

            if (fileOutput == null) {
                throw new Exception(mMessages.getString("FILEBC-E00735.IMP_Invalid_No_FileOutput", mOperationName));
            }

            fileMessage = fileOutput.getFileMessage();
            if (fileMessage == null) {
                throw new Exception(mMessages.getString("FILEBC-E00731.IMP_Invalid_No_FileMessage", mOperationName));
            }

            if (fileMessage.getFileName() == null) {
                throw new Exception(mMessages.getString("FILEBC-E00732.IMP_Invalid_No_FileMessage_FileName", mOperationName));
            }

            if (!fileMessage.getFileType().equals(FileMessage.FILE_TYPE_TEXT) &&
                    !fileMessage.getFileType().equals(FileMessage.FILE_TYPE_BINARY)) {
                throw new Exception(mMessages.getString("FILEBC-E00733.IMP_Invalid_File_TYPE", new Object[]{fileMessage.getFileType(), mOperationName}));
            }

            if (fileMessage.getFileUseType().equals(FileMessage.FILE_USE_TYPE_ENCODED) &&
                    (fileMessage.getFileEncodingStyle() == null ||
                    fileMessage.getFileEncodingStyle().equals(""))) {
                throw new Exception(mMessages.getString("FILEBC-E00734.IMP_Invalid_No_FileMessage_EncodingStyle", mOperationName));
            }
        }
    }

    /** Retrieves all activated endpoints for a given service,
     * and explicitly choose which endpoint to route to.
     */
    private ServiceEndpoint locateServiceEndpoint() {
        ServiceEndpoint activatedEndpoint = null;
        QName fullServiceName = mEndpoint.getServiceName();
        String endpointName = mEndpoint.getEndpointName();
        activatedEndpoint = mContext.getEndpoint(fullServiceName, endpointName);

        if (mLogger.isLoggable(Level.FINE) && activatedEndpoint != null) {
            mLogger.log(Level.FINE, "IMP_locate_EP",
                    new Object[]{mEndpoint.getServiceName(), mEndpoint.getEndpointName()});
        }
        return (activatedEndpoint);
    }

    private String replaceUUID(String fname, int pos, String uuid) {
        StringBuffer result = new StringBuffer();
        if (pos + uuid.length() <= fname.length()) {
            if (pos > 0) {
                result.append(fname.substring(0, pos)).append(uuid);
                if (pos + uuid.length() < fname.length()) {
                    // there is remainig tail
                    result.append(fname.substring(pos + uuid.length()));
                }
            } else {
                // the UUID starts at begining of the original name
                result.append(uuid);
                if (pos + uuid.length() < fname.length()) {
                    // there is remainig tail
                    result.append(fname.substring(pos + uuid.length()));
                }
            }
        } else {
            // use the UUID to tag (suffix) the name as usual
            result.append(fname).append(".").append(uuid);
        }
        return result.toString();
    }

    public static Map getInboundExchanges() {
        return mInboundExchanges;
    }

    public static void setInboundExchangeIds(Map exchangeIds) {
        mInboundExchanges = exchangeIds;
    }

    public Map getInboundReplyIds() {
        return mInboundReplys;
    }

    public void setInboundReplyIds(Map replyIds) {
        mInboundReplys = replyIds;
    }

    public MessageExchangeFactory getMsgExchangeFactory() {
        if (mMsgExchangeFactory == null) {
            throw new IllegalStateException(mMessages.getString("FILEBC-E00736.IMP_Object_Not_Available_When_Accessed", "MessageExchangeFactory"));
        }
        return mMsgExchangeFactory;
    }

    public LinkedBlockingQueue getInputFileQueue() {
        if (mFiles == null) {
            throw new IllegalStateException(mMessages.getString("FILEBC-E00737.IMP_Object_Not_Initialized_When_Accessed", "Input File Queue"));
        }
        return mFiles;
    }

//    public DeliveryChannel getDelivaryChannel() {
//        if ( mChannel == null )
//            throw new IllegalStateException(mMessages.getString("FILEBC-E00736.IMP_Object_Not_Available_When_Accessed", "DeliveryChannel"));
//        return mChannel;
//    }
//    
    public Endpoint getEndpoint() {
        if (mEndpoint == null) {
            throw new IllegalStateException(mMessages.getString("FILEBC-E00736.IMP_Object_Not_Available_When_Accessed", "Endpoint"));
        }
        return mEndpoint;
    }

    public ServiceEndpoint getServiceEndpoint() {
        if (mServiceEndpoint == null) {
            throw new IllegalStateException(mMessages.getString("FILEBC-E00736.IMP_Object_Not_Available_When_Accessed", "ServiceEndpoint"));
        }
        return mServiceEndpoint;
    }

    public QName getOperationName() {
        if (mOperationName == null) {
            throw new IllegalStateException(mMessages.getString("FILEBC-E00736.IMP_Object_Not_Available_When_Accessed", "Operation name"));
        }
        return mOperationName;
    }

    public boolean isStopRequested() {
        return mStopRequested.get();
    }

    public boolean isStopped() {
        return mStopped.get();
    }

    public void setStopped(boolean b) {
        mStopped.set(b);
    }

    public int getWorkerCount() {
        //return mWorkers != null && mWorkers.size() > 0 ? mWorkers.size() : NUM_IB_WORKER;
    	return mWorkers != null && mWorkers.size() > 0 ? mWorkers.size() : mNumIBWorkers;
    }

    public void setLock(Lock l) {
        mLock = l;
    }

    public Lock getLock() {
        return mLock;
    }

    public void setWorkers(Map workers) {
        mWorkers = workers;
    }

    public Map getWorkers() {
        return mWorkers;
    }

    public void addWorker(IBFileWorker worker) {
        if (mWorkers == null) {
            setWorkers(new HashMap());
        }
        Thread t = new Thread(worker);
        t.setName(IB_WORKER_THREAD_NAME_PREFIX.concat(t.getName()));
        mWorkers.put(worker, t);
    }

    public void setFiles(LinkedBlockingQueue<FileMeta> files) {
        mFiles = files;
    }

    public LinkedBlockingQueue<FileMeta> getFiles() {
        return mFiles;
    }

    public void setFileMessage(FileMessage msg) {
        mFileMessage = msg;
    }

    public FileMessage getFileMessage() {
        return mFileMessage;
    }

    public void setTargetDirectory(String s) {
        mTargetDir = s;
    }

    public String getTargetDirectory() {
        return mTargetDir;
    }

    private PortType getPortType(Endpoint endpoint) {
        String serviceName = endpoint.getServiceName().toString();
        String endpointName = endpoint.getEndpointName();
        Definition def = endpoint.getDefinition();
        Map services = def.getServices();

        // DO NOT use the getService() method.
        // It checks all imported WSDLs.
        Service svc = (Service) services.get(QName.valueOf(serviceName));
        if (svc == null) {
            return null;
        }

        Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());
        if (port == null) {
            return null;
        }

        Binding binding = port.getBinding();
        if (binding == null) {
            return null;
        }
        return binding.getPortType();
    }
}
