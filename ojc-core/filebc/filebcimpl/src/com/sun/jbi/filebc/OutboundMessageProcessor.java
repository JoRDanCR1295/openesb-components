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
 * @(#)OutboundMessageProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * Portions Copyright 2011 IntegratedApps LLC.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.filebc.PendingExchangeManager.PendingExchangeHolder;
import com.sun.jbi.filebc.extensions.FileAddress;
import com.sun.jbi.filebc.extensions.FileInput;
import com.sun.jbi.filebc.extensions.FileMessage;
import com.sun.jbi.filebc.extensions.FileOperation;
import com.sun.jbi.filebc.extensions.FileOutput;
import com.sun.jbi.filebc.management.FileBCManagementMBean;
import com.sun.jbi.filebc.util.FileStreamHandler;
import com.sun.jbi.filebc.util.MessageUtil;
import com.sun.jbi.filebc.util.OutputFilenameFormatter;
import com.sun.jbi.filebc.util.InputFilenameFilter;
import com.sun.jbi.filebc.util.InputDirFilter;
import com.sun.jbi.filebc.util.AlertsUtil;
import com.sun.jbi.filebc.util.FileNamePatternUtil;
import com.sun.jbi.filebc.util.FileUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.exchange.ExchangePattern;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.channels.FileLock;
import java.util.Iterator;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.atomic.AtomicInteger;

import javax.jbi.messaging.*;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanException;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;

import net.java.hulp.measure.Probe;

import org.xml.sax.SAXException;

/**
 * This class processes request and reply
 * messages received from the SE.
 *
 * @author Sherry Weng
 * @author Jim Fu
 * @author Kir Sorokin, kir.sorokin@integrated-apps.com
 */
public class OutboundMessageProcessor implements Runnable {

    private static final int TWO_MB = 2 * 1024 * 1024;
    private static Messages mMessages =
            Messages.getMessages(OutboundMessageProcessor.class);
    private static Logger mLogger = Messages.getLogger(OutboundMessageProcessor.class);
    private static final String PROVISIONING_ID = "Provider";
    private static final String CONSUMING_ID = "Consumer";
    private MessagingChannel mChannel;
    private Map mInboundExchanges;
    private Map mServiceUnits;
    private Object mMonitor = null;
    private FileDenormalizer mDenormalizer = null;

    public OutboundMessageProcessor(MessagingChannel channel,
            Map serviceUnits,
            Map inboundMessageExchanges) {
        mChannel = channel;
        mServiceUnits = serviceUnits;
        mInboundExchanges = inboundMessageExchanges;
        mMonitor = new Object();
        mDenormalizer = new FileDenormalizer();
    }

    /**
     * Entry point to execute this thread
     * to handle the message exchanges.
     */
    public void run() {
        try {
            do {
                MessageExchange msgExchange = mChannel.accept(5000);
                if (msgExchange != null) {
                    String exchangeId = msgExchange.getExchangeId();
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "OMP_Accept_msg", exchangeId);
                    }

                    //First check in there are any pending exchanges
                    if (msgExchange.getStatus() == ExchangeStatus.DONE || msgExchange.getStatus() == ExchangeStatus.ERROR) {
                        PendingExchangeHolder holder = PendingExchangeManager.getInstance().getExchangeHolder(msgExchange);
                        if (holder != null && holder.fileStreamHandler.isDonotClose()) {
                            holder.fileStreamHandler.close();
                        }
                    }

                    boolean inbound = mInboundExchanges.containsKey(exchangeId) &&
                            msgExchange.getRole().equals(MessageExchange.Role.CONSUMER);
                    ListenerMeta listenerMeta = (ListenerMeta) mInboundExchanges.get(exchangeId);

                    MessageExchangeReplyListener listener = null;
                    if (listenerMeta != null) {
                        listener = listenerMeta.getMessageExchangeReplyListener();
                    }
                    if (inbound) {
                        long invocationTime = listenerMeta.getRequestInvocationTime();
                        if (mLogger.isLoggable(Level.FINE)) {
                            long difference = System.currentTimeMillis() - invocationTime;
                            mLogger.log(Level.FINE, "OMP_Resp_Ex", new Object[]{exchangeId, difference});
                        }
                    }
                    URI pattern = msgExchange.getPattern();
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "OMP_Pattern", new Object[]{exchangeId, pattern});
                    }

                    Endpoint endpoint = findEndpoint(msgExchange);
                    if (endpoint == null) {
                        String errMsg = mMessages.getString("FILEBC-E00738.OMP_no_endpoint_match",
                                new Object[]{msgExchange.getEndpoint().getServiceName(),
                                    msgExchange.getEndpoint().getEndpointName()});
                        msgExchange.setError(new Exception(errMsg));
                        throw new Exception(errMsg);
                    }

                    QName operation = msgExchange.getOperation();
                    switch (ExchangePattern.valueOf(msgExchange)) {
                        case IN_OUT:
                            if (mLogger.isLoggable(Level.FINE)) {
                                mLogger.log(Level.FINE, "OMP_Recv_InOut", exchangeId);
                            }
                            if (inbound) {
                                processRequestReplyInbound((InOut) msgExchange, endpoint, operation, listener);
                            } else {
                                processRequestReplyOutbound((InOut) msgExchange, endpoint, operation);
                            }
                            break;
                        case IN_ONLY:
                            if (mLogger.isLoggable(Level.FINE)) {
                                mLogger.log(Level.FINE, "OMP_Recv_InOnly", exchangeId);
                            }
                            if (inbound) {
                                processOneWayInbound((InOnly) msgExchange, endpoint, listener);
                            } else {
                                processOneWayOutbound((InOnly) msgExchange, endpoint, operation);
                            }
                            break;
                        case ROBUST_IN_ONLY:
                            if (mLogger.isLoggable(Level.WARNING)) {
                                mLogger.log(Level.WARNING, "FILEBC-E00739.Unsupported_exchange_pattern_robust_inonly", exchangeId);
                            }
                            AlertsUtil.getAlerter().warning(mMessages.getString("FILEBC-E00739.Unsupported_exchange_pattern_robust_inonly", exchangeId),
                                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                                    endpoint.getServiceUnitID(),
                                    AlertsUtil.getServerType(),
                                    AlertsUtil.COMPONENT_TYPE_BINDING,
                                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                    NotificationEvent.EVENT_TYPE_ALERT,
                                    "FILEBC-E00739");
                            break;
                        case IN_OPTIONAL_OUT:
                            if (mLogger.isLoggable(Level.WARNING)) {
                                mLogger.log(Level.WARNING, "FILEBC-E00740.Unsupported_exchange_pattern_inout", exchangeId);
                            }
                            AlertsUtil.getAlerter().warning(mMessages.getString("FILEBC-E00740.Unsupported_exchange_pattern_inout", exchangeId),
                                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                                    endpoint.getServiceUnitID(),
                                    AlertsUtil.getServerType(),
                                    AlertsUtil.COMPONENT_TYPE_BINDING,
                                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                    NotificationEvent.EVENT_TYPE_ALERT,
                                    "FILEBC-E00740");
                            break;
                        default:
                            invalidMEP(endpoint, exchangeId);
                            return;
                    }
                }
            } while (mMonitor != null);
        } catch (Throwable ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("FILEBC-E00742.Message_processing_exception", ex.getLocalizedMessage()), ex);
            AlertsUtil.getAlerter().critical(mMessages.getString("FILEBC-E00742.Message_processing_exception", ex.getLocalizedMessage()),
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00742");
        }

        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FILEBC-E00743.Message_processing_done");
        }
    }

    private void invalidMEP(Endpoint endpoint, String exchangeId) {
        if (mLogger.isLoggable(Level.SEVERE)) {
            mLogger.log(Level.SEVERE, "FILEBC-E00741.Invalid_exchange_pattern", exchangeId);
        }

        AlertsUtil.getAlerter().critical(mMessages.getString("FILEBC-E00741.Invalid_exchange_pattern", exchangeId),
                FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                endpoint.getServiceUnitID(),
                AlertsUtil.getServerType(),
                AlertsUtil.COMPONENT_TYPE_BINDING,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT,
                "FILEBC-E00741");
        return;
    }

    public void processRequestReplyOutbound(InOut inout, Endpoint endpoint,
            QName operation) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "OMP_Processing_inout_outbound");
        }

        if (inout.getStatus() == ExchangeStatus.DONE) {
            endpoint.getEndpointStatus().incrementReceivedDones();
        } else if (inout.getStatus() == ExchangeStatus.ERROR) {
            endpoint.getEndpointStatus().incrementReceivedErrors();
        } else {
            endpoint.getEndpointStatus().incrementReceivedRequests();

            try {
                FileOperation fileOperation = locateFileOperation(operation,
                        endpoint);

                FileOutput fileOutput = fileOperation.getFileOperationOutput();

                validateOutboundMessageExchangeProperties(fileOperation,
                        operation);

                endpoint.getEndpointStatus().incrementReceivedRequests();

                FileMessage outputFileMessage = fileOutput.getFileMessage();

                // do solicited read.
                processSyncRead(endpoint, inout, outputFileMessage);

            } catch (Exception e) {
                String msg = mMessages.getString("FILEBC-E00791.Solicited_Read_Failed",
                        new Object[]{endpoint.getServiceName(),
                            endpoint.getEndpointName(),
                            operation.toString(),
                            e.getLocalizedMessage()});

                mLogger.log(Level.SEVERE, msg);
                AlertsUtil.getAlerter().critical(
                        msg,
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        endpoint.getServiceUnitID(),
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-E00791");

                try {
                    setError(inout, e, msg, MessageUtil.FAULTCODE_SERVER, "");
                    mChannel.send(inout);
                    endpoint.getEndpointStatus().incrementSentErrors();
                } catch (MessagingException me) {
                    msg = mMessages.getString(
                            "FileBC-E00792.Solicited_Read_Could_Not_Send_Error_Message",
                            new Object[]{me.getLocalizedMessage()});
                    mLogger.log(Level.SEVERE, msg);
                    AlertsUtil.getAlerter().critical(
                            msg,
                            FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                            endpoint.getServiceUnitID(),
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "FILEBC-E00792");

                    endpoint.getEndpointStatus().incrementSentErrors();
                }
                return;
            }
        }

    }

    private void processSyncRead(Endpoint endpoint, InOut inOutME, FileMessage fileMessage)
            throws Exception {
        // update fileMessage with NM properties
        NormalizedMessage msg = inOutME.getInMessage();
        fileMessage = mergeFileMessageProperties(fileMessage, msg);

        FileAddress address = endpoint.getFileAddress();

        // update fileAddress with NM properties
        address = mergeFileAddressProperties(address, msg);


        Boolean relativePath = (address.getRelativePath() != null) ? address.getRelativePath() : Boolean.FALSE;
        String rootPath = address.getPathRelativeTo();
        String fileDir = (relativePath.booleanValue() && rootPath != null) ? (rootPath + File.separator + address.getFileDirectory())
                : address.getFileDirectory();
        String fileName = fileMessage.getFileName();

        boolean removeEOL = (fileMessage.getRemoveEOL() != null) ? fileMessage.getRemoveEOL().booleanValue() : false;
        boolean multipleRecords = (fileMessage.getMultipleRecordsPerFile() != null) ? fileMessage.getMultipleRecordsPerFile().booleanValue()
                : false;

        // multiple records are not supported for solicited read
        if (multipleRecords) {
            throw new Exception(mMessages.getString("FileBC-E00790.Solicited_Read_Multiple_Records_Per_File", new Object[]{endpoint.getUniqueName()}));
        }

        long maxBytesPerRecord = (fileMessage.getMaxBytesPerRecord() != null) ? fileMessage.getMaxBytesPerRecord().longValue()
                : -1; // -1 means not defined
        byte[] recordDelim = (fileMessage.getRecordDelimiter() != null) ? fileMessage.getRecordDelimiter().getBytes()
                : new byte[]{};



        File rootDir = new File(fileDir);
        String errString = null;
        FileStreamHandler streamHandler = null;
        FileLock fileLock = null;
        ReentrantLock reentrantLock = null;

        try {
            /*
             * (1) lock to protect the root dir
             * (2) scan the root dir for 1 entry (can be recursive)
             * (3) move the file to staging area (requires: deleteOnRead = true || archive = true )
             * (4) unlock the root dir if staging is on (refer to (3)) and effective
             *     otherwise, the target is assumed to be a configuration and will be kept where it is
             *     do not stage it, unlock the lock - since there is no need to sync the access
             *     of a configuration (not suppose to change dynamically).
             * (5) read file, normalize, send into NMR
             * (6) archive or delete the file in staging area
             */

            //lock here if read file to be deleted

            // comment out the condition
            // because, concurrent threads
            // doing directory listing for a file (specified by a literal, pattern, regex)
            // need to be synchronized (on a two level locking)

            // if (fileMessage.isDeleteFileOnRead()) {
            /*****************************
             *
             * (1) lock to protect the root directory scanning
             *
             *****************************/
            Lock cLock = LockRegistry.get(endpoint.getEPUUID());
            reentrantLock = cLock.getLock();
            reentrantLock.lock(); // blocking until lock acquired
            fileLock = cLock.getFileChannel().lock(); // blocking until get the lock
            msg.setProperty(FileMeta.NMPROP_INBOUND_LOCK_FILE, cLock.getLockFilePath() != null ? cLock.getLockFilePath() : "");
            //}

            String uuid4Archive = null;

            AtomicInteger maxFilesPerPoll = new AtomicInteger(1); // for OB fetch - one match is enough
            InputDirFilter dirFilter = address.getRecursive().booleanValue() ? new InputDirFilter(address.getExcludeRegex()) : null;

            InputFilenameFilter fileFilter = null;
            if (fileMessage.getFileNameIsRegex().booleanValue()) {
                try {
                    fileFilter = new InputFilenameFilter(fileName, address.getExcludeRegex(), maxFilesPerPoll, true);
                } catch (Exception ex) {
                    throw new IllegalArgumentException(mMessages.getString("FILEBC-E00772.Invalid_regex_pattern", ex.getMessage()), ex);
                }
            } else if (fileMessage.getFileNameIsPattern().booleanValue()) {
                fileFilter = new InputFilenameFilter(fileName, address.getExcludeRegex(), maxFilesPerPoll, false);
            }

            /********************************
             *
             * (2) Scan the root directory (may be recursively)
             * for the first file matching the pattern / regex / literal name
             *
             ********************************/
            File[] files = FileUtil.extractFiles(rootDir, fileName, fileFilter, dirFilter, maxFilesPerPoll,endpoint.getMaxConcurrencyLimit());
            File inputFile = null;
            File targetFile = null;
            File workBaseDir = null;
            File workArea = null;

            if (files != null && files.length == 1) {
                inputFile = files[0];
            }

            if (inputFile != null && inputFile.exists()) {
                MessageUtil.setNMProperties(msg,
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
                    FileMeta.NMPROP_INBOUND_DEL_ON_READ
                },
                new String[]{
                    fileDir != null ? fileDir : "",
                    inputFile.getName() != null ? inputFile.getName() : "",
                    address.getRecursive() != null ? address.getRecursive().toString() : "",
                    address.getExcludeRegex() != null ? address.getExcludeRegex() : "",
                    address.getRelativePath() != null ? address.getRelativePath().toString() : "false",
                    address.getPathRelativeTo() != null ? address.getPathRelativeTo() : "",
                    address.getPersistenceBaseLoc() != null ? address.getPersistenceBaseLoc() : "",
                    address.getLockName() != null ? address.getLockName() : "",
                    "", /* filled up later when lock is needed */
                    address.getWorkArea(),
                    "", /* filled up later - working dir */
                    "NA", /* filled up later - file in procesing - no inbound staging for on demand read */
                    fileMessage.getArchive() != null ? fileMessage.getArchive().toString() : "true",
                    fileMessage.getArchiveDirIsRelative() != null ? fileMessage.getArchiveDirIsRelative().toString() : "false",
                    fileMessage.getArchiveDirectory() != null ? fileMessage.getArchiveDirectory() : "",
                    "", /* file processed - archived file path - fabricated */
                    "NA", /* file in error, NA for on demand read */
                    fileMessage.isDeleteFileOnRead() ? "true" : "false"
                });
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "On-Demand Read: Found file [" + inputFile.getName() + "] in directory [" + (inputFile.getParentFile() != null ? inputFile.getParentFile().getCanonicalPath() : "") + "]...");
                }
                /**
                 *
                 * (3) do inbound staging if deleteOnRead or archive is true - indicate that inputFile is
                 * read as a message (not a configuration)
                 *
                 */
                workBaseDir = new File(cLock.getLockFilePath()).getParentFile();
                if (fileMessage.getArchive() || fileMessage.isDeleteFileOnRead()) {
                    workArea = new File(workBaseDir, address.getWorkArea());
                    workArea.mkdirs();
                    String errmsg = null;
                    if (workArea.exists()) {
                        if (!workArea.isDirectory()) {
                            errmsg = "Error creating work area [" + workArea.getPath() + "], mkdirs() return false and [" + workArea.getPath() + "] is not directory.";
                            if (mLogger.isLoggable(Level.SEVERE)) {
                                mLogger.log(Level.SEVERE, errmsg);
                            }
                            throw new Exception(errmsg);
                        }
                    } else {
                        errmsg = "Error creating work area [" + workArea.getPath() + "], mkdirs() return false and [" + workArea.getPath() + "] does not exist.";
                        if (mLogger.isLoggable(Level.SEVERE)) {
                            mLogger.log(Level.SEVERE, errmsg);
                        }
                        throw new Exception(errmsg);
                    }
                    targetFile = new File(workArea, FileNamePatternUtil.replaceUUID(inputFile.getName()));
                    if (!inputFile.renameTo(targetFile)) {
                        errmsg = "Error moving target file [" + inputFile.getCanonicalPath() + "] to work area [" + targetFile.getPath() + "], rename() return false.";
                        if (mLogger.isLoggable(Level.SEVERE)) {
                            mLogger.log(Level.SEVERE, errmsg);
                        }
                        throw new Exception(errmsg);
                    }
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Target file [" + inputFile.getCanonicalPath() + "] staged, about to read staged file [" + targetFile.getCanonicalPath() + "]");
                    }
                } else {
                    // read the target as configuration
                    targetFile = inputFile;
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "On-Demand Read: About to read file [" + inputFile.getName() + "] in directory [" + (inputFile.getParentFile() != null ? inputFile.getParentFile().getCanonicalPath() : "") + "]");
                    }
                }

                streamHandler = targetFile != null ? new FileStreamHandler(new FileInputStream(targetFile), removeEOL, recordDelim, maxBytesPerRecord, targetFile.length()) : null;
                uuid4Archive = fileMessage.getArchive().booleanValue() ? UUID.randomUUID().toString() : null;

                try {
                    msg.setProperty(FileMeta.NMPROP_INBOUND_WORK_AREA_DIR, workBaseDir.getCanonicalPath());
                    msg.setProperty(FileMeta.NMPROP_INBOUND_ARCHIVED_FILE,
                            FileUtil.fabricateFilePath(workBaseDir, fileMessage.getArchiveDirectory(), inputFile.getName() + uuid4Archive + mMessages.getString("processed")));
                } catch (Exception e) {
                    // ignore
                }
            } else {
                MessageUtil.setNMProperties(msg,
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
                    FileMeta.NMPROP_INBOUND_DEL_ON_READ
                },
                new String[]{
                    fileDir != null ? fileDir : "",
                    "",
                    address.getRecursive() != null ? address.getRecursive().toString() : "",
                    address.getExcludeRegex() != null ? address.getExcludeRegex() : "",
                    address.getRelativePath() != null ? address.getRelativePath().toString() : "false",
                    address.getPathRelativeTo() != null ? address.getPathRelativeTo() : "",
                    address.getPersistenceBaseLoc() != null ? address.getPersistenceBaseLoc() : "",
                    address.getLockName() != null ? address.getLockName() : "",
                    "", /* filled up later when lock is needed */
                    address.getWorkArea(),
                    "", /* filled up later - working dir */
                    "NA", /* filled up later - file in procesing - no inbound staging for on demand read */
                    fileMessage.getArchive() != null ? fileMessage.getArchive().toString() : "true",
                    fileMessage.getArchiveDirIsRelative() != null ? fileMessage.getArchiveDirIsRelative().toString() : "false",
                    fileMessage.getArchiveDirectory() != null ? fileMessage.getArchiveDirectory() : "",
                    "", /* file processed - archived file path - fabricated */
                    "NA", /* file in error, NA for on demand read */
                    fileMessage.isDeleteFileOnRead() ? "true" : "false"
                });
                // if message is not found, send an empty payload;
                // the scan does not find any matching file
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "On-Demand Read: No file entry found given fileName = [" + fileName + "] and fileDirectory = [" + fileDir + "], empty payload will be used in a normalized JBI message.");
                }
            }

            /**
             *
             * (4) unlock the lock - because the input file is already in staging area - will not be scanned by
             * another on-demand reader, or if it is a configuration, there is no need to lock
             *
             */
            if (fileLock != null) {
                fileLock.release();
                fileLock = null;
            }

            if (reentrantLock != null) {
                reentrantLock.unlock();
                reentrantLock = null;
            }

            try {
                /**
                 *
                 * (5) read file content, normalize payload into JBI message and send into NMR
                 *
                 */
                if (!processSyncRead(endpoint, inOutME, fileMessage, streamHandler, targetFile)) {
                    errString = mMessages.getString("FILEBC-E00715.File_not_found_or_empty", targetFile.getAbsolutePath());
                    throw new Exception(errString);
                }
            } finally {
                try {
                    if (!streamHandler.isDonotClose()) {
                        streamHandler.close();
                    }
                } catch (Throwable t) {
                    // ignore
                    }
            }

            /**
             *
             * (6) archive or delete the target if such configured
             *
             */
            if (fileMessage.getArchive().booleanValue()) {
                if (targetFile != null && targetFile.exists()) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "On-Demand Read: After read file = [" + inputFile.getPath() + "] about to archive the staged input file [" + targetFile.getCanonicalPath() + "]...");
                    }
                    doArchive(endpoint, fileMessage, targetFile, workBaseDir, uuid4Archive);
                } else if (targetFile != null) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "On-Demand Read: Can not locate staged file = [" + targetFile.getPath() + "] when attempt to archive it...");
                    }
                } else {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "On-Demand Read: Staged file is NULL when attempt to archive it...");
                    }
                }
            } else if (fileMessage.isDeleteFileOnRead()) {
                if (targetFile != null && targetFile.exists()) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "On-Demand Read: Perform deleting staged target file = [" + targetFile.getPath() + "]");
                    }
                    boolean delOK = targetFile.delete();
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "On-Demand Read: Perform deleting staged target file = [" + targetFile.getPath() + "], delete return = " + delOK);
                    }
                } else if (targetFile != null) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "On-Demand Read: Can not locate staged file = [" + targetFile.getPath() + "] when attempt to delete it...");
                    }
                } else {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "On-Demand Read: Staged file is NULL when attempt to delete it...");
                    }
                }
            } else {
                // read the input as configuration
                // no need to do either DELETE or ARCHIVE
            }
        } finally {
            // unlock here if read file to be deleted or archived
            //
            // comment out the condition, need to sync always
            //
            //if (fileMessage.isDeleteFileOnRead()) {
            if (fileLock != null) {
                fileLock.release();
            }

            if (reentrantLock != null) {
                reentrantLock.unlock();
            }
            //}
        }
    }

    private void doArchive(Endpoint endpoint, FileMessage fileMessage, File targetFile, File workBaseDir, String uuid4Archive) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Perform archiving target = [" + targetFile.getPath() + "]");
        }
        String archiveDir = fileMessage.getArchiveDirectory();
        if (archiveDir != null && archiveDir.trim().length() > 0) {
            String targetFileName = targetFile.getName();

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Archive dir = [" + archiveDir + "]");
            }

            File archiveDirObj = fileMessage.getArchiveDirIsRelative().booleanValue() ? new File(workBaseDir.getAbsolutePath(), archiveDir) : new File(archiveDir);

            if (!archiveDirObj.mkdirs()) {
                if (!archiveDirObj.exists()) {
                    throw new Exception(mMessages.getString("FILEBC-E00725.IMP_Failed_archiving_file_create_archive_dir", new Object[]{archiveDirObj.getAbsolutePath()}));
                }
            } else if (!archiveDirObj.isDirectory()) {
                throw new Exception(mMessages.getString("FILEBC-E00726.IMP_Archive_dir_not_dir_type", new Object[]{archiveDirObj.getAbsolutePath()}));
            }

            if (targetFile.exists()) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Moving input file = [" + targetFile.getPath() + "] to [" + archiveDirObj.getPath() + "]");
                }
                File dest = new File(archiveDirObj, targetFileName + uuid4Archive + mMessages.getString("processed"));
                if (!targetFile.renameTo(dest)) {
                    // move failed
                    mLogger.log(Level.SEVERE, mMessages.getString("FILEBC-E00727.Archive_rename_failed", new Object[]{targetFile.getAbsolutePath(), dest.getAbsolutePath()}));
                    AlertsUtil.getAlerter().critical(
                            mMessages.getString("FILEBC-E00727.Archive_rename_failed", new Object[]{targetFile.getAbsolutePath(), dest.getAbsolutePath()}),
                            FileBindingLifeCycle.SHORT_DISPLAY_NAME, endpoint.getServiceUnitID(), AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "FILEBC-E00727");
                }
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Archive input file, [" + targetFile.getPath() + "] does not exists...");
                }
            }
        } else {
            // error - archive directory must not be empty or unspecified
            throw new Exception(mMessages.getString("FILEBC-E00728.IMP_Failed_archive_file_null_or_empty_archive_dir"));
        }
    }

    /**
     * Read the file, create a normalized message from the payload, send it into NMR.
     * 
     * @param endpoint - the endpoint representing this on-demand read service
     * @param inOutME
     * @param fileMessage
     * @param streamHandler
     * @param targetFile - the file to be read - it can be the original input file or the staged file
     * @return - true file is read and send into NMR, false otherwise.
     * @throws MessagingException
     * @throws org.xml.sax.SAXException
     * @throws java.io.IOException
     * @throws java.lang.Exception
     */
    private boolean processSyncRead(Endpoint endpoint, InOut inOutME, FileMessage fileMessage,
            FileStreamHandler streamHandler, File targetFile) throws MessagingException,
            SAXException, IOException, Exception {
        MessageExchange exchange = inOutME;

        Exception err = null;
        try {
            String endPointID = (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) ? (createConsumingEndpointIdentifier(
                    endpoint.getServiceName(), endpoint.getEndpointName()))
                    : (createProvisioningEndpointIdentifier(endpoint.getServiceName(), endpoint.getEndpointName()));

            Probe normalizationMeasurement = Probe.info(getClass(), endPointID,
                    FileBindingLifeCycle.PERF_CAT_NORMALIZATION);

            NormalizedMessage outMsg = new FileNormalizer().normalize(exchange,
                    inOutME.getOperation(),
                    endpoint, fileMessage, streamHandler, true);

            if (normalizationMeasurement != null) {
                normalizationMeasurement.end();
            }

            if (outMsg == null) {
                // looking into FileNormalizer().normalize()
                // it seems always return a NormalizedMessage instance
                // so this code won't be reached
                if (mLogger.isLoggable(Level.FINE)) {
                    if (targetFile != null) {
                        mLogger.log(Level.FINE, "No content in file [" + targetFile.getPath() + "]");
                    } else {
                        mLogger.log(Level.FINE, "File to be read does not exist.");
                    }
                }
                return false;
            }

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "About to send the payload in file [" + (targetFile != null ? targetFile.getName() : "") + " ] to the invoking component...");
            }
            // if the target does not exists - then outMsg will be a JBI message wrapper without a real message in it
            inOutME.setOutMessage(outMsg);
            mChannel.send(inOutME);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Successfully sent the message exchange containing the payload in file [" + (targetFile != null ? targetFile.getPath() : "") + "].");
            }

            endpoint.getEndpointStatus().incrementSentReplies();

            //save message exchange
            if (streamHandler != null && streamHandler.isDonotClose()) {
                PendingExchangeManager.PendingExchangeHolder holder = new PendingExchangeManager.PendingExchangeHolder();
                holder.fileStreamHandler = streamHandler;
                PendingExchangeManager.getInstance().saveExchangeHolder(exchange, holder);
            }
        } catch (MessagingException ex) {
            err = ex;
        } catch (SAXException ex) {
            err = ex;
        } catch (IOException ex) {
            err = ex;
        } catch (Exception ex) {
            err = ex;
        }
        if (err != null) {
            throw err;
        }

        return true;
    }

    public void processRequestReplyInbound(InOut inout, Endpoint endpoint, QName operation, MessageExchangeReplyListener listener) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "OMP_Processing_inout_inbound");
        }

        boolean isRetry = isRedeliveryConfigured(inout);

        // If redelivery is configured and when a message exchange exhausts its
        // redelivery attempts,
        // redelivery library may re-route the exchange to a different endpoint.
        // That means the endpoint associated with the exchange could be
        // changed.
        // We need to call Redelivery.getEndpoint(exchange) to get the original
        // endpoint back.
        // The following if block tries to locate the inbound endpoint using the
        // above redelivery utility
        // when the first attempt to get it based on the endpoint associated
        // with the exchange fails.
        if (isRetry && endpoint == null) {
            endpoint = getInboundEndpoint(inout, true);
        }

        if (inout.getStatus() == ExchangeStatus.DONE) {
            endpoint.getEndpointStatus().incrementReceivedDones();

            if (isRetry) {
                MessageExchangeSupport.removeRedeliveryListener(inout.getExchangeId());
            }

        } else if (inout.getStatus() == ExchangeStatus.ERROR) {

            String msg = mMessages.getString("FILEBC-W00709.OMP_RequestReplyInbound_Returned_ERROR",
                    new Object[]{inout.getEndpoint().getServiceName(),
                        inout.getEndpoint().getEndpointName(),
                        inout.getOperation()});

            mLogger.log(Level.WARNING, msg);

            endpoint.getEndpointStatus().incrementReceivedErrors();

            sendAlerts(inout, endpoint);

            checkAndDoRedelivery(inout, endpoint, listener);

        } else {
            if (isRetry) {
                MessageExchangeSupport.removeRedeliveryListener(inout.getExchangeId());
            }
            endpoint.getEndpointStatus().incrementReceivedReplies();
            try {

                NormalizedMessage inMsg = inout.getOutMessage();

                // The inMsg might be null, e.g. in case when the invoked service throws a fault
                // instead (a BPEL doing a fault <reply>). We should account for such situations.
                if (inMsg == null) {
                    String msg = mMessages.getString(
                            "FILEBC-W00793.Message_from_nmr_null_trying_fault");
                    mLogger.log(Level.WARNING, msg);

                    inMsg = inout.getFault();
                }

                boolean success = true;

                FileOperation fileOperation = locateFileOperation(operation, endpoint);
                FileOutput fileOutput = fileOperation.getFileOperationOutput();
                try {
                    validateRequestReplyInboundMessageExchangeProperties(fileOperation, operation);
                } catch (Exception e) {
                    String msg = mMessages.getString("FILEBC-W00707.Invalid_Properties",
                            new Object[]{endpoint.getServiceName(),
                                endpoint.getEndpointName(),
                                operation.toString(),
                                e.getLocalizedMessage()});
                    mLogger.log(Level.SEVERE, msg, e);
                    endpoint.getEndpointStatus().incrementSentErrors();
                    setError(inout, e, msg, MessageUtil.FAULTCODE_SERVER, "");
                    mChannel.send(inout);
                    return;
                }

                FileMessage fileMessage = fileOutput.getFileMessage();
                try {
                    writeMessage(inMsg, endpoint, operation, fileMessage);
                } catch (Exception e) {
                    String msg = mMessages.getString("FILEBC-W00702.Message_requestreply_outbound_failed_exception",
                            new Object[]{endpoint.getServiceName(),
                                endpoint.getEndpointName(),
                                operation.toString(),
                                e.getLocalizedMessage()});
                    mLogger.log(Level.WARNING, msg, e);
                    AlertsUtil.getAlerter().warning(msg,
                            FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                            endpoint.getServiceUnitID(),
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "FILEBC-W00702");
                    setError(inout, e, msg, MessageUtil.FAULTCODE_SERVER, "");
                    success = false;
                } finally {

                    if (success) {
                        if (inMsg != null && ("true".equals(inMsg.getProperty(FileMeta.NMPROP_INBOUND_LASTRECORD)))) {
                            PendingExchangeHolder holder = PendingExchangeManager.getInstance().getExchangeHolder(inout);
                            if (holder != null) {
                                holder.fileStreamHandler.close();
                            }
                        }
                        inout.setStatus(ExchangeStatus.DONE);
                        endpoint.getEndpointStatus().incrementSentDones();
                    } else {
                        inout.setStatus(ExchangeStatus.ERROR);
                        endpoint.getEndpointStatus().incrementSentErrors();
                        //Close the input stream if any available
                        PendingExchangeHolder holder = PendingExchangeManager.getInstance().getExchangeHolder(inout);
                        if (holder != null) {
                            holder.fileStreamHandler.close();
                        }
                    }
                    //close the stream
                    PendingExchangeHolder holder = PendingExchangeManager.getInstance().getExchangeHolder(inout);
                    if (holder != null && holder.fileStreamHandler.isDonotClose()) {
                        holder.fileStreamHandler.close();
                    }
                }
                listener.processReplyMessage(inout);
                mChannel.send(inout);
            } catch (Exception ex) {
                mLogger.log(Level.WARNING,
                        mMessages.getString("FILEBC-W00703.Message_requestreply_inbound_failed_exception",
                        ex.getLocalizedMessage()),
                        ex);
                AlertsUtil.getAlerter().warning(mMessages.getString("FILEBC-W00703.Message_requestreply_inbound_failed_exception",
                        ex.getLocalizedMessage()),
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        endpoint.getServiceUnitID(),
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-W00703");
            }
        }
    }

    public void processOneWayOutbound(InOnly inonly, Endpoint endpoint, QName operation) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "OMP_Processing_oneway_outbound");
        }

        try {
            endpoint.getEndpointStatus().incrementReceivedRequests();
            NormalizedMessage inMsg = inonly.getInMessage();
            boolean success = true;

            FileOperation fileOperation = locateFileOperation(operation, endpoint);
            FileInput fileInput = fileOperation.getFileOperationInput();

            try {
                validateOutboundMessageExchangeProperties(fileOperation, operation);
            } catch (Exception e) {
                String msg = mMessages.getString("FILEBC-W00707.Invalid_Properties",
                        new Object[]{endpoint.getServiceName(),
                            endpoint.getEndpointName(),
                            operation.toString(),
                            e.getLocalizedMessage()});
                mLogger.log(Level.SEVERE, msg, e);
                endpoint.getEndpointStatus().incrementSentErrors();
                setError(inonly, e, msg, MessageUtil.FAULTCODE_SERVER, "");
                mChannel.send(inonly);
                return;
            }

            FileMessage fileMessage = fileInput.getFileMessage();
            try {
                writeMessage(inMsg, endpoint, operation, fileMessage);
                inonly.setStatus(ExchangeStatus.DONE);
            } catch (Exception e) {
                success = false;
                String msg = mMessages.getString("FILEBC-W00701.Message_write_failed_exception",
                        new Object[]{endpoint.getServiceName(),
                            endpoint.getEndpointName(),
                            operation.toString(),
                            e.getLocalizedMessage()});
                mLogger.log(Level.WARNING, msg, e);
                AlertsUtil.getAlerter().warning(msg,
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        endpoint.getServiceUnitID(),
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-W00701");
                setError(inonly, e, msg, MessageUtil.FAULTCODE_SERVER, "");

            }

            mChannel.send(inonly);
            if (success) {
                endpoint.getEndpointStatus().incrementSentDones();
            } else {
                endpoint.getEndpointStatus().incrementSentErrors();
            }
        } catch (Exception ex) {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING,
                        mMessages.getString("FILEBC-W00704.Message_onway_outbound_failed_exception",
                        ex.getLocalizedMessage()),
                        ex);
            }
        }
    }

    public void processOneWayInbound(InOnly inonly, Endpoint endpoint, MessageExchangeReplyListener listener) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "OMP_Processing_oneway_inbound");
        }

        boolean isRetry = isRedeliveryConfigured(inonly);

        // If redelivery is configured and when a message exchange exhausts its redelivery attempts, 
        // redelivery library may re-route the exchange to a different endpoint.
        // That means the endpoint associated with the exchange could be changed.
        // We need to call Redelivery.getEndpoint(exchange) to get the original endpoint back.
        // The following if block tries to locate the inbound endpoint using the above redelivery utility
        // when the first attempt to get it based on the endpoint associated with the exchange fails.
        if (isRetry && endpoint == null) {
            endpoint = getInboundEndpoint(inonly, true);
        }

        if (inonly.getStatus() == ExchangeStatus.DONE) {
            endpoint.getEndpointStatus().incrementReceivedDones();
            if (isRetry) {
                MessageExchangeSupport.removeRedeliveryListener(inonly.getExchangeId());
            }


            try {
                listener.processReplyMessage(inonly);
            } catch (Exception ex) {
                mLogger.log(Level.WARNING,
                        mMessages.getString("FILEBC-W00706.Message_onway_inbound_failed_exception",
                        ex.getLocalizedMessage()), ex);
            }


        } else if (inonly.getStatus() == ExchangeStatus.ERROR) {

            String msg = mMessages.getString("FILEBC-W00708.OMP_OneWayInbound_Returned_ERROR",
                    new Object[]{inonly.getEndpoint().getServiceName(),
                        inonly.getEndpoint().getEndpointName(),
                        inonly.getOperation()});

            mLogger.log(Level.WARNING, msg);

            endpoint.getEndpointStatus().incrementReceivedErrors();

            sendAlerts(inonly, endpoint);

            checkAndDoRedelivery(inonly, endpoint, listener);

        } else {
            mLogger.log(Level.WARNING, "FILEBC-W00705.Message_exchange_status_unexpected",
                    new Object[]{inonly.getExchangeId(), inonly.getStatus().toString()});
        }
    }

    private void setError(MessageExchange me, Throwable e, String msg,
            String faultCode, String faultDetail) throws MessagingException {

        me.setStatus(ExchangeStatus.ERROR);
        me.setError(new Exception(msg, e));
        me.setProperty(MessageUtil.PROP_FAULTCODE, faultCode);
        me.setProperty(MessageUtil.PROP_FAULTSTRING, msg);
        me.setProperty(MessageUtil.PROP_FAULTACTOR, FileBindingLifeCycle.SHORT_DISPLAY_NAME);
        me.setProperty(MessageUtil.PROP_FAULTDETAIL, faultDetail);
    }

    private void finishInputMessageProcessing(MessageExchange me, MessageExchangeReplyListener listener) {
        try {
            listener.processReplyMessage(me);
        } catch (Exception ex) {
            mLogger.log(Level.WARNING,
                    mMessages.getString("FILEBC-W00706.Message_onway_inbound_failed_exception",
                    ex.getLocalizedMessage()),
                    ex);
        }
    }

    private void checkAndDoRedelivery(MessageExchange me, Endpoint endpoint, MessageExchangeReplyListener listener) {

        RedeliveryConfig retryConfig = endpoint.getRedeliveryConfiguration();
        if (retryConfig == null || (retryConfig != null && !(retryConfig.getMaxRetries() > 0))) {
            finishInputMessageProcessing(me, listener);
            return;
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Redelivery is configured for file endpoint with service name [" + endpoint.getServiceName() + "] and endpoint name [" +
                    endpoint.getEndpointName() + "]");
        }

        RedeliveryStatus retryStatus = Redelivery.getRedeliveryStatus(me);

        if (retryStatus != null) {
            // Redelivery.getRedeliveryStatus(me) depends on property on me to set the failed flag to true/false
            // in qos.jar this is done be checking attempt done and the remaining
            // for file bc, it needs to be done by filebc itself since filebc does not call out of box
            // redelivery

            if (retryStatus.getRemainingRetries() > 0) {

                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Retry attempts have not been exhausted, about to resend the message...");
                }
                handleRedelivery(me, endpoint);
                return;
            } else {
                // Re-delivery has failed or exhausted.
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Max retry attempts exhausted, there will be no more retries. File BC will proceed with the configured on-failure recourse action now");
                }
                // move input file to error.
                finishInputMessageProcessing(me, listener);

                // Handle Re-delivery failure options
                Failure onFailureOption = retryConfig.getFailure();
                if (onFailureOption == Failure.suspend) {
                    // suspend the inbound endpoint
                    try {
                        FileBCManagementMBean managementMBean =
                                FileComponentContext.getInstance().getManagementMBean();

                        managementMBean.suspend(endpoint.getUniqueName());

                        // emit warning logs and send alerts 
                        String msg = mMessages.getString("FILEBC-E00765.About_to_suspend_endpoint",
                                new Object[]{
                                    String.valueOf(endpoint.getServiceName()),
                                    endpoint.getEndpointName()
                                });
                        mLogger.log(Level.WARNING, msg);
                        AlertsUtil.getAlerter().warning(msg,
                                FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                                endpoint.getServiceUnitID(),
                                AlertsUtil.getServerType(),
                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                NotificationEvent.EVENT_TYPE_ALERT,
                                "FILEBC-E00765");
                    } catch (MBeanException e) {
                        String errorMsg = e.getTargetException().getMessage();
                        if (errorMsg != null) {
                            String msg = mMessages.getString("FILEBC-E00766.Failed_to_suspend_endpoint",
                                    new Object[]{
                                        String.valueOf(endpoint.getServiceName()),
                                        endpoint.getEndpointName(),
                                        errorMsg
                                    });
                            mLogger.log(Level.SEVERE, msg);
                            AlertsUtil.getAlerter().warning(msg,
                                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                                    endpoint.getServiceUnitID(),
                                    AlertsUtil.getServerType(),
                                    AlertsUtil.COMPONENT_TYPE_BINDING,
                                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                    NotificationEvent.EVENT_TYPE_ALERT,
                                    "FILEBC-E00766");
                        }
                    }
                }
            }
        }

    }

    private void sendAlerts(MessageExchange me, Endpoint endpoint) {
        // send alerts
        String errorMsg = (me.getError() != null) ? me.getError().getMessage() : null;
        if (errorMsg != null) {
            String msg = mMessages.getString("FILEBC-E00767.Message_exchange_error",
                    new Object[]{
                        String.valueOf(endpoint.getServiceName()),
                        endpoint.getEndpointName(),
                        errorMsg
                    });
            mLogger.log(Level.SEVERE, msg);
            AlertsUtil.getAlerter().warning(msg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    endpoint.getServiceUnitID(),
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00767");
        } else {
            String msg = mMessages.getString("FILEBC-E00768.Message_exchange_error_no_detail",
                    new Object[]{
                        String.valueOf(endpoint.getServiceName()),
                        endpoint.getEndpointName()
                    });
            mLogger.log(Level.SEVERE, msg);
            AlertsUtil.getAlerter().warning(msg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    endpoint.getServiceUnitID(),
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00768");
        }


    }

    public Endpoint findEndpoint(MessageExchange msgExchange) {
        Endpoint endpoint = null;
        for (Iterator it = mServiceUnits.values().iterator(); it.hasNext();) {
            ServiceUnit su = (ServiceUnit) it.next();
            for (Iterator it2 = su.getEndpoints().iterator(); it2.hasNext();) {
                Endpoint aEndPoint = (Endpoint) it2.next();
                QName serviceName = msgExchange.getEndpoint().getServiceName();
                String endpointName = msgExchange.getEndpoint().getEndpointName();

                if (aEndPoint.getServiceName().equals(serviceName) &&
                        aEndPoint.getEndpointName().equals(endpointName)) {
                    endpoint = aEndPoint;
                    endpoint.setServiceUnitID(su.getServiceUnitId());
                }
            }
        }

        return endpoint;
    }

    public Endpoint findEndpoint(QName serviceName, String endpointName) {
        Endpoint endpoint = null;
        for (Iterator it = mServiceUnits.values().iterator(); it.hasNext();) {
            ServiceUnit su = (ServiceUnit) it.next();
            for (Iterator it2 = su.getEndpoints().iterator();
                    it2.hasNext();) {
                Endpoint aEndPoint = (Endpoint) it2.next();
                if (aEndPoint.getServiceName().equals(serviceName) &&
                        aEndPoint.getEndpointName().equals(endpointName)) {
                    endpoint = aEndPoint;
                    endpoint.setServiceUnitID(su.getServiceUnitId());
                }
            }
        }

        return endpoint;
    }

    private FileMessage mergeFileMessageProperties(FileMessage fileMessage,
            NormalizedMessage msg) throws CloneNotSupportedException {

        FileMessage newFileMessage = fileMessage.clone();
        String fileName = (String) msg.getProperty(FileMeta.NMPROP_OUTBOUND_FILENAME);
        if (fileName != null && (fileName.trim().length() > 0)) {
            newFileMessage.setFileName(fileName);
        }

        String dataType = (String) msg.getProperty(FileMeta.NMPROP_OUTBOUND_DATATYPE);
        if (dataType != null && (dataType.trim().length() > 0)) {
            newFileMessage.setFileType(dataType);
        }

        String strAddEol = (String) msg.getProperty(FileMeta.NMPROP_OUTBOUND_ADDEOL);
        if (strAddEol != null && (strAddEol.trim().length() > 0)) {
            Boolean addEol = Boolean.valueOf(strAddEol);
            newFileMessage.setAddEOL(addEol);
        }

        String strAppend = (String) msg.getProperty(FileMeta.NMPROP_OUTBOUND_APPEND);
        if (strAppend != null && (strAppend.trim().length() > 0)) {
            Boolean append = Boolean.valueOf(strAppend);
            newFileMessage.setMultipleRecordsPerFile(append);
        }

        String delim = (String) msg.getProperty(FileMeta.NMPROP_OUTBOUND_APPENDDELIMITER);
        if (delim != null && (delim.trim().length() > 0)) {
            newFileMessage.setRecordDelimiter(delim);
        }

        String strProtect = (String) msg.getProperty(FileMeta.NMPROP_OUTBOUND_OVERWRITE);
        if (strProtect != null && (strProtect.trim().length() > 0)) {
            Boolean protect = Boolean.valueOf(strProtect);
            newFileMessage.setProtect(protect);
        }

        return newFileMessage;
    }

    private FileAddress mergeFileAddressProperties(FileAddress fileAddress,
            NormalizedMessage msg) throws CloneNotSupportedException {

        FileAddress newFileAddress = fileAddress.clone();
        String tmp = (String) msg.getProperty(FileMeta.NMPROP_OUTBOUND_DIRRELATIVETO);
        if (tmp != null && (tmp.trim().length() > 0)) {
            newFileAddress.setPathRelativeTo(tmp);
        }

        tmp = (String) msg.getProperty(FileMeta.NMPROP_OUTBOUND_FILEDIR);
        if (tmp != null && (tmp.trim().length() > 0)) {
            newFileAddress.setFileDirectory(tmp);
        }

        tmp = (String) msg.getProperty(FileMeta.NMPROP_OUTBOUND_ONDEMAND_READ_RECURSIVE);
        if (tmp != null && (tmp.trim().length() > 0)) {
            newFileAddress.setRecursive(new Boolean(tmp.trim().equalsIgnoreCase("TRUE")));
        }

        tmp = (String) msg.getProperty(FileMeta.NMPROP_OUTBOUND_ONDEMAND_READ_EXCLUDE_REGEX);
        if (tmp != null && (tmp.trim().length() > 0)) {
            newFileAddress.setExcludeRegex(tmp.trim());
        }

        return newFileAddress;
    }

    /**
     * Write the message to a given file destination
     */
    void writeMessage(NormalizedMessage msg,
            Endpoint endpoint,
            QName operation,
            FileMessage fileMessage) throws Exception {
        if (msg == null) {
            throw new Exception(mMessages.getString("FILEBC-E00744.Message_from_nmr_null"));
        }
        // update fileMessage with NM properties
        fileMessage = mergeFileMessageProperties(fileMessage, msg);

        File outFile = null;
        // update fileAddress with NM properties
        FileAddress address = mergeFileAddressProperties(endpoint.getFileAddress(), msg);

        boolean relativePath = (address.getRelativePath() != null) ? address.getRelativePath().booleanValue() : false;
        String rootPath = address.getPathRelativeTo();
        String fileDir = (relativePath && rootPath != null) ? (rootPath + File.separator + address.getFileDirectory()) : address.getFileDirectory();
        String fileName = fileMessage.getFileName();

        if (fileName == null || fileName.equals("")) {
            throw new Exception(mMessages.getString("OMP_Invalid_FileName"));
        }

        String lineSeparator = System.getProperty("line.separator", "\n");
        String recordDelimiter = fileMessage.getRecordDelimiter();
        boolean toAppend = (fileMessage.getMultipleRecordsPerFile() != null) ? fileMessage.getMultipleRecordsPerFile().booleanValue() : false;
        boolean addEOL = (fileMessage.getAddEOL() != null) ? fileMessage.getAddEOL().booleanValue() : false;
        boolean isPattern = (fileMessage.getFileNameIsPattern() != null) ? fileMessage.getFileNameIsPattern().booleanValue() : false;
        boolean hasGUID = FileNamePatternUtil.isGUIDPatterned(fileName);
        boolean hasPersistedSeq = FileNamePatternUtil.isPersistedSeqPatterned(fileName);
        String endpointKey = endpoint.getEPUUID();
        String workArea = endpoint.getWorkAreaDir();

        // need to enforce overwrite protect if it is enabled
        // need to do staging if it is enabled
        if (isPattern) {
            // SU deployment validation will make sure that
            // no identical file write attributes are allowed

            // NOTE: append to a target that is not literal causes
            // problem (for file name that does not contain a UUID):
            //
            // concurrent outbound providers and clustered providers
            // provisioning to the same end point does not know
            // the resolved file to write to (note the writers can be running on different JVM
            // or even different host).
            //
            // also the write can be lost due to lack of locking
            // on the target file which multiple threads are writing to

            // two things need to be done:
            // (1) appending to pattern generated target file is invalid - use literal target file
            // (2) need thread lock and file lock to sync the concurrent write to the append target
            //
            fileName = toAppend ? OutputFilenameFormatter.getOutputFileName(fileName, endpointKey, new File(workArea, address.getSeqName()))
                    : OutputFilenameFormatter.getNextOutputFileName(fileName, endpointKey, new File(workArea, address.getSeqName()));
        }

        outFile = new File(fileDir, fileName);
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "OMP_destination_file", new Object[]{outFile.getAbsolutePath(), toAppend});
        }

        File destDir = null;
        File dest = null;
        FileLock fileLock = null;
        ReentrantLock reentrantLock = null;
        Lock cLock = null;
        String protectedFilePath = "";
        String stagedFilePath = "";
        Logger logger = null;

        try {
            //
            // open esb issue: #2411 : https://open-esb.dev.java.net/issues/show_bug.cgi?id=2411
            // actually, only when the target file name is UUID tagged
            // it is possible that more than one writer writes to same file
            // 
            // GUID tagged or persisted sequence tagged file name - assume that this is the only writer
            // no need to use any sync since there is no concurrent writers
            if (!isPattern || toAppend || (!hasGUID && !hasPersistedSeq)) {
                // get the per EP composite lock
                // and do the two level locking to sync
                // writer threads associated with the endpoint on this JVM
                // and sync writer threads across JVMs (e.g. in a clustered deployment)
                cLock = LockRegistry.get(endpoint.getEPUUID());
                reentrantLock = cLock.getLock();
                // sync OB threads on this JVM
                // for accessing the target file
                reentrantLock.lock();
                // sync threads across JVM (process) using physical file lock
                // for accessing the target file
                // blocking call
                fileLock = cLock.getFileChannel().lock();
            }

            File path = outFile.getParentFile();
            if (path != null) {
                path.mkdirs();
            }

            if (!toAppend) {
                // when it is NOT append, do protect
                if (outFile.exists() && fileMessage.getProtect()) {
                    // output target already exists, not multi-record,
                    // overwrite protect enabled - do overwrite protect:
                    // moving the outFile to protectDirectory and tag it with UUID
                    String protectDir = fileMessage.getProtectDirectory();
                    if (protectDir != null && protectDir.trim().length() > 0) {
                        destDir = fileMessage.getProtectDirIsRelative() ? new File(workArea, protectDir) : new File(protectDir);
                    } else {
                        // if protectDir is not specified, rename existing file in the same dir.
                        destDir = outFile.getParentFile();
                    }
                    // create protect area anyway
                    destDir.mkdirs();
                    if (!destDir.isDirectory()) {
                        // error - protect directory is not a directory
                        throw new Exception(mMessages.getString("FILEBC-E00747.OMP_Protect_dir_not_dir_type", new Object[]{destDir.getAbsolutePath()}));
                    }
                    dest = new File(destDir, outFile.getName() + UUID.randomUUID().toString() + ".protected");
                    if (!outFile.renameTo(dest)) {
                        throw new Exception(mMessages.getString("FILEBC-E00748.OMP_Failed_protect_file_rename", new Object[]{outFile.getAbsolutePath(),
                                    dest.getAbsolutePath()}));
                    }
                }
            }

            String endPointID = (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) ? createConsumingEndpointIdentifier(endpoint.getServiceName(), endpoint.getEndpointName()) : createProvisioningEndpointIdentifier(endpoint.getServiceName(), endpoint.getEndpointName());
            Probe deNormalizationMeasurement = Probe.info(getClass(), endPointID, FileBindingLifeCycle.PERF_CAT_DENORMALIZATION);

            // Push the context
            logger = Logger.getLogger("com.sun.EnterContext");
            logger.fine(endpoint.getServiceUnitID() + "-" + operation);

            InputStream in = mDenormalizer.denormalize(msg, operation, endpoint, fileMessage);
            deNormalizationMeasurement.end();
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "OMP_Writing_file", outFile);
            }

            if (toAppend) {
                // ignore staging
                outFile.createNewFile();
                FileOutputStream fos = new FileOutputStream(outFile, true);
                writeToOutputStream(in, fos);
                if (addEOL) {
                    fos.write(lineSeparator.getBytes());
                }
                if (toAppend && recordDelimiter != null) {
                    fos.write(recordDelimiter.getBytes());
                }
                fos.flush();
                fos.close();
            } else {
                if (fileMessage.getStage()) {
                    // staging enabled and it is NOT append
                    String stageDir = fileMessage.getStageDirectory();
                    if (stageDir != null && stageDir.trim().length() > 0) {
                        destDir = fileMessage.getStageDirIsRelative() ? new File(workArea, stageDir) : new File(stageDir);
                    } else {
                        throw new Exception(mMessages.getString("FILEBC-E00749.OMP_Failed_stage_file_null_or_empty_stage_dir"));
                    }
                    // create staging area anyway
                    destDir.mkdirs();
                    if (!destDir.isDirectory()) {
                        throw new Exception(mMessages.getString("FILEBC_E00751.OMP_stage_dir_not_dir_type", new Object[]{destDir.getAbsolutePath()}));
                    }

                    dest = new File(destDir, outFile.getName() + UUID.randomUUID().toString());
                    dumpOutput(dest, addEOL, lineSeparator, in);
                    stagedFilePath = dest.getCanonicalPath();

                    //this may happen if protect is false and staging is true
                    if (outFile.exists()) {
                        outFile.delete();
                    }
                    // publish to the real destination
                    if (!dest.renameTo(outFile)) {
                        throw new Exception(mMessages.getString("FILEBC_E00752.OMP_Failed_staging_file_rename", new Object[]{dest.getAbsolutePath(), outFile.getAbsolutePath()}));
                    }
                } else {
                    // write directly to the destination
                    dumpOutput(outFile, addEOL, lineSeparator, in);
                }
            }
        } finally {
            // Pop the context
            if (logger != null) {
                Logger.getLogger("com.sun.ExitContext").fine(endpoint.getServiceUnitID() + "-" + operation);
            }
            // populate the NM properties
            MessageUtil.setNMProperties(msg,
                    new String[]{
                        FileMeta.NMPROP_OUTBOUND_RELATIVE_PATH,
                        FileMeta.NMPROP_OUTBOUND_DIRRELATIVETO,
                        FileMeta.NMPROP_OUTBOUND_PERSIST_BASE_DIR,
                        FileMeta.NMPROP_OUTBOUND_LOCK_NAME,
                        FileMeta.NMPROP_OUTBOUND_LOCK_FILE,
                        FileMeta.NMPROP_OUTBOUND_SEQ_AREA,
                        FileMeta.NMPROP_OUTBOUND_PROTECT_ENABLED,
                        FileMeta.NMPROP_OUTBOUND_PROTECT_AREA_RELATIVE,
                        FileMeta.NMPROP_OUTBOUND_PROTECT_AREA,
                        FileMeta.NMPROP_OUTBOUND_PROTECTED_FILE,
                        FileMeta.NMPROP_OUTBOUND_STAGING_ENABLED,
                        FileMeta.NMPROP_OUTBOUND_STAGING_AREA_RELATIVE,
                        FileMeta.NMPROP_OUTBOUND_STAGING_AREA,
                        FileMeta.NMPROP_OUTBOUND_STAGED_FILE
                    },
                    new String[]{
                        address.getRelativePath().booleanValue() ? "true" : "false",
                        address.getPathRelativeTo() != null ? address.getPathRelativeTo() : "",
                        address.getPersistenceBaseLoc() != null ? address.getPersistenceBaseLoc() : "",
                        address.getLockName() != null ? address.getLockName() : "",
                        cLock != null ? (cLock.getLockFilePath() != null ? cLock.getLockFilePath() : "") : "",
                        address.getSeqName() != null ? address.getSeqName() : "",
                        fileMessage.getProtect().booleanValue() ? "true" : "false",
                        fileMessage.getProtectDirIsRelative().booleanValue() ? "true" : "false",
                        fileMessage.getProtectDirectory() != null ? fileMessage.getProtectDirectory() : "",
                        protectedFilePath,
                        fileMessage.getStage().booleanValue() ? "true" : "false",
                        fileMessage.getStageDirIsRelative().booleanValue() ? "true" : "false",
                        fileMessage.getStageDirectory() != null ? fileMessage.getProtectDirectory() : "",
                        stagedFilePath
                    });

            if (fileLock != null) {
                try {
                    fileLock.release();
                } catch (Exception ex) {
                    // ignore - best tried
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Exception caught when releasing file lock for outbound target: " + fileLock + ", destination file :" + outFile.getPath() + ", Exception: " + ex.getMessage());
                    }
                }
                fileLock = null;
            }
            if (reentrantLock != null) {
                try {
                    reentrantLock.unlock();
                } catch (Exception ex) {
                    // ignore - best tried
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Exception caught when unlocking re-entrant lock for outbound target: " + reentrantLock + ", destination file :" + outFile.getPath() + ", Exception: " + ex.getMessage());
                    }
                }
                reentrantLock = null;
            }
        }
    }

    private void handleRedelivery(MessageExchange exchange, Endpoint epb) {
        try {
            MessageExchangeSupport.notifyOfRedelivery(exchange);
        } catch (Exception e) {
            String groupId = (String) exchange.getProperty(FileComponentContext.CRMP_GROUP_ID);
            String messageId = (String) exchange.getProperty(FileComponentContext.CRMP_MESSAGE_ID);
            if (mLogger.isLoggable(Level.WARNING)) {
                String text = mMessages.getString("FILEBC-E00769.Failed_to_process_redelivery", new Object[]{groupId, messageId});
                mLogger.log(Level.WARNING, text, e);
                AlertsUtil.getAlerter().warning(text,
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        epb.getServiceUnitID(),
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-E00769");
            }
        }
    }

    /**
     * Get the endpoint bean for the inbound service
     * @param inboundExchange the message exchange to get the info for
     * @param isRetry indicates if redelivery QoS is configured for the endpoint
     * @return the endpoint bean corresponding to the inbound service for this
     * message exchange
     */
    Endpoint getInboundEndpoint(MessageExchange inboundExchange, boolean isRetry) {
        if (isRetry) {
            ServiceEndpoint actualEndpoint = Redelivery.getEndpoint(inboundExchange);
            QName serviceName = actualEndpoint.getServiceName();
            String endpointName = actualEndpoint.getEndpointName();
            if (mLogger.isLoggable(Level.FINER)) {
                mLogger.log(Level.FINER, "Getting inbound info for " + serviceName + " " + endpointName);
            }

            Endpoint epb = findEndpoint(serviceName, endpointName);
            return epb;
        }

        return null;
    }

    private boolean isRedeliveryConfigured(MessageExchange exchange) {
        RedeliveryStatus redeliveryStatus = Redelivery.getRedeliveryStatus(exchange);
        return (redeliveryStatus != null);
    }

    private void dumpOutput(File outFile, Boolean addEOL, String lineSeparator, InputStream inputStream) throws Exception {
        FileOutputStream out = new FileOutputStream(outFile);
        writeToOutputStream(inputStream, out);
        if (addEOL.booleanValue()) {
            out.write(lineSeparator.getBytes());
        }
        out.flush();
        out.close();
    }

    private void writeToOutputStream(InputStream inputStream, FileOutputStream out)
            throws IOException {
        if (inputStream == null) {
            return;
        }
        byte[] buffer;
        if (inputStream.available() > TWO_MB) {
            buffer = new byte[1024];
        } else {
            buffer = new byte[128];
        }
        int len;
        while ((len = inputStream.read(buffer)) != -1) {
            out.write(buffer, 0, len);
        }
    }

    private FileOperation locateFileOperation(QName opname, Endpoint endpoint) {
        PortType portType = getPortType(endpoint);
        return (FileOperation) endpoint.getFileOperations().get(
                new QName(portType.getQName().getNamespaceURI(), opname.getLocalPart()));
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

    protected void validateRequestReplyInboundMessageExchangeProperties(FileOperation operation, QName operationName) throws Exception {
        // Need to validate that file:write properties are available and valid
        FileOutput fileOutput = operation.getFileOperationOutput();

        /**
         *  By this time, the Output property should already been validated for the operation.
         *  But we will still validate here...
         */
        if (fileOutput == null) {
            throw new Exception(mMessages.getString("FILEBC_E00755.OMP_Invalid_No_InOut_FileOutput", operationName));
        }

        FileMessage fileMessage = fileOutput.getFileMessage();
        if (fileMessage == null) {
            throw new Exception(mMessages.getString("FILEBC_E00756.OMP_Invalid_No_InOut_FileMessage", operationName));
        }

        if (fileMessage.getFileName() == null) {
            throw new Exception(mMessages.getString("FILEBC_E00757.OMP_Invalid_No_InOut_FileMessage_Name", operationName));
        }

        if (!fileMessage.getFileType().equals(FileMessage.FILE_TYPE_TEXT) &&
                !fileMessage.getFileType().equals(FileMessage.FILE_TYPE_BINARY)) {
            throw new Exception(mMessages.getString("FILEBC_E00758.OMP_Invalid_InOut_File_Type", new Object[]{fileMessage.getFileType(), operationName}));
        }

        if (fileMessage.getFileUseType().equals(FileMessage.FILE_USE_TYPE_ENCODED) &&
                (fileMessage.getFileEncodingStyle() == null ||
                fileMessage.getFileEncodingStyle().equals(""))) {
            throw new Exception(mMessages.getString("FILEBC_E00759.OMP_Invalid_No_InOut_FileMessage_EncodingStyle", operationName));
        }
    }

    protected void validateOutboundMessageExchangeProperties(FileOperation operation, QName operationName) throws Exception {

        // Solicited read has no file:message to validate
        String verb = operation.getVerb();
        if (FileOperation.VERB_READ.equals(verb)) {
            return;
        }

        // Need to validate that file:write properties are available and valid
        FileInput fileInput = operation.getFileOperationInput();

        /**
         *  By this time, the Input property should already been validated for the operation.
         *  But we will still validate here...
         */
        if (fileInput == null) {
            throw new Exception(mMessages.getString("FILEBC_E00760.OMP_Invalid_No_Out_FileInput", operationName));
        }


        FileMessage fileMessage = fileInput.getFileMessage();
        if (fileMessage == null) {
            throw new Exception(mMessages.getString("FILEBC_E00761.OMP_Invalid_No_Out_FileMessage", operationName));
        }

        if (fileMessage.getFileName() == null) {
            throw new Exception(mMessages.getString("FILEBC_E00762.OMP_Invalid_No_Out_FileMessage_Name", operationName));
        }

        if (!fileMessage.getFileType().equals(FileMessage.FILE_TYPE_TEXT) &&
                !fileMessage.getFileType().equals(FileMessage.FILE_TYPE_BINARY)) {
            throw new Exception(mMessages.getString("FILEBC_E00763.OMP_Invalid_Out_File_Type", new Object[]{fileMessage.getFileType(), operationName}));
        }

        if (fileMessage.getFileUseType().equals(FileMessage.FILE_USE_TYPE_ENCODED) &&
                (fileMessage.getFileEncodingStyle() == null ||
                fileMessage.getFileEncodingStyle().equals(""))) {
            throw new Exception(mMessages.getString("FILEBC_E00764.OMP_Invalid_No_Out_FileMessage_EncodingStyle", operationName));
        }
    }

    public void stopReceiving() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "OMP_Stopped_thread");
        }
        mMonitor = null;
    }

    /** Package protected method
     *  Used solely for JUnit test purposes
     */
    void setFileDenormalizer(FileDenormalizer denormalizer) {
        mDenormalizer = denormalizer;
    }

    /**
     * Get a unique provisioning endpoint identifer for use with add/remove endpoints and
     * data retrieval
     * @param serviceName qualified service name the endpoint belongs to
     * @param portName the local name of the port
     * @return unique endpoint identifier string.
     */
    private String createProvisioningEndpointIdentifier(QName serviceName, String portName) {
        return serviceName.getNamespaceURI() + "," + serviceName.getLocalPart() + "," + portName + "," + PROVISIONING_ID;
    }

    /**
     * Get a unique consuming endpoint identifer for use with add/remove endpoints and
     * data retrieval
     * @param serviceName qualified service name the endpoint belongs to
     * @param portName the local name of the port
     * @return unique endpoint identifier string.
     */
    private String createConsumingEndpointIdentifier(QName serviceName, String portName) {
        return serviceName.getNamespaceURI() + "," + serviceName.getLocalPart() + "," + portName + "," + CONSUMING_ID;
    }
}
