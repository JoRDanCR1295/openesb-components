/*
 * IBFileWorker.java
 *
 * Created on May 11, 2007, 9:51:24 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.jbi.filebc;

import java.io.File;
import javax.xml.namespace.QName;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.concurrent.TimeUnit;
import net.java.hulp.measure.Probe;


import org.xml.sax.SAXException;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.filebc.util.AlertsUtil;
import com.sun.jbi.filebc.util.FileUtil;
import com.sun.jbi.filebc.Endpoint.EndpointMessageType;
import com.sun.jbi.filebc.extensions.FileMessage;
import com.sun.jbi.filebc.util.FileStreamHandler;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.exchange.ExchangePattern;

/**
 * IBFileWorker processes one file at a time - normalize
 * the content of the file as one or more (when file contains multiple records)
 * messages and push the messages into NMR, also carry out
 * post message send operations, i.e., labeling & archiving the processed
 * files.
 *
 * IBFileWorker is created by InboundMessageProcessor and it waits on
 * <code>queue</code> of type LinkedBlockingQueue which contains files
 * to be processed, InboundMessageProcessor is responsible to pump File objects
 * into the queue.
 *
 * @author jfu
 */
public class IBFileWorker implements Runnable {

    public static final String PROVISIONING_ID = "Provider";
    public static final String CONSUMING_ID = "Consumer";
    private static final Messages mMessages =
            Messages.getMessages(InboundMessageProcessor.class);
    private static Logger mLogger = Messages.getLogger(IBFileWorker.class);
    private FileNormalizer mNormalizer;
    private InboundMessageProcessor mIBProcessor;
    private QName mOperationName;
    private String mMEP;
    private FileMessage mFileMessage;
    private File mTargetDir;
    private File mWorkAreaBaseDir;
    private MessagingChannel mChannel;

    public IBFileWorker(
            InboundMessageProcessor ibProc,
            String mep,
            FileMessage fileMessage,
            File targetDir, File workAreaBaseDir) throws Exception {
        mIBProcessor = ibProc;
        mMEP = mep;
        mFileMessage = fileMessage;
        // allocate a normalizer for each worker
        // since, e.g. transformer in the normalizer could not be used concurrently
        // by more than one thread;
        mTargetDir = targetDir;
        mNormalizer = new FileNormalizer();
        mWorkAreaBaseDir = workAreaBaseDir;

        mChannel = FileComponentContext.getInstance().getBindingChannel();
    }

    public void run() {

        boolean removeEOL = (mFileMessage.getRemoveEOL() != null) ? mFileMessage.getRemoveEOL().booleanValue() : false;
        boolean multipleRecords = (mFileMessage.getMultipleRecordsPerFile() != null) ? mFileMessage.getMultipleRecordsPerFile().booleanValue() : false;
        long maxBytesPerRecord = (mFileMessage.getMaxBytesPerRecord() != null) ? mFileMessage.getMaxBytesPerRecord().longValue() : -1;  // -1 means not defined
        byte[] recordDelim = (mFileMessage.getRecordDelimiter() != null) ? mFileMessage.getRecordDelimiter().getBytes() : new byte[]{};

        while (true) {
            File inputFile = null;
            FileMeta inFileMeta = null;
            try {
                inFileMeta = (FileMeta) mIBProcessor.getInputFileQueue().poll(5000, TimeUnit.MILLISECONDS);
            } catch (InterruptedException ex) {
                // interrupted - continue after a pause
                try {
                    Thread.sleep(1000);
                } catch (Exception ex2) {
                    // ignore
                }
            }
            if (inFileMeta != null) {
                inputFile = inFileMeta.getInProcessFileHandle();
                try {
                    if (inputFile.exists()) {
                        // add Batch Number NM Property
                        String batchId = inputFile.getName();
                        inFileMeta.setNMProperty(FileMeta.NMPROP_INBOUND_BATCHID, batchId);
                        inFileMeta.setNMProperty(FileMeta.NMPROP_GROUPID, batchId);

                        FileInputStream stream = new FileInputStream(inputFile);
                        FileStreamHandler streamHandler = new FileStreamHandler(stream,
                                removeEOL,
                                recordDelim,
                                maxBytesPerRecord,
                                inputFile.length());

                        //Set that multiple records are available
                        streamHandler.setMultipleRecords(multipleRecords);

                        boolean errFlag = false;
                        Throwable err = null;
                        String errString = "";
                        // now, need to do the archiving if it is enabled
                        try {
                            if (!multipleRecords) {
                                if (mLogger.isLoggable(Level.FINE)) {
                                    mLogger.log(Level.FINE, "Single record processing is configured, so we will treat the whole file content as one record...");
                                }
                                // send single message:
                                // if sent successful - proceed to archive (move or delete)
                                // if send failed - mark it as err and proceed to archive (never delete an err marked file)
                                // if archive is not enabled, the err file remains in the tmp area - not desirable but
                                // won't cause any problem - the user can exam the tmp area for files that failed to be
                                // processed.
                                try {
                                    inFileMeta.setNMProperty(FileMeta.NMPROP_INBOUND_RECORDNUMBER, Integer.toString(1));
                                    inFileMeta.setNMProperty(FileMeta.NMPROP_MESSAGEID, Integer.toString(1));
                                    inFileMeta.setNMProperty(FileMeta.NMPROP_INBOUND_LASTRECORD, "true");
                                    if (!processMessage(mMEP, mFileMessage, streamHandler, inputFile, inFileMeta)) {
                                        errFlag = true;
                                        errString = mMessages.getString("FILEBC-E00715.File_not_found_or_empty", inputFile.getAbsolutePath());
                                        mLogger.log(Level.SEVERE, errString);
                                        AlertsUtil.getAlerter().critical(errString,
                                                FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                                                null,
                                                AlertsUtil.getServerType(),
                                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                                NotificationEvent.EVENT_TYPE_ALERT,
                                                "FILEBC-E00715");
                                    }
                                } catch (Exception ex) {
                                    errFlag = true;
                                    err = ex;
                                    errString = mMessages.getString("FILEBC-E00716.Input_file_process_failed_exception",
                                            new Object[]{inputFile.getAbsolutePath(), ex.getLocalizedMessage()});
                                    mLogger.log(Level.SEVERE,
                                            errString,
                                            ex);
                                    AlertsUtil.getAlerter().critical(errString,
                                            FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                                            null,
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "FILEBC-E00716");
                                }
                            } else {

                                if (mLogger.isLoggable(Level.FINE)) {
                                    mLogger.log(Level.FINE, "FILEBC-R00702: PROCESSING MULTIPLE RECORDS - ComponentContext Instance=" + System.getProperty("com.sun.aas.instanceName"));
                                }
                                // if all sent successful - proceed to archive
                                // if send failed - mark it as err and proceed to archive (never delete an err marked file)
                                // if archive is not enabled, the err file remains in the tmp area - not desirable but
                                // won't cause any problem - the user can exam the tmp area for files that failed to be
                                // processed.
                                for (int ii = 0; streamHandler.hasMoreRecords(); ii++) {
                                    try {
                                        inFileMeta.setNMProperty(FileMeta.NMPROP_INBOUND_RECORDNUMBER, Integer.toString(ii));
                                        inFileMeta.setNMProperty(FileMeta.NMPROP_MESSAGEID, Integer.toString(ii));
                                        if (!processMessage(mMEP, mFileMessage, streamHandler, inputFile, inFileMeta)) {
                                            break;
                                        }
                                    } catch (Exception ex) {
                                        // mark the input file as err and break
                                        // so a file containing multiple records will be marked as err
                                        // when the first record send failure occurred
                                        errFlag = true;
                                        err = ex;
                                        errString = mMessages.getString("FILEBC-E00716.Input_file_process_failed_exception",
                                                new Object[]{inputFile.getAbsolutePath(), ex.getLocalizedMessage()});
                                        mLogger.log(Level.SEVERE,
                                                errString,
                                                ex);
                                        AlertsUtil.getAlerter().critical(errString,
                                                FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                                                null,
                                                AlertsUtil.getServerType(),
                                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                                NotificationEvent.EVENT_TYPE_ALERT,
                                                "FILEBC-E00716");
                                    }
                                }

                            }

                        } finally {
                            try {
                                if (!streamHandler.isDonotClose()) {
                                    streamHandler.close();
                                }
                            } catch (Throwable t) {
                            }
                        }

                        if (errFlag) {
                            // when errFlag is up - the message is not sent out into NMR,
                            // so only need to mark it in the tmp dir
                            // manual intervention needed to recover the err marked input file
                            // for re-sent, etc.
                            try {
//                            	File errDir = new File (mTargetDir + File.separator +
//                            			FileUtil.DEFAULT_ERRORS_DIR_NAME);
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
                                FileUtil.createErrorFile(errFilePath, errString, err);

                                mLogger.log(Level.SEVERE, mMessages.getString("FILEBC-E00717.Input_file_flagged_error",
                                        new Object[]{inputFile.getAbsolutePath(),
                                            errInputFile.getAbsolutePath(),
                                            errFilePath}));
                                AlertsUtil.getAlerter().critical(mMessages.getString("FILEBC-E00717.Input_file_flagged_error",
                                        new Object[]{inputFile.getAbsolutePath(),
                                            errInputFile.getAbsolutePath(),
                                            errFilePath}),
                                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                                        null,
                                        AlertsUtil.getServerType(),
                                        AlertsUtil.COMPONENT_TYPE_BINDING,
                                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                        NotificationEvent.EVENT_TYPE_ALERT,
                                        "FILEBC-E00717");

                            } catch (Exception ex) {
                                // can not even mark the input as err - log error
                                mLogger.log(Level.SEVERE,
                                        mMessages.getString("FILEBC-E00718.Input_file_flag_error_exception",
                                        new Object[]{inputFile.getAbsolutePath(), ex.getLocalizedMessage()}),
                                        ex);
                                AlertsUtil.getAlerter().critical(mMessages.getString("FILEBC-E00718.Input_file_flag_error_exception",
                                        new Object[]{inputFile.getAbsolutePath(), ex.getLocalizedMessage()}),
                                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                                        null,
                                        AlertsUtil.getServerType(),
                                        AlertsUtil.COMPONENT_TYPE_BINDING,
                                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                        NotificationEvent.EVENT_TYPE_ALERT,
                                        "FILEBC-E00718");
                            }
                        } else {
                            // the message is pushed into NMR now, some outbound processor will
                            // denormalize the message routed to it and upon completion of external system
                            // interaction, the outbound processor will call back into processReply()
                            // of InboundMessageProcessor (which implements MessageExchangeReplyListener)
                            // where depend on status of the Exchange, the input file will be marked
                            // "processed" or "error"
                        }
                    }
                } catch (Exception ex) {
                    mLogger.log(Level.SEVERE,
                            mMessages.getString("FILEBC-E00716.Input_file_process_failed_exception",
                            new Object[]{inputFile.getAbsolutePath(), ex.getLocalizedMessage()}),
                            ex);
                    AlertsUtil.getAlerter().critical(mMessages.getString("FILEBC-E00716.Input_file_process_failed_exception",
                            new Object[]{inputFile.getAbsolutePath(), ex.getLocalizedMessage()}),
                            FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                            null,
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "FILEBC-E00716");
                }
            } else {
                // timeout - no work to do - check if the inbound processor is stopping
                if (mIBProcessor.isStopped()) {
                    // stop when inbound processor stopped
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "FILEBC-R00703: Shutdown since inbound processor has been shutdown...");
                    }
                    break;
                }
            }
        }
    }

    private boolean processMessage(String mep, FileMessage fileMessage, FileStreamHandler fileStreamHandler, File inputFile, FileMeta fileMeta)
            throws MessagingException, SAXException, IOException, Exception {
        MessageExchange exchange = null;
        String exchangeId = null;
        if (fileStreamHandler == null) {
            throw new Exception(mMessages.getString("FILEBC-E00719.IMP_Invalid_Data"));
        }

        Exception err = null;
        boolean loadWholeFile = false;
        Logger logger = null;

        try {
            if (mep.equals(EndpointMessageType.IN_ONLY)) {
                exchange = mIBProcessor.getMsgExchangeFactory().createInOnlyExchange();
                loadWholeFile = true;
            } else if (mep.equals(EndpointMessageType.IN_OUT)) {
                exchange = mIBProcessor.getMsgExchangeFactory().createInOutExchange();
            }

            Endpoint endpoint = mIBProcessor.getEndpoint();
            String endPointID = (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND) ? (createConsumingEndpointIdentifier(
                    endpoint.getServiceName(), endpoint.getEndpointName()))
                    : (createProvisioningEndpointIdentifier(
                    endpoint.getServiceName(), endpoint.getEndpointName()));

            Probe normalizationMeasurement = Probe.info(getClass(),
                    endPointID,
                    FileBindingLifeCycle.PERF_CAT_NORMALIZATION);

            // Push the context
            logger = Logger.getLogger("com.sun.EnterContext");
            logger.fine(mIBProcessor.getEndpoint().getServiceUnitID() + "-" + mIBProcessor.getOperationName());

            NormalizedMessage inMsg = mNormalizer.normalize(exchange,
                    mIBProcessor.getOperationName(),
                    mIBProcessor.getEndpoint(),
                    fileMessage,
                    fileStreamHandler, loadWholeFile);

            if (normalizationMeasurement != null) {
                normalizationMeasurement.end();
            }
            if (inMsg == null) {
                return false;
            }

            boolean reTryEnabled = false;
            if (isRetryEnabled(exchange, mIBProcessor.getEndpoint())) {
                reTryEnabled = true;
            }

            // Add Inbound NM Properties
            if (!fileStreamHandler.hasMoreRecords() && fileStreamHandler.isMultipleRecords()) {
                fileMeta.setNMProperty(FileMeta.NMPROP_INBOUND_LASTRECORD, "true");
                //Close stream if not binary
                if (!fileStreamHandler.isDonotClose()) {
                    fileStreamHandler.close();
                }
            }
            fileMeta.setNMProperty(FileMeta.NMPROP_INBOUND_ENDPOINTNAME,
                    getNMEndpointName(endpoint.getServiceName(), endpoint.getEndpointName()));
            mIBProcessor.send(exchange, inMsg, fileMeta, reTryEnabled);

            //save message exchange
            if (fileStreamHandler.isDonotClose()) {
                PendingExchangeManager.PendingExchangeHolder holder = new PendingExchangeManager.PendingExchangeHolder();
                holder.fileStreamHandler = fileStreamHandler;
                PendingExchangeManager.getInstance().saveExchangeHolder(exchange, holder);
            }
            //--------------------

        } catch (MessagingException ex) {
            err = ex;
        } catch (SAXException ex) {
            err = ex;
        } catch (IOException ex) {
            err = ex;
        } catch (Exception ex) {
            err = ex;
        } finally {
            // Pop the context
            if (logger != null) {
                Logger.getLogger("com.sun.ExitContext").fine(mIBProcessor.getEndpoint().getServiceUnitID() + "-" + mIBProcessor.getOperationName());
            }

        }
        if (err != null) {
            if (exchangeId != null) {
                mIBProcessor.getInboundExchanges().remove(exchangeId);
            }
            throw err;
        }
        return true;
    }

    public void setNormalizer(FileNormalizer normlzer) {
        mNormalizer = normlzer;
    }

    public FileNormalizer getNormalizer() {
        return mNormalizer;
    }

    private boolean isRetryEnabled(MessageExchange exchange, Endpoint ep) {
        boolean shouldRetry = false;
        EndpointInfo info = new EndpointInfo(false,
                ep.getEndpointName(),
                null,
                ep.getServiceName(),
                null);
        RedeliveryConfig retryConfig = mChannel.getServiceQuality(info, RedeliveryConfig.class);
        if (retryConfig == null) {
            return shouldRetry;
        }

        Failure onFailureOption = retryConfig.getFailure();

        switch (ExchangePattern.valueOf(exchange)) {
            case IN_OUT:
                if (onFailureOption == Failure.redirect || onFailureOption == Failure.delete) {
                    String option = (onFailureOption == Failure.redirect) ? "redirect" : "delete";
                    String warningMsg = mMessages.getString("FILEBC-W00807.Unsupported_onfailure_option", option);
                    if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING, warningMsg);
                    }
                    AlertsUtil.getAlerter().warning(warningMsg,
                            FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                            ep.getServiceUnitID(),
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "FILEBC-W00807");
                }
                shouldRetry = (retryConfig != null && retryConfig.getMaxRetries() > 0 &&
                        (onFailureOption == Failure.suspend || onFailureOption == Failure.error));

                break;
            case IN_ONLY:
                shouldRetry = (retryConfig != null && retryConfig.getMaxRetries() > 0);
                break;
            default:
                break;
        }

        return shouldRetry;
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

    private String getNMEndpointName(QName serviceName, String portName) {
        return "{" + serviceName.getNamespaceURI() + "}" + serviceName.getLocalPart() + "," + portName;
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
