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
package com.sun.jbi.ftpbc;

import com.sun.jbi.ftpbc.ftp.FtpFileConfigConstants;
import com.sun.jbi.ftpbc.ftp.FtpInterface;
import com.sun.jbi.ftpbc.ftp.FtpFileClient;
import com.sun.jbi.ftpbc.ftp.FtpFileProvider;
import com.sun.jbi.ftpbc.Endpoint.EndpointMessageType;
import com.sun.jbi.ftpbc.connection.Connection;
import com.sun.jbi.ftpbc.connection.ConnectionPool;
import com.sun.jbi.ftpbc.extensions.FTPAddress;
import com.sun.jbi.ftpbc.extensions.FTPInput;
import com.sun.jbi.ftpbc.extensions.FTPMessageExtension;
import com.sun.jbi.ftpbc.extensions.FTPOperation;
import com.sun.jbi.ftpbc.extensions.FTPOutput;
import com.sun.jbi.ftpbc.extensions.FTPTransfer;
import com.sun.jbi.ftpbc.extensions.FTPTransferExtension;
import com.sun.jbi.ftpbc.extensions.ProxyAddressURL;
import com.sun.jbi.ftpbc.ftp.FtpFileConfiguration;
import com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands;
import com.sun.jbi.ftpbc.ftp.connection.FTPBCConnectionManager;
import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;
import com.sun.jbi.ftpbc.persistence.FTPBCPersistStore;
import com.sun.jbi.ftpbc.qos.messaging.MessageExchangeTemplatesWithAttachments;
import com.sun.jbi.ftpbc.util.AlertsUtil;
import com.sun.jbi.ftpbc.util.FTPInputStreamWrapper;
import com.sun.jbi.ftpbc.util.NMPropertyUtil;
import com.sun.jbi.ftpbc.util.Utils;

import com.sun.jbi.bindings.synchronization.CompositeLockRegistry;
import com.sun.jbi.bindings.synchronization.CompositeLock;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.messaging.SendFailureListener;
import com.sun.jbi.internationalization.Messages;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.activation.DataHandler;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import net.java.hulp.measure.Probe;

/**
 * @author jfu jim.fu@sun.com
 */
public class InboundMessageProcessor implements Runnable, MessageExchangeReplyListener, SendFailureListener {

    private static final Messages mMessages =
            Messages.getMessages(InboundMessageProcessor.class);
    private static Logger mLogger = Messages.getLogger(InboundMessageProcessor.class);

    public enum PRE_POST_TRANSFER_CMDS {

        NONE, COPY, DELETE, RENAME
    }

    public enum FTPBC_SUB_BINDING_TYPES {

        MESSAGE, TRANSFER
    }
    /**
     * Defines the JBI identifier for an 'in' message, for use with exchange.getMessage
     */
    private static Map mInboundExchanges = Collections.synchronizedMap(new HashMap());
    private AtomicBoolean bStopReceivingRequested;
    private FTPNormalizer mNormalizer;
    private MessagingChannel mChannel;
    private ComponentContext mContext;
    private MessageExchangeFactory mMsgExchangeFactory;
    private Endpoint mEndpoint;
    private QName mOperationName;
    private ServiceEndpoint mServiceEndpoint;
    private RuntimeConfiguration mRtCfg;
    private ProxyAddressURL mProxy;
    private AtomicBoolean isSuspended;
    public static final String REDELIVERY_QOS_MSG_ID = "REDELIVERY_QOS_MSG_ID";

    public InboundMessageProcessor(ComponentContext context,
            MessagingChannel channel,
            Endpoint endpoint,
            QName operationName,
            RuntimeConfiguration runtimeConfig) throws IBProcCreationException {
        mContext = context;
        mChannel = channel;
        mEndpoint = endpoint;
        mOperationName = new QName(endpoint.getServiceName().getNamespaceURI(), operationName.getLocalPart());
        bStopReceivingRequested = new AtomicBoolean();
        try {
            mNormalizer = new FTPNormalizer();
        } catch (Exception ex) {
            throw new IBProcCreationException(ex);
        }
        mRtCfg = runtimeConfig;
        mChannel.addSendFailureListener(this);
        isSuspended = new AtomicBoolean(false);
    }

    public static Map getInboundExchanges() {
        return mInboundExchanges;
    }

    public void run() {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R004001.IMP_EP_status", new Object[]{mEndpoint.getServiceName(), mEndpoint.getEndpointName()}));
        }

        String mep = getMessageExchangePattern(mOperationName);
        FTPOperation operation = locateFTPOperation(mOperationName);
        FTPAddress address = mEndpoint.getAddress();

        try {
            validateInboundMessageExchangeProperties(address, operation, mep);
        } catch (Exception e) {
            logAndAlert(Level.SEVERE, "FTPBC-E004077", "FTPBC-E004077.[ALERT].Err_Validate_IB_Mex_Props", new Object[]{e.getLocalizedMessage()}, e);
            // permenent error - i.e. - invalid extensibility element attributes value 
            // break out the poller thread
            return;
        }
        /**
         * We have an one-way or request-response inbound operation.
         * The ftp "polling" properties will be provided in
         * BindingInput extensibility element.
         * The BindingInput and its corresponding required ftp:transfer
         * properties are guaranteed or else we won't even reach here.
         */
        FTPInput ftpInput = operation.getFTPOperationInput();
        FTPTransferExtension extElem = ftpInput.getExtension();
        FTPBC_SUB_BINDING_TYPES subbnd =
                extElem instanceof FTPMessageExtension ? FTPBC_SUB_BINDING_TYPES.MESSAGE : FTPBC_SUB_BINDING_TYPES.TRANSFER;
        Properties params = new Properties();

        try {
            FtpClientParameterGenerator.createProperties(false,
                    params,
                    address,
                    extElem,
                    mProxy,
                    mOperationName,
                    null, // for inbound if messageCorrelate is on - the UUID should be reverted from target name
                    false, // we are service provider polling for request
                    mMessages,
                    mLogger);
        } catch (Exception ex) {
            logAndAlert(Level.SEVERE, "FTPBC-E004026", "FTPBC-E004026.[ALERT].Exception_Preparing_Conn_Parms", new Object[]{
                        mEndpoint.getServiceName(),
                        mEndpoint.getEndpointName(),
                        ex
                    }, ex);
            // permenent error - i.e. - some malformed parameters
            // break out the poller thread
            return;
        }

        params.put(FtpFileConfigConstants.C_P_FTP_PASSIVE_ON, mRtCfg.getUsePassiveFTP() != null && mRtCfg.getUsePassiveFTP().booleanValue() ? "Yes" : "No");

        // pass in the connection configuration parameters
        params.put(ConnectionPool.POOL_MIN_SIZE, mRtCfg.getConnectionPoolMinSize());
        params.put(ConnectionPool.POOL_MAX_SIZE, mRtCfg.getConnectionPoolMaxSize());
        params.put(Connection.CONN_MAX_IDEL_TIMEOUT, mRtCfg.getConnectionMaxIdleTimeout());

        // extract ftpbc specific NM properties info
        // from address and extElement
        Map nmProps = mRtCfg.getEnableNMProps().booleanValue() ? NMPropertyUtil.extractNMProperties(address, extElem) : new HashMap();

        Connection connection = null;
        FtpInterface ftp = null;
        FtpFileClient client = null;
        FtpFileProvider provider = null;
        FtpFileConfiguration config = null;

        Semaphore sema = null;
        String targetDir = null;
        String connKey = null;
        // when target dir is a pattern, the synch range covers all derived dirs
        String semaKey = null;

        //
        // Recovery takes place here:
        // before the inbound poller goes into 
        // the polling loop, it needs to 
        // look into the poll staging area [1],
        // 
        // If there is already files, it must be 
        // left over messages not be able to delivered
        // from the previous run of the poller:
        // try to continue to deliver the messages
        // at the completion, proceed to regular polling
        // if error occurred, log error, emit alert and exit the poller
        //
        // Note [1]: 
        //
        // For ftp:message, the inbound poll staging area is
        // the dedicated sub-directory "inselect" under the ftp base directory
        // as specified by ftp:message -> messageRepository
        //
        // For ftp:transfer, the inbound poll staging area is
        // the directory specified by ftp:transfer -> preReceiveLocation
        //
        // Note:
        //
        // During recovery, redelivery QoS is not enforced
        // a plain message channel will be used;
        //
        // It is decided to be tolerant on recovery failure, i.e.,
        // if the recovery not complete due to error,
        // the inbound poller will still go into 
        // the polling loop.
        //
        doRecovery(params, nmProps, extElem, mep);

        //
        // start polling
        //
        do {
            //
            // the poller can be suspended by redelivery mechanism option - suspend
            // when suspended, the poller will loop on a sleep of poll interval
            // without doing actual polling.
            //
            // resume is suppose to be controlled
            // from a JMX console, e.g.
            //
            if (!isSuspended.get()) {
                if ((connection = getFTPInterfaceAndValidate(params)) == null) // it is fatal to have a bad connection
                // so break out
                {
                    break;
                }

                ftp = (FtpInterface) connection.getClientObject();
                client = ftp.getClient();
                provider = ftp.getProvider();
                config = ftp.getConfiguration();

                Utils.prepareFTPInterface(client, provider, subbnd);

                targetDir = config.getTargetDirectoryName();
                connKey = connection.getKey();

                CompositeLock lock = CompositeLockRegistry.get(mEndpoint.getOperationUUID(mOperationName));
                if (lock == null) {
                    logAndAlert(Level.SEVERE,
                            "FTPBC-E004099",
                            "FTPBC-E004099.[ALERT].No_CompositeLock_Registered",
                            new Object[]{
                                mOperationName.toString(),
                                Thread.currentThread().getName(),
                                "run()"
                            },
                            null);
                    break;
                }

                try {
                    try {
                        lock = lock.lock();
                    } catch (Exception e) {
                        // exception when acquire lock
                        logAndAlert(Level.SEVERE,
                                "FTPBC-E004101",
                                "FTPBC-E004101.[ALERT].Exception_when_invoke_composite_lock",
                                new Object[]{
                                    mOperationName.toString(),
                                    Thread.currentThread().getName(),
                                    lock.toString(),
                                    e.getLocalizedMessage()
                                },
                                e);
                        break;
                    }
                    if (lock != null) {
                        // granted a composite lock
                        // when target dir is a pattern, the synch range covers all derived dirs
                        semaKey = connKey.concat(targetDir);
                        sema = RemoteTargetSemaphoreManager.get(semaKey);

                        try {
                            sema.acquire();
                        } catch (InterruptedException e) {
                            logAndAlert(Level.SEVERE,
                                    "FTPBC-E004078",
                                    "FTPBC-E004078.[ALERT].Thread_Interrupted_Sema_Acquire",
                                    new Object[]{
                                        Thread.currentThread().getName(),
                                        semaKey
                                    },
                                    e);
                            break;
                        }

                        boolean connOK = true;

                        try {
                            if (!client.isConnected()) {
                                client.connect();
                            }
                        } catch (Exception ex) {
                            // release the sema
                            sema.release();
                            if (connection != null) {
                                connection.discard();
                                connection = null;
                            }
                            logAndAlert(Level.SEVERE,
                                    "FTPBC-E004079",
                                    "FTPBC-E004079.[ALERT].Exception_when_attempt_connect",
                                    new Object[]{
                                        connKey,
                                        ex.getLocalizedMessage()
                                    },
                                    ex);
                            // problem on the connection:
                            // can be permenent (wrong user credential) or temporary (network outage)
                            // but currently the ftp client API error and exception reporting
                            // does not provide accurate enough info for the caller to do the 
                            // appropriate handling, and also for temporary conditions there is not
                            // a re-try defined yet, so
                            //
                            // break out the poller thread

                            // per issue # 1566 - should continue the polling
                            // until the ftp server is up again
                            // note, since it is hard to distinguish server down 
                            // and user login failure, we just continue anyway
                            connOK = false;
                            //break;
                        }

                        if (connOK) {
                            try {
                                validateEndpoint();
                                //
                                // interact with the external system
                                // and do the message normalization and NMR routing
                                // business
                                //
                                // processRemote(mep, extElem, client, provider, config, nmProps);
                                //
                                // lift method body of processRemote() up here
                                // so that CRITICAL REGION boundary can be 
                                // handled well
                                //
                                ListenerMeta meta = null;
                                String dir = null;
                                String file = null;
                                String preDir = null;
                                String preFile = null;
                                InputStream is = null;

                                FtpFileTransferNamesAndCommands tnc = (FtpFileTransferNamesAndCommands) client.getResolvedNamesForGet();

                                tnc.resolveTargetLocation();
                                file = tnc.getTargetFileName();
                                dir = tnc.getTargetDirectoryName();

                                if (file != null && file.trim().length() > 0) {
                                    // message expected arrived
                                    // do further processing
                                    // do pre get operation
                                    String msgNameTag = extractUUIDFromInboundFile(extElem, file);

                                    // the rule is:
                                    // if there is an ID bound to the message from external system (e.g. UUID derived from ftp file name)
                                    // use it as the message id, otherwise, assign a new UUID to the message;
                                    String uniqMsgID = (msgNameTag == null || msgNameTag.trim().length() == 0) ? UUID.randomUUID().toString() : msgNameTag.trim();
                                    meta = new ListenerMeta(this);
                                    meta.setConnectionParams(params);
                                    meta.setMessageUUIDTag(msgNameTag);
                                    meta.setDir(dir);
                                    meta.setFile(file);
                                    meta.setSubBindingType(extElem instanceof FTPMessageExtension ? FTPBC_SUB_BINDING_TYPES.MESSAGE : FTPBC_SUB_BINDING_TYPES.TRANSFER);
                                    if (config.isPostTransferCommandDelete()) {
                                        //meta.settnc.getPostDirectoryName()
                                        meta.setCommand(InboundMessageProcessor.PRE_POST_TRANSFER_CMDS.DELETE);
                                        meta.setPostDir(tnc.getPostDirectoryName());
                                        meta.setPostFile(tnc.getPostFileName());
                                    } else if (config.isPostTransferCommandRename()) {
                                        meta.setCommand(InboundMessageProcessor.PRE_POST_TRANSFER_CMDS.RENAME);
                                        meta.setPostDir(tnc.getPostDirectoryName());
                                        meta.setPostFile(tnc.getPostFileName());
                                    } else {
                                        meta.setCommand(InboundMessageProcessor.PRE_POST_TRANSFER_CMDS.NONE);
                                    }

                                    if (config.isPreTransferCommandRename() || config.isPreTransferCommandCopy()) {
                                        preDir = tnc.getPreDirectoryName();
                                        preFile = tnc.getPreFileName();
                                        if (preFile == null || preFile.trim().length() == 0) {
                                            // pre transfer operation specified
                                            // but can not resolve the destination 
                                            // for the pre operation, this is permenent,
                                            // throw exception so that it can be handled
                                            // at caller's level
                                            throw new FtpFileException(mMessages.getString("FTPBC-E004081.Error_Resolve_Pre_Op_Dest",
                                                    new Object[]{
                                                        config.getPreTransferCommand(),
                                                        Thread.currentThread().getName(),
                                                        config.getPreDirectoryName(),
                                                        config.getPreDirectoryNameIsPattern(),
                                                        config.getPreFileName(),
                                                        config.getPreFileNameIsPattern()
                                                    }));
                                        }

                                        meta.setPreDir(tnc.getPreDirectoryName());
                                        meta.setPreFile(tnc.getPreFileName());

                                        client.doPreTransferGet(tnc);

                                        // END CRITICAL REGION earlier if pre operation is COPY/RENAME
                                        sema.release();
                                        sema = null;
                                        // according to the recovery protocol, only 
                                        // when inbound staging occurred, there is need to record 
                                        // the resolved target, and pre and post dirs and files
                                        try {
                                            saveRecoverLog(tnc, uniqMsgID, meta);
                                        } catch (Exception e) {
                                            // warn that the capability of
                                            // re-send this inbound message is lost.
                                            // this might not be an issue, if the
                                            // message send is done without error,
                                            // however, if the message is in processing
                                            // and the component terminated for
                                            // whatever reason, the recovery process
                                            // at the beginning of the poller
                                            // will not have info to re-send it.
                                            if (mLogger.isLoggable(Level.WARNING)) {
                                                mLogger.log(Level.WARNING,
                                                        mMessages.getString("FTPBC-W004014.Save_Recovery_Log_Error",
                                                        new Object[]{
                                                            uniqMsgID,
                                                            mEndpoint.getServiceName(),
                                                            mEndpoint.getUniqueName()
                                                        }));
                                            }
                                            deleteRecoverLog(uniqMsgID);
                                        }
                                        is = provider.retrieveFileStream(preDir, preFile);
                                    } else {
                                        is = provider.retrieveFileStream(dir, file);
                                    }
                                    //
                                    // if pre operation is NONE, then the subsequent operations are still
                                    // on the original target - hence stay in the critical region so other 
                                    // competing poller won't step on each other
                                    //
                                    if (is == null) {
                                        throw new FtpFileException(mMessages.getString("FTPBC-E004031.IMP_Invalid_Data"));
                                    }

                                    FTPInputStreamWrapper inStream = new FTPInputStreamWrapper(is);
                                    Source inMsg = null;
                                    Probe normMeasure = Probe.info(getClass(), mEndpoint.getUniqueName(), FTPBindingLifeCycle.PERF_CAT_NORMALIZATION);
                                    Map<String, DataHandler> attachments = new HashMap<String, DataHandler>();
                                    Exception exception = null;
                                    try {
                                        inMsg = mNormalizer.normalize(mOperationName,
                                                mEndpoint,
                                                extElem,
                                                inStream,
                                                true,
                                                attachments);
                                        // coarse grained error processing
                                        // treat all normalization exception
                                        // as malformed message
                                        // handling note:
                                        // mark the file (with suffix err)
                                        // keep its URI in a local journal
                                        // continue the poller loop
                                        //
                                        // exception thrown:
                                        // NormalizationException,
                                        // EncoderException,
                                        // EncoderNotFoundException,
                                        // IOException,
                                    } catch (IOException ex) {
                                        // IOException from reading 
                                        // the file from remote
                                        // 
                                        exception = ex;
                                    } catch (Exception ex) {
                                        // malformed message structure or encoder
                                        // can not be found or encoder can not
                                        // encode the payload.
                                        //
                                        // NormalizationException, 
                                        // EncoderException,
                                        // EncoderNotFoundException,
                                        // for now, treat these exception
                                        // as malformed message:
                                        // mark it as err so that
                                        // it won't be subject to processing
                                        // until the issue is fixed by 
                                        // administrative means.
                                        exception = ex;
                                    } finally {
                                        if (normMeasure != null) {
                                            normMeasure.end();
                                        }
                                        // since, the whole content is already loaded
                                        // close the inStream
                                        try {
                                            inStream.close();
                                        } catch (Exception e) {
                                            // ignore
                                        }
                                    }

                                    postProcessingNormalization(uniqMsgID, provider, config, exception, tnc);

                                    // do the post operation / archiving 
                                    // in reply processing:
                                    //

                                    // enforce the throttling here, seems a per endpoint max concurrency limit
                                    // get one throttle permit
                                    // if there is no token available,
                                    // we won't poll the external and pump messages 
                                    // into the system
                                    try {
                                        mEndpoint.acquireThrottle();
                                    } catch (InterruptedException ie) {
                                        // warning but allow message routing proceed
                                        if (mLogger.isLoggable(Level.WARNING)) {
                                            mLogger.log(Level.WARNING,
                                                    mMessages.getString("FTPBC-W004015.Acquire_Throttle_Interrupted",
                                                    new Object[]{
                                                        mEndpoint.getUniqueName()
                                                    }));
                                        }
                                    }

                                    MessageExchangeTemplatesWithAttachments template = new MessageExchangeTemplatesWithAttachments(mServiceEndpoint,
                                            mOperationName,
                                            mep.equals(EndpointMessageType.IN_ONLY),
                                            null, // Group ID
                                            uniqMsgID,
                                            inMsg,
                                            attachments,
                                            mMsgExchangeFactory);
                                    //
                                    // Comments copied from qos library:
                                    // 
                                    // These properties provided to the BaseExchangeTemplates
                                    // populate every message exchange that the BaseExchangeTemplates
                                    // create (i.e., for the initial exchange + retries.
                                    //
                                    // * I pass myself so I can be notified for the reply/done/error
                                    //   exchange status; see notifyDone().
                                    //
                                    // * If syncpoint is involved, I also pass myself so I can be
                                    //   informed of rollback disposition; see setSyncpointRollback().
                                    //
                                    // * If transactions are involved, Transaction object does the
                                    //   equivalent of the previous.
                                    //
                                    Properties MEProp = new Properties();
                                    MEProp.setProperty(REDELIVERY_QOS_MSG_ID, template.getUniqueId());
                                    template.setPropExchange(MEProp);

                                    if (mRtCfg.getEnableNMProps().booleanValue()) {
                                        NMPropertyUtil.mergeNMPropertiesResolvedParams4Get(tnc, nmProps, extElem instanceof FTPTransfer);
                                    }

                                    // for now, other NM props takes default:
                                    // group id : blank, only group
                                    // last record : true - always one record in one group
                                    //
                                    nmProps.put(FTPBCComponentContext.NM_PROP_MESSAGE_ID, uniqMsgID != null ? uniqMsgID : "");
                                    nmProps.put(FTPBCComponentContext.NM_PROP_EP_NAME, mServiceEndpoint.getEndpointName() != null ? mServiceEndpoint.getEndpointName() : "");
                                    nmProps.put(FTPBCComponentContext.NM_PROP_GROUP_ID, uniqMsgID != null ? uniqMsgID : "");
                                    // for now, ftp always treats data as one rec
                                    nmProps.put(FTPBCComponentContext.NM_PROP_MESSAGE_LASTREC, new Boolean(true));

                                    Properties nmp = new Properties();
                                    nmp.putAll(nmProps);
                                    template.setPropNM(nmp);
                                    mInboundExchanges.put(template.getUniqueId(), meta);
                                    meta.setRequestInvocationTime(System.currentTimeMillis());
                                    mChannel.send(template);
                                    mEndpoint.getEndpointStatus().incrementSentRequests();
                                }
                            } catch (FtpFileException ex) {
                                // problem interact with external
                                if (!client.isConnected()) {
                                    // probably already bad,
                                    // discard the connection to prevent
                                    // it from being re-used again and again
                                    connection.discard();
                                    connection = null;
                                }
                                logAndAlert(Level.SEVERE,
                                        "FTPBC-E004030",
                                        "FTPBC-E004030.[ALERT].IMP_Failed_send_msg",
                                        new Object[]{ex.getLocalizedMessage()},
                                        ex);
                            } catch (Exception ex2) {
                                // other error such as 
                                // message normalization exception etc.
                                logAndAlert(Level.SEVERE,
                                        "FTPBC-E004030",
                                        "FTPBC-E004030.[ALERT].IMP_Failed_send_msg",
                                        new Object[]{ex2.getLocalizedMessage()},
                                        ex2);
                            } finally {
                                // END CRITICAL REGION if not yet
                                if (sema != null) {
                                    sema.release();                    // always return the connection to the pool
                                    // Note, disconnect the connection here
                                    // only to temporary work around a issue:
                                    // force a reset of pwd to user login home
                                    // directory
                                }
                                if (client != null) {
                                    try {
                                        client.disconnect();
                                    } catch (Exception e) {
                                        // ignore
                                    }
                                }

                                if (connection != null) {
                                    try {
                                        FTPBCConnectionManager.returnConnection(connection);
                                    } catch (Exception ex) {
                                        // the messaging has completed,
                                        // but could not put the connection back into pool,
                                        // this seems to be ignorable
                                        // emit warning & alert and continue.
                                        logAndAlert(Level.WARNING,
                                                "FTPBC-E004080",
                                                "FTPBC-E004080.[ALERT].Exception_return_connection",
                                                new Object[]{
                                                    connKey,
                                                    ex.getLocalizedMessage()
                                                },
                                                ex);
                                    }
                                }
                            }
                        }
                    }
                } finally {
                    if (lock != null) {
                        lock.unlock();
                    }
                }
            }

            try {
                // the poll interval
                Thread.currentThread().sleep(extElem.getPollInterval() < 1000 ? 1000 : extElem.getPollInterval());
            } catch (Exception e) {
                // log this as debug info
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE,
                            "FTPBC-D004010.Thread_Interrupted_Sleep",
                            new Object[]{
                                Thread.currentThread().getName(),
                                getServiceID()
                            });
                }
            }

        } while (!bStopReceivingRequested.get());
    }

    public synchronized void processReplyMessage(MessageExchange exchange) throws Exception {
        if (!(exchange instanceof InOnly) &&
                !(exchange instanceof InOut)) {
            throw new Exception(mMessages.getString("FTPBC-E004032.IMP_Unsupported_exchange_pattern", exchange.getPattern().toString()));
        }

        // put back one permit
        mEndpoint.releaseThrottle();

        // do the POST transfer operation here
        // if it is configured
        // DELETE/RENAME
        String messageId = (String) exchange.getProperty(REDELIVERY_QOS_MSG_ID);
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "messageID with QoS Redelivery =" + messageId);
        }

        ListenerMeta meta = null;

        if ((meta = (ListenerMeta) mInboundExchanges.remove(messageId)) != null) {
            if (exchange.getStatus() != ExchangeStatus.DONE) {
                // some how the message exchange did not go through,
                // the message needs to be re-send at a later time, 
                // e.g., next time the poller is started.
                // - subject to recovery
                //
                logAndAlert(Level.SEVERE,
                        "FTPBC-E004033",
                        "FTPBC-E004033.[ALERT].IMP_Message_Exchange_Error_Status_Not_Done_id",
                        new Object[]{exchange.getStatus(), messageId},
                        null);
            } else {
                // got ACK:
                // remove the recovery log entry
                // do post operation / archiving
                processACK(meta, messageId);
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-R004003.IMP_Remove_exchange_msg_id", messageId));
            }
        } else {
            // if this happens, then there is something fundamentally wrong
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, mMessages.getString("FTPBC-E004034.IMP_Invalid_reply_msgId", messageId));
            }
        }
    }

    private void doRecovery(Properties params, Map nmProps, FTPTransferExtension extElem, String mep) {
        // if no pre transfer operation configured - no inbound staging
        // no need to do recovery
        // skip recovery
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "================ Start Recovery =======================");
        }
        if (extElem instanceof FTPTransfer) {
            // for ftp:transfer, if there is preReceiveCommand,
            // then there is need to do recovery
            FTPTransfer transfer = (FTPTransfer) extElem;
            String cmd = transfer.getPreReceiveCommand();
            if (cmd == null || cmd.equalsIgnoreCase("NONE")) {
                if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.log(Level.INFO, "================ End Recovery : no inbound staging turned on - no need to do recovery =======================");
                }
                return;
            }
        } else {
            // assume it is ftp:message
            if (!((FTPMessageExtension) extElem).getStagingEnabled()) {
                // no staging, no need to recover
                if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.log(Level.INFO, "================ End Recovery : no inbound staging turned on - no need to do recovery =======================");
                }
                return;
            }
        }

        MessageExchange exchange = null;
        Connection connection = null;
        FtpInterface ftp = null;
        FtpFileClient client = null;
        FtpFileProvider provider = null;
        FtpFileConfiguration config = null;
        String connKey = null;

        CompositeLock lock = CompositeLockRegistry.get(mEndpoint.getOperationUUID(mOperationName));
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, CompositeLock.class.getName() + ":" + (lock == null ? "NULL" : lock) + ":::" + mOperationName.toString() + " :::" + mEndpoint.getOperationUUID(mOperationName));
        }
        if (lock == null) {
            logAndAlert(Level.SEVERE,
                    "FTPBC-E004099",
                    "FTPBC-E004099.[ALERT].No_CompositeLock_Registered",
                    new Object[]{
                        mOperationName.toString(),
                        Thread.currentThread().getName(),
                        "doRecovery()"
                    },
                    null);
            return;
        }

        File baseDir = ((FTPBCPersistStore) lock.getPersistStore()).getRecoveryLogDir(false);
        if (baseDir == null || !baseDir.exists()) // no recovery persist store
        // skip recovery
        {
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "================ End Recovery : no recovery log found =======================");
            }
            return;
        }

        // in clustered context, the recovery log entries can be polled
        // by multiple threads (pollers associated with same operation clustered) doing recovery
        String uniqMsgID = null;
        File logEntry = null;

        while (true) {
            try {
                if (lock.lock() != null) {
                    File[] recoveryEntries = baseDir.listFiles(new RecoveryLogFileFilter());

                    if (recoveryEntries != null && recoveryEntries.length > 0) {
                        logEntry = recoveryEntries[0];
                        if (mLogger.isLoggable(Level.INFO)) {
                            mLogger.log(Level.INFO, "Thread: " + Thread.currentThread().getName() + "======= Recovery Processing Entry : [" + logEntry.getPath() + "] =======================");
                        }

                        if ((connection = getFTPInterfaceAndValidate(params)) == null) {
                            break;
                        }

                        ftp = (FtpInterface) connection.getClientObject();
                        client = ftp.getClient();
                        provider = ftp.getProvider();
                        config = ftp.getConfiguration();

                        Utils.prepareFTPInterface(client,
                                provider,
                                extElem instanceof FTPMessageExtension ? FTPBC_SUB_BINDING_TYPES.MESSAGE : FTPBC_SUB_BINDING_TYPES.TRANSFER);
                        connKey = connection.getKey();

                        // for now assume for the one poller serving one endpoint
                        // there is only one thread doing the recovery
                        try {
                            if (!client.isConnected()) {
                                client.connect();
                            }
                        } catch (Exception ex) {
                            if (connection != null) {
                                connection.discard();
                                connection = null;
                            }
                            logAndAlert(Level.SEVERE,
                                    "FTPBC-E004079",
                                    "FTPBC-E004079.[ALERT].Exception_when_attempt_connect",
                                    new Object[]{
                                        connKey,
                                        ex.getLocalizedMessage()
                                    },
                                    ex);
                            // problem on the connection:
                            // can be permenent (wrong user credential) or temporary (network outage)
                            // but currently the ftp client API error and exception reporting
                            // does not provide accurate enough info for the caller to do the 
                            // appropriate handling, and also for temporary conditions there is not
                            // a re-try defined yet, so
                            //
                            break;
                        }

                        String dir = null;
                        String file = null;
                        InputStream is = null;

                        try {
                            validateEndpoint();
                        } catch (Exception e) {
                            if (client != null && !client.isConnected()) {
                                // a connection not connected is an indication 
                                // of IO problem, discard it
                                if (connection != null) {
                                    connection.discard();
                                    connection = null;
                                    client = null;
                                }
                            }

                            if (connection != null) {
                                try {
                                    FTPBCConnectionManager.returnConnection(connection);
                                } catch (Exception ex) {
                                    // the messaging has completed,
                                    // but could not put the connection back into pool,
                                    // this seems to be ignorable
                                    // emit warning & alert and continue.
                                    logAndAlert(Level.WARNING,
                                            "FTPBC-E004080",
                                            "FTPBC-E004080.[ALERT].Exception_return_connection",
                                            new Object[]{
                                                connKey,
                                                ex.getLocalizedMessage()
                                            },
                                            ex);
                                }
                            }

                            if (mLogger.isLoggable(Level.SEVERE)) {
                                mLogger.log(Level.SEVERE, e.getLocalizedMessage());
                            }
                            // abort recovery
                            break;
                        }

                        FtpFileTransferNamesAndCommands tnc = (FtpFileTransferNamesAndCommands) client.getResolvedNamesForGet();
                        tnc.resolveTargetLocation();
                        if ((uniqMsgID = parseRecoveryEntry(logEntry, tnc)) != null) {
                            File tmpDir = new File(logEntry.getParentFile(), "recovering_tmp");
                            if (!tmpDir.exists()) {
                                tmpDir.mkdirs();
                            }
                            if (!tmpDir.exists()) {
                                if (mLogger.isLoggable(Level.SEVERE)) {
                                    mLogger.log(Level.INFO, "Thread: " + Thread.currentThread().getName() + "======= Recovery Processing Entry : tmp dir not available for recovery processing, tmp dir = " + tmpDir.getPath() + ".");
                                }
                                break;
                            }
                            File dest = new File(tmpDir, logEntry.getName());
                            if (logEntry.renameTo(dest)) {
                                if (mLogger.isLoggable(Level.INFO)) {
                                    mLogger.log(Level.INFO, "Thread: " + Thread.currentThread().getName() + "======= Recovery Processing Entry : moved [" + logEntry.getPath() + "] to [" + dest.getPath() + "]=======================");
                                }
                            } else {
                                // failed to rename
                                if (mLogger.isLoggable(Level.SEVERE)) {
                                    mLogger.log(Level.INFO, "Thread: " + Thread.currentThread().getName() + "======= Recovery Processing Entry : moved [" + logEntry.getPath() + "] to [" + dest.getPath() + "] failed, recovery process exit for this thread =======================");
                                }
                                break;
                            }

                            // rename the recovery log entry to a temp file
                            // starts with the pre file
                            // retrieve it
                            // normalize it 
                            // send it into NMR
                            dir = tnc.getPreDirectoryName();
                            file = tnc.getPreFileName();

                            // verify that the file is there (in staging area)
                            if (!client.isFileExist(dir, file)) {
                                logAndAlert(Level.SEVERE,
                                        "FTPBC-E004098",
                                        "FTPBC-E004098.[ALERT].Recovery_Msg_Not_Found",
                                        new Object[]{
                                            uniqMsgID,
                                            tnc.getPreDirectoryName(),
                                            tnc.getPreFileName(),
                                            dest.getCanonicalPath()
                                        },
                                        null);
                                // process next recovery log
                                continue;
                            }

                            // revert the message ID from the file name
                            String msgNameTag = extractUUIDFromInboundFile(extElem, file);
                            ListenerMeta meta = new ListenerMeta(this);
                            meta.setRecoverLogEntry(dest);
                            meta.setConnectionParams(params);
                            meta.setPreDir(tnc.getPreDirectoryName());
                            meta.setPreFile(tnc.getPreFileName());
                            meta.setDir(tnc.getTargetDirectoryName());
                            meta.setFile(tnc.getTargetFileName());
                            meta.setPostDir(tnc.getPostDirectoryName());
                            meta.setPostFile(tnc.getPostFileName());
                            meta.setSubBindingType(extElem instanceof FTPMessageExtension ? FTPBC_SUB_BINDING_TYPES.MESSAGE : FTPBC_SUB_BINDING_TYPES.TRANSFER);
                            if (config.isPostTransferCommandDelete()) {
                                meta.setCommand(InboundMessageProcessor.PRE_POST_TRANSFER_CMDS.DELETE);
                            } else if (config.isPostTransferCommandRename()) {
                                meta.setCommand(InboundMessageProcessor.PRE_POST_TRANSFER_CMDS.RENAME);
                            } else {
                                throw new Exception("Invalid post transfer operation encountered processing recovery log: cmd = " + config.getPostTransferCommand());
                            }

                            if ((is = provider.retrieveFileStream(dir, file)) == null) {
                                throw new FtpFileException(mMessages.getString("FTPBC-E004031.IMP_Invalid_Data"));
                            }

                            FTPInputStreamWrapper inStream = new FTPInputStreamWrapper(is);

                            Probe normMeasure = Probe.info(getClass(), mEndpoint.getUniqueName(), FTPBindingLifeCycle.PERF_CAT_NORMALIZATION);

                            boolean oneWay = mep.equals(EndpointMessageType.IN_ONLY);
                            if (oneWay) {
                                exchange = mMsgExchangeFactory.createInOnlyExchange();
                            } else if (mep.equals(EndpointMessageType.IN_OUT)) {
                                exchange = mMsgExchangeFactory.createInOutExchange();
                            } else {
                                throw new Exception("Unsupported message exchange pattern: MEP=" + mep);
                            }

                            Exception exception = null;

                            NormalizedMessage nmsg = null;

                            try {
                                // load the whole content all the time - unitl we figure out how to 
                                // achieve true streaming
                                nmsg = mNormalizer.normalize(exchange,
                                        mOperationName,
                                        mEndpoint,
                                        extElem,
                                        inStream,
                                        true,
                                        true);
                            } catch (IOException ex) {
                                // corse grained error processing
                                // treat all normalization exception 
                                // as malformed message
                                //
                                exception = ex;
                            } catch (Exception ex) {
                                // malformed message or payload etc.
                                exception = ex;
                            } finally {
                                if (normMeasure != null) {
                                    normMeasure.end();
                                }
                                // since, the whole content is already loaded
                                // close the inStream
                                try {
                                    inStream.close();
                                } catch (Exception e) {
                                    // ignore
                                }
                            }

                            postProcessingNormalization(uniqMsgID, provider, config, exception, tnc);

                            if (mRtCfg.getEnableNMProps().booleanValue()) {
                                NMPropertyUtil.addNMPropertiesResolvedParams4Get(nmsg, tnc, extElem instanceof FTPTransfer);
                                if (nmProps != null) {
                                    Iterator it = nmProps.keySet().iterator();
                                    while (it.hasNext()) {
                                        String key = (String) it.next();
                                        nmsg.setProperty(key, nmProps.get(key));
                                    }
                                }
                            }

                            // for now, other NM props takes default:
                            // group id : blank, only group
                            // last record : true - always one record in one group
                            //
                            nmsg.setProperty(FTPBCComponentContext.NM_PROP_MESSAGE_ID, uniqMsgID != null ? uniqMsgID : "");
                            nmsg.setProperty(FTPBCComponentContext.NM_PROP_EP_NAME, mServiceEndpoint.getEndpointName());
                            nmsg.setProperty(FTPBCComponentContext.NM_PROP_GROUP_ID, uniqMsgID != null ? uniqMsgID : "");
                            // for now, ftp always treats data as one rec
                            nmsg.setProperty(FTPBCComponentContext.NM_PROP_MESSAGE_LASTREC, new Boolean(true));

                            exchange.setProperty(InboundMessageProcessor.REDELIVERY_QOS_MSG_ID, uniqMsgID);
                            meta.setRequestInvocationTime(System.currentTimeMillis());
                            meta.setMessageUUIDTag(msgNameTag != null ? msgNameTag.toString() : null);
                            exchange.setEndpoint(mServiceEndpoint);
                            exchange.setOperation(mOperationName);
                            mInboundExchanges.put(uniqMsgID, meta);
                            if (oneWay) {
                                ((InOnly) exchange).setInMessage(nmsg);
                            } else {
                                ((InOut) exchange).setInMessage(nmsg);
                            }
                            mChannel.send(exchange);
                            mEndpoint.getEndpointStatus().incrementSentRequests();
                        }
                    } else {
                        // no more recovery entry - break out
                        break;
                    }
                }
            } catch (Exception e) {
                if (client != null && !client.isConnected()) {
                    // a connection not connected is an indication 
                    // of IO problem, discard it
                    if (connection != null) {
                        connection.discard();
                        connection = null;
                        client = null;
                    }
                }
                logAndAlert(Level.SEVERE,
                        "FTPBC-E004089",
                        "FTPBC-E004089.[ALERT].Exception_Processing_Recover_Log_Entry",
                        new Object[]{
                            uniqMsgID,
                            logEntry.getPath(),
                            e.getLocalizedMessage()
                        }, e);
            } finally {
                if (lock != null) {
                    lock.unlock();
//                if (sync != null) {
//                    try {
//                        sync.release();
//                    } catch (Exception e) {
//                        // ignored
//                        e.printStackTrace();
//                    }
//                    sync = null;
//                }
//                if (tLock != null) {
//                    tLock.unlock();
//                    tLock = null;
//                }
                }
                if (client != null) {
                    try {
                        client.disconnect();
                    } catch (Exception e) {
                        // ignore
                    }
                }

                if (connection != null) {
                    try {
                        FTPBCConnectionManager.returnConnection(connection);
                    } catch (Exception ex) {
                        // the messaging has completed,
                        // but could not put the connection back into pool,
                        // this seems to be ignorable
                        // emit warning & alert and continue.
                        logAndAlert(Level.WARNING,
                                "FTPBC-E004080",
                                "FTPBC-E004080.[ALERT].Exception_return_connection",
                                new Object[]{
                                    connKey,
                                    ex.getLocalizedMessage()
                                },
                                ex);
                    }
                }
            }
            try {
                Thread.currentThread().sleep(1000);
            } catch (Exception e) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE,
                            "FTPBC-D004010.Thread_Interrupted_Sleep",
                            new Object[]{
                                Thread.currentThread().getName(),
                                getServiceID()
                            });
                }
            }
        }
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "================ End Recovery =======================");
        }
    }

    private String extractUUIDFromInboundFile(FTPTransferExtension extElem, String file) {
        // extract UUID from actual file selected
        // (actually - it is a string containing UUID as part of the name)
        String msgNameTag = null;
        boolean msgNameValid = false;
        String prefix = extElem.getMessageNamePrefixIB();
        if (prefix != null && prefix.trim().length() > 0) {
            if (file.length() > prefix.trim().length()) {
                msgNameTag = file.substring(prefix.trim().length());
                // the tail should start with a UUID - by UUID tagging contract
                if (msgNameTag.length() >= 36) {
                    try {
                        UUID.fromString(msgNameTag.substring(0, 36));
                        msgNameValid = true;
                    } catch (IllegalArgumentException e) {
                        // not starting with a UUID - be tollerent here
                        // by giving warning
                    }
                } else {
                    msgNameTag = null;
                }
            }
        }

        if (extElem.getMessageCorrelate()) {
            // correlate synchronous request/response
            // requires that there is a UUID within 
            // the file name polled.
            if (!msgNameValid) {
                logAndAlert(Level.WARNING,
                        "FTPBC-W004002",
                        "FTPBC-W004002.[ALERT].IMP_Expecting_Req_UUID_file_name",
                        new Object[]{prefix, file},
                        null);
            }
        }

        return msgNameTag;
    }

    /**
     * Helper to allocate and validate a healthy connection 
     * object that can be used further to interact with the remote
     * FTP server.
     * @param params - the connection parameters
     * @return null if some invalid thing detected, a good connection
     * from the pool otherwise.
     */
    private Connection getFTPInterfaceAndValidate(Properties params) {
        Connection connection = null;
        try {
            // lookup a connection from the pool
            connection = FTPBCConnectionManager.getConnection(params);
        } catch (Exception e) {
            logAndAlert(Level.SEVERE, "FTPBC-E004052", "FTPBC-E004052.[ALERT].Failed_allocate_connection",
                    new Object[]{FtpFileConfiguration.getKey(params), e.getLocalizedMessage()}, e);
            // permenent error, not to be corrected by re-try or whatever
            // tell the caller to break out the poller thread by return null
            return null;
        }

        if (connection.getClientObject() == null) {
            logAndAlert(Level.SEVERE,
                    "FTPBC-E004074",
                    "FTPBC-E004074.[ALERT].FTP_Interface_Not_Available",
                    new Object[]{
                        this.getClass().getName(),
                        FtpFileConfiguration.getKey(params)
                    },
                    null);
            // permenent error, not to be corrected by re-try or whatever
            // tell the caller to break out the poller thread by return null
            return null;
        }

        if (((FtpInterface) connection.getClientObject()).getClient() == null) {
            logAndAlert(Level.SEVERE,
                    "FTPBC-E004075",
                    "FTPBC-E004075.[ALERT].FTP_Client_Not_Available",
                    new Object[]{
                        this.getClass().getName(),
                        FtpFileConfiguration.getKey(params)
                    },
                    null);
            // permenent error, not to be corrected by re-try or whatever
            // tell the caller to break out the poller thread by return null
            return null;
        }

        if (((FtpInterface) connection.getClientObject()).getProvider() == null) {
            logAndAlert(Level.SEVERE,
                    "FTPBC-E004076",
                    "FTPBC-E004076.[ALERT].FTP_Provider_Not_Available",
                    new Object[]{
                        this.getClass().getName(),
                        FtpFileConfiguration.getKey(params)
                    },
                    null);
            // permenent error, not to be corrected by re-try or whatever
            // tell the caller to break out the poller thread by return null
            return null;
        }

        if (((FtpInterface) connection.getClientObject()).getConfiguration() == null) {
            logAndAlert(Level.SEVERE,
                    "FTPBC-E004084",
                    "FTPBC-E004084.[ALERT].FTP_Config_Not_Available",
                    new Object[]{
                        this.getClass().getName(),
                        FtpFileConfiguration.getKey(params)
                    },
                    null);
            // permenent error, not to be corrected by re-try or whatever
            // tell the caller to break out the poller thread
            return null;
        }
        return connection;
    }

    private FTPOperation locateFTPOperation(QName opname) {
        return (FTPOperation) mEndpoint.getOperations().get(opname);
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
    protected void validateInboundMessageExchangeProperties(FTPAddress address, FTPOperation operation, String mep) throws Exception {
        if (address != null) {
            address.parse();
            address.validate(mOperationName);
            // check ftp user and password to overwrite
            // user and password inside URL
            if (!FtpClientParameterGenerator.isEmpty(address.getUser())) {
                address.getFTPURL().setUser(address.getUser());
            }

            if (!FtpClientParameterGenerator.isEmpty(address.getPassword())) {
                address.getFTPURL().setPassword(address.getPassword());
            }
        }
        // 1. Check if the Message Exchange Pattern is valid.
        if (mep == null ||
                (!mep.equals(EndpointMessageType.IN_ONLY) &&
                !mep.equals(EndpointMessageType.IN_OUT))) {
            throw new Exception(mMessages.getString("FTPBC-E004035.IMP_Invalid_mep", mOperationName));
        }

        // 2. Check if required ftp:transfer properties are available and valid
        FTPInput ftpInput = operation.getFTPOperationInput();
        if (ftpInput == null) {
            throw new Exception(mMessages.getString("FTPBC-E004036.IMP_Invalid_No_FtpInput", mOperationName));
        }

        FTPTransferExtension extElem = ftpInput.getExtension();
        if (extElem == null) {
            throw new Exception(mMessages.getString("FTPBC-E004037.IMP_Invalid_No_FtpTransfer", mOperationName));
        }

        if (mRtCfg != null && mRtCfg.getUseProxy().booleanValue()) {
            mProxy = new ProxyAddressURL(mRtCfg.getProxyURL());
            mProxy.parse();
            mProxy.validate();
            // since introduction of separate user ID and password for proxy - SOCKS5
            // in Runtime Configuration, the following rule apply:
            // if the proxy URL has user login credential already, then keep using that (this will keep 
            // backward compatiblity for those projects deployed previously)
            // otherwise, i.e., the proxy URL does not include any user login credential
            // check runtime configuration proxy parameters:
            // ProxyUserID nad ProxyUserPassword
            if ((mProxy.getUser() == null || mProxy.getUser().trim().length() == 0) && (mProxy.getPassword() == null || mProxy.getPassword().trim().length() == 0)) {
                // further check runtime parameters
                if (mRtCfg.getProxyUserID() != null && mRtCfg.getProxyUserID().trim().length() > 0) {
                    // a good indication of proxy user credential in separate parameters
                    mProxy.setUser(mRtCfg.getProxyUserID());
                    mProxy.setPassword(mRtCfg.getProxyUserPassword());
                }
            }
        }

        // validate ftp:transfer attrs
        if (extElem instanceof FTPTransferExtension) {
            ((FTPTransferExtension) extElem).validate(mOperationName.toString());
        } else {
            throw new Exception(mMessages.getString("FTPBC-E004038.IMP_Invalid_In_FtpTransfer", mOperationName));
        }

        // 3. Check if required ftp:transfer (for output) properties are available and valid
        if (mep.equals(EndpointMessageType.IN_OUT)) {
            FTPOutput ftpOutput = operation.getFTPOperationOutput();

            if (ftpOutput == null) {
                throw new Exception(mMessages.getString("FTPBC-E004039.IMP_Invalid_No_FtpOutput", mOperationName));
            }

            extElem = ftpOutput.getExtension();

            if (extElem == null) {
                throw new Exception(mMessages.getString("FTPBC-E004037.IMP_Invalid_No_FtpTransfer", mOperationName));
            }
            if (extElem instanceof FTPTransferExtension) {
                ((FTPTransferExtension) extElem).validate(mOperationName.toString());
            } else {
                throw new Exception(mMessages.getString("FTPBC-E004040.IMP_Invalid_Out_FtpTransfer", mOperationName));
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

        if (activatedEndpoint != null) {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("FTPBC-R004004.IMP_locate_EP", new Object[]{mEndpoint.getServiceName(), mEndpoint.getEndpointName()}));
            }
        }
        return activatedEndpoint;
    }

    /** Package protected method
     *  Used solely for JUnit test purposes
     */
    void setNormalizer(FTPNormalizer normalizer) {
        mNormalizer = normalizer;
    }

    /** Package protected method
     *  Used solely for JUnit test purposes
     */
    static void setInboundExchangeIds(Map exchangeIds) {
        mInboundExchanges = exchangeIds;
    }

    private String getServiceID() {
        String id = "";
        if (mEndpoint != null) {
            if (mEndpoint.getServiceEndpoint() != null) {
                id = mEndpoint.getServiceEndpoint().getServiceName() + mEndpoint.getServiceEndpoint().getEndpointName();
            }
        }
        return id;
    }

    public void stopReceiving() {
        mLogger.log(Level.INFO, mMessages.getString("FTPBC-R004002.IMP_Inbound_stopped"));
        bStopReceivingRequested.set(true);
    }

    public void suspend() {
        // Each processor is associated with only one endpoint.
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "Suspend FTPBC Inbound Processor: " + this);
        }
        isSuspended.set(true);
    }

    public void resume() {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "Resume FTPBC Inbound Processor: " + this);
        }
        isSuspended.set(false);
    }

    public void handleSendFailure(MessagingException error, MessageExchange mex) {
        String messageId = (String) mex.getProperty(REDELIVERY_QOS_MSG_ID);
        if (mInboundExchanges.containsKey(messageId)) {
            // remove it from IB exchange map
            mInboundExchanges.remove(messageId);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "handleSendFailure() : messageID with QoS Redelivery =" + messageId + " removed from pending inbound exchange.");
            }
        } else {
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "handleSendFailure() : messageID with QoS Redelivery =" + messageId + " not in pending inbound exchange.");
            }
        }
    }

    /**
     * helper to log and alert
     * @param logLevel
     * @param keyCode
     * @param key
     * @param parms
     * @param ex
     */
    private void logAndAlert(Level logLevel, String keyCode, String key, Object[] parms, Exception ex) {
        String msg = parms != null ? mMessages.getString(key, parms) : mMessages.getString(key);
        if (mLogger.isLoggable(logLevel)) {
            if (ex != null) {
                mLogger.log(logLevel, msg, ex);
            } else {
                mLogger.log(logLevel, msg);
            }
        }
        AlertsUtil.getAlerter().critical(msg,
                FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                getServiceID(),
                AlertsUtil.getServerType(),
                AlertsUtil.COMPONENT_TYPE_BINDING,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT,
                keyCode);
    }

    private File getRecoveryLogBaseDir(boolean create) {
        return ((FTPBCPersistStore) CompositeLockRegistry.get(mEndpoint.getOperationUUID(mOperationName)).getPersistStore()).getRecoveryLogDir(create);
    }

    private void deleteRecoverLog(String uniqMsgID) {
        File baseDir = getRecoveryLogBaseDir(false);
        if (baseDir != null && baseDir.exists()) {
            File recoverlog = new File(baseDir, uniqMsgID.concat(FTPBCPersistStore.RECOVER_LOG_SUFFIX));
            if (recoverlog.exists() && recoverlog.isFile()) {
                recoverlog.delete();
            } else {
                logAndAlert(Level.WARNING,
                        "FTPBC-W004013",
                        "FTPBC-W004013.[ALERT].Recovery_Log_Not_Found",
                        new Object[]{
                            uniqMsgID,
                            recoverlog.getPath(),
                            mEndpoint.getServiceName(),
                            mEndpoint.getUniqueName()
                        },
                        null);
            }
        }
    }

    private void saveRecoverLog(FtpFileTransferNamesAndCommands tnc, String uniqMsgID, ListenerMeta meta) throws Exception {
        File baseDir = getRecoveryLogBaseDir(true);
        if (baseDir != null && baseDir.exists()) {
            PrintWriter pw = null;
            File tmpDir = new File(baseDir, "tmp_logs");
            tmpDir.mkdirs();
            File tmp = File.createTempFile("tmprecovery", null, tmpDir);
            tmp.deleteOnExit(); // register to be deleted on exit, if the code does not have a change to delete it

            try {
                pw = new PrintWriter(tmp);
                //pw = new PrintWriter();
                pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_PRE_DIR.concat(tnc.getPreDirectoryName() != null ? tnc.getPreDirectoryName() : ""));
                pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_PRE_FILE.concat(tnc.getPreFileName() != null ? tnc.getPreFileName() : ""));
                pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_DIR.concat(tnc.getTargetDirectoryName() != null ? tnc.getTargetDirectoryName() : ""));
                pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_FILE.concat(tnc.getTargetFileName() != null ? tnc.getTargetFileName() : ""));
                pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_POST_DIR.concat(tnc.getPostDirectoryName() != null ? tnc.getPostDirectoryName() : ""));
                pw.println(FTPBCPersistStore.RECOVER_LOG_ENTRY_POST_FILE.concat(tnc.getPostFileName() != null ? tnc.getPostFileName() : ""));
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "saveRecoverLog(): resolved remote directories/files: " + tnc.toString());
                }
            } finally {
                if (pw != null) {
                    pw.close();
                }
            }

            try {
                File dest = new File(baseDir, uniqMsgID.concat(FTPBCPersistStore.RECOVER_LOG_SUFFIX));
                if (!tmp.renameTo(dest)) {
                    throw new Exception("Failed to create recover log entry for : msgID : " + uniqMsgID);
                }
                meta.setRecoverLogEntry(dest);
            } finally {
                tmp.delete();
            }
        } else {
            throw new Exception("Failed to create recover logs directory to save recovery log : msgID : " + uniqMsgID);
        }
    }

    private void validateEndpoint() throws MessagingException {
        if (mMsgExchangeFactory == null) {
            mMsgExchangeFactory = mChannel.createExchangeFactory();
        }
        if (mServiceEndpoint == null) {
            mServiceEndpoint = locateServiceEndpoint();
        }
        if (mServiceEndpoint == null) {
            String msg = mMessages.getString("FTPBC-E004029.[ALERT].IMP_Failed_locate_EP",
                    new Object[]{mEndpoint.getServiceName(), mEndpoint.getEndpointName()});
            AlertsUtil.getAlerter().critical(msg,
                    FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                    getServiceID(),
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FTPBC-E004029");
            throw new MessagingException(msg);
        }
    }

    /**
     * 
     * @param meta
     * @param messageId
     */
    private void processACK(ListenerMeta meta, String messageId) {
        //
        // Important Note:
        // POST transfer operation must be paired with PRE operation,
        // in another word, the inbound poller must move or copy the file
        // to be processed aside into a staging area to start processing (retrieving)
        // and for further processing (e.g. apply POST operation on it)
        // this contraint prevents racing condition when doing POST operation
        // here, since we are no longer in the semaphore protected CRITICAL REGION
        //
        if (meta.getRecoverLogEntry() != null) {
            try {
                if (meta.getRecoverLogEntry().exists()) {
                    meta.getRecoverLogEntry().delete();
                }
            } catch (Exception e) {
                // ignore
            }
            if (meta.getCommand() != InboundMessageProcessor.PRE_POST_TRANSFER_CMDS.NONE) {
                doPostOperation(meta, false);
            }
        } else {
            // without CRITICAL region synchronization,
            // still do the post processing
            // but give warning - you are warned
            if (meta.getCommand() != InboundMessageProcessor.PRE_POST_TRANSFER_CMDS.NONE) {
                doPostOperation(meta, true);
            }
        }
    }

    private void doPostOperation(ListenerMeta meta, boolean not_in_critical_region) {
        if (meta.getCommand() == InboundMessageProcessor.PRE_POST_TRANSFER_CMDS.DELETE || meta.getCommand() == InboundMessageProcessor.PRE_POST_TRANSFER_CMDS.RENAME) {

            Connection connection = getFTPInterfaceAndValidate(meta.getConnectionParams());

            if (connection == null) {
                return;
            }

            FtpInterface ftp = (FtpInterface) connection.getClientObject();
            FtpFileClient client = ftp.getClient();
            FtpFileProvider provider = ftp.getProvider();

            Utils.prepareFTPInterface(client, provider, meta.getSubBindingType());

            try {
                if (!client.isConnected()) {
                    client.connect();
                }
            } catch (Exception ex) {
                if (connection != null) {
                    connection.discard();
                    connection = null;
                }
                logAndAlert(Level.SEVERE,
                        "FTPBC-E004079",
                        "FTPBC-E004079.[ALERT].Exception_when_attempt_connect",
                        new Object[]{
                            connection.getKey(),
                            ex.getLocalizedMessage()
                        },
                        ex);
                // problem on the connection:
                // can be permenent (wrong user credential) or temporary (network outage)
                // but currently the ftp client API error and exception reporting
                // does not provide accurate enough info for the caller to do the 
                // appropriate handling, and also for temporary conditions there is not
                // a re-try defined yet, so
                //
                // 
                return;
            }
            boolean operationOK = false;
            String operation = null;
            try {
                // for ftp:transfer
                // when pre dir / file does not exists
                // fall back to dir / file
                String dir = meta.getDir();
                String file = meta.getFile();
                String preDir = meta.getPreDir();
                String preFile = meta.getPreFile();
                String postDir = meta.getPostDir();
                String postFile = meta.getPostFile();
                // note, there is a shift of semantics for the inter-relationships 
                // among target dir/file, and pre/post dir / files for ftp:transfer inbound (receiveFrom):
                // now the file migration order is :
                // receiveFrom (dir + file): the target
                // pre receive loc (dir + file) : the inbound staging
                // post receive loc (dir + file) : the archive
                String opDir = preDir;
                String opFile = preFile;
                switch (meta.getCommand()) {
                    case DELETE:
                        operation = FtpFileConfiguration.CMD_DELETE;
                        if (meta.getSubBindingType() == InboundMessageProcessor.FTPBC_SUB_BINDING_TYPES.TRANSFER) {
                            if (preFile == null || preFile.trim().length() == 0) {
                                opDir = dir;
                                opFile = file;
                            }
                        }
                        // need to delete or move the staged file
                        // this could causing concurrency control issue
                        // outside CRITICAL region
                        if (not_in_critical_region) {
                            if (mLogger.isLoggable(Level.WARNING)) {
                                mLogger.log(Level.WARNING, "FTPBC-W004018.Post_Trans_Delete_Out_Critical_Region",
                                        new Object[]{
                                            opDir,
                                            opFile
                                        });
                            }
                        }
                        operationOK = provider.deleteFile(opDir, opFile);
                        break;
                    case RENAME:
                        operation = FtpFileConfiguration.CMD_RENAME;
                        // issue # 1579:
                        // collision occurs when rename a literal target file name, e.g. test.dat
                        // into archive directory second time...
                        // FTP return code 550: rename failed the file already exists ...
                        if (meta.getSubBindingType() == InboundMessageProcessor.FTPBC_SUB_BINDING_TYPES.MESSAGE) {
                            FtpFileConfiguration config = ftp.getConfiguration();
                            if (!config.getTargetFileNameIsPattern()) {
                                // literal target name will poll the same file name
                                // repeatedly, and that can cause collision when
                                // moving the target file into another directory, e.g. archive area
                                postFile = postFile.concat(UUID.randomUUID().toString());
                            }
                        }
                        if (meta.getSubBindingType() == InboundMessageProcessor.FTPBC_SUB_BINDING_TYPES.TRANSFER) {
                            if (preFile == null || preFile.trim().length() == 0) {
                                opDir = dir;
                                opFile = file;
                            }
                        }
                        if (not_in_critical_region) {
                            if (mLogger.isLoggable(Level.WARNING)) {
                                mLogger.log(Level.WARNING, "FTPBC-W004019.Post_Trans_Rename_Out_Critical_Region",
                                        new Object[]{
                                            opDir,
                                            opFile,
                                            postDir,
                                            postFile
                                        });
                            }
                        }
                        operationOK = provider.archiveFile(opDir, opFile, postDir, postFile);
                        break;
                    default:
                }
                if (!operationOK) {
                    logAndAlert(Level.SEVERE,
                            "FTPBC-E004091",
                            "FTPBC-E004091.[ALERT].ERR_EXT_FTP_ACTION_FAIL",
                            new Object[]{
                                "doPostOperation(meta)",
                                operation,
                                provider.getReplyString()
                            }, null);
                }
            } catch (Exception ex) {
                logAndAlert(Level.SEVERE,
                        "FTPBC-E004092",
                        "FTPBC-E004092.[ALERT].ERR_EXT_FTP_METHOD_EXCEPTION",
                        new Object[]{
                            "doPostOperation(meta)",
                            operation,
                            provider.getReplyString()
                        }, ex);
            } finally {
                if (connection != null) {
                    try {
                        FTPBCConnectionManager.returnConnection(connection);
                    } catch (Exception ex) {
                        logAndAlert(Level.WARNING,
                                "FTPBC-E004080",
                                "FTPBC-E004080.[ALERT].Exception_return_connection",
                                new Object[]{
                                    connection.getKey(),
                                    ex.getLocalizedMessage()
                                },
                                ex);
                    }
                }
            }
        }
    }

    private String parseRecoveryEntry(File file, FtpFileTransferNamesAndCommands tnc) {
        BufferedReader br = null;
        String mexID = null;
        try {
            br = new BufferedReader(new FileReader(file));
            // assuming the order of path entries are:
            // pre dir
            String preDirEntry = br.readLine();
            preDirEntry = extractValue(FTPBCPersistStore.RECOVER_LOG_ENTRY_PRE_DIR, preDirEntry);
            String preFileEntry = br.readLine();
            preFileEntry = extractValue(FTPBCPersistStore.RECOVER_LOG_ENTRY_PRE_FILE, preFileEntry);
            String dirEntry = br.readLine();
            dirEntry = extractValue(FTPBCPersistStore.RECOVER_LOG_ENTRY_DIR, dirEntry);
            String fileEntry = br.readLine();
            fileEntry = extractValue(FTPBCPersistStore.RECOVER_LOG_ENTRY_FILE, fileEntry);
            String postDirEntry = br.readLine();
            postDirEntry = extractValue(FTPBCPersistStore.RECOVER_LOG_ENTRY_POST_DIR, postDirEntry);
            String postFileEntry = br.readLine();
            postFileEntry = extractValue(FTPBCPersistStore.RECOVER_LOG_ENTRY_POST_FILE, postFileEntry);
            tnc.setPreDirectoryName(preDirEntry);
            tnc.setPreFileName(preFileEntry);
            tnc.setTargetDirectoryName(dirEntry);
            tnc.setTargetFileName(fileEntry);
            tnc.setPostDirectoryName(postDirEntry);
            tnc.setPostFileName(postFileEntry);
            mexID = file.getName();
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "parseRecoveryEntry() : remote directories/files from recovery entry: " + tnc.toString());
            }
            if (mexID.endsWith(FTPBCPersistStore.RECOVER_LOG_SUFFIX)) {
                mexID = mexID.substring(0, mexID.lastIndexOf(FTPBCPersistStore.RECOVER_LOG_SUFFIX));
            } else {
                throw new IllegalArgumentException("Invalid recovery log file encountered, file=" + file.getPath());
            }
        } catch (Exception ex) {
            // log the error
            logAndAlert(Level.SEVERE,
                    "FTPBC-E004088",
                    "FTPBC-E004088.[ALERT].Error_Parsing_Recover_Log_Entry",
                    new Object[]{
                        file.getPath(),
                        ex.getLocalizedMessage()
                    },
                    ex);
        } finally {
            if (br != null) {
                try {
                    br.close();
                } catch (IOException ex) {
                    // ignore
                }
            }
        }
        return mexID;
    }

    private String extractValue(String prefix, String line) throws Exception {
        String value = null;
        if (line == null || line.trim().length() == 0) {
            throw new Exception(mMessages.getString("FTPBC-E004087.Bad_Recover_Log_Entry", new Object[]{
                        prefix, line == null ? "NULL" : ""
                    }));
        }
        if (!line.startsWith(prefix)) {
            throw new Exception(mMessages.getString("FTPBC-E004087.Bad_Recover_Log_Entry", new Object[]{
                        prefix, line
                    }));
        }
        if (line.length() > prefix.length()) {
            value = line.substring(prefix.length());
        } else {
            value = "";
        }
        return value;
    }

    private void postProcessingNormalization(String uniqMsgID,
            FtpFileProvider provider,
            FtpFileConfiguration config,
            Exception exception,
            FtpFileTransferNamesAndCommands tnc)
            throws FtpFileException, IOException, Exception {
        if (exception != null) {
            if (exception instanceof IOException) {
                // IO problem
                // best effort to bring FTP channel to a sync state
                try {
                    provider.completePendingCommand();
                } catch (Exception ex2) {
                    // ignore
                    ex2.printStackTrace();
                }
                throw new FtpFileException("IO Errro while normalizing message, e=" + exception.getLocalizedMessage(), exception);
            } else {
                // malformed message etc
                // do not subject to recovery
                if (!provider.completePendingCommand()) {
                    throw new FtpFileException(mMessages.getString("FTPBC-E004082.Error_Complete_Pending_FTP_Cmd",
                            new Object[]{
                                provider.getReplyCode(),
                                provider.getReplyString()
                            }));
                }
                // then mark the malformed message as error
                // only when it is in inbound staging area
                if (config.isPreTransferCommandRename() || config.isPreTransferCommandCopy()) {
                    if (!provider.archiveFile(tnc.getPreDirectoryName(),
                            tnc.getPreFileName(),
                            tnc.getPreDirectoryName(),
                            tnc.getPreFileName().concat(FTPBCPersistStore.MESSAGE_ERROR_LOG_SUFFIX))) {
                        // log error
                        String msg = mMessages.getString("FTPBC-E004095.ERR_EXT_FTP_ACTION_FAIL",
                                new Object[]{
                                    "mark as malformed message",
                                    "rename",
                                    provider.getReplyString()
                                });
                        if (mLogger.isLoggable(Level.SEVERE)) {
                            mLogger.log(Level.SEVERE, msg);
                        }
                        throw new FtpFileException(msg);
                    } else {
                        logAndAlert(Level.SEVERE,
                                "FTPBC-E004093",
                                "FTPBC-E004093.[ALERT].MALFORMED_MSG_WHEN_NORMALIZATION",
                                new Object[]{
                                    this.getClass().getName(),
                                    uniqMsgID,
                                    tnc.getPreDirectoryName(),
                                    tnc.getPreFileName().concat(FTPBCPersistStore.MESSAGE_ERROR_LOG_SUFFIX)
                                },
                                null);
                    }
                }
                // save error info for this message exchange
                try {
                    CompositeLock l = CompositeLockRegistry.get(mEndpoint.getOperationUUID(mOperationName));
                    Utils.saveMessageNormalizationFailureInfo(((FTPBCPersistStore) l.getPersistStore()).getMessageErrorDir4Norm(true), tnc, uniqMsgID, exception);
                } catch (Exception ex) {
                    if (mLogger.isLoggable(Level.SEVERE)) {
                        mLogger.log(Level.SEVERE,
                                "FTPBC-E004090.Exception_Saving_Malformed_Message_Info",
                                new Object[]{
                                    "normalization",
                                    uniqMsgID,
                                    ex.getLocalizedMessage()
                                });
                    }

                }
                deleteRecoverLog(uniqMsgID);
                // continue the normal error handling
                throw exception;
            }
        } else {
            if (!provider.completePendingCommand()) {
                throw new FtpFileException(mMessages.getString("FTPBC-E004082.Error_Complete_Pending_FTP_Cmd",
                        new Object[]{
                            provider.getReplyCode(),
                            provider.getReplyString()
                        }));
            }
        }
    }
}
