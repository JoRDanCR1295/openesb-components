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
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc;

import com.sun.jbi.ftpbc.ftp.FtpFileConfigConstants;
import com.sun.jbi.ftpbc.ftp.FtpInterface;
import com.sun.jbi.ftpbc.ftp.FtpFileClient;
import com.sun.jbi.ftpbc.ftp.FtpFileProvider;
import com.sun.jbi.ftpbc.Endpoint.EndpointState;
import com.sun.jbi.ftpbc.connection.Connection;
import com.sun.jbi.ftpbc.connection.ConnectionPool;
import com.sun.jbi.ftpbc.extensions.FTPAddress;
import com.sun.jbi.ftpbc.extensions.FTPInput;
import com.sun.jbi.ftpbc.extensions.FTPMessage;
import com.sun.jbi.ftpbc.extensions.FTPMessageExtension;
import com.sun.jbi.ftpbc.extensions.FTPOperation;
import com.sun.jbi.ftpbc.extensions.FTPOutput;
import com.sun.jbi.ftpbc.extensions.FTPTransfer;
import com.sun.jbi.ftpbc.extensions.FTPTransferExtension;
import com.sun.jbi.ftpbc.extensions.ProxyAddressURL;
import com.sun.jbi.ftpbc.ftp.FtpFileConfiguration;
import com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommands;
import com.sun.jbi.ftpbc.ftp.FtpFileTransferNamesAndCommandsPut;
import com.sun.jbi.ftpbc.ftp.connection.FTPBCConnectionManager;
import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;
import com.sun.jbi.ftpbc.management.FTPBCManagementMBean;
import com.sun.jbi.ftpbc.persistence.FTPBCPersistStore;
import com.sun.jbi.ftpbc.util.AlertsUtil;
import com.sun.jbi.ftpbc.util.FTPInputStreamWrapper;
import com.sun.jbi.ftpbc.util.NMPropertyUtil;
import com.sun.jbi.ftpbc.util.Utils;

import com.sun.jbi.bindings.synchronization.CompositeLockRegistry;
import com.sun.jbi.bindings.synchronization.CompositeLock;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.exchange.ExchangePattern;

import com.sun.encoder.EncoderException;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.Iterator;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;
import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.jbi.messaging.*;
import javax.management.MBeanException;
import javax.xml.namespace.QName;
import javax.xml.transform.TransformerException;

import net.java.hulp.measure.Probe;

/**
 * This class processes request and reply
 * messages received from the SE.
 *
 * Implementation notes:
 * number of outbound processor threads are configurable
 * by runtime configuration parameter: Number of Outbound Threads : default = 10
 * 
 * @author jfu
 */
public class OutboundMessageProcessor implements Runnable {

    private static Messages mMessages =
            Messages.getMessages(OutboundMessageProcessor.class);
    private static Logger mLogger = Messages.getLogger(OutboundMessageProcessor.class);
    private MessagingChannel mChannel;
    private Map mInboundExchanges;
    private Map mServiceUnits;
    private AtomicBoolean bStopRequested;
    private FTPDenormalizer mDenormalizer;
    private RuntimeConfiguration mRtCfg;
    private FTPBCManagementMBean mManagementMbean;
    private ProxyAddressURL mProxy;

    public OutboundMessageProcessor(MessagingChannel channel,
            Map serviceUnits,
            Map inboundMessageExchanges,
            RuntimeConfiguration runtimeConfig,
            FTPBCManagementMBean managementMbean) {
        mChannel = channel;
        mServiceUnits = serviceUnits;
        mInboundExchanges = inboundMessageExchanges;
        bStopRequested = new AtomicBoolean(false);
        mDenormalizer = new FTPDenormalizer();
        mRtCfg = runtimeConfig;
        mManagementMbean = managementMbean;
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
                    String redeliveryQoSMsgID = (String) msgExchange.getProperty(InboundMessageProcessor.REDELIVERY_QOS_MSG_ID);

                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D004001.OMP_Accept_msg", exchangeId));
                    }

                    boolean inbound = false;

                    if (redeliveryQoSMsgID != null && mInboundExchanges.containsKey(redeliveryQoSMsgID) && msgExchange.getRole().equals(MessageExchange.Role.CONSUMER)) {
                        inbound = true;
                    }

                    ListenerMeta listenerMeta = (ListenerMeta) mInboundExchanges.get(redeliveryQoSMsgID);

                    if (inbound) {
                        long invocationTime = listenerMeta.getRequestInvocationTime();
                        if (mLogger.isLoggable(Level.FINE)) {
                            long difference = System.currentTimeMillis() - invocationTime;
                            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D004002.OMP_Resp_Ex", new Object[]{exchangeId, difference}));
                        }
                    }

                    URI pattern = msgExchange.getPattern();

                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, mMessages.getString("FTPBC-D004003.OMP_Pattern", new Object[]{exchangeId, pattern}));
                    }

                    Endpoint endpoint = findEndpoint(msgExchange);

                    if (endpoint == null) {
                        String msg = mMessages.getString("FTPBC-E004041.[ALERT].OMP_no_endpoint_match",
                                new Object[]{msgExchange.getEndpoint().getServiceName(),
                                    msgExchange.getEndpoint().getEndpointName()
                                });
                        msgExchange.setError(new Exception(msg));
                        AlertsUtil.getAlerter().critical(msg,
                                FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                                null,
                                AlertsUtil.getServerType(),
                                AlertsUtil.COMPONENT_TYPE_BINDING,
                                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                                NotificationEvent.EVENT_TYPE_ALERT,
                                "FTPBC-E004041");
                        throw new Exception(msg);
                    }

                    QName operation = msgExchange.getOperation();
                    int status = endpoint.getState();
                    if (status != EndpointState.RUNNING) {
                        // Just ignore the message if the endpoint is not in the
                        // running state (i.e. stopped or shutdown).
                        String strState = (status == EndpointState.STOPPED ? "stopped" : "shutdown");

                        if (mLogger.isLoggable(Level.INFO)) {
                            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R004005.OMP_EP_state", new Object[]{endpoint.getEndpointName(), strState, exchangeId}));
                        }
                    } else {
                        switch (ExchangePattern.valueOf(msgExchange)) {
                            case IN_OUT:
                                if (mLogger.isLoggable(Level.FINE)) {
                                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D004004.OMP_Recv_InOut", exchangeId));
                                }
                                if (inbound) {
                                    processRequestReplyInbound((InOut) msgExchange, endpoint, operation, listenerMeta);
                                } else {
                                    processRequestReplyOutbound((InOut) msgExchange, endpoint, operation);
                                }
                                break;
                            case IN_ONLY:
                                if (mLogger.isLoggable(Level.FINE)) {
                                    mLogger.log(Level.FINE, mMessages.getString("FTPBC-D004005.OMP_Recv_InOnly", exchangeId));
                                }
                                if (inbound) {
                                    processOneWayInbound((InOnly) msgExchange, endpoint, listenerMeta);
                                } else {
                                    processOneWayOutbound((InOnly) msgExchange, endpoint, operation);
                                }
                                break;
                            case ROBUST_IN_ONLY:
                                if (mLogger.isLoggable(Level.WARNING)) {
                                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W004003.OMP_Not_supported_inonly", exchangeId));
                                }
                                break;
                            case IN_OPTIONAL_OUT:
                                if (mLogger.isLoggable(Level.WARNING)) {
                                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W004004.OMP_Not_supported_outonly", exchangeId));
                                }
                                break;
                            default:
                                if (mLogger.isLoggable(Level.SEVERE)) {
                                    mLogger.log(Level.SEVERE, mMessages.getString("FTPBC-E004102.OMP_Invalid_pattern", exchangeId));
                                }
                                return;
                        }
                    }
                }
            } while (!bStopRequested.get());
        } catch (Throwable ex) {
            String msg = mMessages.getString("FTPBC-E004042.[ALERT].OMP_Unexpected_exception", ex.getLocalizedMessage());
            AlertsUtil.getAlerter().critical(msg,
                    FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                    null,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FTPBC-E004042");
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg, ex);
            }
        }
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R004006.OMP_Complete_processing"));
        }
    }

    public void processRequestReplyOutbound(InOut inout, Endpoint endpoint, QName operation) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D004006.OMP_Processing_inout_outbound"));
        }

        if (inout.getStatus() == ExchangeStatus.DONE) {
            endpoint.getEndpointStatus().incrementReceivedDones();
        } else if (inout.getStatus() == ExchangeStatus.ERROR) {
            endpoint.getEndpointStatus().incrementReceivedErrors();
        } else {
            endpoint.getEndpointStatus().incrementReceivedRequests();
            NormalizedMessage inMsg = inout.getInMessage();
            FTPOperation ftpOperation = locateFTPOperation(operation, endpoint);
            FTPInput ftpInput = ftpOperation.getFTPOperationInput();
            FTPOutput ftpOutput = ftpOperation.getFTPOperationOutput();
            FTPAddress address = endpoint.getAddress();
            boolean isSolicit = isSolicit(ftpOperation);
            FTPTransferExtension extElem = null;

            // if runtime config EnableMProps = true,
            // and the message has associated with NMproperties
            // that overwrite the current endpoint binding info,
            // then use the endpoint binding info derived
            // from the NM properties.

            // note that runtime config parameter EnableNMProps = true
            // only indicate that dynamic endpoint binding is allowed,
            // but if the NM properties of the message does not
            // indicate that, then there is no dynamic endpoint binding.

            // to handle solicit get / receive in outbound
            // check if there is an empty <input> and there is a bound <output>
            // then it is an indication of outbound get (solicit get)
            if (mRtCfg.getEnableNMProps().booleanValue() && NMPropertyUtil.useDynamicEndpoint(inMsg)) {
                // fabricate address and ftpInput (or ftpOutput)
                // using values from NM props
                try {
                    address = NMPropertyUtil.fabricateAddress(inMsg, address);
                    if (ftpInput != null) {
                        extElem = ftpInput.getExtension();
                        if (extElem != null) {
                            if (extElem instanceof FTPMessage) {
                                extElem = NMPropertyUtil.fabricateMessage(inMsg, (FTPMessage) extElem);
                            } else {
                                extElem = NMPropertyUtil.fabricateTransfer(inMsg, (FTPTransfer) extElem);
                            }
                            ftpInput = new FTPInput();
                            ftpInput.setExtension(extElem);
                        }
                    }
                    if (ftpOutput != null) {
                        extElem = ftpOutput.getExtension();
                        if (extElem != null) {
                            if (extElem instanceof FTPMessage) {
                                extElem = NMPropertyUtil.fabricateMessage(inMsg, (FTPMessage) extElem);
                            } else {
                                extElem = NMPropertyUtil.fabricateTransfer(inMsg, (FTPTransfer) extElem);
                            }
                            ftpOutput = new FTPOutput();
                            ftpOutput.setExtension(extElem);
                        }
                    }
                } catch (Exception e) {
                    // blame the client for any surprise occurred here
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_CLIENT,
                            e,
                            "FTPBC-E004068",
                            "FTPBC-E004068.[ALERT].Exception_when_applying_NM_props",
                            new Object[]{
                                endpoint.getServiceUnitID(),
                                address.toString(),
                                extElem != null ? extElem.toString() : "",
                                e.getLocalizedMessage()
                            });
                    return;
                }
            }

            try {
                if (!isSolicit) {
                    if (ftpInput == null) {
                        throw new Exception(mMessages.getString("FTPBC-E004045.OMP_Invalid_No_Out_FtpInput", operation));
                    }
                    extElem = ftpInput.getExtension();
                    validateOutboundMessageExchangeProperties(address, ftpInput.getExtension(), operation);
                }
                // validate the output leg always
                extElem = ftpOutput.getExtension();
                validateResponseMessagePollProperties(address, ftpOutput, operation);
            } catch (Exception e) {
                // blame the client
                reportError(inout,
                        endpoint,
                        FTPBCComponentContext.FAULTCODE_CLIENT,
                        e,
                        "FTPBC-E004062",
                        "FTPBC-E004062.ValidationError",
                        new Object[]{
                            endpoint.getServiceUnitID(),
                            address.toString(),
                            extElem != null ? extElem.toString() : "",
                            e.getLocalizedMessage()
                        });
                return;
            }

            boolean putFailed = false;

            // this is INVOKE
            // have to gen GUID if messageCorrelate is on
            String msgID = null;

            if (!isSolicit) {
                msgID = (String) inMsg.getProperty(FTPBCComponentContext.NM_PROP_MESSAGE_ID);
                extElem = ftpInput.getExtension();
                if (extElem.getMessageCorrelate()) {
                    if (msgID == null || msgID.trim().length() == 0) {
                        msgID = UUID.randomUUID().toString();
                    }
                }

                // synchronized request / response processing
                // SE invoke: put request and get response
                try {
                    writeMessage(inMsg, endpoint, address, operation, msgID, extElem, true); // we are consumer sending a request
                } catch (Exception ex) {
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_SERVER,
                            ex,
                            "FTPBC-E004060",
                            "FTPBC-E004060.OMP_Failed_writing",
                            new Object[]{endpoint.getServiceUnitID(), address.toString(), extElem.toString(), ex});
                    putFailed = true;
                }

                if (!putFailed) {
                    // need to poll the response from the output
                    // in another thread;
                    ResponsePoller poller = null;

                    try {
                        poller = new ResponsePoller(this,
                                mChannel,
                                inout,
                                ftpOutput.getExtension(), // already applied with NM props associated with the message
                                address,
                                endpoint,
                                mRtCfg,
                                mProxy,
                                operation,
                                msgID);
                    } catch (Exception e) {
                        reportError(inout,
                                endpoint,
                                FTPBCComponentContext.FAULTCODE_SERVER,
                                e,
                                "FTPBC-E004070",
                                "FTPBC-E004070.Exception_Create_Resp_Poller",
                                new Object[]{
                                    endpoint.getServiceUnitID(),
                                    address.toString(),
                                    ftpOutput.getExtension() != null ? ftpOutput.getExtension().toString() : "",
                                    e.getLocalizedMessage()
                                });
                        return;
                    }

                    try {
                        Thread t = new Thread(poller);
                        t.setName("ftpbc-resp-poller-".concat(t.getName()));
                        t.start();
                    } catch (Exception e) {
                        reportError(inout,
                                endpoint,
                                FTPBCComponentContext.FAULTCODE_SERVER,
                                e,
                                "FTPBC-E004066",
                                "FTPBC-E004066.Error_Init_Start_Response_Poller",
                                new Object[]{
                                    endpoint.getServiceUnitID(),
                                    address.toString(),
                                    ftpOutput.getExtension() != null ? ftpOutput.getExtension().toString() : "",
                                    e.getLocalizedMessage()
                                });
                        return;
                    }
                }
            } else {
                // solicit get
                msgID = inout.getExchangeId();
                extElem = ftpOutput.getExtension();

                // prepare the connection to the FTP server
                Properties params = new Properties();

                try {
                    FtpClientParameterGenerator.createProperties(false,
                            params,
                            address,
                            extElem,
                            mProxy,
                            operation,
                            null, // for inbound if messageCorrelate is on - the UUID should be reverted from target name
                            false, // we are soliciter retrieving a target
                            mMessages,
                            mLogger);
                } catch (Exception ex) {
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_SERVER,
                            ex,
                            "FTPBC-E004026",
                            "FTPBC-E004026.[ALERT].Exception_Preparing_Conn_Parms",
                            new Object[]{
                                endpoint.getServiceName(),
                                endpoint.getEndpointName(),
                                ex
                            });
                    return;
                }

                params.put(FtpFileConfigConstants.C_P_FTP_PASSIVE_ON, mRtCfg.getUsePassiveFTP() != null && mRtCfg.getUsePassiveFTP().booleanValue() ? "Yes" : "No");

                // pass in the connection configuration parameters
                params.put(ConnectionPool.POOL_MIN_SIZE, mRtCfg.getConnectionPoolMinSize());
                params.put(ConnectionPool.POOL_MAX_SIZE, mRtCfg.getConnectionPoolMaxSize());
                params.put(Connection.CONN_MAX_IDEL_TIMEOUT, mRtCfg.getConnectionMaxIdleTimeout());

                Map nmProps = null;

                if (mRtCfg.getEnableNMProps().booleanValue()) {
                    // extract ftpbc specific NM properties info
                    // from address and extElement
                    nmProps = NMPropertyUtil.extractNMProperties(address, extElem);
                }

                Connection connection = null;
                FtpInterface ftp = null;
                FtpFileClient client = null;
                FtpFileProvider provider = null;
                FtpFileConfiguration config = null;

                Semaphore sema = null;
                String targetDir = null;
                String connKey = null;
                String semaKey = null; // when target dir is a pattern, the synch range covers all derived dirs

                // lookup a connection from the pool
                try {
                    connection = FTPBCConnectionManager.getConnection(params);
                } catch (Exception e) {
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_SERVER,
                            e,
                            "FTPBC-E004052",
                            "FTPBC-E004052.[ALERT].Failed_allocate_connection",
                            new Object[]{FtpFileConfiguration.getKey(params), e.getMessage()});
                    return;
                }

                if ((ftp = (FtpInterface) connection.getClientObject()) == null) {
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_SERVER,
                            null,
                            "FTPBC-E004074",
                            "FTPBC-E004074.[ALERT].FTP_Interface_Not_Available",
                            new Object[]{
                                this.getClass().getName(),
                                FtpFileConfiguration.getKey(params)
                            });
                    return;
                }

                if ((client = ftp.getClient()) == null) {
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_SERVER,
                            null,
                            "FTPBC-E004075",
                            "FTPBC-E004075.[ALERT].FTP_Client_Not_Available",
                            new Object[]{
                                this.getClass().getName(),
                                FtpFileConfiguration.getKey(params)
                            });
                    return;
                }

                if ((provider = ftp.getProvider()) == null) {
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_SERVER,
                            null,
                            "FTPBC-E004076",
                            "FTPBC-E004076.[ALERT].FTP_Provider_Not_Available",
                            new Object[]{
                                this.getClass().getName(),
                                FtpFileConfiguration.getKey(params)
                            });
                    return;
                }

                if ((config = ftp.getConfiguration()) == null) {
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_SERVER,
                            null,
                            "FTPBC-E004084",
                            "FTPBC-E004084.[ALERT].FTP_Config_Not_Available",
                            new Object[]{
                                this.getClass().getName(),
                                FtpFileConfiguration.getKey(params)
                            });
                    return;
                }

                Utils.prepareFTPInterface(client, provider,
                        extElem instanceof FTPTransfer ? InboundMessageProcessor.FTPBC_SUB_BINDING_TYPES.TRANSFER : InboundMessageProcessor.FTPBC_SUB_BINDING_TYPES.MESSAGE);

                targetDir = config.getTargetDirectoryName();
                connKey = connection.getKey();
                // when target dir is a pattern, the synch range covers all derived dirs
                semaKey = connKey.concat(targetDir);
                sema = RemoteTargetSemaphoreManager.get(semaKey);

                try {
                    sema.acquire();
                    if (!client.isConnected()) {
                        client.connect();
                    }
                } catch (Throwable ex) {
                    sema.release();
                    if (connection != null) {
                        connection.discard();
                        connection = null;
                    }
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_SERVER,
                            ex,
                            "FTPBC-E004063",
                            "FTPBC-E004063.FTP_Connection_error",
                            new Object[]{connKey, ex.getLocalizedMessage()});
                    return;
                }

                String dir = null;
                String file = null;
                String preDir = null;
                String preFile = null;
                InputStream is = null;
                FTPInputStreamWrapper inStream = null;
                boolean noTargetFound = false;

                CompositeLock lock = null;

                try {
                    lock = CompositeLockRegistry.get(endpoint.getOperationUUID(operation));
                    if (lock == null) {
                        reportError(inout,
                                endpoint,
                                FTPBCComponentContext.FAULTCODE_SERVER,
                                null,
                                "FTPBC-E004099",
                                "FTPBC-E004099.[ALERT].No_CompositeLock_Registered",
                                new Object[]{
                                    operation.toString(),
                                    Thread.currentThread().getName(),
                                    "processRequestReplyOutbound()"
                                });
                        return;
                    }

                    FtpFileTransferNamesAndCommands tnc = (FtpFileTransferNamesAndCommands) client.getResolvedNamesForGet();

                    tnc.resolveTargetLocation();
                    file = tnc.getTargetFileName();
                    dir = tnc.getTargetDirectoryName();

                    if (file != null && file.trim().length() > 0) {
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
                            client.doPreTransferGet(tnc);
                            is = provider.retrieveFileStream(preDir, preFile);
                        } else {
                            // no pre transfer specified
                            is = provider.retrieveFileStream(dir, file);
                        }

                        if (is == null) {
                            throw new FtpFileException(mMessages.getString("FTPBC-E004031.IMP_Invalid_Data"));
                        }

                        inStream = new FTPInputStreamWrapper(is);
                    } else {
                        // no target - return empty payload
                        noTargetFound = true;
                        //is = new ByteArrayInputStream("".getBytes());
                    }

                    NormalizedMessage returnMsg = null;
                    Exception exception = null;
                    Probe normMeasure = Probe.info(getClass(), endpoint.getUniqueName(), FTPBindingLifeCycle.PERF_CAT_NORMALIZATION);

                    try {
                        returnMsg = new FTPNormalizer().normalize(
                                inout,
                                operation,
                                endpoint,
                                extElem,
                                inStream,
                                true,
                                false);
                    } catch (IOException ioe) {
                        exception = ioe;
                    } catch (Exception ex) {
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

                    if (!noTargetFound) {
                        postProcessingNormalization(msgID, endpoint, operation, provider, config, exception, tnc);
                        // issue 1659 - avoid rename collision when target file is literal
                        doPostOperation(ftp, tnc, extElem);
                        //client.doPostTransferGet(tnc);                    // pass nm props on to the message
                    }

                    if (mRtCfg.getEnableNMProps().booleanValue()) {
                        NMPropertyUtil.mergeNMPropertiesResolvedParams4Get(tnc, nmProps, extElem instanceof FTPTransfer);
                        NMPropertyUtil.setNMProperties(returnMsg, nmProps);
                    }

                    inout.setOutMessage(returnMsg);
                    mChannel.send(inout);
                    endpoint.getEndpointStatus().incrementSentReplies();
                } catch (Throwable e) {
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_SERVER,
                            e,
                            "FTPBC-E004065",
                            "FTPBC-E004065.Exception_Retrieving_Target",
                            new Object[]{connKey, e.getLocalizedMessage()});
                } finally {
                    if (lock != null) {
                        lock.unlock();
                    }
                    sema.release();
                    // always return the connection to the pool
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
                                    endpoint.getUniqueName(),
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
    }

    public void processRequestReplyInbound(InOut inout, Endpoint endpoint, QName operation, ListenerMeta listenerMeta) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D004007.OMP_Processing_inout_inbound"));
        }
        if (inout.getStatus() == ExchangeStatus.DONE) {
            endpoint.getEndpointStatus().incrementReceivedDones();
        } else if (inout.getStatus() == ExchangeStatus.ERROR) {
            endpoint.getEndpointStatus().incrementReceivedErrors();
            boolean isRetry = isRedeliveryConfigured(inout);
            // redelivery
            // if redelivery onFailure = suspend
            // do it here
            if (isRetry) {
                RedeliveryConfig config = endpoint.getRedeliveryConfiguration();
                if (config.getFailure() == RedeliveryConfig.Failure.suspend) {
                    // Suspend endpoint thru the mbean.
                    try {
                        mManagementMbean.suspend(InboundReceiver.getKeyForIBProcessor(endpoint, inout.getOperation()));
                        // emit warning logs and send alerts 
                        logAndAlert(Level.WARNING,
                                endpoint.getUniqueName(),
                                "FTPBC-W004012",
                                "FTPBC-W004012.[ALERT].About_to_suspend_endpoint",
                                new Object[]{
                                    String.valueOf(endpoint.getServiceName()),
                                    endpoint.getEndpointName()
                                },
                                null);
                    } catch (MBeanException ex) {
                        logAndAlert(Level.SEVERE,
                                endpoint.getUniqueName(),
                                "FTPBC-E004053",
                                "FTPBC-E004053.[ALERT].Failed_to_suspend_endpoint",
                                new Object[]{
                                    String.valueOf(endpoint.getServiceName()),
                                    endpoint.getEndpointName(),
                                    ex.getTargetException().getMessage()
                                },
                                ex);
                    }
                }
            } else {
                processACKorNACK(inout, endpoint, listenerMeta);
            }
        } else {
            endpoint.getEndpointStatus().incrementReceivedReplies();
            NormalizedMessage inMsg = null;
            FTPOperation ftpOperation = null;
            FTPOutput ftpOutput = null;
            FTPAddress address = null;
            FTPTransferExtension extElem = null;

            try {
                try {

                    inMsg = inout.getOutMessage();
                    ftpOperation = locateFTPOperation(operation, endpoint);
                    ftpOutput = ftpOperation.getFTPOperationOutput();
                    address = endpoint.getAddress();
                    extElem = null;

                    // process dynamic endpoint binding here
                    if (mRtCfg.getEnableNMProps().booleanValue() && NMPropertyUtil.useDynamicEndpoint(inMsg)) {
                        // fabricate the address and ftpOutput 
                        // using info derived from NM props
                        address = NMPropertyUtil.fabricateAddress(inMsg, address);
                        if (ftpOutput != null) {
                            extElem = ftpOutput.getExtension();
                            if (extElem != null) {
                                if (extElem instanceof FTPMessage) {
                                    extElem = NMPropertyUtil.fabricateMessage(inMsg, (FTPMessage) extElem);
                                } else {
                                    extElem = NMPropertyUtil.fabricateTransfer(inMsg, (FTPTransfer) extElem);
                                }
                                ftpOutput = new FTPOutput();
                                ftpOutput.setExtension(extElem);
                            }
                        }
                    }
                } catch (Exception e) {
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_CLIENT,
                            e,
                            "FTPBC-E004068",
                            "FTPBC-E004068.[ALERT].Exception_when_applying_NM_props",
                            new Object[]{
                                endpoint.getServiceUnitID(),
                                address.toString(),
                                extElem != null ? extElem.toString() : "",
                                e.getLocalizedMessage()
                            });
                    return;
                }

                try {
                    validateRequestReplyInboundMessageExchangeProperties(address, ftpOutput, operation);
                } catch (Exception e) {
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_SERVER,
                            e,
                            "FTPBC-E004062",
                            "FTPBC-E004062.ValidationError",
                            new Object[]{
                                endpoint.getServiceUnitID(),
                                address.toString(),
                                ftpOutput != null ? ftpOutput.getExtension() : "",
                                e.getLocalizedMessage()
                            });
                    return;
                }

                try {
                    writeMessage(inMsg,
                            endpoint,
                            address,
                            operation,
                            listenerMeta.getMessageUUIDTag(),
                            ftpOutput.getExtension(),
                            false); // we are provider write out a response
                    inout.setStatus(ExchangeStatus.DONE);
                } catch (Exception ex) {
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_SERVER,
                            ex,
                            "FTPBC-E004060",
                            "FTPBC-E004060.OMP_Failed_writing",
                            new Object[]{
                                endpoint.getServiceUnitID(),
                                address.toString(),
                                ftpOutput.getExtension() != null ? ftpOutput.getExtension().toString() : "",
                                ex.getLocalizedMessage()
                            });
                    return;
                }

                try {
                    mChannel.send(inout);
                    endpoint.getEndpointStatus().incrementSentDones();
                } catch (Exception ex) {
                    reportError(inout,
                            endpoint,
                            FTPBCComponentContext.FAULTCODE_SERVER,
                            ex,
                            "FTPBC-W004008",
                            "FTPBC-W004008.OMP_Failed_inout_inbound",
                            new Object[]{ex});
                }
            } finally {
                try {
                    listenerMeta.getMessageExchangeReplyListener().processReplyMessage(inout);
                } catch (Throwable t) {
                    t.printStackTrace();
                }
            }
        }
    }

    /**
     * one way invoked by a consumer
     */
    public void processOneWayOutbound(InOnly inonly, Endpoint endpoint, QName operation) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, mMessages.getString("FTPBC-D004008.OMP_Processing_oneway_outbound"));
        }

        try {
            endpoint.getEndpointStatus().incrementReceivedRequests();
            NormalizedMessage inMsg = inonly.getInMessage();

            FTPOperation ftpOperation = locateFTPOperation(operation, endpoint);
            FTPInput ftpInput = ftpOperation.getFTPOperationInput();
            FTPAddress address = endpoint.getAddress();
            FTPTransferExtension extElem = null;

            // processing dynamic endpoint
            if (mRtCfg.getEnableNMProps().booleanValue() && NMPropertyUtil.useDynamicEndpoint(inMsg)) {
                // fabricate address and ftpInput
                // from NM props
                address = NMPropertyUtil.fabricateAddress(inMsg, address);
                if (ftpInput != null) {
                    extElem = ftpInput.getExtension();
                    if (extElem != null) {
                        if (extElem instanceof FTPMessage) {
                            extElem = NMPropertyUtil.fabricateMessage(inMsg, (FTPMessage) extElem);
                        } else {
                            extElem = NMPropertyUtil.fabricateTransfer(inMsg, (FTPTransfer) extElem);
                        }
                        ftpInput = new FTPInput();
                        ftpInput.setExtension(extElem);
                    }
                }
            }

            try {
                if (ftpInput == null) {
                    throw new Exception(mMessages.getString("FTPBC-E004045.OMP_Invalid_No_Out_FtpInput", operation));
                }
                validateOutboundMessageExchangeProperties(address, ftpInput.getExtension(), operation);
            } catch (Exception e) {
                reportError(inonly,
                        endpoint,
                        FTPBCComponentContext.FAULTCODE_CLIENT,
                        e,
                        "FTPBC-E004062",
                        "FTPBC-E004062.ValidationError",
                        new Object[]{
                            endpoint.getServiceUnitID(),
                            address.toString(),
                            ftpInput != null ? ftpInput.getExtension() : null,
                            e.getLocalizedMessage()
                        });
                return;
            }

            extElem = ftpInput.getExtension();

            String msgID = (String) inMsg.getProperty(FTPBCComponentContext.NM_PROP_MESSAGE_ID);

            if (extElem.getMessageCorrelate()) {
                if (msgID == null || msgID.trim().length() == 0) {
                    msgID = UUID.randomUUID().toString();
                }
            }

            try {
                writeMessage(inMsg,
                        endpoint,
                        address,
                        operation,
                        msgID,
                        extElem,
                        true); // should be a invoke - consumer
            } catch (Exception ex) {
                reportError(inonly,
                        endpoint,
                        FTPBCComponentContext.FAULTCODE_SERVER,
                        ex,
                        "FTPBC-E004060",
                        "FTPBC-E004060.OMP_Failed_writing",
                        new Object[]{
                            endpoint.getServiceUnitID(),
                            address.toString(),
                            extElem != null ? extElem.toString() : "",
                            ex.getLocalizedMessage()
                        });
                return;
            }

            inonly.setStatus(ExchangeStatus.DONE);
            mChannel.send(inonly);
            endpoint.getEndpointStatus().incrementSentDones();
        } catch (Exception ex) {
            reportError(inonly,
                    endpoint,
                    FTPBCComponentContext.FAULTCODE_SERVER,
                    ex,
                    "FTPBC-W004009",
                    "FTPBC-W004009.OMP_Failed_processing_oneway_outbound",
                    new Object[]{
                        ex.getLocalizedMessage()
                    });
        }
    }

    public void processOneWayInbound(InOnly inonly, Endpoint endpoint, ListenerMeta listenerMeta) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R004007.OMP_Processing_oneway_inbound"));
        }
        if (inonly.getStatus() == ExchangeStatus.DONE) {
            endpoint.getEndpointStatus().incrementReceivedDones();
            processACKorNACK(inonly, endpoint, listenerMeta);
        } else if (inonly.getStatus() == ExchangeStatus.ERROR) {
            endpoint.getEndpointStatus().incrementReceivedErrors();
            processACKorNACK(inonly, endpoint, listenerMeta);
            // if redelivery onFailure = suspend
            // do it here
            boolean isRetry = isRedeliveryConfigured(inonly);
            if (isRetry) {
                RedeliveryConfig config = endpoint.getRedeliveryConfiguration();
                if (config.getFailure() == RedeliveryConfig.Failure.suspend) {
                    // Suspend endpoint thru the mbean.
                    try {
                        mManagementMbean.suspend(InboundReceiver.getKeyForIBProcessor(endpoint, inonly.getOperation()));
                        // emit warning logs and send alerts 
                        logAndAlert(Level.WARNING,
                                endpoint.getUniqueName(),
                                "FTPBC-W004012",
                                "FTPBC-W004012.[ALERT].About_to_suspend_endpoint",
                                new Object[]{
                                    String.valueOf(endpoint.getServiceName()),
                                    endpoint.getEndpointName()
                                },
                                null);
                    } catch (MBeanException ex) {
                        logAndAlert(Level.SEVERE,
                                endpoint.getUniqueName(),
                                "FTPBC-E004053",
                                "FTPBC-E004053.[ALERT].Failed_to_suspend_endpoint",
                                new Object[]{
                                    String.valueOf(endpoint.getServiceName()),
                                    endpoint.getEndpointName(),
                                    ex.getTargetException().getMessage()
                                },
                                ex);
                    }
                }
            }
        } else {
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W004010.OMP_Unexpected_ME_status", new Object[]{inonly.getEndpoint(), inonly.getStatus()}));
            }
        }

    }

    public Endpoint findEndpoint(MessageExchange msgExchange) {
        Endpoint endpoint = null;
        for (Iterator it = mServiceUnits.values().iterator(); it.hasNext();) {
            ServiceUnit su = (ServiceUnit) it.next();
            for (Iterator it2 = su.getEndpoints().iterator();
                    it2.hasNext();) {
                Endpoint aEndPoint = (Endpoint) it2.next();
                QName serviceName = msgExchange.getEndpoint().getServiceName();
                String endpointName = msgExchange.getEndpoint().getEndpointName();

                if (aEndPoint.getServiceName().equals(serviceName) &&
                        aEndPoint.getEndpointName().equals(endpointName)) {
                    endpoint = aEndPoint;
                    if (endpoint.getServiceUnit() == null) {
                        endpoint.setServiceUnit(su);
                    }
                    endpoint.setServiceUnitID(su.getServiceUnitId());
                }
            }
        }
        return endpoint;
    }

    private boolean isSolicit(FTPOperation ftpOperation) {
        boolean result = false;
        if (ftpOperation != null) {
            FTPInput input = ftpOperation.getFTPOperationInput();
            FTPOutput output = ftpOperation.getFTPOperationOutput();
            if (input == null && output != null && output.getExtension() != null) {
                result = true;
            }
        }
        return result;
    }

    private void logAndAlert(Level logLevel, String serviceID, String keyCode, String key, Object[] parms, Exception ex) {
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
                serviceID,
                AlertsUtil.getServerType(),
                AlertsUtil.COMPONENT_TYPE_BINDING,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT,
                keyCode);
    }

    private void processACKorNACK(MessageExchange mex, Endpoint endpoint, ListenerMeta listenerMeta) {
        try {
            // ACK
            listenerMeta.getMessageExchangeReplyListener().processReplyMessage(mex);
        } catch (Exception ex) {
            logAndAlert(Level.SEVERE,
                    endpoint.getUniqueName(),
                    "FTPBC-W004011",
                    "FTPBC-W004011.[ALERT].OMP_Failed_processing_oneway_inbound",
                    new Object[]{
                        ex.getLocalizedMessage()
                    },
                    ex);
        }
    }

    /**
     * Write the message to a given file destination
     */
    private void writeMessage(NormalizedMessage msg,
            Endpoint endpoint,
            FTPAddress address,
            QName operation,
            String uuid,
            FTPTransferExtension extElem,
            boolean isConsumer) throws Exception {

        InputStream outputData = null;
        Probe denormMeasure = Probe.info(getClass(), endpoint.getUniqueName(), FTPBindingLifeCycle.PERF_CAT_DENORMALIZATION);

        Exception exception = null;

        try {
            outputData = mDenormalizer.denormalize(msg, operation, endpoint, extElem);
        } catch (IOException ex) {
            //
            throw ex;
        } catch (EncoderNotFoundException ex) {
            //
            throw ex;
        } catch (EncoderException ex) {
            //
            exception = ex;
        } catch (NormalizationException ex) {
            //
            exception = ex;
        } catch (TransformerException ex) {
            //
            exception = ex;
        } finally {
            if (denormMeasure != null) {
                denormMeasure.end();
            }
        }

        if (exception != null) {
            // all the exceptions fall thru here
            // indicate a malformed message or bad payload,
            // the corresponding exception stack is saved
            // to a local log to facilitate the diagnosis
            //
            try {
                CompositeLock l = CompositeLockRegistry.get(endpoint.getOperationUUID(operation));
                Utils.saveMessageNormalizationFailureInfo(((FTPBCPersistStore) l.getPersistStore()).getMessageErrorDir4DeNorm(true), null, uuid, exception);
            } catch (Exception ex) {
                // log error for malformed message handling
                // but do not interrupt regular error handling
                // i.e. error handling for message routing proceed...
                // by re-throwing the original exception from de-normalization;
                if (mLogger.isLoggable(Level.SEVERE)) {
                    mLogger.log(Level.SEVERE,
                            "FTPBC-E004090.Exception_Saving_Malformed_Message_Info",
                            new Object[]{
                                "de-normalization",
                                uuid,
                                ex.getLocalizedMessage()
                            });
                }
            }
            // rethrow
            throw exception;
        }

        if (outputData == null) {
            outputData = new ByteArrayInputStream("".getBytes());
        }

        Properties params = new Properties();

        try {
            FtpClientParameterGenerator.createProperties(
                    true,
                    params,
                    address,
                    extElem,
                    mProxy,
                    operation,
                    uuid,
                    isConsumer,
                    mMessages,
                    mLogger);
        } catch (Exception ex) {
            throw new Exception(mMessages.getString("FTPBC-E004026.[ALERT].Exception_Preparing_Conn_Parms",
                    new Object[]{
                        endpoint.getServiceName(),
                        endpoint.getEndpointName(),
                        ex
                    }),
                    ex);
        }

        params.put(FtpFileConfigConstants.C_P_FTP_PASSIVE_ON, mRtCfg.getUsePassiveFTP() != null && mRtCfg.getUsePassiveFTP().booleanValue() ? "Yes" : "No");

        // pass in the connection configuration parameters
        params.put(ConnectionPool.POOL_MIN_SIZE, mRtCfg.getConnectionPoolMinSize());
        params.put(ConnectionPool.POOL_MAX_SIZE, mRtCfg.getConnectionPoolMaxSize());
        params.put(Connection.CONN_MAX_IDEL_TIMEOUT, mRtCfg.getConnectionMaxIdleTimeout());

        Connection ftpConn = FTPBCConnectionManager.getConnection(params);
        assert ftpConn != null : "Can not obtain ftp connection in OutboundProcessor...";

        FtpInterface ftp = null;
        FtpFileClient client = null;
        FtpFileProvider provider = null;
        FtpFileConfiguration config = null;

        if ((ftp = (FtpInterface) ftpConn.getClientObject()) == null) {
            throw new Exception(mMessages.getString("FTPBC-E004074.[ALERT].FTP_Interface_Not_Available",
                    new Object[]{
                        this.getClass().getName(),
                        FtpFileConfiguration.getKey(params)
                    }));
        }

        if ((client = ftp.getClient()) == null) {
            throw new Exception(mMessages.getString("FTPBC-E004075.[ALERT].FTP_Client_Not_Available",
                    new Object[]{
                        this.getClass().getName(),
                        FtpFileConfiguration.getKey(params)
                    }));
        }

        if ((provider = ftp.getProvider()) == null) {
            throw new Exception(mMessages.getString("FTPBC-E004076.[ALERT].FTP_Provider_Not_Available",
                    new Object[]{
                        this.getClass().getName(),
                        FtpFileConfiguration.getKey(params)
                    }));
        }

        if ((config = ftp.getConfiguration()) == null) {
            throw new Exception(mMessages.getString("FTPBC-E004084.[ALERT].FTP_Config_Not_Available",
                    new Object[]{
                        this.getClass().getName(),
                        FtpFileConfiguration.getKey(params)
                    }));
        }

        Utils.prepareFTPInterface(client, provider,
                extElem instanceof FTPTransfer ? InboundMessageProcessor.FTPBC_SUB_BINDING_TYPES.TRANSFER : InboundMessageProcessor.FTPBC_SUB_BINDING_TYPES.MESSAGE);

        String targetDir = config.getTargetDirectoryName();
        String connKey = ftpConn.getKey();
        String semaKey = connKey.concat(targetDir); // when target dir is a pattern, the synch range covers all derived dirs

        // tunnel some objects so that name expansion class NamePattern could access
        // per operation persist base dir when necessary
        config.setCurrentEndpoint(endpoint);
        config.setCurrentOperationName(operation);

        Semaphore sema = RemoteTargetSemaphoreManager.get(semaKey);
        Exception err = null;

        try {
            sema.acquire();

            if (!client.isConnected()) {
                client.connect();
            }

            FtpFileTransferNamesAndCommandsPut tncp = (FtpFileTransferNamesAndCommandsPut) client.getResolvedNamesForPut();

            client.doPreTransferPut(tncp);

            String dir = null;
            String file = null;

            if (config.getStageEnabled()) {
                tncp.resolveStageLocation();
                dir = tncp.getStageDirectoryName();
                file = tncp.getStageFileName();
            } else {
                tncp.resolveTargetLocation();
                dir = tncp.getTargetDirectoryName();
                file = tncp.getTargetFileName();
            }

            // make sure the dest exists
            if (!provider.mkdirs(dir)) {
                throw new Exception(mMessages.getString("FTPBC-E004083.Error_Mkdir",
                        new Object[]{
                            dir,
                            provider.getReplyCode(),
                            provider.getReplyString()
                        }));
            }

            if (config.getAppend()) {
                if (!provider.appendFile(dir, file, outputData)) {
                    throw new Exception(mMessages.getString("FTPBC-E004100.Error_Append_File",
                            new Object[]{
                                dir,
                                file,
                                provider.getReplyCode(),
                                provider.getReplyString()
                            }));
                }
            } else {
                if (!provider.storeFile(dir, file, outputData)) {
                    throw new Exception(mMessages.getString("FTPBC-E004085.Error_Put_File",
                            new Object[]{
                                dir,
                                file,
                                provider.getReplyCode(),
                                provider.getReplyString()
                            }));
                }
            }

            outputData.close();

            if (config.getStageEnabled()) {
                if (!provider.archiveFile(dir, file, tncp.getTargetDirectoryName(), tncp.getTargetFileName())) {
                    throw new Exception(mMessages.getString("FTPBC-E004086.Error_Move_Stage_File",
                            new Object[]{
                                dir,
                                file,
                                tncp.getTargetDirectoryName(),
                                tncp.getTargetFileName(),
                                provider.getReplyCode(),
                                provider.getReplyString()
                            }));
                }
            }

            // do post put operation
            client.doPostTransferPut(tncp);

            // collect NM properties
            // and populate them in the normalized message
            // pass nm props on to the message
            if (mRtCfg.getEnableNMProps().booleanValue()) {
                Map nmProps = NMPropertyUtil.extractNMProperties(address, extElem);
                NMPropertyUtil.mergeNMPropertiesResolvedParams4Put(tncp, nmProps, extElem instanceof FTPTransfer);
                NMPropertyUtil.setNMProperties(msg, nmProps);
            }
        } catch (Exception e) {
            // throw the connection away - something bad happened
            if (ftpConn != null) {
                ftpConn.discard();
                ftpConn = null;
            }
            err = e;
        } finally {
            if (sema != null) {
                sema.release();
            }
            if (ftpConn != null) {
                try {
                    client.disconnect();
                } catch (Exception e) {
                    // ignore
                }

                try {
                    FTPBCConnectionManager.returnConnection(ftpConn);
                } catch (Exception ex) {
                    // ignore but log warning
                    if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING,
                                "FTPBC-E004080.[ALERT].Exception_return_connection",
                                new Object[]{
                                    connKey,
                                    ex.getLocalizedMessage()
                                });
                    }

                }
            }
        }

        if (err != null) {
            throw err;
        }
    }

    private FTPOperation locateFTPOperation(QName opname, Endpoint endpoint) {
        // use qualified name for FTP BC operation binding
        //return (FTPOperation) endpoint.getOperations().get(opname);
        FTPOperation fop = null;
        if (opname.getNamespaceURI() == null ||
                opname.getNamespaceURI().trim().length() == 0) {
            fop = (FTPOperation) endpoint.getOperations().get(new QName(endpoint.getServiceName().getNamespaceURI(), opname.getLocalPart()));
        } else {
            fop = (FTPOperation) endpoint.getOperations().get(opname);
        }
        return fop;
    }

    protected void validateRequestReplyInboundMessageExchangeProperties(FTPAddress address, FTPOutput ftpOutput, QName operationName) throws Exception {
        if (ftpOutput == null) {
            throw new Exception(mMessages.getString("FTPBC-E004043.OMP_Invalid_No_InOut_FtpOutput", operationName));
        }

        if (ftpOutput.getExtension() == null) {
            throw new Exception(mMessages.getString("FTPBC-E004044.OMP_Invalid_No_InOut_FtpTransfer", operationName));
        }

        if (address != null) {
            address.parse();
            address.validate(operationName);
            // check ftp user and password to overwrite
            // user and password inside URL
            if (!FtpClientParameterGenerator.isEmpty(address.getUser())) {
                address.getFTPURL().setUser(address.getUser());
            }

            if (!FtpClientParameterGenerator.isEmpty(address.getPassword())) {
                address.getFTPURL().setPassword(address.getPassword());
            }
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

        if (ftpOutput.getExtension() instanceof FTPTransferExtension) {
            ((FTPTransferExtension) ftpOutput.getExtension()).validate(operationName.toString());
        } else {
            throw new Exception(mMessages.getString("FTPBC-E004044.OMP_Invalid_No_InOut_FtpTransfer", operationName));
        }
    }

    protected void validateOutboundMessageExchangeProperties(FTPAddress address, FTPTransferExtension ftpExt, QName operationName) throws Exception {
        if (ftpExt == null) {
            throw new Exception(mMessages.getString("FTPBC-E004046.OMP_Invalid_No_Out_FtpTransfer", operationName));
        }

        if (address != null) {
            address.parse();
            address.validate(operationName);
            // check ftp user and password to overwrite
            // user and password inside URL
            if (!FtpClientParameterGenerator.isEmpty(address.getUser())) {
                address.getFTPURL().setUser(address.getUser());
            }

            if (!FtpClientParameterGenerator.isEmpty(address.getPassword())) {
                address.getFTPURL().setPassword(address.getPassword());
            }
        }

        // proxy now is in runtime config
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

        if (ftpExt != null) {
            ftpExt.validate(operationName.toString());
        }
    }

    protected void validateResponseMessagePollProperties(FTPAddress address, FTPOutput ftpOutput, QName operationName) throws Exception {
        if (address != null) {
            address.parse();
            address.validate(operationName);
            // check ftp user and password to overwrite
            // user and password inside URL
            if (!FtpClientParameterGenerator.isEmpty(address.getUser())) {
                address.getFTPURL().setUser(address.getUser());
            }

            if (!FtpClientParameterGenerator.isEmpty(address.getPassword())) {
                address.getFTPURL().setPassword(address.getPassword());
            }
        }

        /**
         *  By this time, the Output property should already been validated for the operation.
         *  But we will still validate here...
         */
        if (ftpOutput == null) {
            throw new Exception(mMessages.getString("FTPBC-E004048.RMP_Invalid_No_Out_FtpOutput", operationName));
        }

        if (ftpOutput.getExtension() == null) {
            throw new Exception(mMessages.getString("FTPBC-E004049.RMP_Invalid_No_Out_FtpTransfer", operationName));
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

        ((FTPTransferExtension) ftpOutput.getExtension()).validate(operationName.toString());
    }

    public void stopReceiving() {
        // stop receiving from NMR
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R004008.OMP_Stopped_thread"));
        }
        bStopRequested.set(true);
    }

    /**
     * needed by poller for a voluntary shutdown
     */
    public boolean isRequestedToStop() {
        return bStopRequested.get();
    }

    /** Package protected method
     *  Used solely for JUnit test purposes
     */
    void setFtpDenormalizer(FTPDenormalizer denormalizer) {
        mDenormalizer = denormalizer;
    }

    /**
     * helper
     * @param me
     * @param e
     * @param msg
     * @param faultCode
     * @param faultDetail
     * @throws MessagingException
     */
    private void setError(MessageExchange me, Throwable e, String msg,
            String faultCode, String faultDetail) throws MessagingException {
        me.setError(new Exception(msg, e));
        me.setProperty(FTPBCComponentContext.PROP_FAULTCODE, faultCode);
        me.setProperty(FTPBCComponentContext.PROP_FAULTSTRING, msg);
        me.setProperty(FTPBCComponentContext.PROP_FAULTACTOR, FTPBindingLifeCycle.SHORT_DISPLAY_NAME);
        me.setProperty(FTPBCComponentContext.PROP_FAULTDETAIL, faultDetail);
        me.setStatus(ExchangeStatus.ERROR); // MessagingException can be thrown here, but me has been populated with other properties
    }

    private void reportError(MessageExchange mex, Endpoint endpoint, String faultCode, Throwable exp, String msgCode, String msgKey, Object[] msgParms) {
        String msg = mMessages.getString(
                msgKey, msgParms);

        String detail = Utils.getStackTrace(exp);

        // log error including the stack of the original throwable
        if (mLogger.isLoggable(Level.SEVERE)) {
            mLogger.log(Level.SEVERE, msg + "\r\n" + detail);
        }

        // send alert, do not include stack trace for alert
        // to reduce the traffic
        AlertsUtil.getAlerter().critical(
                msg,
                FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                endpoint != null ? endpoint.getServiceUnitID() : "NULL ENDPOINT",
                AlertsUtil.getServerType(),
                AlertsUtil.COMPONENT_TYPE_BINDING,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT,
                msgCode);

        try {
            setError(mex, exp, msg, faultCode, detail);
        } catch (MessagingException me) {
            msg = mMessages.getString(
                    "FTPBC-E004067.[ALERT].Exception_when_create_fault",
                    new Object[]{me.getLocalizedMessage()});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg + "\r\n" + Utils.getStackTrace(me));
            }
            AlertsUtil.getAlerter().critical(
                    msg,
                    FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                    endpoint != null ? endpoint.getServiceUnitID() : "NULL ENDPOINT",
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FTPBC-E004067");
            if (endpoint != null) {
                endpoint.getEndpointStatus().incrementSentErrors();
            }
            return;
        }

        try {
            mChannel.send(mex);
        } catch (MessagingException me) {
            msg = mMessages.getString(
                    "FTPBC-E004061.[ALERT].Exception_when_send_error",
                    new Object[]{me.getLocalizedMessage()});
            if (mLogger.isLoggable(Level.SEVERE)) {
                mLogger.log(Level.SEVERE, msg + "\r\n" + Utils.getStackTrace(me));
            }
            AlertsUtil.getAlerter().critical(
                    msg,
                    FTPBindingLifeCycle.SHORT_DISPLAY_NAME,
                    endpoint != null ? endpoint.getServiceUnitID() : "NULL ENDPOINT",
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FTPBC-E004061");
        } finally {
            if (endpoint != null) {
                endpoint.getEndpointStatus().incrementSentErrors();
            }
        }
    }

    private void postProcessingNormalization(String uniqMsgID,
            Endpoint endpoint,
            QName operation,
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
                // not subject to recovery
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
                        String msg = mMessages.getString("FTPBC-E004091.[ALERT].ERR_EXT_FTP_ACTION_FAIL",
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
                        // alert and log error
                        // a malformed message encountered
                        logAndAlert(Level.SEVERE,
                                endpoint.getUniqueName(),
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
                if (uniqMsgID == null || uniqMsgID.trim().length() == 0) {
                    uniqMsgID = UUID.randomUUID().toString();
                    if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING, "FTPBC-W004016.MEX_ID_NULL_WHEN_PROCESS_NORMLAIZATION_ERR",
                                new Object[]{
                                    uniqMsgID
                                });
                    }
                }

                try {
                    CompositeLock l = CompositeLockRegistry.get(endpoint.getOperationUUID(operation));
                    Utils.saveMessageNormalizationFailureInfo(((FTPBCPersistStore) l.getPersistStore()).getMessageErrorDir4Norm(true), tnc, uniqMsgID, exception);
                } catch (Exception e) {
                    if (mLogger.isLoggable(Level.WARNING)) {
                        mLogger.log(Level.WARNING,
                                "FTPBC-W004017.Exception_Save_Denormalization_Failure_Info",
                                new Object[]{
                                    uniqMsgID,
                                    exception.getLocalizedMessage()
                                });
                    }
                }
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

    private boolean isRedeliveryConfigured(MessageExchange exchange) {
        RedeliveryStatus redeliveryStatus = Redelivery.getRedeliveryStatus(exchange);
        return (redeliveryStatus != null);
    }

    private void doPostOperation(FtpInterface ftp, FtpFileTransferNamesAndCommands tnc, FTPTransferExtension extElem) throws FtpFileException, IOException {
        String workingDirectoryName = null;
        String workingFileName = null;

        if (ftp.getConfiguration().isPreTransferCommandRename()) {
            workingDirectoryName = tnc.getPreDirectoryName();
            workingFileName = tnc.getPreFileName();
        } else {
            workingDirectoryName = tnc.getTargetDirectoryName();
            workingFileName = tnc.getTargetFileName();
        }

        // No qualified file is available to get. Nothing needs to do.
        if (workingDirectoryName.length() == 0 &&
                workingFileName.length() == 0) {
            return;
        }

        // 'None'
        if (tnc.getPostTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_NONE)) {
            return;
        }

        // 'Delete'
        if (tnc.getPostTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_DELETE)) {
            if (!ftp.getProvider().deleteFile(workingDirectoryName, workingFileName)) {
                String msg = mMessages.getString("FTPBC-E004095.ERR_EXT_FTP_ACTION_FAIL",
                        new Object[]{
                            "OutboundMessageProcessor:::doPostOperation()",
                            "delete",
                            ftp.getProvider().getReplyString()
                        });
                throw new FtpFileException(msg);
            }
        } else if (tnc.getPostTransferCommand().equalsIgnoreCase(FtpFileConfiguration.CMD_RENAME)) {
            // 'Rename'
            // note: for ftp rename function, if the target file exists,
            //       different ftp servers behave differently.
            //       For UNIX ftp server, the target file just is overwritten without extra message.
            //       For NT ftp server, we'll fail and get exception.
            //       Now we don't do extra work for this, we don't want to define unified behavior,
            //       we just follow the native behavior of the corresponding ftp server.
            if (workingDirectoryName.equals(tnc.getPostDirectoryName()) &&
                    workingFileName.equals(tnc.getPostFileName())) {
                if (mLogger.isLoggable(Level.WARNING)) {
                    mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W004100.WRN_EXT_FTP_OP_TO_SELF", new Object[]{
                                "OutboundMessageProcessor:::doPostOperation()",
                                tnc.getPostTransferCommand()}));
                }
            } else {
                String destDir = tnc.getPostDirectoryName();
                String destFile = tnc.getPostFileName();
                if (extElem instanceof FTPMessageExtension) {
                    if (!ftp.getConfiguration().getTargetFileNameIsPattern()) {
                        // literal target, to avoid rename collision
                        // add UUID suffix
                        destFile = destFile.concat(UUID.randomUUID().toString());
                    }
                }
                if (ftp.getProvider().archiveFile(
                        workingDirectoryName,
                        workingFileName,
                        destDir,
                        destFile)) {
                } else {
                    String msg = mMessages.getString("FTPBC-E004095.ERR_EXT_FTP_ACTION_FAIL",
                            new Object[]{
                                "OutboundMessageProcessor:::doPostOperation()",
                                "rename",
                                ftp.getProvider().getReplyString()
                            });
                    throw new FtpFileException(msg);
                }
            }
        }
    }
}
