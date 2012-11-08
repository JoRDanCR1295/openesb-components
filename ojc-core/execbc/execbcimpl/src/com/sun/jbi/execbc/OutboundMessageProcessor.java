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

package com.sun.jbi.execbc;

import ch.ethz.ssh2.ChannelCondition;
import ch.ethz.ssh2.Connection;
import ch.ethz.ssh2.Session;
import ch.ethz.ssh2.StreamGobbler;

import com.sun.jbi.execbc.extensions.Delimiters;
import com.sun.jbi.execbc.extensions.ExecAddress;
import com.sun.jbi.execbc.extensions.ExecInput;
import com.sun.jbi.execbc.extensions.ExecMessage;
import com.sun.jbi.execbc.extensions.ExecOperation;
import com.sun.jbi.execbc.extensions.ExecOutput;
import com.sun.jbi.execbc.extensions.Delimiters.Match;
import com.sun.jbi.execbc.util.ExecUtil;
import com.sun.jbi.execbc.util.OutputFilenameFormatter;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.util.Iterator;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import javax.jbi.messaging.*;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Document;


/**
 * This class processes request and reply
 * messages received from the SE.
 *
 * @author Sherry Weng
 */
public class OutboundMessageProcessor implements Runnable {
    private static Messages mMessages =
            Messages.getMessages(OutboundMessageProcessor.class);
    private static Logger mLogger = Messages.getLogger(OutboundMessageProcessor.class);
    
    private DeliveryChannel mChannel;
    private Map mInboundExchanges;
    private Map mServiceUnits;
    private Object mMonitor;
    private ExecDenormalizer mDenormalizer;
    private ExecNormalizer mNormalizer;
    
    public OutboundMessageProcessor(DeliveryChannel channel,
            Map serviceUnits,
            Map inboundMessageExchanges) {
        mChannel = channel;
        mServiceUnits = serviceUnits;
        mInboundExchanges = inboundMessageExchanges;
        mMonitor = new Object();
        mDenormalizer = new ExecDenormalizer();
        try {
            mNormalizer = new ExecNormalizer();
        } catch (Exception e) {
            String errMsg = mMessages.getString("OMP_Failed_to_init_normalizer",
                    new Object[0]);
            throw new RuntimeException(errMsg, e);
        }
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
//N/A EXEC BC
//                    boolean inbound = mInboundExchanges.containsKey(exchangeId) && 
//                            msgExchange.getRole().equals(MessageExchange.Role.CONSUMER);
//                    ListenerMeta listenerMeta = (ListenerMeta) mInboundExchanges.get(exchangeId);
//                    
//                    MessageExchangeReplyListener listener = null;
//                    if (listenerMeta != null) {
//                        listener = listenerMeta.getMessageExchangeReplyListener();
//                    }
//                    if (inbound) {
//                        long invocationTime = listenerMeta.getRequestInvocationTime();
//                        if (mLogger.isLoggable(Level.INFO)){
//                            long difference = System.currentTimeMillis() - invocationTime;
//                            mLogger.log(Level.INFO, "OMP_Resp_Ex", new Object[] {exchangeId, difference});
//                        }
//                    }
                    
                    URI pattern = msgExchange.getPattern();
                    if (mLogger.isLoggable(Level.FINE)){
                        mLogger.log(Level.FINE, "OMP_Pattern", new Object[] {exchangeId, pattern});
                    }
                    
                    Endpoint endpoint = findEndpoint(msgExchange);
                    if (endpoint == null) {
                        String errMsg = mMessages.getString("OMP_no_endpoint_match",
                                new Object[] {msgExchange.getEndpoint().getServiceName(),
                                msgExchange.getEndpoint().getEndpointName()});
                        msgExchange.setError(new Exception(errMsg));
                        throw new Exception(errMsg);
                    }
                    
                    QName operation = msgExchange.getOperation();
                    switch(ExchangePattern.valueOf(msgExchange)) {
                    case IN_OUT:
                        mLogger.log(Level.INFO, "OMP_Recv_InOut", exchangeId);
//N/A EXEC BC
//                        if (inbound) {
//                            processRequestReplyInbound((InOut)msgExchange, endpoint, operation, listener);
//                        } else {
                            processRequestReplyOutbound((InOut)msgExchange, endpoint, operation);
//                        }
                        break;
                    case IN_ONLY:
                        mLogger.log(Level.INFO, "OMP_Recv_InOnly", exchangeId);
//N/A EXEC BC
//                        if (inbound) {
//                            processOneWayInbound((InOnly)msgExchange, endpoint, listener);
//                        } else {
                            processOneWayOutbound((InOnly)msgExchange, endpoint, operation);
//                        }
                        break;
                    case ROBUST_IN_ONLY:
                        mLogger.log(Level.WARNING, "OMP_Not_supported_inonly", exchangeId);
                        break;
                    case IN_OPTIONAL_OUT:
                        mLogger.log(Level.WARNING, "OMP_Not_supported_outin", exchangeId);
                        break;
                    default:
                        mLogger.log(Level.WARNING, "OMP_Invalid_pattern", exchangeId);
                        return;
                    }
                }
            } while (mMonitor != null);
        } catch (Throwable ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("OMP_Unexpected_exception", ex.getLocalizedMessage()), ex);
        }
        
        mLogger.log(Level.INFO, "OMP_Complete_processing");
    }
    
    public void processRequestReplyOutbound(InOut inout, Endpoint endpoint, QName operation) {
        mLogger.info("OMP_Processing_inout_outbound");
        
        if (inout.getStatus() == ExchangeStatus.DONE) {
            endpoint.getEndpointStatus().incrementReceivedDones();
        } else if (inout.getStatus() == ExchangeStatus.ERROR) {
            endpoint.getEndpointStatus().incrementReceivedErrors();
        } else {
            endpoint.getEndpointStatus().incrementReceivedRequests();
            try {
                NormalizedMessage inMsg = inout.getInMessage();
                
                ExecOperation execOperation = locateExecOperation(operation, endpoint);
                ExecInput execInput = execOperation.getExecOperationInput();
                ExecOutput execOutput = execOperation.getExecOperationOutput();

                try {
                    validateOutboundMessageExchangeProperties(execOperation, operation);
                } catch (Exception e) {
                    mLogger.log(Level.SEVERE, e.getLocalizedMessage());
                    endpoint.getEndpointStatus().incrementSentErrors();
                    inout.setError(e);
                    inout.setStatus(ExchangeStatus.ERROR);
                    mChannel.send(inout);
                    return;
                }
                
                NormalizedMessage outMsg;
                try {
                    ExecMessage execMessage = execInput.getExecMessage();
                    byte[] result =
                        executeCommand(inMsg, endpoint, operation,
                                execInput.getExecMessage(), execOutput.getExecMessage());
                    //Normalize the result
                    outMsg = mNormalizer.normalize(inout,
                            operation,
                            endpoint,
                            execOutput.getExecMessage(),
                            result);
                    inout.setOutMessage(outMsg);
                } catch (Exception ex) {
                    mLogger.log(Level.WARNING, mMessages.getString("OMP_Failed_writing", ex));
                    // populate a full fault instead?
                    inout.setError(ex);
                    String statusMessage = mMessages.getString("OMP_Failed_writing", ex.getMessage());
                    // Send back a "Status" message
                    WrapperBuilder builder = HelperFactory.createBuilder();
                    Document doc = builder.getStatusDocument(statusMessage);
                    DOMSource source = new DOMSource(doc);
                    outMsg = inout.createMessage();
                    outMsg.setContent(source);
                    outMsg.setProperty("MessageType", "Status");
                    inout.setOutMessage(outMsg);
                }
                
                mChannel.send(inout);
                endpoint.getEndpointStatus().incrementSentReplies();
            } catch (Exception ex) {
                mLogger.log(Level.WARNING, mMessages.getString("OMP_Failed_processing_inout_outbound", ex));
            }
        }
    }

    
//N/A EXEC BC
//    public void processRequestReplyInbound(InOut inout, Endpoint endpoint, QName operation, MessageExchangeReplyListener listener) {
//        mLogger.info("OMP_Processing_inout_inbound");
//        
//        if (inout.getStatus() == ExchangeStatus.DONE) {
//            endpoint.getEndpointStatus().incrementReceivedDones();
//        } else if (inout.getStatus() == ExchangeStatus.ERROR) {
//            endpoint.getEndpointStatus().incrementReceivedErrors();
//        } else {
//            endpoint.getEndpointStatus().incrementReceivedReplies();
//            try {
//                NormalizedMessage inMsg = inout.getOutMessage();
//                boolean success = true;
//                
//                ExecOperation fileOperation = locateFileOperation(operation, endpoint);
//                ExecOutput fileOutput = fileOperation.getFileOperationOutput();
//                try {
//                    validateRequestReplyInboundMessageExchangeProperties(fileOperation, operation);
//                } catch (Exception e) {
//                    mLogger.log(Level.SEVERE, e.getLocalizedMessage());
//                    endpoint.getEndpointStatus().incrementSentErrors();
//                    inout.setError(e);
//                    inout.setStatus(ExchangeStatus.ERROR);
//                    mChannel.send(inout);
//                    return;
//                }
//                
//                ExecMessage fileMessage = fileOutput.getExecMessage();
//                try {
//                    writeMessage(inMsg, endpoint, operation, fileMessage);
//                } catch (Exception ex) {
//                    mLogger.log(Level.WARNING, "", ex);
//                    inout.setError(ex);
//                    success = false;
//                } finally {
//                    if (success) {
//                        inout.setStatus(ExchangeStatus.DONE);
//                        endpoint.getEndpointStatus().incrementSentDones();
//                    } else {
//                        inout.setStatus(ExchangeStatus.ERROR);
//                        endpoint.getEndpointStatus().incrementSentErrors();
//                    }
//                }
//                mChannel.send(inout);
//                listener.processReplyMessage(inout);
//            } catch (Exception ex) {
//                mLogger.log(Level.WARNING, mMessages.getString("OMP_Failed_inout_inbound"), ex);
//            }
//        }
//    }


    public void processOneWayOutbound(InOnly inonly, Endpoint endpoint, QName operation) {
        mLogger.info("OMP_Processing_oneway_outbound");
        
        if (inonly.getStatus() == ExchangeStatus.DONE) {
            endpoint.getEndpointStatus().incrementReceivedDones();
            return;
        }
        if (inonly.getStatus() == ExchangeStatus.ERROR) {
            endpoint.getEndpointStatus().incrementReceivedErrors();
            return;
        }
        try {
            endpoint.getEndpointStatus().incrementReceivedRequests();
            NormalizedMessage inMsg = inonly.getInMessage();
            boolean success = true;
            
            ExecOperation execOperation = locateExecOperation(operation, endpoint);
            ExecInput execInput = execOperation.getExecOperationInput();
            
            try {
                validateOutboundMessageExchangeProperties(execOperation, operation);
            } catch (Exception e) {
                mLogger.log(Level.SEVERE, e.getLocalizedMessage());
                endpoint.getEndpointStatus().incrementSentErrors();
                inonly.setError(e);
                inonly.setStatus(ExchangeStatus.ERROR);
                mChannel.send(inonly);
                return;
            }
            
            try {
                executeCommand(inMsg, endpoint, operation, execInput.getExecMessage(), null);
                inonly.setStatus(ExchangeStatus.DONE);
            } catch (Exception ex) {
                success = false;
                mLogger.log(Level.WARNING, mMessages.getString("OMP_Failed_writing", ex));
                inonly.setError(ex);
                inonly.setStatus(ExchangeStatus.ERROR);
            }
            
            mChannel.send(inonly);
            if (success) {
                endpoint.getEndpointStatus().incrementSentDones();
            } else {
                endpoint.getEndpointStatus().incrementSentErrors();
            }
        } catch (Exception ex) {
            mLogger.log(Level.WARNING, mMessages.getString("OMP_Failed_processing_oneway_outbound"), ex);
        }
    }

//N/A EXEC BC
//    public void processOneWayInbound(InOnly inonly, Endpoint endpoint, MessageExchangeReplyListener listener) {
//        mLogger.info("OMP_Processing_oneway_inbound");
//        
//        if (inonly.getStatus() == ExchangeStatus.DONE) {
//            endpoint.getEndpointStatus().incrementReceivedDones();
//        } else if (inonly.getStatus() == ExchangeStatus.ERROR) {
//            endpoint.getEndpointStatus().incrementReceivedErrors();
//        } else {
//            mLogger.log(Level.WARNING, "OMP_Unexpected_ME_status",
//                    new Object[] {inonly.getEndpoint(), inonly.getStatus()});
//        }
//        try {
//            listener.processReplyMessage(inonly);
//        } catch (Exception ex) {
//            mLogger.log(Level.WARNING, mMessages.getString("OMP_Failed_processing_oneway_inbound"), ex);
//        }
//    }

    public Endpoint findEndpoint(MessageExchange msgExchange) {
        Endpoint endpoint = null;
        for (Iterator it = mServiceUnits.values().iterator(); it.hasNext();) {
            for (Iterator it2 = ((ServiceUnit)it.next()).getEndpoints().iterator();
            it2.hasNext();) {
                Endpoint aEndPoint = (Endpoint)it2.next();
                QName serviceName = msgExchange.getEndpoint().getServiceName();
                String endpointName = msgExchange.getEndpoint().getEndpointName();
                
                if (aEndPoint.getServiceName().equals(serviceName) &&
                        aEndPoint.getEndpointName().equals(endpointName)) {
                    endpoint = aEndPoint;
                }
            }
        }
        
        return endpoint;
    }
    
    /**
     * Executes a command locally or remotely using SSH-2
     */
    byte[] executeCommand(NormalizedMessage msg,
            Endpoint endpoint,
            QName operationName,
            ExecMessage inputMessage,
            ExecMessage outputMessage) throws Exception {
        
        ExecAddress address = endpoint.getExecAddress();
        ExecOperation operation = locateExecOperation(operationName, endpoint);

        String endpointKey = endpoint.getServiceName().toString() +
                endpoint.getEndpointName();

        if (msg == null) {
            throw new Exception(mMessages.getString("OMP_Invalid_null_nmsg"));
        }

        byte[] cmdParameters = mDenormalizer.denormalize(msg,
                operationName,
                endpoint,
                inputMessage);
        
        String command = operation.getCommand();
        if (cmdParameters.length > 0) {
            command = command + " " + new String(cmdParameters);
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "OMP_Executing_command", command);
        }

        String host = address.getHostName();
        if (host == null || host.length() == 0) {
            host = "localhost";
        }
        ByteArrayOutputStream stdoutStream = new ByteArrayOutputStream();
        byte[] buf = new byte[2048];
        int read;
        if (ExecUtil.isRemote(host)) {
            //Execute the command on a remote machine using SSH
            Connection conn = new Connection(host);
            conn.connect();
            boolean isAuthenticated =
                conn.authenticateWithPassword(address.getUserName(),
                        address.getPassword());

            if (isAuthenticated == false) {
                conn.close();
                throw new IOException("Authentication failed.");
            }
            
            Session sess = conn.openSession();
            sess.execCommand(command);
            InputStream stdout = new StreamGobbler(sess.getStdout());
            InputStream errout = new StreamGobbler(sess.getStderr());
            sess.waitForCondition(ChannelCondition.EXIT_STATUS, 5000);
            int errorCode;
            if ((errorCode = sess.getExitStatus()) != 0) {
                BufferedReader br =
                    new BufferedReader(new InputStreamReader(errout));
                StringBuilder sb = new StringBuilder();
                sb.append("Error code=" + errorCode + ", ");
                while (true) {
                    String line = br.readLine();
                    if (line == null) {
                        break;
                    }
                    sb.append(line);
                }
                sess.close();
                conn.close();
                throw new IOException("Execution of command: '" + command
                        + "' on remote host: '" + address.getHostName()
                        + "' failed. " + sb.toString());
            } else {
                while ((read = stdout.read(buf)) >= 0) {
                    stdoutStream.write(buf, 0, read);
                }
            }
            sess.close();
            conn.close();
        } else {
            Process proc = Runtime.getRuntime().exec(command);
            InputStream stdout = new StreamGobbler(proc.getInputStream());
            InputStream errout = new StreamGobbler(proc.getErrorStream());
            int errorCode;
            if ((errorCode = proc.waitFor()) != 0) {
                BufferedReader br =
                    new BufferedReader(new InputStreamReader(errout));
                StringBuilder sb = new StringBuilder();
                sb.append("Error code=" + errorCode + ", ");
                while (true) {
                    String line = br.readLine();
                    if (line == null) {
                        break;
                    }
                    sb.append(line);
                }
                stdout.close();
                errout.close();
                if (proc.getOutputStream() != null) {
                    proc.getOutputStream().close();
                }
                throw new IOException("Execution of command: '" + command
                        + "' on local host failed. " + sb.toString());
            } else {
                while ((read = stdout.read(buf)) >= 0) {
                    stdoutStream.write(buf, 0, read);
                }
                stdout.close();
                if (proc.getOutputStream() != null) {
                    proc.getOutputStream().close();
                }
                errout.close();
            }
        }
        
        if (outputMessage == null) {
            stdoutStream.close();
            return null;
        }
        
        byte[] result = stdoutStream.toByteArray();
        stdoutStream.close();
        Delimiters delims = outputMessage.getDelimitersOfRecord();
        ByteArrayOutputStream recDataStream = new ByteArrayOutputStream();
        if (outputMessage.getInjectContextInfo()) {
            recDataStream.write(host.getBytes());
            recDataStream.write("\n".getBytes());
            recDataStream.write(command.getBytes());
            recDataStream.write("\n".getBytes());
        }
        if (delims != null) {
            int skipCount = outputMessage.getRecordsToBeSkipped();
            Match match;
            int start = 0;
            int len = result.length;
            int recordCount = 0;
            while ((recordCount < skipCount)
                    && (match = delims.match(result, start, len)) != null) {
                start = match._nextStart;
                len = result.length - start;
                recordCount++;
            }
            if (len > 0)
            recDataStream.write(result, start, len);
        } else {
            recDataStream.write(result);
        }
        result = recDataStream.toByteArray();
        recDataStream.close();
        return result;
    }

    private ExecOperation locateExecOperation(QName opname, Endpoint endpoint) {
        PortType portType = getPortType(endpoint);
        return (ExecOperation) endpoint.getExecOperations().get(
            new QName(portType.getQName().getNamespaceURI(), opname.getLocalPart()));
    }

    private PortType getPortType(Endpoint endpoint) {
        String serviceName = endpoint.getServiceName().toString();
        String endpointName = endpoint.getEndpointName();
        Definition def = endpoint.getDefinition();
        Map services = def.getServices();

        // DO NOT use the getService() method.
        // It checks all imported WSDLs.
        Service svc = (Service)services.get(QName.valueOf(serviceName));
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

//N/A EXEC BC
//    protected void validateRequestReplyInboundMessageExchangeProperties(ExecOperation operation, QName operationName) throws Exception {
//        // Need to validate that file:write properties are available and valid
//        ExecOutput fileOutput = operation.getFileOperationOutput();
//        
//        /**
//         *  By this time, the Output property should already been validated for the operation.
//         *  But we will still validate here...
//         */
//        if (fileOutput == null) {
//            throw new Exception(mMessages.getString("OMP_Invalid_No_InOut_FileOutput", operationName));
//        }
//        
//        ExecMessage fileMessage = fileOutput.getExecMessage();
//        if (fileMessage == null) {
//            throw new Exception(mMessages.getString("OMP_Invalid_No_InOut_FileMessage", operationName));
//        }
//        
//        if (fileMessage.getFileName() == null) {
//            throw new Exception(mMessages.getString("OMP_Invalid_No_InOut_FileMessage_Name", operationName));
//        }
//        
//        if (!fileMessage.getFileType().equals(ExecMessage.FILE_TYPE_TEXT) &&
//                !fileMessage.getFileType().equals(ExecMessage.FILE_TYPE_BINARY)) {
//            throw new Exception(mMessages.getString("OMP_Invalid_InOut_File_Type", new Object[] {fileMessage.getFileType(), operationName}));
//        }
//        
//        if (fileMessage.getFileUseType().equals(ExecMessage.FILE_USE_TYPE_ENCODED) &&
//                (fileMessage.getFileEncodingStyle() == null ||
//                fileMessage.getFileEncodingStyle().equals(""))) {
//            throw new Exception(mMessages.getString("OMP_Invalid_No_InOut_FileMessage_EncodingStyle", operationName));
//        }
//    }
//    
    protected void validateOutboundMessageExchangeProperties(ExecOperation operation, QName operationName) throws Exception {
        // Need to validate that file:write properties are available and valid
        ExecInput execInput = operation.getExecOperationInput();
        
        /**
         *  By this time, the Input property should already been validated for the operation.
         *  But we will still validate here...
         */
        if (execInput == null) {
            throw new Exception(mMessages.getString("OMP_Invalid_No_Out_ExecInput", operationName));
        }
        
        ExecMessage execMessage = execInput.getExecMessage();
        if (execMessage == null) {
            throw new Exception(mMessages.getString("OMP_Invalid_No_Out_ExecMessage", operationName));
        }
        
        if (execMessage.getExecUseType().equals(ExecMessage.EXEC_USE_TYPE_ENCODED) &&
                (execMessage.getExecEncodingStyle() == null ||
                execMessage.getExecEncodingStyle().equals(""))) {
            throw new Exception(mMessages.getString("OMP_Invalid_No_Out_ExecMessage_EncodingStyle", operationName));
        }
    }

    public void stopReceiving() {
        mLogger.info("OMP_Stopped_thread");
        mMonitor = null;
    }
    
    /** Package protected method
     *  Used solely for JUnit test purposes
     */
    void setExecDenormalizer(ExecDenormalizer denormalizer) {
        mDenormalizer = denormalizer;
    }
    
    void setExecNormalizer(ExecNormalizer normalizer) {
        mNormalizer = normalizer;
    }
}
