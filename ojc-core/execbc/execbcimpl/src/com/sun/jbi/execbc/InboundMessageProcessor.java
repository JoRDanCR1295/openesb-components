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

package com.sun.jbi.execbc;

import ch.ethz.ssh2.ChannelCondition;
import ch.ethz.ssh2.Connection;
import ch.ethz.ssh2.Session;
import ch.ethz.ssh2.StreamGobbler;

import com.sun.jbi.execbc.extensions.Delimiters;
import com.sun.jbi.execbc.extensions.ExecAddress;
import com.sun.jbi.execbc.extensions.ExecInput;
import com.sun.jbi.execbc.extensions.ExecMessage;
import com.sun.jbi.execbc.extensions.PollingPattern;
import com.sun.jbi.execbc.util.ExecUtil;
import com.sun.jbi.execbc.util.FileNamePatternType;
import com.sun.jbi.execbc.util.FileNamePatternUtil;
import com.sun.jbi.execbc.extensions.ExecOperation;
import com.sun.jbi.execbc.extensions.ExecOutput;
import com.sun.jbi.execbc.extensions.Delimiters.Match;
import com.sun.jbi.execbc.util.InputFilenameFilter;
import com.sun.jbi.execbc.Endpoint.EndpointMessageType;
import com.sun.jbi.internationalization.Messages;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.InterruptedIOException;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
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
import javax.xml.parsers.*;

/** This is the thread that checks for inbound messages from the
 *  file system based on end point configuration.
 *
 * @author Sherry Weng
 * @author Qian Fu jim.fu@sun.com
 * @author Jun Xu
 */
public class InboundMessageProcessor implements Runnable, MessageExchangeReplyListener {
    private static final Messages mMessages =
            Messages.getMessages(InboundMessageProcessor.class);
    private static Logger mLogger = Messages.getLogger(InboundMessageProcessor.class);

    // local constants
    public static final int NUM_IB_WORKER = 5;
    public static final String SUFFIX_PROCESSED_LOCALE_KEY = "processed";
    public static final String SUFFIX_ERROR_LOCALE_KEY = "error";
    public static final String IB_WORKER_THREAD_NAME_PREFIX = "execbc-ib-worker";
    
    /**
     * Defines the JBI identifier for an 'in' message, for use with exchange.getMessage
     */
    public static final String IN_MSG = mMessages.getString("in");
    private static Map mInboundExchanges = Collections.synchronizedMap(new HashMap());
    
    private AtomicBoolean mStopRequested;
    private AtomicBoolean mStopped; // set when inbound processor stopped

    private Map mInboundReplys;
    
    private DeliveryChannel mChannel;
    private ComponentContext mContext;
    private MessageExchangeFactory mMsgExchangeFactory;
    private Endpoint mEndpoint;
    private final String mServiceEPRef;
    private QName mOperationName;
    private ServiceEndpoint mServiceEndpoint;
    private ExecMessage mExecMessage;
    private Map mWorkers; // list of inbound file workers + their thread
    private AtomicReference<LinkedBlockingQueue> mResults; // list of byte arrays to be processed by workers
    private int mIBWorkerCount = NUM_IB_WORKER;
    private List<StreamingStdoutProcessor> mStreamingStdoutProcessors;
    private volatile Thread mThread;
    private volatile boolean mWorkerStarted;

    public InboundMessageProcessor(ComponentContext context,
            DeliveryChannel channel,
            Endpoint endpoint,
            QName operationName) throws IBProcCreationException {
        mContext = context;
        mChannel = channel;
        mEndpoint = endpoint;
        mOperationName = new QName(getPortType(endpoint).getQName().getNamespaceURI(),
                operationName.getLocalPart());
        mServiceEPRef = mEndpoint.getServiceName().toString() + mEndpoint.getEndpointName();
        mStopRequested = new AtomicBoolean(false);
        mStopped = new AtomicBoolean(false);
        mInboundReplys = Collections.synchronizedMap(new HashMap());
        mResults = new AtomicReference<LinkedBlockingQueue>();
    }
    
    public void run() {
        mThread = Thread.currentThread();
        try {
            mLogger.log(Level.INFO, "IMP_EP_status",
                    new Object[] {mEndpoint.getServiceName(), mEndpoint.getEndpointName()});
            
            ExecOperation operation = locateExecOperation(mOperationName);
            String mep = getMessageExchangePattern(mOperationName);
            try {
                validateInboundMessageExchangeProperties(operation, mep);
            } catch (Exception e) {
                mLogger.log(Level.SEVERE, e.getLocalizedMessage());
                return;
            }
            
            ExecInput execInput = operation.getExecOperationInput();
            /**
             * We have an one-way or request-response inbound operation.
             * The file "read" properties will be provided in
             * BindingInput extensibility element.
             * The BindingInput and its corresponding required file:read
             * properties are guaranteed or else we won't even reach here.
             */
            mExecMessage = execInput.getExecMessage();
            ExecAddress address = (ExecAddress) mEndpoint.getExecAddress();
            long pollingIntervalMillis = operation.getPollingInterval() * 1000;
    
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "IMP_Exec_properties",
                        new Object[] {operation.getCommand(), pollingIntervalMillis});
            }

            ensureExchangeFactoryAndEndPoint();
            
            if (PollingPattern.INVOKE_ONCE_AND_KEEP_RECEIVING.equals(
                    operation.getPollingPattern())) {
                synchronized (this) {
                    mStreamingStdoutProcessors =
                        new ArrayList<StreamingStdoutProcessor>();
                    String[] hosts = ExecUtil.getHosts(address);
                    for (int i = 0; i < hosts.length; i++) {
                        StreamingStdoutProcessor ssProc =
                            new StreamingStdoutProcessor(
                                    mep, operation.getCommand(),
                                    execInput, hosts[i], address.getUserName(),
                                    address.getPassword());
                        mStreamingStdoutProcessors.add(ssProc);
                    }
                    for (StreamingStdoutProcessor proc
                            : mStreamingStdoutProcessors) {
                        Thread t = new Thread(proc);
                        t.start();
                    }
                }
                while (!mStopRequested.get() && !mWorkerStarted) {
                    // lazy init workers and input file queue
                    mResults.compareAndSet(null, new LinkedBlockingQueue());
                    
                    try {
                        startWorkers(mep, execInput);
                        Thread.sleep(500);
                    } catch (Exception e) {
                        mLogger.log(Level.SEVERE,
                                "Starting worker threads failed.", e);
                    }
                }
                while (!mStopRequested.get()) {
                    try {
                        Thread.sleep(3600000);
                    } catch (InterruptedException e) {
                        //stopped
                    }
                }
            } else {
                do {
                    try {
                        execute(mep, operation.getCommand(), execInput, address);
                    } catch (Exception e) {
                        mLogger.log(Level.SEVERE,
                                mMessages.getString("IMP_Failed_send_msg",
                                        e.getMessage()), e);
                    }
                    try {
                        startWorkers(mep, execInput);
                    } catch (Exception e) {
                        mLogger.log(Level.SEVERE,
                                "Starting worker threads failed.", e);
                    }
                    try {
                        Thread.currentThread().sleep(pollingIntervalMillis);
                    } catch (Exception e) {
                        // nothing to do...
                    }
                } while (!mStopRequested.get());
            }
            // workers will check this flag 
            // to shutdown accordingly
            mStopped.set(true);
        } finally {
            mThread = null;
        }
    }
    
    public void execute(
        String mep,
        String command, 
        ExecInput execInput,
        ExecAddress execAddress
    
    ) throws Exception {
        MessageExchange exchange = null;
        String exchangeId = null;
        
        if (command == null || command.length() == 0) {
            throw new Exception(mMessages.getString("IMP_Invalid_command"));
        }

        //Execute the command
        String resultID = command + " - " + new Date() + "." + System.nanoTime();
        String[] hosts = ExecUtil.getHosts(execAddress);
        for (int i = 0; i < hosts.length; i++) {
            ByteArrayOutputStream stdoutStream = new ByteArrayOutputStream();
            byte[] buf = new byte[2048];
            int read;
            if (ExecUtil.isRemote(hosts[i])) {
                //Execute the command on a remote machine using SSH
                Connection conn = new Connection(hosts[i]);
                conn.connect();
                boolean isAuthenticated =
                    conn.authenticateWithPassword(execAddress.getUserName(),
                            execAddress.getPassword());
    
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
                            + "' on remote host: '" + execAddress.getHostName()
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
            
            // lazy init workers and input file queue
            mResults.compareAndSet(null, new LinkedBlockingQueue());
            
            byte[] rawData = stdoutStream.toByteArray();
            stdoutStream.close();
            Delimiters delims =
                execInput.getExecMessage().getDelimitersOfRecord();
            boolean injectCtxInfo =
                execInput.getExecMessage().getInjectContextInfo();
            int skipCount =
                execInput.getExecMessage().getRecordsToBeSkipped();
            if (delims != null) {
                Match match;
                int start = 0;
                int len = rawData.length;
                int recordCount = 0;
                while (len > 0) {
                    match = delims.match(rawData, start, len);
                    if (recordCount < skipCount) {
                        start = match != null ? match._nextStart : rawData.length;
                        len = rawData.length - start;
                        recordCount++;
                        continue;
                    }
                    byte[] recordData;
                    int recordLen =
                        (match != null ? match._pos : rawData.length) - start;
                    if (injectCtxInfo) {
                        ByteArrayOutputStream recordStream =
                            new ByteArrayOutputStream();
                        recordStream.write(hosts[i].getBytes());
                        recordStream.write("\n".getBytes());
                        recordStream.write(command.getBytes());
                        recordStream.write("\n".getBytes());
                        recordStream.write(rawData, start, recordLen);
                        recordData = recordStream.toByteArray();
                        recordStream.close();
                    } else {
                        recordData =
                            new byte[recordLen];
                        System.arraycopy(rawData, start,
                                recordData, 0, recordData.length);
                    }
                    // this will wake up any worker waiting on the queue
                    getResults().put(
                            new CommandResult(
                                    resultID, recordData));
                    start = match != null ? match._nextStart : rawData.length;
                    len = rawData.length - start;
                    recordCount++;
                }
            } else {
                if (injectCtxInfo) {
                    ByteArrayOutputStream recordStream =
                        new ByteArrayOutputStream();
                    recordStream.write(hosts[i].getBytes());
                    recordStream.write("\n".getBytes());
                    recordStream.write(command.getBytes());
                    recordStream.write("\n".getBytes());
                    recordStream.write(rawData);
                    rawData = recordStream.toByteArray();
                    recordStream.close();
                }
                // this will wake up any worker waiting on the queue
                getResults().put(
                        new CommandResult(
                                resultID, rawData));
            }
        }
    }
    
    public void stopReceiving() {
        mLogger.log(Level.INFO, "IMP_Inbound_stopped");
        synchronized (this) {
            if (mStreamingStdoutProcessors != null) {
                for (StreamingStdoutProcessor proc
                        : mStreamingStdoutProcessors) {
                    proc.stop();
                }
            }
        }
        Thread t = mThread;
        if (t != null) {
            //Interrupt the running inbound processing thread in case
            //it is blocked by IO
            t.interrupt();
            t = null;
        }
        mStopRequested.set(true);
    }

    private ExecOperation locateExecOperation(QName opname) {
        return (ExecOperation) mEndpoint.getExecOperations().get(opname);
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
    protected void validateInboundMessageExchangeProperties(ExecOperation operation, String mep) throws Exception {
        // 1. Check if the Message Exchange Pattern is valid.
        if (mep == null ||
                (!mep.equals(EndpointMessageType.IN_ONLY))) {
//IN_OUT is not supported
//        if (mep == null ||
//                (!mep.equals(EndpointMessageType.IN_ONLY) &&
//                !mep.equals(EndpointMessageType.IN_OUT)) ) {
            throw new Exception(mMessages.getString("IMP_Invalid_mep", mOperationName));
        }
        
        // 2. Check if required file:message properties are present and valid on File binding Input element
        ExecInput execInput = operation.getExecOperationInput();
        if (execInput == null) {
            throw new Exception(mMessages.getString("IMP_Invalid_No_FileInput", mOperationName));
        }
        
        ExecMessage execMessage = execInput.getExecMessage();
        if (execMessage == null) {
            throw new Exception(mMessages.getString("IMP_Invalid_No_FileMessage", mOperationName));
        }

//Not applicable to Exec BC
//        if (fileMessage.getFileName() == null) {
//            throw new Exception(mMessages.getString("IMP_Invalid_No_FileMessage_FileName", mOperationName));
//        }
//        
//        if (!fileMessage.getFileType().equals(ExecMessage.FILE_TYPE_TEXT) &&
//                !fileMessage.getFileType().equals(ExecMessage.FILE_TYPE_BINARY)) {
//            throw new Exception(mMessages.getString("IMP_Invalid_File_TYPE", new Object[] {fileMessage.getFileType(), mOperationName}));
//        }
//        
        if (operation.getCommand() == null || operation.getCommand().length() == 0) {
            throw new Exception(mMessages.getString("IMP_Invalid_command"));
        }

        if (execMessage.getExecUseType().equals(ExecMessage.EXEC_USE_TYPE_ENCODED) &&
                (execMessage.getExecEncodingStyle() == null ||
                execMessage.getExecEncodingStyle().equals(""))) {
            throw new Exception(mMessages.getString("IMP_Invalid_No_ExecMessage_EncodingStyle", mOperationName));
        }

//IN_OUT not supported so far
        // 4. Check if required file:message properties are present and valid on File binding Output element
//        if (mep.equals(EndpointMessageType.IN_OUT)) {
//            ExecOutput fileOutput = operation.getFileOperationOutput();
//            
//            if (fileOutput == null) {
//                throw new Exception(mMessages.getString("IMP_Invalid_No_FileOutput", mOperationName));
//            }
//            
//            execMessage = fileOutput.getExecMessage();
//            if (execMessage == null) {
//                throw new Exception(mMessages.getString("IMP_Invalid_No_FileMessage", mOperationName));
//            }
//            
//            if (execMessage.getFileName() == null) {
//                throw new Exception(mMessages.getString("IMP_Invalid_No_FileMessage_FileName", mOperationName));
//            }
//            
//            if (!execMessage.getFileType().equals(ExecMessage.FILE_TYPE_TEXT) &&
//                    !execMessage.getFileType().equals(ExecMessage.FILE_TYPE_BINARY)) {
//                throw new Exception(mMessages.getString("IMP_Invalid_File_TYPE", new Object[] {execMessage.getFileType(), mOperationName}));
//            }
//            
//            if (execMessage.getFileUseType().equals(ExecMessage.FILE_USE_TYPE_ENCODED) &&
//                    (execMessage.getFileEncodingStyle() == null ||
//                    execMessage.getFileEncodingStyle().equals(""))) {
//                throw new Exception(mMessages.getString("IMP_Invalid_No_FileMessage_EncodingStyle", mOperationName));
//            }
//        }
    }
    
    public synchronized void processReplyMessage(MessageExchange exchange) throws Exception {
        if (!(exchange instanceof InOnly) &&
                !(exchange instanceof InOut)) {
            mLogger.log(Level.SEVERE, "IMP_Unsupported_exchange_pattern", exchange.getPattern().toString());
            throw new Exception(mMessages.getString("IMP_Unsupported_exchange_pattern", exchange.getPattern().toString()));
        }
        
        String messageId = exchange.getExchangeId();
        if (mInboundExchanges.containsKey(messageId)) {
            String messageInfo = (String) mInboundReplys.get(messageId);
            if (exchange.getStatus() != ExchangeStatus.DONE) {
                //For Exec BC, not much to do if the exchange failed,
                //just log it
                if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.log(Level.INFO, "IMP_Request_process_failed",
                            new Object[]{messageId, messageInfo});
                }
            }
            else {
                //For Exec BC, not much to do if the exchange is successful,
                //just log it
                if (mLogger.isLoggable(Level.INFO)) {
                    mLogger.log(Level.INFO, "IMP_Request_process_succeeded",
                            new Object[]{messageId, messageInfo});
                }
            }
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "IMP_Remove_exchange_msg_id", messageId);
            }
            mInboundExchanges.remove(messageId);
        } else {
            mLogger.log(Level.SEVERE, "IMP_Invalid_reply_msgId", messageId);
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
            mLogger.log(Level.INFO, "IMP_locate_EP",
                    new Object[] {mEndpoint.getServiceName(), mEndpoint.getEndpointName()});
        }
        return (activatedEndpoint);
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
        if ( mMsgExchangeFactory == null )
            throw new IllegalStateException(mMessages.getString("IMP_Object_Not_Available_When_Accessed", "MessageExchangeFactory"));
        return mMsgExchangeFactory;
    } 
    
    public LinkedBlockingQueue getInputFileQueue() {
        if ( mResults == null )
            throw new IllegalStateException(mMessages.getString("IMP_Object_Not_Initialized_When_Accessed", "Result Queue"));
        return mResults.get();
    }
    
    public DeliveryChannel getDelivaryChannel() {
        if ( mChannel == null )
            throw new IllegalStateException(mMessages.getString("IMP_Object_Not_Available_When_Accessed", "DeliveryChannel"));
        return mChannel;
    }
    
    public Endpoint getEndpoint() {
        if ( mEndpoint == null )
            throw new IllegalStateException(mMessages.getString("IMP_Object_Not_Available_When_Accessed", "Endpoint"));
        return mEndpoint;
    }

    public ServiceEndpoint getServiceEndpoint() {
        if ( mServiceEndpoint == null )
            throw new IllegalStateException(mMessages.getString("IMP_Object_Not_Available_When_Accessed", "ServiceEndpoint"));
        return mServiceEndpoint;
    }

    public QName getOperationName() {
        if ( mOperationName == null )
            throw new IllegalStateException(mMessages.getString("IMP_Object_Not_Available_When_Accessed", "Operation name"));
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
        return mWorkers != null && mWorkers.size() > 0 ? mWorkers.size() : NUM_IB_WORKER;
    }

    public void setWorkers(Map workers) {
        mWorkers = workers;
    }

    public Map getWorkers() {
        return mWorkers;
    }
    
    public void addWorker(IBExecWorker worker) {
        if ( mWorkers == null ) {
            setWorkers(new HashMap());
        }
        Thread t = new Thread(worker);
        t.setName(IB_WORKER_THREAD_NAME_PREFIX.concat(t.getName()));
        mWorkers.put(worker, t);
    }
    
    public void setResults(LinkedBlockingQueue results) {
        mResults.set(results);
    }

    public LinkedBlockingQueue getResults() {
        return mResults.get();
    }
    
    public void setFileMessage(ExecMessage msg) {
        mExecMessage = msg;
    }

    public ExecMessage getFileMessage() {
        return mExecMessage;
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
    
    // Make this method public so JUnit test can call it
    public void startWorkers(String mep, ExecInput execInput)
            throws Exception {
        if ( getResults().size() > 0 ) {
            if ( getWorkers() == null || getWorkers().size() == 0 ) {
                // if workers are not initialized yet
                // initialize them now - lazy worker init
                // in case there is no input available - no need
                // to pool the workers;
                if ( getWorkers() == null ) {
                    setWorkers(new HashMap());
                }
                // for now, a fixed number of inbound workers are enough
                for (int ii = 0; ii < getWorkerCount(); ii++) {
                    IBExecWorker worker =
                        new IBExecWorker(this, mep,
                                execInput.getExecMessage());
                    addWorker(worker);
                    ((Thread)(getWorkers().get(worker))).start();
                }
            } else {
                //Is this logic for JUnit test?
                if (!mWorkerStarted) {
                    // there are already workers threads
                    Collection threads = getWorkers().values();
                    Iterator it = threads.iterator();
                    while ( it.hasNext() ) {
                        Thread t = (Thread)it.next();
                        if ( !t.isAlive() )
                            t.start();
                    }
                }
            }
            mWorkerStarted = true;
        }
    }

    // Make this method so JUnit test can call
    public void ensureExchangeFactoryAndEndPoint() {
        if (mMsgExchangeFactory == null) {
            mMsgExchangeFactory = mChannel.createExchangeFactory();
        }
        
        // trying to locate the service endpoint again if it's not yet found
        if (mServiceEndpoint == null) {
            mServiceEndpoint = locateServiceEndpoint();
        }
        
        if (mServiceEndpoint == null) {
            mLogger.log(Level.SEVERE, mMessages.getString("IMP_Failed_locate_EP",
                    new Object[] {mEndpoint.getServiceName(), mEndpoint.getEndpointName()}));
        }
    }
    
    /**
     * Processor for handling infinite streaming stdout, e.g., one line per
     * record. Once a line (or maybe some other unit depending on what the
     * delimiter is) is read, it will be normalized into a normalized message
     * and be sent to NMR.
     */
    private class StreamingStdoutProcessor implements Runnable {
        
        private volatile Thread _thread;
        private volatile boolean _stopped;
        private final String _mep;
        private final String _command;
        private final ExecInput _execInput;
        private final String _hostName;
        private final String _userName;
        private final String _password;
        
        public StreamingStdoutProcessor(String mep, String command, 
                ExecInput execInput, String hostName, String userName,
                String password) {
            _mep = mep;
            _command = command;
            _execInput = execInput;
            _hostName = hostName;
            _userName = userName;
            _password = password;
        }

        public void run() {
            mThread = Thread.currentThread();
            try {
                
                boolean isRemote = ExecUtil.isRemote(_hostName);
                
                //Invoke the command
                Connection conn = null;
                Session sess = null;
                Process proc = null;
                InputStream stdout = null;
                InputStream errout = null;
                String errorString = "";
                boolean interrupted = false;
                try {
                    if (isRemote) {
                        //Execute the command on a remote machine using SSH
                        conn = new Connection(_hostName);
                        conn.connect();
                        boolean isAuthenticated =
                            conn.authenticateWithPassword(_userName, _password);
                        if (isAuthenticated == false) {
                            conn.close();
                            throw new IOException("Authentication failed.");
                        }
                        sess = conn.openSession();
                        sess.execCommand(_command);
                        stdout = sess.getStdout();
                        //Automatically buffer all error output
                        errout = new StreamGobbler(sess.getStderr());
                    } else {
                        proc = Runtime.getRuntime().exec(_command);
                        stdout = proc.getInputStream();
                        errout = new StreamGobbler(proc.getErrorStream());
                    }
                    
                    //Receive data
                    byte[] buf = new byte[1024];
                    int bytesRead;
                    Delimiters delims =
                        _execInput.getExecMessage().getDelimitersOfRecord();
                    int skipCount =
                        _execInput.getExecMessage().getRecordsToBeSkipped();
                    boolean injectCtxInfo =
                        _execInput.getExecMessage().getInjectContextInfo();
                    ByteArrayOutputStream rawDataStream =
                        new ByteArrayOutputStream();
                    int recordCount = 0;
                    int start;
                    int len;
                    Match match;
                    mainLoop: while (!_stopped) {
                        bytesRead = stdout.read(buf);
                        if (bytesRead < 0) {
                            break;
                        }
                        rawDataStream.write(buf, 0, bytesRead);
                        byte[] rawData = rawDataStream.toByteArray();
                        start = 0;
                        len = rawData.length;
                        while ((match = delims.match(rawData, start, len))
                                != null) {
                            if (recordCount >= skipCount
                                    && match._pos != start) {
                                String resultID =
                                    _command + " - "
                                        + new Date() + "." + System.nanoTime();
                                ByteArrayOutputStream recData =
                                    new ByteArrayOutputStream();
                                if (injectCtxInfo) {
                                    recData.write(_hostName.getBytes());
                                    recData.write("\n".getBytes());
                                    recData.write(_command.getBytes());
                                    recData.write("\n".getBytes());
                                }
                                recData.write(rawData, start, match._pos - start);
                                try {
                                    mResults.compareAndSet(null, new LinkedBlockingQueue());
                                    getResults().put(
                                            new CommandResult(resultID,
                                                    recData.toByteArray()));
                                } catch (InterruptedException e) {
                                    break mainLoop;
                                }
                            }
                            start = match._nextStart;
                            len = rawData.length - start;
                            if (len <= 0) {
                                break;
                            }
                            recordCount++;
                        }
                        rawDataStream.close();
                        rawDataStream = new ByteArrayOutputStream();
                        if (start < rawData.length && len > 0) {
                            //has data remain unprocessed
                            rawDataStream.write(rawData, start, len);
                        }
                    }
                    
                    //Get error string if any
                    BufferedReader br =
                        new BufferedReader(new InputStreamReader(errout));
                    StringBuilder sb = new StringBuilder();
                    while (true) {
                        String line = br.readLine();
                        if (line == null) {
                            break;
                        }
                        sb.append(line);
                    }
                    errorString = sb.toString();
                
                } catch (InterruptedIOException iioe) {
                    interrupted = true;
                }
                
                //Clean up
                int exitCode = 0;
                if (isRemote) {
                    if (!interrupted) {
                        int cond =
                            sess.waitForCondition(
                                    ChannelCondition.EXIT_STATUS, 5000);
                        if ((ChannelCondition.EXIT_STATUS & cond) != 0) {
                            exitCode = sess.getExitStatus(); 
                        }
                    }
                    if (sess != null) {
                        sess.close();
                    }
                    if (conn != null) {
                        conn.close();
                    }
                    if (exitCode != 0) {
                        throw new IOException("Execution of command: '"
                                + _command + "' on remote host: '"
                                + _hostName + "' failed. " + errorString);
                    }
                } else {
                    if (!interrupted) {
                        try {
                            exitCode = proc.waitFor();
                        } catch (InterruptedException e) {
                            proc.destroy();
                        }
                    } else {
                        if (proc != null) {
                            proc.destroy();
                        }
                    }
                    if (stdout != null) {
                        stdout.close();
                    } else {
                        if (proc != null && proc.getInputStream() != null) {
                            proc.getInputStream().close();
                        }
                    }
                    if (errout != null) {
                        errout.close();
                    } else {
                        if (proc != null && proc.getErrorStream() != null) {
                            proc.getErrorStream().close();
                        }
                    }
                    if (proc != null && proc.getOutputStream() != null) {
                        proc.getOutputStream().close();
                    }
                    if (exitCode != 0) {
                        throw new IOException("Execution of command: '"
                                + _command + "' on local host failed. "
                                + errorString);
                    }
                }
            } catch (IOException e) {
                mLogger.log(Level.SEVERE, "IMP_Inbound_failed on host: '"
                        + _hostName + "', with command: '"
                        + _command + "'. ", e);
            } finally {
                mThread = null;
            }
        }
        
        public void stop() {
            mLogger.log(Level.INFO, "IMP_Inbound_stopped on host: " + _hostName);
            Thread t = _thread;
            if (t != null) {
                //Interrupt the running inbound processing thread in case
                //it is blocked by IO
                t.interrupt();
                t = null;
            }
            _stopped = true;
        }
    }
}
