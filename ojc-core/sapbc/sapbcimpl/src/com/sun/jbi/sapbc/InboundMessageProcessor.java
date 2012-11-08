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

package com.sun.jbi.sapbc;

import com.sun.jbi.sapbc.Endpoint.EndpointMessageType;
import com.sun.jbi.sapbc.extensions.SAPAddress;
import com.sun.jbi.sapbc.extensions.WSDLInput;
import com.sun.jbi.sapbc.extensions.SAPMessage;
import com.sun.jbi.sapbc.extensions.SAPFmOperation;
//import com.sun.jbi.sapbc.util.FileStreamHandler;
//import com.sun.jbi.sapbc.util.InputFilenameFilter;
import com.sun.jbi.internationalization.Messages;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import org.xml.sax.SAXException;

/** This is the thread that checks for inbound messages from the
 *  file system based on end point configuration. 
 *
 * @author Sherry Weng 
 */
public class InboundMessageProcessor implements Runnable, MessageExchangeReplyListener {
    private static final Messages mMessages = 
            Messages.getMessages(InboundMessageProcessor.class);
    private static Logger mLogger = Messages.getLogger(InboundMessageProcessor.class);
    
    /**
     * Defines the JBI identifier for an 'in' message, for use with exchange.getMessage
     */
    private static final String IN_MSG = mMessages.getString("in");    
    private static Map mInboundExchanges = Collections.synchronizedMap(new HashMap());

    private Object mMonitor;
    private Map mInboundReplys;
 //   private SAPNormalizer mNormalizer;
    
    private DeliveryChannel mChannel;
    private ComponentContext mContext;
    private MessageExchangeFactory mMsgExchangeFactory;
    private Endpoint mEndpoint;
    private QName mOperationName;
    private ServiceEndpoint mServiceEndpoint;
    
    public InboundMessageProcessor(ComponentContext context,
                                   DeliveryChannel channel,
                                   Endpoint endpoint,
                                   QName operationName) throws MessagingException {
        mContext = context;
        mChannel = channel;
        mEndpoint = endpoint;
        mOperationName = operationName;
        mMonitor = new Object();
        //mNormalizer = new SAPNormalizer();
        mInboundReplys = new HashMap();
    }
    
    public static Map getInboundExchanges() {
        return mInboundExchanges;
    }
    
    public void run() {
        mLogger.log(Level.INFO, "InboundMessageProcessor.EP_status", 
            new Object[] {mEndpoint.getServiceName(), mEndpoint.getEndpointName()});
        
        SAPFmOperation operation = locateSAPFmOperation(mOperationName);
        String mep = getMessageExchangePattern(mOperationName);
        try {
            validateInboundMessageExchangeProperties(operation, mep);
        } catch (Exception e) {
            mLogger.log(Level.SEVERE, e.getLocalizedMessage());
            return;
        }
        
        WSDLInput wsdlInput = operation.getSAPOperationInput();
        /**
        * We have an one-way or request-response inbound operation. 
        * The file "read" properties will be provided in 
        * BindingInput extensibility element.
        * The BindingInput and its corresponding required file:read 
        * properties are guaranteed or else we won't even reach here.
        */
        SAPMessage sapMessage = wsdlInput.getSAPMessage();
        
        SAPAddress address = (SAPAddress) mEndpoint.getSAPAddress();
        /*
        Boolean relativePath = (address.getRelativePath() != null)? address.getRelativePath(): Boolean.FALSE;
        String rootPath = address.getPathRelativeTo();
        String fileDir = (relativePath.booleanValue() && rootPath != null)? 
            (rootPath + File.separator + address.getFileDirectory()): address.getFileDirectory();
        String fileName = sapMessage.getFileName();
        Boolean isPattern = (sapMessage.getFileNameIsPattern() != null)? sapMessage.getFileNameIsPattern(): Boolean.FALSE;
        long pollingIntervalMillis = (sapMessage.getPollingInterval() != null)? sapMessage.getPollingInterval().longValue(): 1000;
        
        if (mLogger.isLoggable(Level.INFO)) {
            if (isPattern.booleanValue()) {
                mLogger.log(Level.INFO, "IMP_Input_file_pattern_properties", 
                                        new Object[] {fileDir, fileName, pollingIntervalMillis});
            } else {
                mLogger.log(Level.INFO, "IMP_Input_file_properties", 
                                        new Object[] {fileDir, fileName, pollingIntervalMillis});
            } 
        }
        
        do {
            try {
            	execute(mep, fileDir, fileName, isPattern, wsdlInput);
            } catch (Exception e) {
                mLogger.log(Level.SEVERE, mMessages.getString("IMP_Failed_send_msg", e.getMessage()), e);
            }
            try {
                Thread.currentThread().sleep(pollingIntervalMillis);
            } catch (Exception e) {
                // nothing to do...
            }
        } while (mMonitor != null);
         **/
    }
    
    public void execute(String mep, String fileDir, String fileName, Boolean isPattern, WSDLInput wsdlInput) throws Exception {
        MessageExchange exchange = null;
        String exchangeId = null;      
        
        if (fileName == null) {
            throw new Exception(mMessages.getString("InboundMessageProcessor.Invalid_filename"));
        }
        
        if (fileDir == null) {
            throw new Exception(mMessages.getString("InboundMessageProcessor.Invalid_filedir"));
        } 
        
        if (mMsgExchangeFactory == null) {
            mMsgExchangeFactory = mChannel.createExchangeFactory();
        } 
        
        try {
            // trying to locate the service endpoint again if it's not yet found
            if (mServiceEndpoint == null) {
                mServiceEndpoint = locateServiceEndpoint(); 
            }
            if (mServiceEndpoint == null) {
                throw new MessagingException(mMessages.getString("InboundMessageProcessor.Failed_locate_EP",
                    new Object[] {mEndpoint.getServiceName(), mEndpoint.getEndpointName()}));
            } 
            
            /*
            if (isPattern != null && isPattern.booleanValue()) {
                File inDir = new File(fileDir);
                File[] inputFiles = inDir.listFiles(new InputFilenameFilter(fileName));
                for (int ii = 0; ii < inputFiles.length; ii++) {
                    File inFile = inputFiles[ii];
                    processFile(inFile, mep, wsdlInput);
                }
            } else {
                File inFile = new File(fileDir, fileName);
                processFile(inFile, mep, wsdlInput);
            }
             */
        } catch (Exception e) {
            throw e;
        }
    }

    public void stopReceiving() {
        mLogger.log(Level.INFO, "InboundMessageProcessor.Inbound_stopped");
        mMonitor = null;
    }
    
    private void processFile(File inputFile, String mep, WSDLInput wsdlInput) throws Exception {
    	try {
            /*
            * The sap:message properties will be provided in 
            * BindingInput extensibility element.
            * The BindingInput and its corresponding required sap:message 
            * properties are guaranteed or else we won't even reach here.
            */
            SAPMessage sapMessage = wsdlInput.getSAPMessage();
            
            // get all sap:read attributes
            /*
            boolean removeEOL = (sapMessage.getRemoveEOL() != null)? sapMessage.getRemoveEOL().booleanValue(): false;
            boolean multipleRecords = (sapMessage.getMultipleRecordsPerFile() != null)? 
                                       sapMessage.getMultipleRecordsPerFile().booleanValue():false;
            long maxBytesPerRecord = (sapMessage.getMaxBytesPerRecord() != null)?
                                      sapMessage.getMaxBytesPerRecord().longValue(): -1;  // -1 means not defined
            byte[] recordDelim = (sapMessage.getRecordDelimiter() != null)?
                                      sapMessage.getRecordDelimiter().getBytes(): new byte[] {};
            
            if (inputFile.exists()) {
            	FileInputStream stream = new FileInputStream(inputFile);
            	FileStreamHandler streamHandler = new FileStreamHandler(stream,
            	                                                        removeEOL,
                                                                        recordDelim,
                                                                        maxBytesPerRecord,
                                                                        inputFile.length());
                
                
                byte[] data = null;
            	if (!multipleRecords) {
            	    data = streamHandler.getAllContentsAsBytes(stream);
            	    if (data == null)  return;  // nothing to process
            	    
            	    // Close the input stream and rename the input file.
            	    stream.close();
            	    renameInputFile(inputFile);
            	    processMessage(mep, sapMessage, data, inputFile);
            	} else {
            	    List records = new ArrayList();
            	    while (streamHandler.hasMoreRecords()) {
            	        data = streamHandler.readNextRecord();
            	        if (data != null) {
            	            records.add(data);
            	        } else {
            	            break;
            	        }
            	    }
            	    
            	    // Close the input stream and rename the input file
            	    // So we won't be reading the same records over and over again.
            	    stream.close();
            	    renameInputFile(inputFile);
            	    for (int ii = 0; ii < records.size(); ii++) {
            	        processMessage(mep, sapMessage, (byte[])records.get(ii), inputFile);    
            	    }
            	}
            }
             */
        } catch(Exception ex) {
           // mLogger.log(Level.SEVERE, mMessages.getString("IMP_Failed_processing_file", inputFile.getAbsolutePath()), ex);
            throw ex;
        }
    }
    
    private void processMessage(String mep, SAPMessage sapMessage, byte[] data, File inputFile) 
                               throws MessagingException, SAXException, IOException, Exception {
        MessageExchange exchange = null;
        String exchangeId = null;
        
        try {
            if (mep.equals(EndpointMessageType.IN_ONLY)) {
                exchange = mMsgExchangeFactory.createInOnlyExchange();
            } else if (mep.equals(EndpointMessageType.IN_OUT)) {
                exchange = mMsgExchangeFactory.createInOutExchange();
            }
           exchangeId = exchange.getExchangeId();
           if (exchangeId != null) {
               mInboundReplys.put(exchangeId, inputFile);
           }   
           proceedMessageExchange(exchange, sapMessage, data);
       } catch(MessagingException ex) {
            renameProcessedFile(inputFile);
            if (exchangeId != null) {
                mInboundExchanges.remove(exchangeId);
            }      
            throw ex;
        } catch (SAXException ex) {
            renameProcessedFile(inputFile);
            if (exchangeId != null) {
                mInboundExchanges.remove(exchangeId);
            }    
            throw ex;
        } catch(IOException ex) {
            renameProcessedFile(inputFile);
            if (exchangeId != null) {
                mInboundExchanges.remove(exchangeId);
            }        
            throw ex;
        } catch (Exception ex) {
            renameProcessedFile(inputFile);
            if (exchangeId != null) {
                mInboundExchanges.remove(exchangeId);
            }        
            throw ex;
        } 
    }
    
    private void proceedMessageExchange(MessageExchange exchange, 
                                        SAPMessage sapMessage, 
                                        byte[] data) 
                                        throws MessagingException, SAXException, IOException, Exception {
    	if (data == null) {
    	    throw new Exception("InboundMessageProcessor.Invalid_Data");    
    	}
 
    	String exchangeId = null;
        /*
        NormalizedMessage inMsg = mNormalizer.normalize(exchange,
                                                        mOperationName,
                                                        mEndpoint,
                                                        sapMessage,
                                                        data);
        exchange.setEndpoint(mServiceEndpoint);
        exchange.setOperation(mOperationName);
        exchange.setMessage(inMsg, IN_MSG);
        exchangeId = exchange.getExchangeId();
        mInboundExchanges.put(exchangeId, new ListenerMeta(System.currentTimeMillis(), this)); 
        mChannel.send(exchange);
        mEndpoint.getEndpointStatus().incrementSentRequests();
         **/
    } 
    
    public synchronized void processReplyMessage(MessageExchange exchange) throws Exception {
        if (!(exchange instanceof InOnly) &&
            !(exchange instanceof InOut)) {
            mLogger.log(Level.SEVERE, "InboundMessageProcessor.Unsupported_exchange_pattern", exchange.getPattern().toString());
            throw new Exception(mMessages.getString("InboundMessageProcessor.Unsupported_exchange_pattern", exchange.getPattern().toString()));
        }
 
        String messageId = exchange.getExchangeId(); 
        if (mInboundExchanges.containsKey(messageId)) {
            // Any status other than 'DONE' is considered an error
            if (exchange.getStatus() != ExchangeStatus.DONE) {
            	File inputFile = (File) mInboundReplys.get(messageId);
            	renameProcessedFile(inputFile);
            } 
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "InboundMessageProcessor.Remove_exchange_msg_id", messageId);
            }
            mInboundExchanges.remove(messageId);
        } else {
            mLogger.log(Level.SEVERE, "InboundMessageProcessor.Invalid_reply_msgId", messageId);
        }
    }
   
    private void renameInputFile(File inputFile) throws IOException {
        // Rename the input file
        String fileRenamed = inputFile.getAbsolutePath() + mMessages.getString("processed");
        File renameToFile = new File(fileRenamed);
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "InboundMessageProcessor.Rename_file", inputFile.getName());
        }
        
        boolean renamed = inputFile.renameTo(renameToFile);
        if (!renamed) {
            mLogger.log(Level.SEVERE, "InboundMessageProcessor.Failed_Rename", inputFile.getName());
            throw new IOException(mMessages.getString("InboundMessageProcessor.Failed_Rename", inputFile.getName()));
        }
    }
    
    private void renameProcessedFile(File inputFile) throws IOException {
        // Rename the input file name to filename_error.
        File processedFile = new File(inputFile.getAbsolutePath() + mMessages.getString("processed"));
        
        if (processedFile.exists()) {
            String errorFileName = inputFile.getAbsolutePath() + mMessages.getString("error");
            File errorFile = new File(errorFileName);
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "InboundMessageProcessor.Rename_to_error_file", new Object[] {inputFile.getName(), errorFileName});
            }
            
            boolean success = processedFile.renameTo(errorFile);
            if (!success) {
                mLogger.log(Level.SEVERE, "InboundMessageProcessor.Failed_Rename_error_file", 
                                           new Object[] {processedFile.getName(), errorFileName});
                throw new IOException(mMessages.getString("InboundMessageProcessor.Failed_Rename_error_file", 
                                      new Object[] {processedFile.getName(), errorFileName}));
            }
        }
    }
    
    private SAPFmOperation locateSAPFmOperation(QName opname) {
        return (SAPFmOperation) mEndpoint.getSAPOperations().get(opname);
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
    protected void validateInboundMessageExchangeProperties(SAPFmOperation operation, String mep) throws Exception {
    	// 1. Check if the Message Exchange Pattern is valid.
        if (mep == null ||
           (!mep.equals(EndpointMessageType.IN_ONLY) &&
            !mep.equals(EndpointMessageType.IN_OUT)) ) {
            throw new Exception(mMessages.getString("InboundMessageProcessor.Invalid_mep", mOperationName));
        }
        
        // 2. Check if required sap:message properties are present and valid on SAP binding Input element
        WSDLInput wsdlInput = operation.getSAPOperationInput();
        if (wsdlInput == null) {
            throw new Exception(mMessages.getString("InboundMessageProcessor.Invalid_No_SAPInput", mOperationName));
        }
        
        SAPMessage sapMessage = wsdlInput.getSAPMessage();
        if (sapMessage == null) {
            throw new Exception(mMessages.getString("InboundMessageProcessor.Invalid_No_SAPMessage", mOperationName));
        }
        
        /*
        if (sapMessage.getFileName() == null) {
            throw new Exception(mMessages.getString("IMP_Invalid_No_SAPMessage_FileName", mOperationName));
        }
        
        if (!sapMessage.getFileType().equals(SAPMessage.FILE_TYPE_TEXT) &&
            !sapMessage.getFileType().equals(SAPMessage.FILE_TYPE_BINARY)) {
            throw new Exception(mMessages.getString("IMP_Invalid_File_TYPE", new Object[] {sapMessage.getFileType(), mOperationName}));
        }
        
        if (sapMessage.getFileUseType().equals(SAPMessage.FILE_USE_TYPE_ENCODED) &&
            (sapMessage.getFileEncodingStyle() == null ||
             sapMessage.getFileEncodingStyle().equals(""))) {
            throw new Exception(mMessages.getString("IMP_Invalid_No_FileMessage_EncodingStyle", mOperationName));
        }
        
        // 3. Check if required sap:message properties are present and valid on SAP binding Output element
        if (mep.equals(EndpointMessageType.IN_OUT)) {
            WSDLOutput wsdlOutput = operation.getSAPFmOperationOutput();
            
            if (wsdlOutput == null) {
                throw new Exception(mMessages.getString("IMP_Invalid_No_SAPOutput", mOperationName));
            }
            
            sapMessage = wsdlOutput.getSAPMessage();
            if (sapMessage == null) {
                throw new Exception(mMessages.getString("IMP_Invalid_No_SAPMessage", mOperationName));
            }
            
            if (sapMessage.getFileName() == null) {
                throw new Exception(mMessages.getString("IMP_Invalid_No_SAPMessage_FileName", mOperationName));
            }
            
            if (!sapMessage.getFileType().equals(SAPMessage.FILE_TYPE_TEXT) &&
                !sapMessage.getFileType().equals(SAPMessage.FILE_TYPE_BINARY)) {
                throw new Exception(mMessages.getString("IMP_Invalid_File_TYPE", new Object[] {sapMessage.getFileType(), mOperationName}));
            }
            
            if (sapMessage.getFileUseType().equals(SAPMessage.FILE_USE_TYPE_ENCODED) &&
                (sapMessage.getFileEncodingStyle() == null ||
                 sapMessage.getFileEncodingStyle().equals(""))) {
                throw new Exception(mMessages.getString("IMP_Invalid_No_SAPMessage_EncodingStyle", mOperationName));
            }
        }
         */
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
            mLogger.log(Level.INFO, "InboundMessageProcessor.locate_EP",
                new Object[] {mEndpoint.getServiceName(), mEndpoint.getEndpointName()});
        }
        return (activatedEndpoint);
    }
    
    /** Package protected method 
     *  Used solely for JUnit test purposes
     */
    //void setSAPNormalizer(SAPNormalizer normalizer) {
    //    mNormalizer = normalizer;
    //}
    
    /** Package protected method 
     *  Used solely for JUnit test purposes
     */
    static void setInboundExchangeIds(Map exchangeIds) {
        mInboundExchanges = exchangeIds;
    }
    
    /** Package protected method 
     *  Used solely for JUnit test purposes
     */
    void setInboundReplyIds(Map replyIds) {
        mInboundReplys = replyIds;
    }
}
