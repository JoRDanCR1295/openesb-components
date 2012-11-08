/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.tester;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.util.Date;
import java.util.Properties;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;

/**
 *
 * @author radval
 */
public class TestFolder {

    public static final String PROP_CONFIG_INPUT_MESSAGE_COUNT = "inputMessageCount";

    public static final String PROP_CONFIG_DESTINATION_SERVICE_NAME = "destinationServiceName";
    
    public static final String PROP_CONFIG_DESTINATION_ENDPOINT_NAME = "destinationEndpointName";
    
    public static final String PROP_CONFIG_DESTINATION_INTERFACE_NAME = "destinationInterfaceName";
    
    public static final String PROP_CONFIG_DESTINATION_OPERATION_NAME = "destinationOperationName";
    
    public static final String PROP_CONFIG_RECEIVING_SERVICE_NAME = "receivingServiceName";
    
    public static final String PROP_CONFIG_RECEIVING_ENDPOINT_NAME = "receivingEndpointName";
    
    private static final String PROP_CONFIG_INPUT_MESSAGE_FILE = "input.xml";

    private static final String PROP_CONFIG_INPUT_MESSAGE_LOG_FILE = "input.log";
    
    private static final String PROP_CONFIG_OUTPUT_MESSAGE_LOG_FILE = "output.log";
    
    private static final String PROP_CONFIG_IS_WRITE_OUTPUT_MESSAGE_TO_LOG_FILE = "isWriteOutputMessageToLogFile";
    
    private static final String PROP_CONFIG_IS_ALWAYS_READ_INPUT_MESSAGE_FROM_INPUT_FILE = "isAlwaysReadInputMessageFromInputFile";
    
    private File mTestDir = null;
    
    private ComponentContext mContext = null;
    
    private Properties mProp = null;
    
    private String mInputMessageCount;
    
    private int mInputMessageCountInt = 0;
    
    private String mDestinationServiceName;
    
    private String mDestinationEndpointName;
    
    private String mDestinationInterfaceName;
    
    private String mDestinationOperationName;
    
    private QName mDestinationServiceQName;
    
    private QName mDestinationInterfaceQName;
    
    private QName mDestinationOperationQName;
    
    private String mReceivingServiceName;
    
    private String mReceivingEndpointName;
    
    private QName mReceivingServiceQName;
    
    private String mIsWriteOutputMessageToLogFile;
    
    private Boolean mIsWriteOutputMessageToLogFileBool = Boolean.TRUE;
    
    private String mIsAlwaysReadInputMessageFromInputFile;
    
    private Boolean mIsAlwaysReadInputMessageFromInputFileBool = Boolean.TRUE;
    
    private File mInputMessageFile;
    
    private File mInputMessageLogFile;
    
    private File mOutputMessageLogFile;
    
    
    
    private TestContext mTestContext;
    
    private boolean mIsReset = false;
    
    private Integer mNumberOfReceivedMessage = 0;
    
    public TestFolder(File testDir, ComponentContext context, TestContext testContext) throws Exception {
        this.mTestDir = testDir;
        this.mContext = context;
        this.mTestContext = testContext;
        
        reset();
    }
    
    public void execute() {
        try{
            reset();
            
            if(mIsReset) {
                mIsReset = false;
                
                
            }
            
            //activate receiving endpoint
            if(this.mReceivingServiceQName != null && this.mReceivingEndpointName != null) {
                ServiceEndpoint receivingEndpoint = mContext.activateEndpoint(this.mReceivingServiceQName, this.mReceivingEndpointName);
                this.mTestContext.addReceivingEndpoint(receivingEndpoint, this);
                
            }
            
            DeliveryChannel channel = mContext.getDeliveryChannel();
            MessageExchangeFactory factory = channel.createExchangeFactory();
            ServiceEndpoint endpoint = mContext.getEndpoint(this.mDestinationServiceQName, this.mDestinationEndpointName);
            
            if(endpoint != null) {
            		StringBuffer message = new StringBuffer();
            		message.append(new Date());
            		message.append(" Start sending messages.");
                    writeToInputLog(message.toString());
//                    String msg = XmlUtil.toXml(document.getDocumentElement(), "UTF-8", false);
//                    writeToInputLog(new Date() +" Message is: " + msg );
                    
                    message = new StringBuffer();
            		message.append(new Date());
            		message.append(" Number of messages to be sent: ");
    				message.append(this.mInputMessageCount);
                    writeToInputLog(message.toString());
                    Document document = null;
                    DOMSource domSource = null;
                    
                    if(!mIsAlwaysReadInputMessageFromInputFileBool.booleanValue()) {
                    	document = readInputDocument();
                        if(document != null) {
                            domSource = new DOMSource(document.getDocumentElement());
                            message = new StringBuffer();
                    		message.append(new Date());
                    		message.append(" Created message only once for all runs. ");
                    		writeToInputLog(message.toString());
                        }
                    } 
                    
                    int inputMessageCount = mInputMessageCountInt;
                    for(int i =0; i < inputMessageCount; i++) {
	                        //for test we want to simulate that
	                        //we always read document this is default setting
                    		//overriden from test.properties
	                    	if(mIsAlwaysReadInputMessageFromInputFileBool.booleanValue()) {
	                    		document = readInputDocument();
	                    		if(document != null) {
	                    			domSource = new DOMSource(document.getDocumentElement());
	                    		}
	                    	}
	                    	
                            InOnly inOnly = factory.createInOnlyExchange();
                            inOnly.setProperty(Constants.ME_PROP_TESTFOLDER, this);
                            inOnly.setProperty(Constants.ME_PROP_MESSAGE_EXCHANGE_NUMBER, i+1);
                            inOnly.setEndpoint(endpoint);
                            if(this.mDestinationInterfaceQName != null) {
                                    inOnly.setInterfaceName(this.mDestinationInterfaceQName);
                            }

                            if(this.mDestinationOperationQName != null) {
                                    inOnly.setOperation(mDestinationOperationQName);
                            }
                            NormalizedMessage nmsg = inOnly.createMessage();
                            nmsg.setContent(domSource);
                            inOnly.setInMessage(nmsg);
                            channel.send(inOnly);
                        }
                    }
                    
		            StringBuffer message = new StringBuffer();
		    		message.append(new Date());
		    		message.append(" Done sending all messages : ");
		    		message.append(this.mInputMessageCount);
		    		writeToInputLog(message.toString());
    		    
            
        } catch(Exception ex) {
            ex.printStackTrace();
        }
    }
     
    public void reset() {
    	this.mInputMessageCountInt = 0;
    	this.mNumberOfReceivedMessage = 0;
    	this.mIsReset = true;
        this.mIsWriteOutputMessageToLogFileBool = Boolean.TRUE;
        this.mIsAlwaysReadInputMessageFromInputFileBool = Boolean.TRUE;
        
        try {
        File testConfigurationFile = new File(this.mTestDir, Constants.PROP_CONFIG_TEST_CONFIG_FILE);
        if(testConfigurationFile.exists()) {
            mProp = Util.readConfiguration(testConfigurationFile);
            if(mProp != null) {
                this.mInputMessageCount = mProp.getProperty(PROP_CONFIG_INPUT_MESSAGE_COUNT);
                this.mDestinationServiceName = mProp.getProperty(PROP_CONFIG_DESTINATION_SERVICE_NAME);
                this.mDestinationEndpointName = mProp.getProperty(PROP_CONFIG_DESTINATION_ENDPOINT_NAME);
                this.mDestinationInterfaceName = mProp.getProperty(PROP_CONFIG_DESTINATION_INTERFACE_NAME);
                this.mDestinationOperationName = mProp.getProperty(PROP_CONFIG_DESTINATION_OPERATION_NAME);
                
                this.mReceivingServiceName = mProp.getProperty(PROP_CONFIG_RECEIVING_SERVICE_NAME);
                this.mReceivingEndpointName = mProp.getProperty(PROP_CONFIG_RECEIVING_ENDPOINT_NAME);
                
                this.mIsWriteOutputMessageToLogFile = mProp.getProperty(PROP_CONFIG_IS_WRITE_OUTPUT_MESSAGE_TO_LOG_FILE);
                
                if(this.mInputMessageCount != null) {
                    this.mInputMessageCountInt = Integer.parseInt(this.mInputMessageCount);
                }
                
                if(this.mDestinationServiceName != null) {
                    mDestinationServiceQName = QName.valueOf(this.mDestinationServiceName);
                }
                
                if(this.mReceivingServiceName != null) {
                    mReceivingServiceQName = QName.valueOf(this.mReceivingServiceName);
                }
                
                if(this.mDestinationInterfaceName != null) {
                	mDestinationInterfaceQName = QName.valueOf(this.mDestinationInterfaceName);
                }
                
                if(this.mDestinationOperationName != null) {
                	mDestinationOperationQName = QName.valueOf(this.mDestinationOperationName);
                }
                
                if(this.mIsWriteOutputMessageToLogFile != null) {
                    mIsWriteOutputMessageToLogFileBool = Boolean.valueOf(this.mIsWriteOutputMessageToLogFile);
                    
                }
                
                if(this.mIsAlwaysReadInputMessageFromInputFile != null) {
                    mIsAlwaysReadInputMessageFromInputFileBool = Boolean.valueOf(this.mIsAlwaysReadInputMessageFromInputFile);
                    
                }
                
            }
            
        }
        
        
        if(mInputMessageCount == null ) {
            mInputMessageCount = "1";
        }
        
        init();
        }catch(Exception ex) {
            ex.printStackTrace();
        }
    }
    
    private void init() {
        try {
            mInputMessageFile = new File(mTestDir, PROP_CONFIG_INPUT_MESSAGE_FILE);
            mInputMessageLogFile = new File(mTestDir, PROP_CONFIG_INPUT_MESSAGE_LOG_FILE);
            mOutputMessageLogFile = new File(mTestDir, PROP_CONFIG_OUTPUT_MESSAGE_LOG_FILE);
            
            
        } catch(Exception ex) {
            ex.printStackTrace();
        }
    }
    
    public void writeToInputLog(String data) {
        synchronized(mInputMessageLogFile) {
            try {
                FileWriter writer = new FileWriter(mInputMessageLogFile, true);
                writer.write(data);
                writer.write("\n");
                writer.close();
            } catch(Exception ex) {
                ex.printStackTrace();
            }
        }
    }
    
    public void writeToOutputLog(String data) {
        synchronized(mOutputMessageLogFile) {
            try {
                FileWriter writer = new FileWriter(mOutputMessageLogFile, true);
                writer.write(data);
                writer.write("\n");
                writer.close();
            } catch(Exception ex) {
                ex.printStackTrace();
            }
        }
    }
    
    public Document readInputDocument() throws Exception {
        Document document = null;
        if(mInputMessageFile != null) {
            FileInputStream fIn = new FileInputStream(mInputMessageFile);
            document= XmlUtil.createDocument(true, fIn);
            fIn.close();
        }
        
        return document;
    }
    
//    public int getInputMessageCount() {
//    	return mInputMessageCountInt;
//    }
//    
//    public QName getDestinationServiceName() {
//        return mDestinationServiceQName;
//    }
//    
//    public String getDestinationEndpointName() {
//        return this.mDestinationEndpointName;
//    }
//    
//    
//    public QName getReceivingServiceName() {
//        return mReceivingServiceQName;
//    }
//    
//    public String getReceivingEndpointName() {
//        return this.mReceivingEndpointName;
//    }
    
    public File getTestDir() {
        synchronized(this.mTestDir) {
            return this.mTestDir;
        }
    }
   
    
    
    public boolean isWriteResponseMessageToFile() {
        synchronized(mIsWriteOutputMessageToLogFileBool) {
            return mIsWriteOutputMessageToLogFileBool.booleanValue();
        }
    }
    
    public int getNumberOfReceivedMessage() {
    	synchronized(this.mNumberOfReceivedMessage) {
    		return this.mNumberOfReceivedMessage;
    	}
    }
    
    public int incrementNumberOfReceivedMessage() {
    	synchronized(this.mNumberOfReceivedMessage) {
    		return ++this.mNumberOfReceivedMessage;
    	}
    }

}
