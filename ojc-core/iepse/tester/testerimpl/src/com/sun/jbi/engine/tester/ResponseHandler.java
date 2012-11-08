/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.tester;

import java.util.Date;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 *
 * @author radval
 */
public class ResponseHandler implements Runnable {

    private ComponentContext mContext;
    
    private ExecutorService mResponseHandlerService = Executors.newCachedThreadPool();
    
    private TestContext mTestContext;
    
    public ResponseHandler(ComponentContext context, TestContext testContext) {
        this.mContext = context;
        this.mTestContext = testContext;
    }
    
    public void shutdown() {
    	synchronized (mResponseHandlerService) {
    		mResponseHandlerService.shutdown();
		}
    	
    }
    
    public void run() {
        try {
            DeliveryChannel channel = mContext.getDeliveryChannel();
            MessageExchange me = channel.accept();
            synchronized(mResponseHandlerService) {
	            if(!mResponseHandlerService.isShutdown()) {
	                mResponseHandlerService.submit(new ResponseHandlerThread(me));
	            }
            }
        } catch(Exception ex) {
            ex.printStackTrace();
        }
    }

    class ResponseHandlerThread implements Runnable {

        private MessageExchange mME;
        
        public ResponseHandlerThread(MessageExchange me) {
            this.mME = me;
        }
        
        public void run() {
        	if( this.mME == null) {
        		return;
        	}
        	
        	
            ServiceEndpoint se = this.mME.getEndpoint();
            
            TestFolder testFolder = (TestFolder) this.mME.getProperty(Constants.ME_PROP_TESTFOLDER);
            if(testFolder != null && this.mME.getStatus() == ExchangeStatus.DONE) {
                //got a done response for a message sent by tester
                //tester in sender mode
                Integer messageNumber = (Integer) this.mME.getProperty(Constants.ME_PROP_MESSAGE_EXCHANGE_NUMBER);
                StringBuffer message = new StringBuffer();
                message.append(new Date());
                message.append(" Got DONE for message number ");
                message.append(messageNumber);
                message.append(" sent from test: ");
                message.append(testFolder.getTestDir().getName());
                
                testFolder.writeToOutputLog(message.toString());
                
            } else {
                //got message from a component
                //test in receiver mode
                TestFolder folder = mTestContext.getTestFolder(se);
                if(folder != null) {
                	int receiveMessages = folder.incrementNumberOfReceivedMessage();
                	StringBuffer message = new StringBuffer();
                    message.append(new Date());
                    message.append(" Got message ");
                    message.append(receiveMessages);
                    message.append(" at receiving endpoint for test: ");
                    message.append(folder.getTestDir().getName());
                    
                	folder.writeToOutputLog(message.toString());
                    if(this.mME instanceof InOnly) {
                        InOnly inOnly = (InOnly) this.mME;
                        NormalizedMessage inMessage = inOnly.getInMessage();
                        if(inMessage != null) {
                            Source source = inMessage.getContent();
                            Node node = null;
                            
                            if(source instanceof DOMSource) {
                                DOMSource dom = (DOMSource) source;
                                node = dom.getNode();
                            } else {
                                try {
                                    Document doc = XmlUtil.createDocumentFromSource(source);
                                    node = doc.getDocumentElement();
                                } catch(Exception ex) {
                                    ex.printStackTrace();
                                }
                            }
                            
                            boolean isWriteResponseMessageToLogFile = folder.isWriteResponseMessageToFile();
                            if(isWriteResponseMessageToLogFile) {
                                String xmlMessage = XmlUtil.toXml(node, "UTF-8", false);
                                message = new StringBuffer();
                                message.append(new Date());
                                message.append(" Message is: ");
                                message.append(xmlMessage);
                                
                                folder.writeToOutputLog(message.toString());
                            }
                            
                            //now send done
                            try {
                            	DeliveryChannel channel = mContext.getDeliveryChannel();
                            	inOnly.setStatus(ExchangeStatus.DONE);
                            	channel.send(inOnly);
                            	
                            	message = new StringBuffer();
                            	message.append(new Date());
                                message.append(" Sending done for received message ");
                                message.append(receiveMessages);
                                
                            	folder.writeToOutputLog(message.toString());
                            } catch(Exception ex) {
                            	ex.printStackTrace();
                            }
                        }
                    }
                }
                
            }
        }
        
    }
}
