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
 * @(#)ABPEngineChannelSimulatorAdaptor.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.bpel.jbiadaptor.test.atomicbp;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.StringWriter;
import java.rmi.server.UID;
import java.util.Properties;

import javax.jbi.messaging.MessageExchange;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.wsdl.Message;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;

import com.sun.jbi.systemic.quality.propagation.api.ConfigManager;

import junit.framework.Assert;

import org.custommonkey.xmlunit.XMLUnit;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.engine.bpel.DeploymentBindings;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.Event;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainerFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.JBIMessageImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.ResponseInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.util.PropagationContext;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineDriver;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.UtilityClass;

import org.custommonkey.xmlunit.Diff;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 *
 *
 * @author Sun Microsystems
 */
public class ABPEngineChannelSimulatorAdaptor extends EngineChannelSimulatorAdaptor {

	UtilityClass mUtilClass;
	
	Properties mProps;
	
	Engine mEng;
	
	DeploymentBindings mDeplBindings;
	
	RBPELProcess mProcess;
	
	File mOutFile;
	
	public ABPEngineChannelSimulatorAdaptor(UtilityClass utility, Properties props, 
			Engine eng, DeploymentBindings deplBindings, RBPELProcess process) {
		mUtilClass = utility;
		mProps = props;
		mEng = eng;
		mDeplBindings = deplBindings;
		mProcess = process;
		
		String fileName = mProps.getProperty("ACT_OUT_FILE_PATH");
		mOutFile = new File(fileName);
	}

	@Override
	public Object invoke(MessageContainer msgContainer,
			RuntimePartnerLink partnerLink, QName operation1, boolean oneWay,
			RBPELProcess process) {
	
		String operation = operation1.getLocalPart();
	
		writeToOutFile("invoke", operation);
		String operationName = operation.toUpperCase();
        try {
        	// validate the incoming message
        	compareIncomingMessage(msgContainer, operation);
            // Check to see if the transaction needs to be matched.
            String matchTransStr = mProps.getProperty(operationName + "_TRANSACTION_MATCH", "false");
            PropagationContext propContext = msgContainer.getPropagationContext();
            if (propContext == null) {
            	throw new Exception("PropagationContext is null");
            }
            Transaction tran = (Transaction) 
            	propContext.getParentMessageExchange().getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
            if (matchTransStr.equalsIgnoreCase("true")) {
            	// Get the initial transaction.
            	Transaction initTran = EngineDriver.lookupTransaction(EngineDriver.getInitialMsgExId());
            	
            	if ((tran == null) || (!tran.equals(initTran))) {
            		Assert.fail(operation + ": Failed to match transaction.");
            	}
            	
            	if (ConfigManager.TRANSACTIONTYPE.JOIN_PARENT != propContext.getTransactionType()) {
            		Assert.fail(operation + ": Failed to match transaction type.");
            	}
            	
            	
            } else {
            	if (ConfigManager.TRANSACTIONTYPE.NEVER != propContext.getTransactionType()) {
            		Assert.fail(operation + ": Failed to match transaction type.");
            	}
            }
            
            // Get the new invoke operation if any.
            String requestOperation = mProps.getProperty(operationName + "_REQ_OPER_NAME", "NONE");
            if (!requestOperation.equals("NONE")) {
            	sendRequest(requestOperation);
            }

            String messageExchangeId = new UID().toString();
            if (oneWay) {
            	MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(messageExchangeId, null, null);
            	//MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(messageExchangeId, null);
            	statusContainer.setTransaction(msgContainer.getTransaction());
            	ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
            			mProcess, Event.DONE, statusContainer.getId()); 
            	mEng.process(event, statusContainer);
            } else {
            	EngineDriver.registerMsgExId(messageExchangeId, operation);
            	String respMesgType = mProps.getProperty(operationName + "_REPLY_MESG_TYPE");
                QName respMsgTypeQName = QName.valueOf(respMesgType);
                Message wsdlMessage = mProcess.getWSDLMessage(respMsgTypeQName);
                
                String ipFileLoc = mProps.getProperty(operationName + "_INPUT");
                String inputXML = EngineDriver.getXMLString(ipFileLoc);
                System.out.println("Reply Message: \n");
                System.out.println(inputXML);
                Document doc = EngineDriver.getDocument(ipFileLoc);
                JBIMessageImpl outMsg = new JBIMessageImpl(doc, wsdlMessage);

                MessageContainer responseContainer = MessageContainerFactory.createMessage(messageExchangeId, outMsg, 
                		msgContainer.getCRMPInvokeId(), msgContainer.getTransaction());
                ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                		mProcess, Event.REPLY_FAULT, responseContainer.getId());
                mEng.process(event, responseContainer);
            }
            return messageExchangeId;
            
        } catch (Exception e) {
            String messageExchangeId = new UID().toString();
            MessageContainer statusContainer = MessageContainerFactory.createErrorStatus(messageExchangeId,
                    e.getMessage(), null, null);
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
            		mProcess, Event.ERROR, statusContainer.getId()); 
            mEng.process(event, statusContainer);
            e.printStackTrace();
            Assert.fail(e.getMessage());
            return messageExchangeId;
        }
	}

	@Override
	public void sendInOnlyRequestDoneStatus(String msgExchangeId) {
		String operationName = EngineDriver.getOperationName(msgExchangeId);
		if (operationName == null) {
			throw new RuntimeException("Unable to associate messageExchangeId with an operation.");
		} else {
			EngineDriver.unregisterMsgExId(msgExchangeId);
		}
		writeToOutFile("sendInOnlyRequestDoneStatus", operationName);
		
		String initMsgExId = EngineDriver.getInitialMsgExId();
		if (msgExchangeId.equals(initMsgExId)) {
			Transaction transaction = EngineDriver.lookupTransaction(msgExchangeId);
	        TransactionManager tm = 
	        	(TransactionManager) BPELSERegistry.getInstance().lookup(TransactionManager.class.getName());
	        try {
	            tm.resume(transaction);
	            transaction.commit();
	        } catch (Exception e) {
	            e.printStackTrace();
	            Assert.fail(e.getMessage());
	        }
		}
	}

	@Override
	public void sendResponseDoneStatus(String msgExchangeId) {
		String operationName = EngineDriver.getOperationName(msgExchangeId);
		if (operationName == null) {
			throw new RuntimeException("Unable to associate messageExchangeId with an operation.");
		} else {
			EngineDriver.unregisterMsgExId(msgExchangeId);
		}
		writeToOutFile("sendResponseDoneStatus", operationName);
	}
	
	

	@Override
	public void reply(MessageContainer msgContainer) {
		String operation = EngineDriver.getOperationName(msgContainer.getId());
		if (operation == null) {
			Assert.fail("Unable to associate messageExchangeId with an operation.");
		} else {
			EngineDriver.unregisterMsgExId(msgContainer.getId());
		}
		writeToOutFile("reply", operation);
		
		try {
			compareIncomingMessage(msgContainer, operation);
			
			String initMsgExId = EngineDriver.getInitialMsgExId();
			if (msgContainer.getId().equals(initMsgExId)) {
				Transaction transaction = EngineDriver.lookupTransaction(initMsgExId);
		        TransactionManager tm = 
		        	(TransactionManager) BPELSERegistry.getInstance().lookup(TransactionManager.class.getName());
		        try {
		            tm.resume(transaction);
		            transaction.commit();
		        } catch (Exception e) {
		            e.printStackTrace();
		            Assert.fail(e.getMessage());
		        }
			}
			
			MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(msgContainer.getId(), null, null);
        	statusContainer.setTransaction(msgContainer.getTransaction());
        	ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
        			mProcess, Event.DONE, statusContainer.getId()); 
        	mEng.process(event, statusContainer);
		} catch (Exception e) {
			MessageContainer statusContainer = MessageContainerFactory.createErrorStatus(msgContainer.getId(),
                    e.getMessage(), null, null);
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
            		mProcess, Event.ERROR, statusContainer.getId()); 
            mEng.process(event, statusContainer);
            e.printStackTrace();
            Assert.fail(e.getMessage());
		}
	}

	/*
	 * 
	 */
	private void writeToOutFile(String method, String operation) {
		try {
			FileWriter fileWriter = new FileWriter(mOutFile, true);
			fileWriter.write(method + ":" + operation + "\r\n");
			fileWriter.close();
		} catch (Exception e) {
			Assert.fail(e.getMessage());
		}
	}
	
	/*
	 * 
	 */
	private void compareIncomingMessage(MessageContainer msgContainer, String operation) throws Exception {
		String operationName = operation.toUpperCase();
		JBIMessageImpl jbiMsg = (JBIMessageImpl) msgContainer.getContent();
		String receivedXML = EngineDriver.getXMLString(jbiMsg.getElement());
		System.out.println("received message"+ ":\n");
		System.out.println(receivedXML);
		String upFileloc = mProps.getProperty(operationName + "_OUTPUT");
		String expectedOutputXML = EngineDriver.getXMLString(upFileloc);
		System.out.println("Expected output: \n");
		System.out.println(expectedOutputXML);

		DocumentBuilderFactory docFactory =  DocumentBuilderFactory.newInstance();
		docFactory.setNamespaceAware(true);
		XMLUnit.setTestDocumentBuilderFactory(docFactory);
		XMLUnit.setControlDocumentBuilderFactory(docFactory);
		XMLUnit.setIgnoreWhitespace(true);

		Diff diff = XMLUnit.compare(expectedOutputXML, receivedXML);
		Assert.assertTrue(diff.similar());
		/*			TODO FIXME the following is also a good comparision test that we should enable. 
		 * 			But for the time being, i think because of a bug in XMLUnit, this fails.
		 * 			Revisit this later.	
		 *  
                    Diff docDiff = new Diff(expectedDoc, receivedDoc);
                    System.out.println(diff.toString());
                    DetailedDiff docDiff2 = new DetailedDiff(docDiff);
                    System.out.println(docDiff2.getAllDifferences());
                    assertTrue(docDiff.similar());
		 */
	}
	
	/*
	 * 
	 */
	private String sendRequest(String operation) {
		writeToOutFile("sendRequest", operation);
		String operationName = operation.toUpperCase();
		String messageExchangeId = new UID().toString();
		// send message    
		try {
			String ipFileLoc = mProps.getProperty(operationName + "_INPUT");
			String inputXML = EngineDriver.getXMLString(ipFileLoc);
			Document doc = EngineDriver.getDocument(ipFileLoc);

			String service = mProps.getProperty(operationName + "_SERVICE");
	        QName serviceQName = QName.valueOf(service);
	        String endPoint = mProps.getProperty(operationName + "_ENDPOINT");
	        String ipMsgType = mProps.getProperty(operationName + "_IP_MESG_TYPE");
	        QName ipMsgTypeQName = QName.valueOf(ipMsgType);
	        InComingKey key = mDeplBindings.createInComingBindingsKey(
	                serviceQName, endPoint, operation);
	        final InComingEventModel model = mDeplBindings.getInComingEventModel(key);

			Message wsdlMessage = mProcess.getWSDLMessage(ipMsgTypeQName);
			JBIMessageImpl jbiMsg = new JBIMessageImpl(doc, wsdlMessage);

			
			MessageContainer container = 
				MessageContainerFactory.createMessage(messageExchangeId, jbiMsg, null, null);

			System.out.println("sending message:\n"+ inputXML);
			InComingEventKeyImpl event = new InComingEventKeyImpl(model, Event.REQUEST);
			EngineDriver.registerMsgExId(messageExchangeId, operation);
			mEng.process(event, container);
		} catch (Exception e) {
			e.printStackTrace();
            Assert.fail(e.getMessage());
		}
		return messageExchangeId;
	}
	
}
