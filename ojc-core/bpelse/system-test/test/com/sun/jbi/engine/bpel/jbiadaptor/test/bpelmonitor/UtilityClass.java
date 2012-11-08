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
 * @(#)UtilityClass.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpelmonitor;

import java.rmi.server.UID;
import java.util.Properties;

import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;

import junit.framework.Assert;

import org.custommonkey.xmlunit.Diff;
import org.custommonkey.xmlunit.XMLUnit;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

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
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.EngineChannelSimulatorAdaptor;
import com.sun.jbi.engine.bpel.jbiadaptor.test.common.TestDBConnImpl;
import javax.wsdl.Message;

/**
 * @author Sun Inc
 * Aug 6, 2006
 */
public class UtilityClass {
    
    public void associateTestAssign1Channel(final Properties props, 
                                            final Engine eng, 
                                            DeploymentBindings deplBindings) 
            throws Exception {

        final InComingEventModel model = associateChannel(props, deplBindings);
        EngineChannelSimulatorAdaptor channel = new EngineChannelSimulatorAdaptor() {
            public void reply(MessageContainer msgContainer) {
                acceptMessageForTest(props, msgContainer, model, eng);
            }
        };

        eng.setOutChannel(channel);
    }

    private String acceptMessageForTest(Properties props, 
                                      MessageContainer container, 
                                      InComingEventModel model, 
                                      Engine eng) {
        try {
            JBIMessageImpl jbiMsg = (JBIMessageImpl) container.getContent();
            String receivedXML = EngineDriver.getXMLString(jbiMsg.getElement());
            System.out.println("received message"+ ":\n");
            System.out.println(receivedXML);
//            Document receivedDoc = jbiMsg.getElement().getOwnerDocument();
            String upFileloc = props.getProperty("OUTPUT");
            String expectedOutputXML = EngineDriver.getXMLString(upFileloc);
            System.out.println("Expected output: \n");
            System.out.println(expectedOutputXML);
//            Document expectedDoc = EngineDriver.getDocument(upFileloc);
            
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
            
            String messageExchangeId = new UID().toString();
            MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(messageExchangeId, null, null);
            //This is used only for one way invokes.
            statusContainer.setTransaction(container.getTransaction());
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                    model.getBPELProcess(), Event.DONE, statusContainer.getId()); 
            eng.process(event, statusContainer);
            //eng.processDone(statusContainer, model.getBPELProcess());
            return messageExchangeId;
            
        } catch (Exception e) {
            String messageExchangeId = new UID().toString();
            MessageContainer statusContainer = MessageContainerFactory.createErrorStatus(messageExchangeId,
                    e.getMessage(), null, null);
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                    model.getBPELProcess(), Event.ERROR, statusContainer.getId()); 
            eng.process(event, statusContainer);
            //eng.processDone(statusContainer, model.getBPELProcess());
            e.printStackTrace();
            Assert.fail(e.getMessage());
            return messageExchangeId;
        }
    }
    
    public void initiateBPInstance(final Properties props, 
            final Engine engine, DeploymentBindings deplBindings) throws Exception {
        
        // send message         
        String ipFileLoc = props.getProperty("INPUT");
        String inputXML = EngineDriver.getXMLString(ipFileLoc);
        Document doc = EngineDriver.getDocument(ipFileLoc);

        String service = props.getProperty("SERVICE");
        QName serviceQName = QName.valueOf(service);
        String endPoint = props.getProperty("ENDPOINT");
        String oper = props.getProperty("OPERATION");
        String ipMsgType = props.getProperty("IP_MESG_TYPE");
        QName ipMsgTypeQName = QName.valueOf(ipMsgType);
        
        InComingKey key = deplBindings.createInComingBindingsKey(
                serviceQName, endPoint, oper);
        final InComingEventModel model = deplBindings.getInComingEventModel(key);
        Message wsdlMessage = model.getBPELProcess().getWSDLMessage(ipMsgTypeQName);
        
        JBIMessageImpl jbiMsg = new JBIMessageImpl(doc, wsdlMessage);
        
        String messageExchangeId = new UID().toString();
        MessageContainer container = 
                MessageContainerFactory.createMessage(messageExchangeId, jbiMsg, null, null);
        initializeAndSetTransaction(container);
        System.out.println("sending message:\n"+ inputXML);
        InComingEventKeyImpl event = new InComingEventKeyImpl(model, Event.REQUEST);
        engine.process(event, container);
    }
    
    private void initializeAndSetTransaction(MessageContainer container) throws Exception{
        TransactionManager tm = (TransactionManager) BPELSERegistry.getInstance().lookup(TransactionManager.class.getName());
        tm.begin();
        Transaction transaction = tm.getTransaction();
        container.setTransaction(transaction);
        tm.suspend();        
        EngineDriver.registerTransaction(container.getId().toString(), transaction);
    }
    
    /**
     * associates a Channel with the engine. This channel is used to process/direct events
     * for SubBP incocation(InOut message exchange, ex: MainBP -> SubBP).
     * To be consistent with the existing frame work the message exchange id's associated
     * with the BP's are 
     * MainBP : TestID
     * SubBP  : TestIDSubBP
     * @param props
     * @param eng
     * @param deplBindings
     * @throws Exception
     */
    public void associateSubBPInvokeChannel
    	(final Properties props, final Engine eng, 
         final DeploymentBindings deplBindings) 
    throws Exception {
        final InComingEventModel mainBPModel = associateChannel(props, deplBindings);
        final InComingEventModel subBPModel = getSubBPModel(props, deplBindings);
        EngineChannelSimulatorAdaptor channel = new EngineChannelSimulatorAdaptor() {
            public Object invoke(MessageContainer msgContainer, RuntimePartnerLink partnerLink, QName operation, boolean oneWay, RBPELProcess process) {
                if (oneWay) {
                	// this is the last invoke of the MainBP
                	return acceptMessageForTest(props, msgContainer, mainBPModel, eng);
                } else {
                	//process the two-way invocation of the SubBP from the MainBP
                    return process2WaySubBPInvoke(props, msgContainer, eng, mainBPModel, deplBindings);
                }
            }
            
            public void sendResponseDoneStatus(String msgExchangeId){
                MessageContainer statusContainer = MessageContainerFactory.createDoneStatus(msgExchangeId, null, null);
                ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                        subBPModel.getBPELProcess(), Event.DONE, statusContainer.getId()); 
            	//The MainBP after handling the two-way invoke response sends the DONE status
            	// by calling this method, this has to be send to the SubBP for the reply 
            	// actvity to complete.
            	eng.process(event, statusContainer);            	
            }
            /** @see com.sun.jbi.engine.bpel.core.bpel.engine.Channel#reply(com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer)
             */
            public void reply(MessageContainer msgContainer) {
            	// the reply from the SubBP is processed.
            	processSubBPResponse(props, msgContainer, mainBPModel, subBPModel, eng);
            }            
        };
        eng.setOutChannel(channel);        
    	
    }

    public void recoverForCRMP(final Properties props, final Engine eng,
			final DeploymentBindings depBindings) throws Exception {
		new Thread() {
			public void run() {
				try {
					recover(props, eng, depBindings);
				} catch (Exception ex) {
					System.out.println("Exception thrown in recoverForCRMP: "
							+ ex);
				}
			}
		}.start();
		
		recover(props, eng, depBindings);
	}

    
    // utility for gettting the InComingEventModel for the SubBP
    private InComingEventModel getSubBPModel
    	(Properties props, DeploymentBindings deplBindings) 
    throws Exception {
    	return getSubBPModel("", props, deplBindings);
    }

    /*
     * Utility for gettting the InComingEventModel for the SubBP
     */
    private InComingEventModel getSubBPModel(String prefix,
            Properties props,
            DeploymentBindings deplBindings) 
    throws Exception {
    	String service = props.getProperty("SUB_BP_SERVICE");
        QName serviceQName = QName.valueOf(service);
        String endPoint = props.getProperty("SUB_BP_ENDPOINT");
        String oper = props.getProperty("SUB_BP_OPERATION");
        String subBPIPMsgType = props.getProperty("SUB_BP_IP_MESG_TYPE");
        InComingKey key = deplBindings.createInComingBindingsKey(
                serviceQName, endPoint, oper);
        InComingEventModel model = deplBindings.getInComingEventModel(key);    	
    	return model;
    }
    
    /*
     * process the reply from the SubBP
     * container: the container of the SubBP reply activity
     */
    private void processSubBPResponse(Properties props, 
            MessageContainer container, 
            InComingEventModel mainBPModel,
            InComingEventModel subBPModel,
            Engine eng) {      
    	try {
    		// get the response message type of the MainBP 
    		String respMesgType = props.getProperty("INVOKE_2WAY_RESPONSE");
            QName respMsgTypeQName = QName.valueOf(respMesgType);
            Message wsdlMessage = mainBPModel.getBPELProcess().getWSDLMessage(respMsgTypeQName);
            JBIMessageImpl inMsg = (JBIMessageImpl) container.getContent();
            
            //create the message associated with the MainBP two-way invoke response.
            Element elem = inMsg.getElement();
            String typeVal = elem.getAttribute("type");
            QName typeValQName = QName.valueOf(typeVal);
            QName outputType = new QName(null, respMsgTypeQName.getLocalPart(), typeValQName.getPrefix());
            elem.setAttribute("type", outputType.toString());
            JBIMessageImpl outMsg = new JBIMessageImpl(elem.getOwnerDocument(), wsdlMessage);

            Transaction tx = EngineDriver.lookupTransaction(container.getId());
            //create the MainBP two-way invoke activity response container
            MessageContainer responseContainer = 
                MessageContainerFactory.createMessage
                	(container.getId(), outMsg, container.getCRMPInvokeId(), tx);
            
             
            // send the response to the two-invoke of MainBP that is waiting for response
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                    mainBPModel.getBPELProcess(), Event.REPLY_FAULT, responseContainer.getId());
            eng.process(event, responseContainer);

            //eng.processResponse(responseContainer, mainBPModel.getBPELProcess());
            
    	}
        catch (Exception e) {
            MessageContainer statusContainer = MessageContainerFactory
                    .createErrorStatus(container.getId(), e.getMessage(), null, null);
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                    mainBPModel.getBPELProcess(), Event.ERROR, statusContainer.getId());
            eng.process(event, statusContainer);
            //eng.processDone(statusContainer, mainBPModel.getBPELProcess());
            e.printStackTrace();
            Assert.fail(e.getMessage());
        }
    	
    }
    
    /*
     * This is called from the two-way invoke of the MainBP to send the invcation to the 
     * SubBP. 
     * container : the input container of the MainBP two-way invoke activity
     * The SubBP invocation will have the message exchange id of "TestIDSubBP"
     */
    private String process2WaySubBPInvoke(Properties props, 
            MessageContainer container, 
            Engine eng,
            InComingEventModel mainBPModel,
            DeploymentBindings deplBindings) {

    	// create the SubBP InComingEventModel
    	String service = props.getProperty("SUB_BP_SERVICE");
        QName serviceQName = QName.valueOf(service);
        String endPoint = props.getProperty("SUB_BP_ENDPOINT");
        String oper = props.getProperty("SUB_BP_OPERATION");
        String subBPIPMsgType = props.getProperty("SUB_BP_IP_MESG_TYPE");
        QName subBPIPMsgTypeQName = QName.valueOf(subBPIPMsgType);
        InComingKey key = deplBindings.createInComingBindingsKey(
                serviceQName, endPoint, oper);
        InComingEventModel subBPModel = deplBindings.getInComingEventModel(key);
        
    	try {
    		Object crmpInvId = container.getCRMPInvokeId();
    		JBIMessageImpl inMsg = (JBIMessageImpl) container.getContent();
        
    		Element elem = inMsg.getElement();
    		String typeVal = elem.getAttribute("type");
    		QName typeValQName = QName.valueOf(typeVal);
    	
    		QName outputType = new QName(null, subBPIPMsgTypeQName.getLocalPart(), typeValQName.getPrefix());
    		elem.setAttribute("type", outputType.toString());
        
        
    		Message wsdlMessage = subBPModel.getBPELProcess().getWSDLMessage(subBPIPMsgTypeQName);
        
    		JBIMessageImpl outMsg = new JBIMessageImpl(elem.getOwnerDocument(), wsdlMessage);
            
            String messageExchangeId = new UID().toString();
            // set the transaction from the container to the new request container
            Transaction tx = container.getTransaction();
            EngineDriver.registerTransaction(messageExchangeId, tx);
    		MessageContainer subBPInvokeContainer = 
    				MessageContainerFactory.createMessage(messageExchangeId, outMsg, crmpInvId, tx);
    		// call the SubBP with the inbound request
    		InComingEventKeyImpl event = new InComingEventKeyImpl(subBPModel, Event.REQUEST);
            eng.process(event, subBPInvokeContainer);
            return messageExchangeId;
    	}
        catch (Exception e) {
            e.printStackTrace();
            Assert.fail(e.getMessage());
            return "";
        }
    	
    }
    
    
    public void associateInvokeChannel(final Properties props, 
                                       final Engine eng, 
                                       DeploymentBindings deplBindings) 
            throws Exception {

        final InComingEventModel model = associateChannel(props, deplBindings);
        EngineChannelSimulatorAdaptor channel = new EngineChannelSimulatorAdaptor() {
            public Object invoke(MessageContainer msgContainer, RuntimePartnerLink partnerLink, QName operation, boolean oneWay, RBPELProcess process) {
                return acceptMessageForTest(props, msgContainer, model, eng);
            }
        };
        eng.setOutChannel(channel);
    }
    
    public void associate2WayInvokeChannel(final Properties props, 
                                           final Engine eng, 
                                           DeploymentBindings deplBindings) 
            throws Exception {

        final InComingEventModel model = associateChannel(props, deplBindings);
        EngineChannelSimulatorAdaptor channel = new EngineChannelSimulatorAdaptor() {
            public Object invoke(MessageContainer msgContainer, RuntimePartnerLink partnerLink, QName operation, boolean oneWay, RBPELProcess process) {
                if (oneWay) {
                    return acceptMessageForTest(props, msgContainer, model, eng);
                } else {
                    return process2WayInvoke(props, msgContainer, model, eng);
                }
            }
            
            public void sendInvokeStatus(MessageContainer msgContainer, Exception ex) {
                // 2way invoke status is sent from the engine.
                // do nothing.
            }
        };
        eng.setOutChannel(channel);
    }
    
    public void associateInvokeChannelForInvokeThrowsFault(final Properties props, 
            final Engine eng, 
            DeploymentBindings deplBindings) 
    throws Exception {
        
        final InComingEventModel model = associateChannel(props, deplBindings);
        EngineChannelSimulatorAdaptor channel = new EngineChannelSimulatorAdaptor() {
            public Object invoke(MessageContainer msgContainer, RuntimePartnerLink partnerLink, QName operation, boolean oneWay, RBPELProcess process) {
                if (oneWay) {
                    return acceptMessageForTest(props, msgContainer, model, eng);
                } else {
                    return sendFaultFor2WayInvoke(props, msgContainer, model, eng);
                }
            }
            
            public void sendInvokeStatus(MessageContainer msgContainer, Exception ex) {
//              2way invoke status is sent from the engine.
//              do nothing.
            }
        };
        eng.setOutChannel(channel);
    }
    
    private String sendFaultFor2WayInvoke(Properties props, 
            MessageContainer container, 
            InComingEventModel model, 
            Engine eng) {      
        try {
            String respMesgType = props.getProperty("INVOKE_2WAY_RESPONSE");
            QName respMsgTypeQName = QName.valueOf(respMesgType);
            Message wsdlMessage = model.getBPELProcess().getWSDLMessage(respMsgTypeQName);
            JBIMessageImpl inMsg = (JBIMessageImpl) container.getContent();
            
            Element elem = inMsg.getElement();
            String typeVal = elem.getAttribute("type");
            QName typeValQName = QName.valueOf(typeVal);
//          QName outputType = new QName(null, typeValQName.getPrefix(), respMsgTypeQName.getLocalName());
            QName outputType = new QName(null, respMsgTypeQName.getLocalPart(), typeValQName.getPrefix());
            elem.setAttribute("type", outputType.toString());
            JBIMessageImpl outMsg = new JBIMessageImpl(elem.getOwnerDocument(), wsdlMessage);
            
            String messageExchangeId = new UID().toString();
            // register the transaction on the on the responsecontainer
            Transaction tx = container.getTransaction();
            EngineDriver.registerTransaction(messageExchangeId, tx);
            MessageContainer responseContainer = 
                MessageContainerFactory.createFault(messageExchangeId, outMsg, null, tx);
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                    model.getBPELProcess(), Event.REPLY_FAULT, responseContainer.getId());
            eng.process(event, responseContainer);
            //eng.processResponse(responseContainer, model.getBPELProcess());
            return messageExchangeId;            
        } 
        catch (Exception e) {
            String messageExchangeId = new UID().toString();            
            MessageContainer statusContainer = MessageContainerFactory.createErrorStatus(messageExchangeId,
                    e.getMessage(), null, null);
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                    model.getBPELProcess(), Event.ERROR, statusContainer.getId());
            eng.process(event, statusContainer);
            //eng.processDone(statusContainer, model.getBPELProcess());
            e.printStackTrace();
            Assert.fail(e.getMessage());
            return messageExchangeId;            
        }
    }
    
    private String process2WayInvoke(Properties props, 
                                   MessageContainer container, 
                                   InComingEventModel model, 
                                   Engine eng) {      
        try {
            String respMesgType = props.getProperty("INVOKE_2WAY_RESPONSE");
            QName respMsgTypeQName = QName.valueOf(respMesgType);
            Message wsdlMessage = model.getBPELProcess().getWSDLMessage(respMsgTypeQName);
            JBIMessageImpl inMsg = (JBIMessageImpl) container.getContent();
            
            Element elem = inMsg.getElement();
            String typeVal = elem.getAttribute("type");
            QName typeValQName = QName.valueOf(typeVal);
            //QName outputType = new QName(null, typeValQName.getPrefix(), respMsgTypeQName.getLocalName());
            QName outputType = new QName(null, respMsgTypeQName.getLocalPart(), typeValQName.getPrefix());
            elem.setAttribute("type", outputType.toString());
            JBIMessageImpl outMsg = new JBIMessageImpl(elem.getOwnerDocument(), wsdlMessage);

            String messageExchangeId = new UID().toString();
            Transaction tx = container.getTransaction();
            EngineDriver.registerTransaction(messageExchangeId, tx);
            MessageContainer responseContainer = 
                MessageContainerFactory.createMessage(messageExchangeId, outMsg, container.getCRMPInvokeId(), tx);
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                    model.getBPELProcess(), Event.REPLY_FAULT, responseContainer.getId());
            eng.process(event, responseContainer);
            //eng.processResponse(responseContainer, model.getBPELProcess());
            return messageExchangeId;
        } 
        catch (Exception e) {
            String messageExchangeId = new UID().toString();            
            MessageContainer statusContainer = MessageContainerFactory.createErrorStatus(messageExchangeId,
                    e.getMessage(), null, null);
            ResponseInComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                    model.getBPELProcess(), Event.ERROR, statusContainer.getId());
            eng.process(event, statusContainer);
            //eng.processDone(statusContainer, model.getBPELProcess());
            e.printStackTrace();
            Assert.fail(e.getMessage());
            return messageExchangeId;
        }
    }

    public void initiateBPInstanceForWait(final Properties props, 
                                          final Engine eng, 
                                          DeploymentBindings deplBindings) 
            throws Exception {
        initiateBPInstance(props, eng, deplBindings);
        waitAndContinue(props, eng);
    }
    
    public void initiateBPInstanceForWaitTest2(final Properties props, 
            final Engine eng, 
            DeploymentBindings deplBindings) 
    throws Exception {
        initiateBPInstance(props, eng, deplBindings);
        System.exit(TestDBConnImpl.SUCCESSFUL_EXIT_ID);
    }

    public void initiateBPInstanceForOnMsg(final Properties props,
			final Engine eng, DeploymentBindings deplBindings) throws Exception {
		initiateBPInstance(props, eng, deplBindings);
		sendMessage("OnMsg_", props, eng, deplBindings);
	}
    
    public void initiateBPInstanceForWhile(final Properties props, 
                                           final Engine eng, 
                                           DeploymentBindings deplBindings) 
            throws Exception {
        initiateBPInstance(props, eng, deplBindings);
        int count = Integer.parseInt(props.getProperty("ITER_COUNT"));
        for (int i = 0; i < count; i++) {
            waitAndContinue(props, eng);
        }
    }
    
    public void initiateBPInstanceForRepeatUntil(final Properties props,
                                                 final Engine eng,
                                                 DeploymentBindings deplBindings) 
            throws Exception {
        initiateBPInstance(props, eng, deplBindings);
        waitAndContinue(props, eng);
        int count = Integer.parseInt(props.getProperty("ITER_COUNT"));
        for (int i = 0; i < count; i++) {
            waitAndContinue(props, eng);
        }
    }
    
    public void initiateBPInstanceForPickIfWhileRepeatUntil(final Properties props,
                                                 final Engine eng,
                                                 DeploymentBindings deplBindings) 
            throws Exception {
        initiateBPInstance(props, eng, deplBindings);
        waitAndContinue(props, eng);
        int count = Integer.parseInt(props.getProperty("ITER_COUNT"));
        for (int i = 0; i < count; i++) {
            waitAndContinue(props, eng);
        }
    }
    
    public void recoverRepeatUntilWait(final Properties props,
                                       final Engine eng,
                                       DeploymentBindings deplBindings) throws Exception {
        recover(props, eng, deplBindings);
        waitAndContinue(props, eng);
        int count = Integer.parseInt(props.getProperty("ITER_COUNT"));
        for (int i = 0; i < count; i++) {
            waitAndContinue(props, eng);
        }
    }
    
    public void recoverPickIfWhileRepeatUntil(final Properties props,
                                 final Engine eng,
                                 final DeploymentBindings deplBindings) throws Exception {
        int count = Integer.parseInt(props.getProperty("ITER_COUNT"));
        recover(props, eng, deplBindings);
        waitAndContinue(props, eng);
        for (int i = 0; i < count; i++) {
            waitAndContinue(props, eng);
        }
    }

    public void recoverWhileWait(final Properties props,
                                 final Engine eng,
                                 final DeploymentBindings deplBindings) throws Exception {
        int count = Integer.parseInt(props.getProperty("ITER_COUNT"));
        recover(props, eng, deplBindings);
        for (int i = 0; i < count; i++) {
            waitAndContinue(props, eng);
        }
    }
    
    protected void doWait(Properties props) throws Exception {
        long timeOut = 0;
        try {
            timeOut = Long.parseLong(props.getProperty("WAIT_TIME_IN_BPEL")) * 1000;
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("UtilityClass.doWait(): "+ timeOut);
        Object lock = new Object();
        synchronized (lock) {
            lock.wait(timeOut);
        }
    }
    
    public void recover(Properties props, Engine eng, DeploymentBindings depBindings) throws Exception {
        Thread.sleep(eng.getEngineExpiryInterval() + 1000); // Engine Expiration value for the persistence tests is 0, 
        // but to updatedangling instances needs to wait a little time and then it will be able to 
        // update the instances.
        eng.process();
    }
    
    public void simpleWaitRecovery(Properties props, Engine eng, DeploymentBindings depBindings) throws Exception {
        recover(props, eng, depBindings);
        waitAndContinue(props, eng);
    }
    
    public void simpleWaitRecoveryTest2(Properties props, Engine eng, DeploymentBindings depBindings) throws Exception {
        recover(props, eng, depBindings);
        eng.process();
        waitAndContinue(props, eng);
    }
    
    public void recoveryOnOnMsg(Properties props, Engine eng, DeploymentBindings depBindings) throws Exception {
        recover(props, eng, depBindings);
        sendMessage("OnMsg_", props, eng, depBindings);
    }


    private void waitAndContinue(Properties props, Engine eng) throws Exception {
        doWait(props);
        eng.process();
    }
    
    protected InComingEventModel associateChannel(Properties props, 
                                                  DeploymentBindings deplBindings) 
            throws Exception {
        return associateChannel("", props, deplBindings);
    }
    
    protected InComingEventModel associateChannel(String prefix,
                                                  Properties props,
                                                  DeploymentBindings deplBindings) 
            throws Exception {
//        String ipFileLoc = props.getProperty(prefix + "INPUT");
//        EngineDriver.getDocument(ipFileLoc);
        String service = props.getProperty(prefix + "SERVICE");
        QName serviceQName = QName.valueOf(service);
        String endPoint = props.getProperty(prefix + "ENDPOINT");
        String oper = props.getProperty(prefix + "OPERATION");
        
        InComingKey key = 
                deplBindings.createInComingBindingsKey(serviceQName, endPoint, oper);
        InComingEventModel model = deplBindings.getInComingEventModel(key);
        return model;
    }
    
    protected void sendMessage(String prefix, final Properties props, final Engine engine, 
            DeploymentBindings deplBindings) throws Exception {
        
        String fileLoc = props.getProperty(prefix + "INPUT");
        String inputXML = EngineDriver.getXMLString(fileLoc);
        System.out.println("sending message:\n"+ inputXML);
        Document doc = EngineDriver.getDocument(fileLoc);
        
        String service = props.getProperty(prefix + "SERVICE");
        QName serviceQName = QName.valueOf(service);
        String endpoint = props.getProperty(prefix + "ENDPOINT");
        String oper = props.getProperty(prefix + "OPERATION");
        String msgType = props.getProperty(prefix + "IP_MESG_TYPE");
        QName msgTypeQName = QName.valueOf(msgType);
        
        InComingKey key = 
                deplBindings.createInComingBindingsKey(serviceQName, endpoint, oper);
        final InComingEventModel model = deplBindings.getInComingEventModel(key);
        Message wsdlMessage = model.getBPELProcess().getWSDLMessage(msgTypeQName);
        
        JBIMessageImpl jbiMsg = new JBIMessageImpl(doc, wsdlMessage);
        String messageExchangeId = new UID().toString();
        MessageContainer container =
                MessageContainerFactory.createMessage(messageExchangeId, jbiMsg, null, null);
        initializeAndSetTransaction(container);
        InComingEventKeyImpl event = new InComingEventKeyImpl(model, Event.REQUEST);
        engine.process(event, container);
    }
}
