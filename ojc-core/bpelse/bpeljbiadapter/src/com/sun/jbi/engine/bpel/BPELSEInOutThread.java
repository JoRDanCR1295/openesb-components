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
 * @(#)BPELSEInOutThread.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.transaction.Transaction;
import javax.xml.namespace.QName;

import net.sf.hulp.measure.Measurement;

import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryStatus;
import com.sun.jbi.common.util.NDC;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.core.bpel.engine.Event;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainerFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.InComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.ResponseInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.util.PropagationContext;
import com.sun.jbi.engine.bpel.util.I18n;
import com.sun.jbi.nms.exchange.ExchangePattern;


/**
 * A worker thread for the BPEL-SE. It listens on the JBI Delivery Channel. 
 * Once a message is received, it accepts it and processes the message.
 *
 * @author Sun Microsystems
 */
public class BPELSEInOutThread extends Thread {
    private static final Logger LOGGER = Logger.getLogger(BPELSEInOutThread.class.getName());
    private static final String CLASSIDENTIFIER_FORMEASUREMENT = "BPEL-SE.BPELSEInOutThread";

    private static final QName svcKPIAct = new QName("MonitorActivities_iep", "InputService");
    private static final QName svcKPIProcessInst = new QName("MonitorInstances_iep", "InputService");

    private boolean mRunFlag;
    private boolean blockingOnAccept;

    BPELSEHelper mBPELSEHelper;

    /**
     * Creates a new instance of BPELSEInOutThread
     *
     * @param bpelsehelper  Helper class holding incoming BPEL thread context state
     * *@param seqNumber  Sequence number for thread helps to identify thread
     */
    public BPELSEInOutThread(BPELSEHelper bpelsehelper, int seqNumber) {
        super("sun-bpel-engine-thread-" + seqNumber);
        mRunFlag = true;
        mBPELSEHelper = bpelsehelper;
    }

    /**
     * State setter 1. CEASED: stop the running thread 2. RUNNING: running thread is to process
     * messages 3. HALTED: running thread doesn't process incoming messages
     *
     * @param state Thread state
     */
    public synchronized void stopMe() {
        mRunFlag = false;
        releaseFromDeliveryChannel();
    }
    
    /**
     * Checks whether this thread is waiting on the delivery channel. If yes,
     * interrupts it so that it is released from the delivery channel.
     */
    public synchronized void releaseFromDeliveryChannel() {
        if(blockingOnAccept){
            interrupt();
        }
    }
    
    /**
     * thread
     */
    public void run() {
        LOGGER.log(Level.FINE, "BPELSEInOutThread.Started_BPEL_service_engine_in-out_thread");

        while (mRunFlag) {
            if (mBPELSEHelper.getDeliveryChannel() == null) {
                break;
            }

            try {
                MessageExchange msgEx = null;
                long deadline;
                try {
                    deadline = mBPELSEHelper.mEngine.getNextScheduledTime();
                } catch (RuntimeException ex) {
                    // Because the getNextScheduledTime() threw a null pointer, we were
                    // in an infinite loop here. The null pointer happened because of the
                    // timing issues of starting / stopping the engine and the deployment
                    // actions by User. This exception is being treated differently compared
                    // to the runtime exception that might be thrown during the call of 
                    // mEngine.process() API.
                    LOGGER.log(Level.WARNING, I18n.loc("BPJBI-6004: caught exception while processing message"), ex);
                    break;
                }
                long sleepFor = deadline - System.currentTimeMillis();
                synchronized(this){
                    if(!mRunFlag){
                        break;
                    }
                    blockingOnAccept = true;
                }
                try {
                    if (deadline <= 0) {
                        msgEx = mBPELSEHelper.getDeliveryChannel().accept();
                        if (msgEx == null) {
                            LOGGER.log(Level.SEVERE, I18n.loc("BPJBI-7132: Thread released from DeliveryChannel.accept, no Message Exchange and no time-out"));
                        }
                    } else if (sleepFor > 0) {
                        msgEx = mBPELSEHelper.getDeliveryChannel().accept(sleepFor);
                    }
                }catch (MessagingException ex) {
                    if (!(ex.getCause() instanceof InterruptedException)) { //do not need to log if it is InterruptedException
                        LOGGER.log(Level.WARNING, 
                                I18n.loc("BPJBI-6005: caught exception accepting message on channel"), ex);
                    }else {
                        if (!mRunFlag) {
                            break;
                        }
                    }
                }
                synchronized(this){
                    blockingOnAccept = false;
                }
                if (interrupted()) {
                    if (LOGGER.isLoggable(Level.FINE)) {
                        LOGGER.log(Level.FINE, "Thread was interrupted while executing DeliveryChannel.accept()");
                    }
                }

                if (msgEx == null) {
                    if (LOGGER.isLoggable(Level.FINEST)) {
                        LOGGER.log(Level.FINEST, "Thread released from DeliveryChannel.accept perhaps due to accept call with time-out");
                    }
                    // Got triggered by the wait/OnAlarm/failover activities
                    mBPELSEHelper.mEngine.process();
                } else {
                	NDC ndc = null;
                	try {
                		ndc = NDC.enter("Message Exchange Id", msgEx.getExchangeId(), 
                				"Service Name", msgEx.getEndpoint().getServiceName(),
                				"Operation Name", msgEx.getOperation());
                		processMsgEx(msgEx);
                	} finally {
                		// exit NDC
                		if (ndc != null) {
                			ndc.exit();
                		}
                	}
                }
            } catch (MessagingException ex) {
                if (!(ex.getCause() instanceof InterruptedException)) { //do not need to log if it is InterruptedException
                    LOGGER.log(Level.WARNING, 
                    		I18n.loc("BPJBI-6005: caught exception accepting message on channel"), ex);
                }
            } catch (Throwable ex) { // Catch the throwable to handle the thread crashing for any exception or error.
                LOGGER.log(Level.WARNING, I18n.loc("BPJBI-6004: caught exception while processing message"), ex);
            }
        }
        LOGGER.log(Level.FINE, I18n.loc("BPJBI-4017: BPELSE Thread ceases"));
    }

    private void processMsgEx(MessageExchange msgEx) throws MessagingException {

        try {
            URI pattern = msgEx.getPattern();
            if (LOGGER.isLoggable(Level.FINE)) {
                LOGGER.log(Level.FINE, I18n.loc("BPJBI-3002: Pattern for exchange Id {0} is {1}", 
                		msgEx.getExchangeId(), pattern));
            }
            
            ExchangeStatus status;
            switch (ExchangePattern.valueOf(msgEx)) {
            case IN_OUT:
                
                InOut inOut = (InOut) msgEx;
                status = inOut.getStatus();
                
                if (!status.equals(ExchangeStatus.ACTIVE)) {

                    boolean error = status.equals(ExchangeStatus.DONE) ? false : true;
                    String msg = error ? inOut.getError().getMessage()
                            : ExchangeStatus.DONE.toString();

                    if (LOGGER.isLoggable(Level.FINE)) {
                        LOGGER.log(Level.FINE, I18n.loc("BPJBI-3003: Received in-out message {0} for M Ex {1} " + 
                        		"content is {2}", "status", inOut.getExchangeId(), msg));
                    }

                    try {
                        processStatus(inOut, error);
                    } catch (RuntimeException exp) {
                        LOGGER.log(Level.WARNING, 
                        		I18n.loc("BPJBI-6006: Failed to process status of InOut - M Ex {0}", 
                        				inOut.getExchangeId()), exp);
                    }
                } else {
                    boolean in = false;
                    NormalizedMessage nmsg = inOut.getFault();
                    String exchangeId = inOut.getExchangeId();
                    if (nmsg == null) {
                        nmsg = inOut.getOutMessage();
                        if (nmsg == null) {
                            nmsg = inOut.getInMessage();
                            if (nmsg != null) {
                                in = true;
                            }
                        }
                    }
                    if (in) {
                        try{
                            processRequest(inOut);
                        } catch(RuntimeException exp) {
                            //The ME may not have been removed yet
                            mBPELSEHelper.removefromRequestMap(exchangeId);                        
                            LOGGER.log(Level.WARNING, I18n.loc("BPJBI-6007: Failed to process In Out Message M Ex {0}", 
                            		exchangeId), exp);
                            mBPELSEHelper.sendError(inOut, exp);
                        }

                        
                    } else {
                        try{
                            processResponse(inOut);
                        } catch (Exception exp) {
                            mBPELSEHelper.removefromResponseMap(exchangeId);
                            LOGGER.log(Level.WARNING, 
                            		I18n.loc("BPJBI-6008: Failed to process response for InOut Message M Ex {0}", 
                            				exchangeId), exp);
                            mBPELSEHelper.sendError(inOut, exp);
                        }
                    }
                }
                break;
            case IN_ONLY:
                InOnly inOnly = (InOnly) msgEx;
                status = inOnly.getStatus();
                String exchangeId = inOnly.getExchangeId();
                
                if (!status.equals(ExchangeStatus.ACTIVE)) {
                    try{
                        processStatus(inOnly);
                    }catch(RuntimeException exp){
                        LOGGER.log( Level.WARNING, 
                        		I18n.loc("BPJBI-6009: Failed to process status of InOnly Message M Ex {0}", 
                        				inOnly.getExchangeId()), exp);
                    }
                } else {
                    try{
                        processRequest(inOnly);
                    } catch (RuntimeException exp) {
                        //The ME may not have been removed yet
                        mBPELSEHelper.removefromRequestMap(exchangeId);
                        LOGGER.log(Level.WARNING, 
                        		I18n.loc("BPJBI-6010: Failed to process In Only Message M Ex {0}", exchangeId), exp); 
                        mBPELSEHelper.sendError(inOnly, exp);
                    }
                }
                break;
            case IN_OPTIONAL_OUT:
                processUnsupportedME(msgEx);
                break;
                
            case ROBUST_IN_ONLY:
            case CUSTOM:
            default:                    
                processUnsupportedME(msgEx);
                break;
            } 
            
        } catch (Exception exp) {
            LOGGER.log(Level.WARNING, I18n.loc("BPJBI-6004: caught exception while processing message"), exp); 
            mBPELSEHelper.sendError(msgEx, exp);
        }
        
    }
    
    private void processUnsupportedME(MessageExchange msgEx) throws MessagingException {
        
    	String message = null;
        if (ExchangePattern.isRobustInOnly(msgEx)) { // NO I18N
        	message = I18n.loc("BPJBI-7015: Robust in-only is not supported, M Ex {0} received", msgEx.getExchangeId());
        	LOGGER.log(Level.SEVERE, message);
        } else {
        	message = I18n.loc("BPJBI-7016: Received invalid pattern info, M Ex {0} received", msgEx.getExchangeId());
        	LOGGER.log(Level.SEVERE, message);
        } 
        Exception error = new RuntimeException(message);
        mBPELSEHelper.sendError(msgEx, error);
    }
    
    private void processStatus(InOut inOut, boolean error) {
        MessageContainer msgContainer = null;
        String msgExchangeId = inOut.getExchangeId();
        QName serviceName = inOut.getEndpoint().getServiceName();
        String endpointName = inOut.getEndpoint().getEndpointName();
        Object crmpInvokeId = getReliabilityId(inOut);
        
        // retrieve the transaction from the message exchange if present
        Transaction transaction = getTxFromMessageExchange(inOut);
        
        if(!error){ //Status=Done after processing response 
            InComingEventModel model = getInComingEventModel(serviceName, endpointName, 
                    inOut.getOperation().getLocalPart());
            InComingEventKeyImpl event = new ResponseInComingEventKeyImpl(
                    model.getBPELProcess(), Event.DONE, inOut.getExchangeId());

            msgContainer = MessageContainerFactory.createDoneStatus(inOut
                    .getExchangeId(), crmpInvokeId, transaction);
            mBPELSEHelper.mEngine.process(event, msgContainer);
            return;
        }
        int eventType = Event.RESPONSE_ERROR; //Status=Error MessageExchange abruptly terminated when processing request
                                              //It is consuming service
        RBPELProcess process = (RBPELProcess) mBPELSEHelper.removefromResponseMap(msgExchangeId);
        if(process == null){
            eventType = Event.ERROR;//Status=Error MessageExchange abruptly terminated when processing response
                                    //It is provisioning service
            InComingEventModel model = getInComingEventModel(serviceName, endpointName, 
                    inOut.getOperation().getLocalPart());
            process = model.getBPELProcess();
        }
        InComingEventKeyImpl event = new ResponseInComingEventKeyImpl(process, eventType,
                inOut.getExchangeId());
        
        msgContainer = MessageContainerFactory.createErrorStatus(inOut
                .getExchangeId(), inOut.getError(), crmpInvokeId, transaction);

        // obtain the QOS Redelivery status, since there is an error, we have to 
        // determine redelivery based on the status.
        RedeliveryStatus redeliveryStatus = Redelivery.getRedeliveryStatus(inOut);
        msgContainer.setRedeliveryStatus(redeliveryStatus);
        
        mBPELSEHelper.mEngine.process(event, msgContainer);
    }

    private void processRequest(InOut inOut) {
        QName serviceName = inOut.getEndpoint().getServiceName();
        String endpointName = inOut.getEndpoint().getEndpointName();
        String exchangeId = inOut.getExchangeId();
        String operationName = inOut.getOperation().getLocalPart();
        
        // retrieve the transaction from the message exchange if present.
        Transaction transaction = getTxFromMessageExchange(inOut);

        InComingEventModel incomingEventModel = getInComingEventModel(serviceName, endpointName, operationName);

        Measurement m1 = null;
        if (Measurement.isInstalled()) {
            String measure = "Processing InOut request : reading input for [partner-> "
                    + serviceName + ", opeation-> " + operationName + "]";
            m1 = Measurement.begin(CLASSIDENTIFIER_FORMEASUREMENT, measure);
        }

        WSMessage wsMessage = EngineHelper.createWSMessage(inOut.getInMessage(), 
                incomingEventModel.getBPELProcess(), false);

        if (LOGGER.isLoggable(Level.FINEST)) {
            LOGGER.log(Level.FINEST, 
            		I18n.loc("BPJBI-3003: Received in-out message {0} for M Ex {1} content is {2}", 
            				"input", exchangeId, wsMessage));
        }
        
        if (Measurement.isInstalled()) {
            m1.end();
        }

        Object crmpInvokeId = getReliabilityId(inOut);
        
        MessageContainer msgContainer = MessageContainerFactory.createMessage(
                inOut.getExchangeId(), wsMessage, crmpInvokeId, transaction);
        PropagationContext propContext = new PropagationContext(inOut);
        msgContainer.setPropagationContext(propContext);
        //starts measurement counter for InOut request processing
        msgContainer.startMeasurement(serviceName, operationName);

        mBPELSEHelper.putinRequestMap(exchangeId, inOut);
        InComingEventKeyImpl event = new InComingEventKeyImpl(incomingEventModel, Event.REQUEST);

        mBPELSEHelper.mEngine.process(event, msgContainer);
    }    

    private void processResponse(InOut inOut) {
        String msgExchangeId = inOut.getExchangeId();
        RBPELProcess process = (RBPELProcess) mBPELSEHelper.removefromResponseMap(msgExchangeId);
        MessageContainer msgContainer = null;
        
        // retrieve the transaction from the message exchange if present
        Transaction transaction = getTxFromMessageExchange(inOut);
        

        WSMessage wsMessage = null;
        
        if (inOut.getFault() != null) {
            try {
                wsMessage = EngineHelper.createWSMessage(inOut.getFault(), process, true);
                if (LOGGER.isLoggable(Level.FINEST)) {
                	String content = "Unavailable";
                	// For a fault it is possible to get a null wsMessage.
                	if (wsMessage != null) {
                		content = wsMessage.toString();
                	}
                	LOGGER.log(Level.FINEST, 
            				I18n.loc("BPJBI-3003: Received in-out message {0} for M Ex {1} content is {2}", 
            						"fault", inOut.getExchangeId(), content));
                }
                msgContainer = MessageContainerFactory.createFault(msgExchangeId, wsMessage, null, transaction);                
            } catch (RuntimeException e) {
                /*
                 *We were not able to process the response that we got. This is treated as a system fault.
                 *Like any other system fault, this can be caught and handled in a catchAll. Note that
                 *at this point fault name or fault data is not supported for system faults so a null
                 *is passed as the value of wsMessage
                 */
                LOGGER.log(Level.WARNING, I18n.loc("BPJBI-6011: Exception occurred while processing the fault " + 
                		"response for message exchange with id {0} error is {1}", msgExchangeId, extractErrorMsgs(e)));
                msgContainer = MessageContainerFactory.createFault(msgExchangeId, null, null, transaction);                
            }
        } else {
            try {
                wsMessage = EngineHelper.createWSMessage(inOut.getOutMessage(),
                        process, false);
                if (LOGGER.isLoggable(Level.FINEST)) {
                	LOGGER.log(Level.FINEST, 
            				I18n.loc("BPJBI-3003: Received in-out message {0} for M Ex {1} content is {2}", 
            						"reply", inOut.getExchangeId(), wsMessage));
                }
                Object crmpInvokeId = getReliabilityId(inOut);
                msgContainer = MessageContainerFactory.createMessage(
                        msgExchangeId, wsMessage, crmpInvokeId, transaction);
            } catch (RuntimeException e) {
                /*
                 * We were not able to process the response that we got. This is
                 * treated as a system fault. Like any other system fault, this
                 * can be caught and handled in a catchAll. Note that at this
                 * point fault name or fault data is not supported for system
                 * faults so a null is passed as the value of wsMessage
                 */
                LOGGER.log(Level.WARNING, I18n.loc("BPJBI-6012: Exception occurred while processing the response " + 
                		"for message exchange with id {0} error is {1}", msgExchangeId, extractErrorMsgs(e)));
                msgContainer = MessageContainerFactory.createFault(
                        msgExchangeId, null, null, transaction);
            }
        }
        /**
         * Here NMProperties are set on MessageContainer. In the Invoke
         * implementation it is set on the input variable
         */
        msgContainer.setNMProperties(readNMProperties(inOut.getInMessage()));

        mBPELSEHelper.putinResponseMap(msgExchangeId, inOut);
        InComingEventKeyImpl event = new ResponseInComingEventKeyImpl(process,
                Event.REPLY_FAULT, msgExchangeId);
        mBPELSEHelper.mEngine.process(event, msgContainer);
    }

    private void processStatus(InOnly inOnly) {

		String msgExchangeId = inOnly.getExchangeId();
		ExchangeStatus status = inOnly.getStatus();

		QName serviceName = inOnly.getEndpoint().getServiceName();
		if(isKPIService(serviceName)){
		    //BP instance is not waiting status from KPI EP, so no processing required 
		    return;
		}
		Object crmpInvokeId = getReliabilityId(inOnly);

		RBPELProcess process = (RBPELProcess) mBPELSEHelper
				.removefromResponseMap(msgExchangeId);

		MessageContainer msgContainer = null;
		InComingEventKeyImpl event = null;

		// retrieve the transaction from the message exchange if present
		Transaction transaction = getTxFromMessageExchange(inOnly);

		if (status.equals(ExchangeStatus.DONE)) {
			msgContainer = MessageContainerFactory.createDoneStatus(
					msgExchangeId, crmpInvokeId, transaction);
			event = new ResponseInComingEventKeyImpl(process, Event.DONE,
					msgExchangeId);
			/**
			 * Here NMProperties are set on MessageContainer. In the Invoke
			 * implementation it is set on the input variable
			 */
			msgContainer.setNMProperties(readNMProperties(inOnly.getInMessage()));
		} else {
			// The status is ERROR
			msgContainer = MessageContainerFactory
					.createErrorStatus(msgExchangeId, inOnly.getError(),
							crmpInvokeId, transaction);

			// obtain the QOS Redelivery status, since there is an error, we
			// have to
			// determine redelivery based on the status.
			RedeliveryStatus redeliveryStatus = Redelivery
					.getRedeliveryStatus(inOnly);
			msgContainer.setRedeliveryStatus(redeliveryStatus);

			event = new ResponseInComingEventKeyImpl(process, Event.ERROR,
					inOnly.getExchangeId());
		}
		mBPELSEHelper.mEngine.process(event, msgContainer);
	}

    /**
     * It creates a Map, populates it with the NMProperties from the
     * NormalizedMessage object passed in as parameter.
     * 
     * @param normalizedMsg
     * @return readNMProperties as Map
     */
    private Map readNMProperties(NormalizedMessage normalizedMsg) {
        Map<String, Object> properties = new HashMap<String, Object>();
        Set<String> propNames = normalizedMsg.getPropertyNames();
        for (String propName : propNames) {
            Object val = normalizedMsg.getProperty(propName);
            properties.put(propName, val);
        }
        return properties;
    }

    private void processRequest(InOnly inOnly) {
        QName serviceName = inOnly.getEndpoint().getServiceName();
        String endpointName = inOnly.getEndpoint().getEndpointName();
        String exchangeId = inOnly.getExchangeId();

        InComingEventModel incomingEventModel = getInComingEventModel(serviceName, endpointName, 
                inOnly.getOperation().getLocalPart());
        WSMessage wsMessage = EngineHelper.createWSMessage(inOnly.getInMessage(), 
                incomingEventModel.getBPELProcess(), false);
        
        if (LOGGER.isLoggable(Level.FINEST)) {
            LOGGER.log( Level.FINEST, 
            		I18n.loc("BPJBI-3004: Received in-only message for M Ex {0} content is {1}", 
            				exchangeId, wsMessage));
        }

        // retrieve the transaction from the message exchange if present
        Transaction transaction = getTxFromMessageExchange(inOnly);
        
        Object crmpInvokeId = getReliabilityId(inOnly);
        
        MessageContainer msgContainer = MessageContainerFactory.createMessage(exchangeId,
                wsMessage, crmpInvokeId, transaction);
        
        PropagationContext propContext = new PropagationContext(inOnly);
        msgContainer.setPropagationContext(propContext);

        mBPELSEHelper.putinRequestMap(exchangeId, inOnly);

        InComingEventKeyImpl event = new InComingEventKeyImpl(incomingEventModel, Event.REQUEST);
        mBPELSEHelper.mEngine.process(event, msgContainer);
    }

    private InComingEventModel getInComingEventModel(QName serviceName,
            String endpointName, String operationName) {
        InComingKey key = (mBPELSEHelper.getDeploymentBindings()).createInComingBindingsKey(
                serviceName, endpointName, operationName);
        InComingEventModel incomingEventModel = (mBPELSEHelper.getDeploymentBindings()).getInComingEventModel(key);

        if (incomingEventModel == null) {
            String message = I18n.loc("BPJBI-7017: Could not find a business process providing service for the " + 
            		"endpoint specified in the message exchange. Service name: {0}, " + 
            		"endpoint name: {1}, operation name: {2}", serviceName, endpointName, operationName);
            throw new RuntimeException(message);
        }
        return incomingEventModel;
    }

    private String extractErrorMsgs(Throwable ex) {
        String errorMsg = ex.getMessage();
        while (ex.getCause() != null) {
            ex = ex.getCause();
            errorMsg = errorMsg + " is Caused by: " + ex.getMessage();
        }
        return errorMsg;
    }
    
    private Transaction getTxFromMessageExchange(MessageExchange msgExch) {
    	Transaction transaction = null;
    	if (msgExch.isTransacted()) {
            transaction = (Transaction) msgExch.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
    	}
    	return transaction;	
    }

    private boolean isKPIService(QName serviceName){
        if(svcKPIAct.getNamespaceURI().equals(serviceName.getNamespaceURI()) 
                || svcKPIProcessInst.getNamespaceURI().equals(serviceName.getNamespaceURI())){
            return true;
        }
        return false;
    }
    
    private String getReliabilityId(MessageExchange msgEx) {
        String retId = null;
        String tempId = null;

        // the CRMP id that is generated for the bpel-se CRMP processing
        // is the combination of the two id's. 
        // ie: crmpId = mesgId + groupId;

        Object mesgId = msgEx.getProperty(ServiceQuality.MESSAGE_ID);

        if(mesgId != null && mesgId instanceof String) {
            tempId = (String) mesgId;
            if (tempId.trim().length() > 0) {
                retId = tempId;
            } 
        }

        Object groupId = msgEx.getProperty(ServiceQuality.GROUP_ID);

        if (groupId != null && groupId instanceof String) {
            tempId = (String) groupId;
            if (tempId.trim().length() > 0) {
                if (retId != null) {
                    retId = retId.concat(tempId);
                } else {
                    retId = tempId;
                }
            }
        }

        return retId;
    }
    
}
