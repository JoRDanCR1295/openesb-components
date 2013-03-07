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
 * @(#)InboundMessageProcessorListenerEndpoint.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.jms.BytesMessage;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.Queue;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.resource.ResourceException;
import javax.transaction.Status;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.transaction.xa.XAResource;
import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import net.java.hulp.measure.Probe;

import com.sun.encoder.EncoderException;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.BaseExchangeTemplates;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jmsbc.Endpoint.EndpointState;
import com.sun.jbi.jmsbc.NMPropertiesUtil.InboundInOutNMProperties;
import com.sun.jbi.jmsbc.NMPropertiesUtil.NMProperties;
import com.sun.jbi.jmsbc.NMPropertiesUtil.OutBoundInOnlyNMProperties;
import com.sun.jbi.jmsbc.extensions.JMSAddress;
import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.jmsbc.extensions.JMSInput;
import com.sun.jbi.jmsbc.extensions.JMSJCAOptions;
import com.sun.jbi.jmsbc.extensions.JMSMessage;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.extensions.JMSOutput;
import com.sun.jbi.jmsbc.jca.MessageListenerEndpoint;
import com.sun.jbi.jmsbc.jms.Channel;
import com.sun.jbi.jmsbc.jms.ChannelException;
import com.sun.jbi.jmsbc.jms.ReceiveChannelJCA;
import com.sun.jbi.jmsbc.jms.SendChannel;
import com.sun.jbi.jmsbc.jms.SendChannelJCAImpl;
import com.sun.jbi.jmsbc.jms.Util;
import com.sun.jbi.jmsbc.recovery.LoggableXAResource;
import com.sun.jbi.jmsbc.util.GUIDUtil;
import com.sun.jbi.jmsbc.util.JMSBCContext;
import com.sun.jbi.jmsbc.util.NamesUtil;
import com.sun.jbi.jmsbc.util.ShutdownSynchronization;
import com.sun.jbi.jmsbc.util.SynchronizedStateChange;
import com.sun.jbi.jmsbc.util.AlertsUtil;
import com.sun.jbi.jmsbc.JMSBindingComponent;
import com.sun.jbi.alerter.NotificationEvent;
import java.util.Properties;


/**
 *
 * JMS to NMR Inbound message processor
 */
public class InboundMessageProcessorListenerEndpoint 
        implements MessageListenerEndpoint, InboundReplyListener {

    public static final String MESSAGEID = "com.sun.jbi.messaging.messageid";
	public static final String GROUPID = "com.sun.jbi.messaging.groupid";
	public static final String MEXID = "com.sun.jbi.messaging.mexid";
	private static final Messages mMessages =
        Messages.getMessages(InboundMessageProcessorListenerEndpoint.class);
    private static final Logger mLog =
        Messages.getLogger(InboundMessageProcessorListenerEndpoint.class);
    private static final Object SHUTDOWN_PROCESSED = new Object();
    
    private static final String JMSJCA_MSG_PROP_END_OF_BATCH   = "JMSJCA.EndOfBatch";
    private static final String JMSJCA_MSG_PROP_ROLL_BACK_ONLY = "JMSJCA.isRollbackOnly";
    
    private ComponentContext context = null;
    private JMSOperation jmsOp = null;
    private JMSInput jmsInput = null;
    private JMSOutput jmsOutput = null;
    private Endpoint endpoint = null;
    private Map inboundMessageExchanges = null;  // used by receive channels for saving message exchanges for inbound request/reply exchanges    
    
    private MessageExchangeFactory messageXchangeFactory = null;
    private XAResource xar = null;
    
    // Some state objects which are retained from beforeDelivery to afterDelivery
    private boolean txSuspended = false;
    private Transaction currentTx = null;
    private boolean batch = false;
    private Map jmsMessageBatch = null;
    private boolean logTxCommit = false;
        
    // Reply sender
    private SendChannelJCAImpl replySender = null;
    
    private JMSDenormalizer mJMSDenormalizer;
    private JMSNormalizer mJMSNormalizer;
    private String mGUID;
    private Hashtable<String, String> mMessagesMarkedForDeletion = new Hashtable<String, String>();
    private boolean redeliveryApplied;
    
    /** Creates a new instance of InboundMessageProcessorEndpoint */
    public InboundMessageProcessorListenerEndpoint(ComponentContext context,
                                                   XAResource xar,
                                                   Endpoint endpoint,
                                                   JMSOperation jmsOp,
                                                   Map inboundMessageExchanges,
                                                   boolean redeliveryApplied) throws Exception{
        this.context = context;
        this.xar = xar;
        this.endpoint = endpoint;
        this.jmsOp = jmsOp;
        this.inboundMessageExchanges = inboundMessageExchanges;
                
        int batchSize = (this.jmsOp.getBatchSize()==null ? 
                          1 : this.jmsOp.getBatchSize().intValue());        
        jmsMessageBatch = Collections.synchronizedMap(new HashMap(batchSize));
        batch = (this.jmsOp.getBatchSize() != null) && 
                (this.jmsOp.getBatchSize().intValue() > 0);

        jmsInput = this.endpoint.getJMSOperationInput(jmsOp);
        jmsOutput = this.endpoint.getJMSOperationOutput(jmsOp);
        
        mJMSDenormalizer = new JMSDenormalizer();
        mJMSNormalizer = new JMSNormalizer();
        mGUID = GUIDUtil.generateGUID();
        this.redeliveryApplied = redeliveryApplied;
    }

    public void beforeDelivery(java.lang.reflect.Method method) 
    throws NoSuchMethodException, ResourceException {
        if (method.getName().indexOf("onMessage") < 0) {
            throw new NoSuchMethodException (method.getName());
        }

        try {
            // Start XA transaction if in XA mode
            currentTx = startTransaction();
        } catch (Throwable t) {
            String errMsg = mMessages.getString(
                    "JMSBC-E0711.MessageEndpointBeforeDeliveryFailed");
            throw new ResourceException (errMsg, t);
        }
    }

    public void afterDelivery() throws ResourceException {
        try {
            if (currentTx != null) {  // complete xa transaction
            	try {
            		resumeCurrentTransaction(); 
                	delistResource(currentTx);
                } catch (Exception ex) {
            		mLog.log(Level.SEVERE, 
                            mMessages.getString("JMSBC-E0777.DelistResourceFailed", 
                               new Object [] {ex.getLocalizedMessage()}),ex);
            		try {
            			rollbackCurrentTransaction();
            		} finally {
            			throw ex;   
            		}
                }
                if (currentTx.getStatus() == Status.STATUS_MARKED_ROLLBACK) {
                    // exchange failed
                    rollbackCurrentTransaction();                        
                } else {
                    commitCurrentTransaction();                        
                }
            }
        } catch (Throwable t) {
            String errMsg = mMessages.getString(
                    "JMSBC-E0712.MessageEndpointAfterDeliveryFailed");
            throw new ResourceException (errMsg, t);            
        } finally {
            currentTx = null;
        }
    }

    public void release() {
    	try{
        if (replySender != null) {
                closeSender(replySender);
            }
    	}finally{
    		replySender = null;
    	}
        mLog.log(Level.INFO, mMessages.getString("JMSBC-I0717.InboundEndpointStopped", new Object [] {mGUID}));
    }

	private void closeSender(SendChannel sender) {
            try {
			sender.stop();
			sender.close();
            } catch (Exception ex) {
                mLog.log(Level.WARNING, 
                         mMessages.getString("JMSBC-W0701.ReplySenderCloseFailed",
                             new Object [] {ex.getLocalizedMessage()}),
                         ex);
                AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0701.ReplySenderCloseFailed",
                             new Object [] {ex.getLocalizedMessage()}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    endpoint.getServiceUnitID(), 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-W0701");  
            }
        }
    
    public void onMessage (Message jmsMsg) {
        long jmsMsgReceiveTime = System.currentTimeMillis();
                                    
        try {
            MessageExchange msgXchange = null;      
            
            // end of message batch?
            if (batch && jmsMsg.getObjectProperty(JMSJCA_MSG_PROP_END_OF_BATCH) != null) {
                if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLog.log(LogSupport.LEVEL_DEBUG,
                            "InboundMessageProcessorListenerEndpoint_END_OF_BATCH_MSG_RECEIVED",
                            new Object [] {JMSJCA_MSG_PROP_END_OF_BATCH});
                }                    
                onEndOfBatch(jmsMsg);
                return;
            } else {
            	//Check if message is marked for deletion
            	if(mMessagesMarkedForDeletion.remove(jmsMsg.getJMSMessageID()) != null){
                    // ack JMS message delivery from ra
                    acknowledgeMessage(jmsMsg);
                    return;
            	}
                msgXchange = processInboundRequest (jmsMsgReceiveTime, 
                                                    jmsMsg);
            } 
            
            // If asynch message exchange reply is not expected, complete
            // the jms message delivery now
            if (!expectsAsynchOnReply() && jmsOp.getMEP().equals(Endpoint.EndpointMessageType.IN_OUT)) {
                onReply (msgXchange, true, false);
            }            
        } catch (Throwable ex) {
            mLog.log(Level.SEVERE, 
                     mMessages.getString("JMSBC-E0713.MessageEndpointOnMessageFailed", 
                         new Object [] {jmsMsg.toString(), ex.getLocalizedMessage()}),
                     ex);
            AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0713.MessageEndpointOnMessageFailed",
                             new Object [] {ex.getLocalizedMessage()}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    endpoint.getServiceUnitID(),
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0713");
           
            if (currentTx != null) {
                try {
                    setRollbackCurrentTransaction();
                    suspendThreadTx();
                } catch (Exception ex2) {
                    mLog.log(Level.SEVERE, 
                             mMessages.getString("JMSBC-E0707.SetRollbackOnlyFailed", 
                                new Object [] {currentTx.toString(), ex2.getLocalizedMessage()}),
                             ex2);
                   AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0707.SetRollbackOnlyFailed", 
                                new Object [] {currentTx.toString(), ex2.getLocalizedMessage()}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    endpoint.getServiceUnitID(), 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0707");
                }
            }
            
            // "nack" JMS message
            // if error occurs then throw runtime exception to jmsjca as last resort
            try {
                onUnexpectedError(jmsMsg);
            } catch (Exception ex2) {
                throw new RuntimeException (ex2);
            }
        }
    }

    public void onReply (MessageExchange msgEx,
                         boolean processReplySuccess, boolean markedForDeletion) throws Exception {
        
        // Get the JMS message instance 
        String messageExchangeId = getExchangeId(msgEx);
        
        Object obj = jmsMessageBatch.remove(messageExchangeId);
        if(obj == null){
			String errMsg = mMessages.getString(
					"JMSBC-E0773.NoJMSMessagePending", new Object[] {
							endpoint.toString(), msgEx.getExchangeId() });
			mLog.log(Level.SEVERE, errMsg);
        	throw new Exception(errMsg);
        }
        
        if(obj == SHUTDOWN_PROCESSED){
        	//shutdown has been triggered mean while
			String errMsg = mMessages.getString(
					"JMSBC-E0774.ShutdownInitiated", new Object[] {
							endpoint.toString(), msgEx.getExchangeId() });
			mLog.log(Level.SEVERE, errMsg);
        	throw new Exception(errMsg);
        }
        
        Message jmsMsg = (Message)obj;

        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)){
            mLog.log(LogSupport.LEVEL_DEBUG,
                    "InboundMessageProcessorListenerEndpoint_ON_REPLY_CALLED",
                    new Object[]{messageExchangeId, 
                                 new Boolean(processReplySuccess)});
        }                                    
        
        Exception theEx = null;
        
        if (!processReplySuccess) {
            //Check if message is marked for deletion
            if(markedForDeletion){
            	mMessagesMarkedForDeletion.put(jmsMsg.getJMSMessageID(), "");
            }
        		
            try {
                setRollBackPropertyOnMessage(jmsMsg);
            } catch (Exception ex) {
                theEx = ex;
            } 
        } else {  // flag transaction commit on "non-empty" message only - synch mode
            logTxCommit = true;
        }

        // ack JMS message delivery from ra
        acknowledgeMessage(jmsMsg);
        
        if (theEx != null) {
            throw theEx;
        }
    }

	private String getExchangeId(MessageExchange msgExchange) {
		String msgId = (String) msgExchange.getProperty(MEXID);
		/*
		 * if (msgId == null) { return msgExchange.getExchangeId(); } String
		 * exchangeId = (String) inboundMessageExchanges.get(msgId);
		 * 
		 * if (exchangeId == null) { return msgExchange.getExchangeId(); }
		 * return exchangeId;
		 */
		return msgId;
	}
	
    public void onOutput (InOut inout) throws Exception {
        boolean isXARollback = false;

        // For EJB based project, EJB may set rollback only on context
        // In such a case, don't send response on temporary destination
        if (currentTx != null) {  
            //resumeCurrentTransaction();
            isXARollback = currentTx.getStatus() == Status.STATUS_MARKED_ROLLBACK;
        }

        if (!isXARollback) {
            // Get the JMS message instance 
            String messageExchangeId = getExchangeId(inout);
            Message jmsMsg = (Message)jmsMessageBatch.get(messageExchangeId);
            Destination jmsReplyTo = jmsMsg.getJMSReplyTo();

            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)){
                mLog.log(LogSupport.LEVEL_DEBUG,
                        "InboundMessageProcessorListenerEndpoint_ON_OUTPUT_CALLED",
                        new Object[]{messageExchangeId});
            }                                    

            sendToJMSReplyTo (inout, jmsReplyTo, jmsMsg);
        } else {
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)){
                String messageExchangeId = getExchangeId(inout);
                mLog.log(LogSupport.LEVEL_DEBUG,
                        "InboundMessageProcessorListenerEndpoint_ON_OUTPUT_CALLED_TX_ROLLBACK",
                        new Object[]{messageExchangeId}); 
            }
        }
    }

    private void sendToJMSReplyTo (InOut inout,
                                   Destination jmsReplyTo,
                                   Message jmsMsg) throws Exception {
        if (replySender == null) {
            replySender = new SendChannelJCAImpl ();
            replySender.initialize(endpoint.getJMSAddress(),
                                   jmsOp);
            replySender.open();
            replySender.start();
        }
    	//Get NM Properties
    	InboundInOutNMProperties nmProperties = NMPropertiesUtil
				.getInboundInOutNMProperties(inout.getOutMessage());
    	setJMSCorrelationID(inout,nmProperties);
        String dest = null;
        boolean isTopic = false;
        boolean overrideReplyToDestination = false;
        if (jmsReplyTo == null &&
    			nmProperties.getReplytodestination() == null) {
    				String errMsg = mMessages.getString(
    						"JMSBC-E0715.ExpectedJMSReplyTo", new Object[] {
    								jmsOp.getBindingOperation().getName(),
    								jmsOp.getMEP(), jmsMsg.getJMSMessageID() });
    			throw new MessageExchangeProcessingException(errMsg);
   		}else if(nmProperties.getReplytodestination() != null){
   			overrideReplyToDestination = true;
        	dest = nmProperties.getReplytodestination();
        	isTopic = nmProperties.getReplytodestinationtype().equalsIgnoreCase(JMSConstants.TOPIC);
   		}

        //We should use the transaction context passed in the message exchange
	    Transaction tx = (Transaction)inout.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
	    SendChannel sender;
	    try{
		    if(tx!= null){
		        //Override transaction attribute from NMProperties
		        String transaction = jmsOp.getTransaction();
		        if(nmProperties.getXatransaction() != null){
		        	transaction = nmProperties.getXatransaction();
		        }
		        //----------------------------------
				if(transaction.equals(JMSConstants.TRANSACTION_XA)){
		            ((TransactionManager)context.getTransactionManager()).resume(tx);
				}
		    }
	        //------------------------------------------
	        sender = NMPropertiesUtil.getNewSendChannelIfRequired(replySender, nmProperties, endpoint);
	        Message jmsReplyMsg = denormalizeMessage(inout.getOutMessage(),
					jmsOutput.getJMSMessage(), createJMSMessage(jmsOutput
							.getJMSMessage(), sender), ((SendChannelJCAImpl)sender).getJMSOperation());
	        
	        //Now set user properties from NM properties
	        NMPropertiesUtil.fillJMSMessage(jmsReplyMsg, nmProperties);
	        //--------------------------------------------
	        
	       	if(overrideReplyToDestination)
	       		sender.send(jmsReplyMsg, dest, isTopic);
	       	else
	       		sender.send(jmsReplyMsg, jmsReplyTo);
	    }finally{
	    	if(tx != null){
                ((TransactionManager)context.getTransactionManager()).suspend();
	    	}
	    }
       	

        //Check if sender is new then close
        if(sender != replySender){
        	closeSender(sender);
        }
        
    }
        
    private void setJMSCorrelationID(InOut inout, InboundInOutNMProperties outNMProps) {
	
	if(outNMProps.getCorrelationid()==null){
	    NormalizedMessage in =   inout.getInMessage();
	    if(in != null){
		String corrid = (String) in.getProperty(NMPropertiesUtil.INOUT_CORRELATIONID);
		if(corrid!=null){
		    outNMProps.setCorrelationid(corrid);
		}
		
	    }
	}
    }

    private void onEndOfBatch (Message endOfBatch) throws Exception {
        // ack end-of-batch message
        acknowledgeMessage(endOfBatch);
    }
    
    private void onUnexpectedError(Message jmsMsg) throws Exception {
        try {
            setRollBackPropertyOnMessage (jmsMsg);
        } catch (Exception ex) {
            throw ex;
        } finally {
            // ack message
            acknowledgeMessage(jmsMsg);            
        }        
    }
    
    private void setRollBackPropertyOnMessage (Message jmsMsg) throws Exception {
        try {
            jmsMsg.setBooleanProperty(JMSJCA_MSG_PROP_ROLL_BACK_ONLY, true);
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLog.log (LogSupport.LEVEL_DEBUG, 
                          "InboundMessageProcessorListenerEndpoint_SET_JMS_MSG_ROLLBACKONLY",
                          new Object [] {JMSJCA_MSG_PROP_ROLL_BACK_ONLY, jmsMsg.toString()});
            }
        } catch (Exception ex) {
            mLog.log (Level.SEVERE,
                      mMessages.getString("JMSBC-E0714.SetRollbackOnlyPropFailed",
                          new Object[] {JMSJCA_MSG_PROP_ROLL_BACK_ONLY, 
                                        jmsMsg.toString(), 
                                        ex.getLocalizedMessage()}),
                      ex);
            AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0714.SetRollbackOnlyPropFailed",
                             new Object [] {ex.getLocalizedMessage()}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    endpoint.getServiceUnitID(), 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0714");
            throw ex;
        }        
    }
    
    private MessageExchange processInboundRequest (long requestInvocationTime,
                                                   Message jmsRequestMsg) 
        throws Throwable {
        
        String msgXchangeID = null;
        MessageExchange msgXchange = null;
        try {            
            // Create MessageExchange first base on MEP
            String mep = jmsOp.getMEP();
            
            // Check for JMSReplyTo if InOut exchange; required for inbound request/reply
            if (mep.equals(Endpoint.EndpointMessageType.IN_OUT)) {
                Destination jmsReplyTo = jmsRequestMsg.getJMSReplyTo();
                if (jmsReplyTo == null) {
//                    String errMsg = mMessages.getString(
//                            "JMSBC-E0715.ExpectedJMSReplyTo",
//                            new Object [] {jmsOp.getBindingOperation().getName(),
//                                           jmsOp.getMEP(),
//                                           jmsRequestMsg.getJMSMessageID()});     
//                    throw new MessageExchangeProcessingException (errMsg);
                } else {
                    if (this.mLog.isLoggable(LogSupport.LEVEL_DEBUG)){
                        mLog.log(LogSupport.LEVEL_DEBUG,
                                "InboundMessageProcessorListenerEndpoint_LOG_JMSREPLYTO",
                                new Object[]{ jmsOp.getBindingOperation().getName(),
                                              jmsOp.getMEP(),
                                              ((jmsReplyTo instanceof Topic)?
                                              ((Topic)jmsReplyTo).getTopicName() : 
                                              ((Queue)jmsReplyTo).getQueueName()),
                                              jmsRequestMsg.getJMSMessageID()});
                    }                                    
                }
            }
            
            msgXchange = createMessageExchange(mep);
            msgXchangeID = msgXchange.getExchangeId();
            
            // Store the JMS message - messages can be batched and each 
            // instance of mdb can process any random number of messages in the batch.
            jmsMessageBatch.put(msgXchangeID, jmsRequestMsg);
            
            NormalizedMessage inNormalizedMsg = msgXchange.createMessage();
            
            try{
            normalizeMessage(jmsRequestMsg,
                             inNormalizedMsg,
                             msgXchangeID,
                             jmsInput.getJMSMessage());
                        
            }catch(EncoderException ex){
            	String redelivery = jmsOp.getRedeliveryHandling();
            	boolean logError = false;
            	if(redelivery == null){
            		logError = true;
            	}else{
                	redelivery = redelivery.replaceAll(" ","");
                	if(!redelivery.endsWith(":delete") && redelivery.indexOf(":move(")==-1){
                		logError = true;
                	}
            	}
            	if(logError){
                    String errMsg = mMessages.getString("JMSBC-E0772.DecoderException",
                            new Object[]{redelivery});
                    mLog.log(Level.SEVERE, errMsg);
                    AlertsUtil.getAlerter().warning (errMsg, 
		    		             AlertsUtil.SUN_JMS_BINDING, 
		                         null, 
		                         AlertsUtil.getServerType(),
		                         AlertsUtil.COMPONENT_TYPE_BINDING,
		                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		                         NotificationEvent.EVENT_TYPE_ALERT,
		                         "JMSBC-E0772");
            	}
            	throw ex;
            }
                        
            if (msgXchange instanceof InOnly) {
                ((InOnly)msgXchange).setInMessage(inNormalizedMsg);
            } else if (msgXchange instanceof InOut) {
                ((InOut)msgXchange).setInMessage(inNormalizedMsg);
            }

            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                Source inContentSrc = null;
                if (msgXchange instanceof InOnly) {
                    inContentSrc = ((InOnly)msgXchange).getInMessage().getContent();
                } else if (msgXchange instanceof InOut) {
                    inContentSrc = ((InOut)msgXchange).getInMessage().getContent();
                }
                logNormalizedMessage(msgXchangeID, inContentSrc);
            }
            
            InboundReplyContext replyContext = 
                    new InboundReplyContext(requestInvocationTime,
                                            jmsRequestMsg,
                                            this,
                                            expectsAsynchOnReply(),
                                            jmsOp,
                                            endpoint,
                                            msgXchangeID);
                                    
            //before sending the messages make sure that shutdown has not started 
            String key = endpoint.getKey();
            //Acquire the life cycle lock
            int rs = ShutdownSynchronization.acquire(key);
            try{
                if(rs == SynchronizedStateChange.CHANGE_IN_PROGRESS){
                    String errMsg = mMessages.getString("JMSBC-E0759.EndpointShutdownInProcess",
                            new Object[]{endpoint.getEndpointName(),
                            endpoint.getServiceName(),
                            jmsRequestMsg.getJMSMessageID()});
                    throw new MessagingException(errMsg);
                }
                int state = endpoint.getState();
                if (!(state == EndpointState.RUNNING)) {
                    String strState = (state==EndpointState.STOPPED?"STOPPED":"SHUTDOWN");
                    String errMsg = mMessages.getString("JMSBC-E0745.EndpointNotInRunState",
                            new Object[]{endpoint.getEndpointName(),
                                         endpoint.getServiceName(),
                                         strState,
                                         jmsRequestMsg.getJMSMessageID()});

                    throw new MessagingException(errMsg);
                } 
                
                // Save the reply context in inbound message exchange map
                // so that the reply from the NMR can be processed accordingly.
                // The reply will be processed by the outbound message exchange
                // loop using this context.
                inboundMessageExchanges.put(msgXchangeID, replyContext);
                
                //before sending to NMR set GroupId and MessageId
                msgXchange.setProperty(GROUPID, mGUID);
                msgXchange.setProperty(MESSAGEID, jmsRequestMsg.getJMSMessageID());
                //to get back the mexID in case of redelivery
                msgXchange.setProperty(MEXID,msgXchangeID );
                //inboundMessageExchanges.put(jmsRequestMsg.getJMSMessageID(), msgXchangeID);
                
                inNormalizedMsg.setProperty(NMPropertiesUtil.INBOUND_GROUPID, mGUID);
                inNormalizedMsg.setProperty(NMPropertiesUtil.INBOUND_MESSAGEID, jmsRequestMsg.getJMSMessageID());
                inNormalizedMsg.setProperty(NMPropertiesUtil.MESSAGEID, jmsRequestMsg.getJMSMessageID());
                inNormalizedMsg.setProperty(NMPropertiesUtil.INOUT_MESSAGEID, jmsRequestMsg.getJMSMessageID());
                inNormalizedMsg.setProperty(NMPropertiesUtil.INBOUND_ENDPOINTNAME,
						endpoint.getServiceName().toString() + ","
								+ endpoint.getEndpointName());
                NMPropertiesUtil.fillNMProperties(jmsRequestMsg,
						inNormalizedMsg, jmsOp, endpoint.getJMSAddress(), jmsOp
								.getDestination(), jmsOp.getDestinationType(),jmsInput.getJMSMessage().forwardAsAttachment());
                
                // Send the MessageExchange to the NMR
                sendMessageToNMR(msgXchange, currentTx, jmsRequestMsg);
                
            }finally{
                //make sure to release the lock
                if(rs == SynchronizedStateChange.PASS)
                	ShutdownSynchronization.release(key);
            	
            }

        } catch (Throwable t) {
            
            inboundMessageExchanges.remove(msgXchangeID);
            
            throw t;
        }
        
        return msgXchange;
    }

    private void acknowledgeMessage(Message jmsMsg) throws Exception {
        if (jmsMsg != null) {
            try {
                jmsMsg.acknowledge();
                if (this.mLog.isLoggable(LogSupport.LEVEL_DEBUG)){
                    mLog.log(LogSupport.LEVEL_DEBUG,
                            "InboundMessageProcessorListenerEndpoint_MESSAGE_ACK_SUCCEEDED",
                            new Object[]{jmsMsg.toString()});
                }                
            } catch (Throwable t) {
                String errMsg =  mMessages.getString(
                   "JMSBC-E0710.MessageAcknowledgeFailed",
                   new Object [] {jmsMsg.toString()});
                throw new MessageExchangeProcessingException(errMsg,t);
            }
        }        
    }
    
    private boolean expectsAsynchOnReply () {
        // expect asynchronous reply only for the following:
        // XA transaction, batch, or inbound request/reply
        //return xar != null || batch || jmsOutput != null;
        return true;
    }
    
    private void setRollbackCurrentTransaction() throws Exception {
        if (currentTx != null) {
            currentTx.setRollbackOnly();
        }
        
    }
    
    private void rollbackCurrentTransaction() throws Exception {
        if (currentTx != null) {
            try {
                currentTx.rollback();
                if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)){
                    mLog.log(LogSupport.LEVEL_DEBUG,
                            "InboundMessageProcessorListenerEndpoint_TX_ROLLBACK_SUCCEEDED",
                            new Object[]{currentTx.toString()});
                }
            } catch (Exception t) {
                mLog.log(Level.SEVERE,
                         mMessages.getString("JMSBC-E0709.TransactionRollbackFailed",
                             new Object[]{currentTx.toString(), 
                                          LogSupport.getStackTraceAsString(t)}),
                          t);
                AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0709.TransactionRollbackFailed",
                             new Object[]{currentTx.toString(), 
                                          LogSupport.getStackTraceAsString(t)}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    endpoint.getServiceUnitID(), 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0709");
                throw t;
            } 
        }
    }

    private void commitCurrentTransaction() throws Exception {
        if (currentTx != null) {
            try {
                // Crash test!!
                if (logTxCommit && (xar instanceof LoggableXAResource)) {
                    ((LoggableXAResource)this.xar).logCalls(true);
                    if (JMSBindingComponent.ALLOW_CRASH_ON_COMMIT) {
                        ((LoggableXAResource)this.xar).crashOnCommit(true);                        
                    }
                }                 
                currentTx.commit();
                if (logTxCommit && mLog.isLoggable(LogSupport.LEVEL_DEBUG)){
                    mLog.log(LogSupport.LEVEL_DEBUG,
                            "InboundMessageProcessorListenerEndpoint_TX_COMMIT_SUCCEEDED",
                            new Object[]{currentTx.toString()});
                }
            } catch (Exception t) {
                mLog.log(Level.SEVERE,
                         mMessages.getString("JMSBC-E0708.TransactionCommitFailed",
                            new Object[]{currentTx.toString(), t.getLocalizedMessage()}),
                         t);
                AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0708.TransactionCommitFailed",
                            new Object[]{currentTx.toString(), t.getLocalizedMessage()}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    endpoint.getServiceUnitID(), 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0708");
                  
                throw t;
            } finally {
                logTxCommit = false;  // reset logging of transaction commit - synch mode
            }
        }
    }

    // resumes current transaction on executing thread
    private void resumeCurrentTransaction () throws Exception {
        if (txSuspended) {
            if (currentTx != null) {                
            	Transaction tx = ((TransactionManager)context.getTransactionManager()).getTransaction();
            	if(tx == null){
            		//Current thread is not associated with any transaction
                    ((TransactionManager)context.getTransactionManager()).resume(currentTx);
                    if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)){
                        mLog.log(LogSupport.LEVEL_DEBUG,
                                "InboundMessageProcessorListenerEndpoint_TX_RESUME_SUCCEEDED",
                                new Object[]{currentTx.toString()}); 
                    }
            	}else if(tx != currentTx){
        			//Current thread is associated with different transaction. 
        			//This is serious error report this.
                    mLog.log(Level.SEVERE,
                            "InboundMessageProcessorListenerEndpoint_TX_RESUME_FAILED_REPORT_THIS",
                            new Object[]{currentTx.toString(), tx.toString()}); 
            	}else{
                    if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)){
                        mLog.log(LogSupport.LEVEL_DEBUG,
                                "InboundMessageProcessorListenerEndpoint_TX_ACTIVE",
                                new Object[]{currentTx.toString()}); 
                    }    
           		}
            	
                /*
                Transaction tx = ((TransactionManager)context.getTransactionManager()).getTransaction();
                if (tx == null || (tx != null && tx.getStatus() == Status.STATUS_NO_TRANSACTION)) {
                 */
            	
            	/*
                if (currentTx.getStatus() == Status.STATUS_NO_TRANSACTION) {
                    ((TransactionManager)context.getTransactionManager()).resume(currentTx);
                    if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)){
                        mLog.log(LogSupport.LEVEL_DEBUG,
                                "InboundMessageProcessorListenerEndpoint_TX_RESUME_SUCCEEDED",
                                new Object[]{currentTx.toString()}); 
                    }
                } else {
                    if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)){
                        mLog.log(LogSupport.LEVEL_DEBUG,
                                "InboundMessageProcessorListenerEndpoint_TX_ACTIVE",
                                new Object[]{currentTx.toString()}); 
                    }    
                }
                */
            }
            txSuspended = false;
        }
    }
    
    private void delistResource(Transaction tx) throws ResourceException {
        if (tx != null) {
            try {
                TransactionManager txManager = (TransactionManager)context.getTransactionManager();
                tx = txManager.getTransaction();
                tx.delistResource(xar, XAResource.TMSUCCESS);
            } catch (Exception ex) {
                throw new ResourceException (ex);            
            }
        }
    }
    
    private Transaction startTransaction() throws Exception {
    	TransactionManager txManager = null;
        Transaction tx = null;
        
        if (xar != null) {
        	try{
        		txManager = (TransactionManager)context.getTransactionManager();
	            txManager.begin();
	            tx = txManager.getTransaction();
	            if (tx != null) {
	                tx.enlistResource(xar);
	            } else {
	                throw new MessageExchangeProcessingException(mMessages.getString(
	                        "JMSBC-E0706.GotNullTransaction"));                
	            }
	            txSuspended = false;
        	} catch (Exception exp) {
        		mLog.log(Level.SEVERE, 
                        mMessages.getString("JMSBC-E0776.TransactionBeginFailed", 
                           new Object [] {exp.getLocalizedMessage()}),exp);
        		currentTx = txManager.getTransaction();
        		try{
        			rollbackCurrentTransaction();
        		} finally {
        			throw exp;
        		}
        	}
        }
        return tx;
    }
   
    // suspend thread transactional context
    private void suspendThreadTx () throws Exception {
        if (!txSuspended) {
        	if(currentTx != null){
            ((TransactionManager)context.getTransactionManager()).suspend();
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)){
                mLog.log(LogSupport.LEVEL_DEBUG,
                        "InboundMessageProcessorListenerEndpoint_TX_SUSPEND_SUCCEEDED",
                        new Object[]{currentTx.toString()}); 
            }            
        	}
            txSuspended = true;
        }
    }
    
    private MessageExchange createMessageExchange(String mepType) 
    throws Exception {
        MessageExchange msgEx = null;
        
        if (messageXchangeFactory == null) {
            messageXchangeFactory = JMSBCContext.getRef().getChannel().createExchangeFactory();
        }

        try {
            if (mepType.equals(Endpoint.EndpointMessageType.IN_ONLY)) {
                msgEx = messageXchangeFactory.createInOnlyExchange();
            } else if (mepType.equals(Endpoint.EndpointMessageType.IN_OUT)) {
                msgEx = messageXchangeFactory.createInOutExchange();
            } else {
                throw new MessageExchangeProcessingException(mMessages.getString(
                        "JMSBC-E0703.UnsupportedMessageExchangePattern",
                        new Object[]{mepType}));                
            }
        } catch (MessagingException ex) {
                throw new MessageExchangeProcessingException(
                        mMessages.getString(
                          "JMSBC-E0704.CreateMessageExchangeFailed",
                          new Object[]{mepType}),
                        ex);                
        }
        
        return msgEx;
    }    
    
    private void normalizeMessage (javax.jms.Message jmsMsg,
                                   NormalizedMessage normalizedMsg,
                                   String msgXchangeID,
                                   JMSMessage mappingInfo) throws Exception {

        // Normalize message
        String jmsMessageID = jmsMsg.getJMSMessageID();                
        if (jmsMsg instanceof TextMessage) {
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLog.log(LogSupport.LEVEL_DEBUG,
                        "InboundMessageProcessorListenerEndpoint_JMSMSG_TEXTMSG", 
                        new Object[]{msgXchangeID, jmsMessageID});
            }

            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                // Retrieve the JMS message for logging
                TextMessage jmsTextMsg = (TextMessage)jmsMsg;
                String textMsg = jmsTextMsg.getText();
                
                mLog.log(LogSupport.LEVEL_DEBUG,
                        "InboundMessageProcessorListenerEndpoint_JMSMSG_TEXTMSG_MSG", 
                        new Object[]{textMsg});
            }
        }else if (jmsMsg instanceof MapMessage) {
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLog.log(LogSupport.LEVEL_DEBUG,
                         "InboundMessageProcessorListenerEndpoint_JMSMSG_MAPMSG", 
                         new Object[]{msgXchangeID, jmsMessageID});
            }
        }else if (jmsMsg instanceof BytesMessage) {
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLog.log(LogSupport.LEVEL_DEBUG,
                         "InboundMessageProcessorListenerEndpoint_JMSMSG_BYTESMSG", 
                         new Object[]{msgXchangeID, jmsMessageID});
            }
        }else {
            throw new MessageExchangeProcessingException (
                mMessages.getString("JMSBC-E0702.UnsupportedJMSMessageType",
                    new Object [] {Util.jmsMessageObjectTypeToString(jmsMsg)}));
        }

        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLog.log(LogSupport.LEVEL_DEBUG,
                    "InboundMessageProcessorListenerEndpoint_BEGIN_NORMALIZE_MESSAGE");
        }

        boolean isOperationInput = true;
        NamesUtil helper = new NamesUtil();
        String endPointID = (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND)?
        		(helper.createConsumingEndpointIdentifier(
        				endpoint.getServiceName(), endpoint.getEndpointName()))
        				: (helper.createProvisioningEndpointIdentifier(
        						endpoint.getServiceName(), endpoint.getEndpointName()));
        Probe normalizationMeasurement = Probe.info(getClass(),
        		endPointID,
                JMSBindingComponent.PERF_CAT_NORMALIZATION);
        mJMSNormalizer.normalize(jmsMsg,
                                 normalizedMsg,
                                 endpoint,
                                 jmsOp,
                                 mappingInfo,
                                 isOperationInput);

        if (normalizationMeasurement != null) {
            normalizationMeasurement.end();
        }
        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLog.log(LogSupport.LEVEL_DEBUG,
                    "InboundMessageProcessorListenerEndpoint_END_NORMALIZE_MESSAGE");
        }        
    }
    
    
    private javax.jms.Message denormalizeMessage (NormalizedMessage normalizedMsg,                                                  
                                                  JMSMessage mappingInfo, Message jmsMsg,
                                                  JMSOperation jmsOperation) 
        throws Exception {
        
        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLog.log(LogSupport.LEVEL_DEBUG,
                    "InboundMessageProcessorListenerEndpoint_BEGIN_DENORMALIZE_MESSAGE");
        }

        boolean isOperationInput = false;
        NamesUtil helper = new NamesUtil();
        String endPointID = (endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND)?
        		(helper.createConsumingEndpointIdentifier(
        				endpoint.getServiceName(), endpoint.getEndpointName()))
        				: (helper.createProvisioningEndpointIdentifier(
        						endpoint.getServiceName(), endpoint.getEndpointName()));
        Probe denormalizationMeasurement = Probe.info(getClass(),
        		endPointID,
                JMSBindingComponent.PERF_CAT_DENORMALIZATION);
        
        // Denormalize from NMS message to JMS message
        mJMSDenormalizer.denormalize(normalizedMsg,
                                     jmsMsg,
                                     endpoint,
                                     jmsOperation,
                                     mappingInfo,
                                     isOperationInput);
        if (denormalizationMeasurement != null) {
        	denormalizationMeasurement.end();
        }
        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLog.log(LogSupport.LEVEL_DEBUG,
                    "InboundMessageProcessorListenerEndpoint_END_DENORMALIZE_MESSAGE");
        }       
        
        return jmsMsg;
    }
    
	private Message createJMSMessage(JMSMessage mappingInfo, SendChannel channel)
			throws ChannelException, MessageExchangeProcessingException {
		String strJMSMsgType = mappingInfo.getMessageType();

        // Create the JMS Message to send
        Message jmsMsg = null;            
        int iJMSMsgType = Util.toIntMessageType(strJMSMsgType);
        switch (iJMSMsgType) {
            case Channel.MESSAGE_TYPE_TEXT:
            case Channel.MESSAGE_TYPE_MAP:
            case Channel.MESSAGE_TYPE_BYTES:                
                jmsMsg = channel.createMessage(strJMSMsgType);
                if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLog.log(LogSupport.LEVEL_DEBUG,
                            "InboundMessageProcessorListenerEndpoint_PROCESSING_JMS_MSG_TYPE",
                            new Object[] {strJMSMsgType});                    
                }
                break;
            default:
                throw new MessageExchangeProcessingException (
                         mMessages.getString("JMSBC-E0716.InvalidJMSMessageTypeInOperation",
                         new Object[]{strJMSMsgType}));                
        }
		return jmsMsg;
	}
    
    /**
     * Send a MessageExchange to the NMR
     */
    private void sendMessageToNMR(MessageExchange msgXchange,
                                  Transaction tx,  Message jmsRequestMsg) throws Exception {
        
        ServiceEndpoint se = context.getEndpoint(endpoint.getServiceName(),
                                                 endpoint.getEndpointName());
        
        if (se == null) {
            // Failed to locate provider endpoint
            String errMsg = mMessages.getString("JMSBC-E0705.GetEndpointFailed",
                    new Object[]{endpoint.getServiceName(),
                                 endpoint.getEndpointName()});
            throw new MessagingException(errMsg);
        }
         
        // Set the ServiceEndpoint and Operation "delivery address" on message exchange
        QName jmsBindingOpQName = endpoint.createOperationAddress(jmsOp.getBindingOperation().getName());
        msgXchange.setEndpoint(se);
        msgXchange.setOperation(jmsBindingOpQName);
                
        if (tx != null) {
            // Propagate transaction in message exchange
            msgXchange.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME,
                                   tx);           
            
            // suspend TX
            suspendThreadTx();
        }
        
        DeliveryChannel dc;
        if(redeliveryApplied){
        	dc = JMSBCContext.getRef().getChannel();
        }else{
        	dc = JMSBCContext.getRef().getUnWrapperChannel();
        }
                
        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLog.log(LogSupport.LEVEL_DEBUG, 
                     "InboundMessageProcessorListenerEndpoint_SENDING_MESSAGE_EXCHANGE",
                    new Object[]{msgXchange.getExchangeId(),
                                 se.getServiceName().toString()+se.getEndpointName(),
                                 jmsBindingOpQName.toString()});
            
        }
        
        if(redeliveryApplied){
            	BaseExchangeTemplates t = createBaseExchangeTemplates(msgXchange,dc);
            	((BaseMessagingChannel)dc).send(t);
        }else{
                dc.send(msgXchange);
        }
    }
        
    private BaseExchangeTemplates createBaseExchangeTemplates(MessageExchange me, DeliveryChannel dc) {

	BaseExchangeTemplates t = new BaseExchangeTemplates(me.getEndpoint(), me.getOperation(), (me instanceof InOnly), (String) me.getProperty(GROUPID), (String) me
		.getProperty(MESSAGEID), getSource(me), dc.createExchangeFactory(me.getEndpoint()));

        Properties props = new Properties();
        props.setProperty(MEXID, me.getExchangeId());
        t.setPropExchange(props);
        
	return t;
    }

    private Source getSource(MessageExchange me) {
	if(me instanceof InOnly){
	    return ((InOnly)me).getInMessage().getContent();
	}else{
	    return ((InOut)me).getInMessage().getContent();
	}
	
    }

    private void logNormalizedMessage(String exchangeId, Source msgSrc) {
        if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
            StringWriter out = null;
            if (msgSrc != null) {
                try {
                    TransformerFactory tFactory = TransformerFactory.newInstance();
                    Transformer trans = tFactory.newTransformer();
                    trans.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
                    trans.setOutputProperty(OutputKeys.INDENT, "yes");
                    trans.setOutputProperty(OutputKeys.METHOD, "xml");
                    trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
                    out = new StringWriter();
                    StreamResult result = new StreamResult(out);
                    trans.transform(msgSrc, result);
                    out.flush();
                    out.close();
                } catch (Throwable t) {
                    ;;
                }
            }
        
            if (mLog.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLog.log (LogSupport.LEVEL_DEBUG, 
                          "InboundMessageProcessorListenerEndpoint_NORMALIZED_MESSAGE_CONTENT_DUMP",
                          new Object[] {exchangeId, (out==null?"null":out.toString())});    
            }
        }
    }

	public void shutdown() {
		if(jmsMessageBatch == null)
			return;
		
		ArrayList<Message> msgList = new ArrayList<Message>(10);
		ArrayList<String> exIdList = new ArrayList<String>(10);
		synchronized (jmsMessageBatch) {
			for(Iterator iter = jmsMessageBatch.entrySet().iterator(); iter.hasNext();){
				Map.Entry entry = (Map.Entry)iter.next();
				if(entry.getValue() instanceof Message){
					msgList.add((Message)entry.getValue());
					exIdList.add((String)entry.getKey());
				}
				jmsMessageBatch.put(entry.getKey(), SHUTDOWN_PROCESSED);
			}
		}

		try{
			resumeCurrentTransaction();
			if(currentTx != null){
				currentTx.setRollbackOnly();
			}
		}catch(Throwable t){
			String errMsg = mMessages.getString(
					"JMSBC-W0735.ErrorResumingTransactionWhileShuttingDown", new Object[] {
							currentTx, endpoint});
			mLog.log(Level.SEVERE, errMsg);
		}
		try{
			Iterator<Message> msgIter = msgList.iterator();
			Iterator<String> exIdIterator = exIdList.iterator();
			while(msgIter.hasNext()){
				Message msg = msgIter.next();
				String mid = msg.toString();
				try{
					mid = msg.getJMSMessageID();
				}catch(Throwable t){}
				try{
		            setRollBackPropertyOnMessage(msg);
			        acknowledgeMessage(msg);
					mLog.log(Level.WARNING, mMessages.getString("JMSBC-W0738.AcknowledgePendingMessage",
                            new Object[]{endpoint,
                                         exIdIterator.next(),
                                         mid}));		    		
				}catch(Throwable t){
					String errMsg = mMessages.getString(
							"JMSBC-W0737.ErrorAcknowledgingMessageWhileShutdown", new Object[] {
									mid, endpoint});
					mLog.log(Level.SEVERE, errMsg);
				}
			}
			
			
		}finally{
			try{
				suspendThreadTx();
			}catch(Throwable t){
				String errMsg = mMessages.getString(
						"JMSBC-W0736.ErrorSuspendingTransactionWhileShutdown", new Object[] {
								currentTx, endpoint});
				mLog.log(Level.SEVERE, errMsg);
			}
		}
		
	}
}
