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
 * @(#)SendChannelJCAImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jms;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageConsumer;
import javax.jms.MessageProducer;
import javax.jms.Queue;
import javax.jms.QueueConnection;
import javax.jms.QueueConnectionFactory;
import javax.jms.QueueSession;
import javax.jms.Session;
import javax.jms.TemporaryQueue;
import javax.jms.TemporaryTopic;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicConnectionFactory;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;
import javax.naming.InitialContext;
import javax.resource.spi.ManagedConnection;
import javax.resource.spi.ManagedConnectionFactory;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jmsbc.JMSBindingComponent;
import com.sun.jbi.jmsbc.LogSupport;
import com.sun.jbi.jmsbc.extensions.JMSAddress;
import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.jca.MCFManager;
import com.sun.jbi.jmsbc.util.AlertsUtil;

/**
 *
 * An implementation of the "out" flow channel which utilizes the JMS JCA 
 * for external interaction
 */
public class SendChannelJCAImpl implements SendChannel {
    private JMSAddress jmsAddress = null;
    private JMSOperation jmsOperation = null;
    private boolean topicDomain = true;
    private boolean isOpened = false;
    private boolean isStarted = false;
    
    private static final Messages mMessages =
        Messages.getMessages(SendChannelJCAImpl.class);
    private static final Logger mLogger =
        Messages.getLogger(SendChannelJCAImpl.class);
    
    private ConnectionFactory connFactory = null;
    private boolean managedConnectionFactory = true;
    private Transaction invTx = null;
    
    private int refCount;
    private ClassLoader classloader = null;
    
    /** 
     * Creates a new instance of SendChannelJCAImpl 
     */
    public SendChannelJCAImpl() {
        refCount = 1;
    }

    public void initialize(JMSAddress jmsAddress,
                           JMSOperation jmsOperation) {
        this.jmsAddress = jmsAddress;
        this.jmsOperation = jmsOperation;
        topicDomain = this.jmsOperation.getDestinationType().equalsIgnoreCase(JMSConstants.TOPIC);
        classloader = this.jmsAddress.getClassLoader();
        
    }
    
    public Message createMessage(String jmsMessageType) throws ChannelException {
        Message msg = null;        
        Connection connection = null;
        Session session = null;
        try {
            connection = getConnection();
            boolean transacted = false;
            session = createSession(connection,transacted);
            
            switch (Util.toIntMessageType(jmsMessageType)) {
                case MESSAGE_TYPE_TEXT:
                    msg = session.createTextMessage();
                    break;
                case MESSAGE_TYPE_MAP:
                    msg = session.createMapMessage();
                    break;
                case MESSAGE_TYPE_BYTES:
                    msg = session.createBytesMessage();
                    break;
                /*
                case MESSAGE_TYPE_OBJECT:
                    msg = session.createObjectMessage();
                    break;
                case MESSAGE_TYPE_STREAM:
                    msg = session.createStreamMessage();
                    break;
                case MESSAGE_TYPE_MESSAGE:
                    msg = session.createMessage();
                    break;
                */
                default:
                    String errMsg = mMessages.getString("JMSBC-E0208.UnsupportedJMSMessageType",
                                new Object[]{jmsMessageType});                
                    throw new ChannelException(errMsg);
            }            
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG,
                            "SendChannelJCAImpl_JMS_MSG_CREATE_SUCCESS",
                            new Object[]{jmsMessageType});
            }                                            
        } catch (Throwable ex) {
            String errMsg = mMessages.getString("JMSBC-E0209.CreateJMSMessageFailed",
                        new Object[]{jmsMessageType});
            throw new ChannelException(errMsg, ex);
        } finally {
            closeSessionAndConnection(connection, session);
        }
        
        return msg;
    }

    public void send(Message msg) throws ChannelException { 
    	send(msg, jmsOperation.getDestination(), topicDomain);
    }
    
    public void send(Message msg, Destination dest) throws ChannelException {
        if (isStarted) {
            Connection connection = null;
            Session session = null;
            boolean transacted = false;
            String destination = null;
            try {
		destination = dest instanceof Topic ? ((Topic) dest).getTopicName() : ((Queue) dest).getQueueName();
	    } catch (JMSException e) {
		String errMsg = mMessages.getString("JMSBC-E0211.SendFailed", new Object[] { dest });
		throw new ChannelException(errMsg, e);
	    }
            try {
                connection = getConnection();
                session = createSession(connection,transacted);
                MessageProducer prod = createProducer(session,dest);
                try {
                    setMessageProducerOptions(prod);
                    send(prod,msg);
                } catch (Throwable t) {
                    throw t;
                } finally {
                    prod.close();
                }
                
                if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLogger.log(LogSupport.LEVEL_DEBUG,
                                "SendChannelJCAImpl_SEND_SUCCESS",
                                new Object[]{destination,
                                             jmsAddress.getConnectionURL()});
                }                                            
            } catch (Throwable ex) {
                String errMsg = mMessages.getString("JMSBC-E0211.SendFailed",
                            new Object[]{destination});
                throw new ChannelException(errMsg,ex);                                                
            } finally {
                closeSessionAndConnection(connection, session);
            }
        } else {
            String errMsg = mMessages.getString(
                        "JMSBC-E0210.ChannelStopped",
                        new Object[]{jmsAddress.getConnectionURL(),
                                     "send(Ljavax.jms.Message;Ljavax.jms.Destination;)V"});
            throw new ChannelException(errMsg);                            
        }
    }

   synchronized public void open() throws ChannelException {
        if (!isOpened) {
        	ClassLoader cls = Util.setContextClassLoader(this.classloader);
            try {
            	ConnectionFactory cf = null;
	        	if(jmsAddress.getConnectionURL().startsWith("lookup://")){
	        		String jndiName = jmsAddress.getConnectionURL().substring("lookup://".length());
	        		InitialContext ic = new InitialContext();
	        		cf = (ConnectionFactory)ic.lookup(jndiName);
	        		managedConnectionFactory = false;
	        	}else{
	                ManagedConnectionFactory mcf = 
	                    MCFManager.getInstance()
	                              .acquireMCF(jmsAddress, jmsOperation);
	                cf = (ConnectionFactory)mcf.createConnectionFactory();
	        	}
                
	        	connFactory = (ConnectionFactory) Proxy.newProxyInstance(getClass().getClassLoader(),
                          cf.getClass().getInterfaces(),
                          new InvocationHandlerImpl(cf));
	        	
            } catch (Throwable t) {
                String errMsg = mMessages.getString("JMSBC-E0203.ChannelOpenFailed",
                            new Object[]{jmsAddress.getConnectionURL()});
                throw new ChannelException(errMsg, t);
            } finally{
            	Util.setContextClassLoader(cls);
            }
            isOpened = true;
        }
    }

    private Connection getConnection() throws Exception {
        if (isOpened) {
        	beforeCreatingConnection();
        	Connection conn = createConnection(connFactory);
        	return (Connection) Proxy.newProxyInstance(getClass().getClassLoader(),
        			conn.getClass().getInterfaces(),
                    new InvocationHandlerImpl(conn));
        	
        } else {
            String errMsg = mMessages.getString("JMSBC-E0206.ConnectionClosed",
                        new Object[]{jmsAddress.getConnectionURL(), "getConnection()Ljavax.jms.Connection"});
            
            throw new ChannelException(errMsg);
        }
    }

    synchronized public void close() throws ChannelException {
        if (isOpened  && refCount == 0) {
        	if(managedConnectionFactory){
        		MCFManager.getInstance().releaseMCF(jmsAddress, jmsOperation);
        	}
            isStarted = false;
            isOpened = false;
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG,
                            "SendChannelJCAImpl_CLOSE_SUCCESS",
                            new Object[]{jmsAddress.getConnectionURL()});
            }
        }
    }

    public Message send(Message msg, long timeout) throws ChannelException {
	return send(msg, jmsOperation.getDestination(), topicDomain, null, false, timeout);

    }
    
    synchronized public void start() throws ChannelException {
        isStarted = true;
    }

    synchronized public void stop() throws ChannelException {
        isStarted = false;
    }

    synchronized public boolean isOpen() {
        return isOpened;
    }

    synchronized public boolean isStarted() {
        return isStarted;
    }
        
    synchronized protected int incrementRefCount() {
        return ++refCount;
    }
    
    synchronized protected int decrementRefCount() {
        return --refCount;
    }
    
    private void setMessageProducerOptions (MessageProducer producer) 
    throws JMSException {
        // Set the MessageProducer delivery attributes
        Boolean bDisableMessageID = jmsOperation.getDisableMessageID();
        boolean disableMessageID = 
                (bDisableMessageID==null? 
                    false:bDisableMessageID.booleanValue());

        Boolean bDisableMessageTimeStamp = jmsOperation.getDisableMessageTimeStamp();
        boolean disableMessageTimeStamp = 
                (bDisableMessageTimeStamp==null? 
                false:bDisableMessageTimeStamp.booleanValue());

        String deliveryMode = jmsOperation.getDeliveryMode();
        if (deliveryMode == null || deliveryMode.length()==0) {
            deliveryMode=JMSConstants.DELIVERYMODE_NON_PERSISTENT;
        }

        producer.setDisableMessageID(disableMessageID);
        producer.setDisableMessageTimestamp(disableMessageTimeStamp);
        producer.setDeliveryMode(Util.toIntDeliveryMode(deliveryMode));

        //
        // Set optional producer attributes
        //
        Long timeToLive = jmsOperation.getTimeToLive();
        if (timeToLive != null) {
            producer.setTimeToLive(timeToLive.longValue());                    
        }

        Integer priority = jmsOperation.getPriority();
        if (priority != null) {
            producer.setPriority(priority.intValue());
        }                
    }
    
    private ManagedConnection getManagedConnection(Session s) {
	ManagedConnection mc = null;
	if (s != null) {
	    com.stc.jmsjca.core.WSession ws = (com.stc.jmsjca.core.WSession) s;
	    com.stc.jmsjca.core.JSession js = ws.getJSession();
	    mc = js.getManagedConnection();
	}
	return mc;
    }

    public Message synchronousReceive(long timeout) throws ChannelException {
	return synchronousReceive(jmsOperation.getDestination(), topicDomain, timeout);
    }

	private void closeSessionAndConnection(Connection connection, Session session) {
		if (session != null) {
		    try {
		        session.close();
		    } catch (Exception ex) {
		        mLogger.log(Level.WARNING,
		                    mMessages.getString("JMSBC-W0202.SessionCloseFailed",
		                    new Object[]{jmsAddress.getConnectionURL(), ex.getLocalizedMessage()}),
		                    ex);
		       AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0202.SessionCloseFailed",
		                    new Object[]{jmsAddress.getConnectionURL(), ex.getLocalizedMessage()}), 
		                JMSBindingComponent.SHORT_DISPLAY_NAME, 
		                null, 
		                AlertsUtil.getServerType(),
		                AlertsUtil.COMPONENT_TYPE_BINDING,
		                NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		                NotificationEvent.EVENT_TYPE_ALERT,
		                "JMSBC-W0202");  
		    }
		}
		if (connection !=null) {
		    try {
		        connection.close();
		    } catch (Exception ex) {
		        mLogger.log(Level.WARNING,
		                    mMessages.getString("JMSBC-W0203.ConnectionCloseFailed",
		                    new Object[]{jmsAddress.getConnectionURL(), ex.getLocalizedMessage()}),
		                    ex);
		        AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0203.ConnectionCloseFailed",
		                    new Object[]{jmsAddress.getConnectionURL(), ex.getLocalizedMessage()}), 
		                JMSBindingComponent.SHORT_DISPLAY_NAME, 
		                null, 
		                AlertsUtil.getServerType(),
		                AlertsUtil.COMPONENT_TYPE_BINDING,
		                NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		                NotificationEvent.EVENT_TYPE_ALERT,
		                "JMSBC-W0203");  
		    }
	        afterClosingConnection();
		}
	}

	public Message send(Message msg, String dest, boolean isDestTopic,
			String replyToDest, boolean isReplyToDestTopic, long timeout)
			throws ChannelException {
        Destination jmsReplyTo = null;
        MessageConsumer consumer = null;
        Connection connection = null;
        Session session = null;
        boolean transacted = false;
        Message replyMsg = null;
        boolean deleteTempDest = false;
        
        try {                
            connection = getConnection();
            session = createSession(connection, transacted);
            if(replyToDest != null){
            	jmsReplyTo = getDestination(replyToDest, isReplyToDestTopic,
						session);
            }else{
            	deleteTempDest = true;
                if (jmsOperation.getDestinationType().equals(JMSConstants.TOPIC)) {
                    jmsReplyTo = session.createTemporaryTopic();
                    replyToDest = ((Topic)jmsReplyTo).getTopicName();
                } else {
                    jmsReplyTo = session.createTemporaryQueue();
                    replyToDest = ((Queue)jmsReplyTo).getQueueName();
                }
            }
            msg.setJMSReplyTo(jmsReplyTo);
            consumer = createConsumer(session,jmsReplyTo,null);
            connection.start();
            send(msg, dest, isDestTopic);
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG,
                            "SendChannelJCAImpl_SET_JMS_REPLY_TO",
                            new Object [] {jmsOperation.getDestinationType(),
                		replyToDest});
            }
            
            
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG,
                            "SendChannelJCAImpl_RECEIVING_REPLY_ON_JMS_REPLY_TO",
                            new Object [] {jmsOperation.getDestinationType(),
                							replyToDest});
            }                
            replyMsg = consumer.receive(timeout);                
        } catch (Throwable t) {
            String errMsg = mMessages.getString(
                        "JMSBC-E0212.RequestReplyFailed");
            throw new ChannelException(errMsg, t);
        } finally {                
            // clean up
            if (consumer != null) {
                try {
                    consumer.close();
                } catch (Throwable t) {
                    mLogger.log(Level.WARNING,
                            mMessages.getString("JMSBC-W0206.MessageConsumerCloseFailed",
                              new Object [] {jmsOperation.getDestinationType(),
                            				replyToDest,
                                             t.getLocalizedMessage()}),
                            t);
                   AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0206.MessageConsumerCloseFailed",
                              new Object [] {jmsOperation.getDestinationType(),
                		   					replyToDest,
                                             t.getLocalizedMessage()}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0206"); 
                }
            }

            if (jmsReplyTo != null && deleteTempDest) {
                try {
                    if (jmsReplyTo instanceof TemporaryTopic) {
                        ((TemporaryTopic)jmsReplyTo).delete();
                    } else {
                        ((TemporaryQueue)jmsReplyTo).delete();                        
                    }
                } catch (Throwable t) {
                    mLogger.log(Level.WARNING,
                            mMessages.getString("JMSBC-W0205.DeleteTemporaryDestinationFailed",
                              new Object [] {jmsOperation.getDestinationType(),
                            				replyToDest,
                                             t.getLocalizedMessage()}),
                            t);
                    AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0205.DeleteTemporaryDestinationFailed",
                              new Object [] {jmsOperation.getDestinationType(),
                    						replyToDest,
                                             t.getLocalizedMessage()}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0205"); 
                }
                jmsReplyTo = null;                
            }
            
            closeSessionAndConnection(connection, session);
        }

        if (replyMsg != null && this.mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "SendChannelJCAImpl_RECEIVED_JMS_MSG_REPLY",
                        new Object []{jmsOperation.getDestinationType(),
            							replyToDest});
        } else if (replyMsg == null) {
            mLogger.log(Level.WARNING,
                        "JMSBC-W0204.WaitTimedOutForReplyMessage",
                        new Object []{jmsOperation.getDestinationType(),
            						replyToDest,
                                      timeout});                
           AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0204.WaitTimedOutForReplyMessage",
                        new Object []{jmsOperation.getDestinationType(),
        		   					 replyToDest,
                                      timeout}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0718");                   
        }
        
        return replyMsg;
	}

	public void send(Message msg, String dest, boolean isDestTopic,
			String replyToDest, boolean isReplyToDestTopic) throws ChannelException {
        if (isStarted) {
            Connection connection = null;
            Session session = null;
            boolean transacted = false;
            try {
                connection = getConnection();
                session = createSession(connection, transacted);
                Destination destination = null;
                destination = getDestination(dest, isDestTopic, session);
                	
                if(replyToDest != null){
                	msg.setJMSReplyTo(getDestination(replyToDest,
							isReplyToDestTopic, session));
                }
                	
                MessageProducer prod = createProducer(session, destination);
                try {
                    setMessageProducerOptions(prod);
                    send(prod,msg);
                } catch (Throwable t) {
                    throw t;
                } finally {
                    prod.close();
                }
                
                if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLogger.log(LogSupport.LEVEL_DEBUG,
                                "SendChannelJCAImpl_SEND_SUCCESS",
                                new Object[]{destination,
                                             jmsAddress.getConnectionURL()});
                }                                            
            } catch (Throwable ex) {
                String errMsg = mMessages.getString("JMSBC-E0211.SendFailed",
                            new Object[]{dest});
                throw new ChannelException(errMsg,ex);                                                
            } finally {
                closeSessionAndConnection(connection, session);
            }
        } else {
            String errMsg = mMessages.getString(
                        "JMSBC-E0210.ChannelStopped",
                        new Object[]{jmsAddress.getConnectionURL(),
                                     "send(Ljavax.jms.Message;Ljavax.jms.Destination;)V"});
            throw new ChannelException(errMsg);                            
        }
	}

	public Message synchronousReceive(String dest, boolean isTopic, long timeout)
			throws ChannelException {
        Message reply = null;
        MessageConsumer consumer = null;
                
        Connection connection = null;
        Session session = null;
        boolean transacted = false;
        
        try {                
            connection = getConnection();
            //Check for durability
            boolean durable = isTopic && jmsOperation.getSubscriptionDurability() != null &&
                    jmsOperation.getSubscriptionDurability().equals(JMSConstants.DURABLE);
            
            if(jmsOperation.getClientID()!=null){
            	connection.setClientID(jmsOperation.getClientID());
            }else{
            	if(durable){
            		//Just for sanity this check has already been made much before.
            		//must provide clientId
                    String errMsg = mMessages.getString(
                            "JMSBC-E0213.MustProvideClientIdWithDurableSubscriber",
                            new Object[]{ jmsOperation.getSubscriptionName()});
                    throw new RuntimeException(errMsg);                            
            	}
            }
            session = createSession(connection, transacted);
            Destination destination = getDestination(dest, isTopic, session);
            String messageSelector = jmsOperation.getMessageSelector();
            if(durable){
            	if(messageSelector == null)
            		consumer = session.createDurableSubscriber((Topic)destination, jmsOperation.getSubscriptionName());
            	else
            		consumer = session.createDurableSubscriber((Topic)destination, jmsOperation.getSubscriptionName(), messageSelector, false);
            }else{
            	consumer = createConsumer(session, destination,messageSelector);
            }
            connection.start();
            reply = consumer.receive(timeout);
            
        } catch (Throwable t) {
            String errMsg = mMessages.getString(
                        "JMSBC-E0212.RequestReplyFailed");
            throw new ChannelException(errMsg, t);
        } finally {                
            // clean up
            if (consumer != null) {
                try {
                    consumer.close();
                } catch (Throwable t) {
                    String msg = mMessages.getString("JMSBC-W0206.MessageConsumerCloseFailed",
					  new Object [] {jmsOperation.getDestinationType(), dest,
					                 t.getLocalizedMessage()});
					mLogger.log(Level.WARNING, msg, t);
                   AlertsUtil.getAlerter().warning(msg, 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            null, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0206"); 
                }
            }

            closeSessionAndConnection(connection, session);
        }
        return reply;
	}

	private Destination getDestination(String dest, boolean isTopic,
			Session session) throws JMSException {
		Destination destination;
		if(isTopic)
			destination = session.createTopic(dest);
		else
			destination = session.createQueue(dest);
		return destination;
	}

	public JMSAddress getJMSAddress(){
		return jmsAddress;
	}
	
	public JMSOperation getJMSOperation(){
		return jmsOperation;
	}

	public void send(Message msg, String dest, boolean isDestTopic)
			throws ChannelException {
		
		this.send(msg, dest, isDestTopic, null, false);
	}

	public Message send(Message msg, String dest, boolean isDestTopic,
			long timeout) throws ChannelException {
		return send(msg, dest, isDestTopic, null, false, timeout);
	}
	
	public boolean isManagedConnection(){
		return managedConnectionFactory;
	}

	private Transaction setTransactionInComponentContext(Transaction tx){
		Class switchClass = null;
		Class invmgrclass = null;
		Class invclass = null;
		Transaction result = null;
		
		try {
			switchClass = Class.forName("com.sun.enterprise.Switch");
			invmgrclass = Class.forName("com.sun.enterprise.InvocationManager");
			invclass = Class.forName("com.sun.enterprise.ComponentInvocation");
		} catch (Throwable t) {
		}
		if (switchClass != null && invmgrclass != null && invclass != null) {
			Object[] noParam = new Object[0];
			Class[] noArg = new Class[0];
			try{
				java.lang.reflect.Method getSwitchMethod = switchClass.getMethod(
						"getSwitch", noArg);
				java.lang.reflect.Method getInvocationManagerMethod = switchClass
						.getMethod("getInvocationManager", noArg);
				Object switchObj = getSwitchMethod.invoke(null, noParam);
				Object invmgr = getInvocationManagerMethod.invoke(switchObj,
						noParam);

				// Get ComponentInvocation
				java.lang.reflect.Method getCurrentInvocationMethod = invmgrclass
						.getMethod("getCurrentInvocation", noArg);
				java.lang.reflect.Method setTransactionMethod = invclass.getMethod(
						"setTransaction", new Class[]{Transaction.class});
				java.lang.reflect.Method getTransactionMethod = invclass.getMethod(
						"getTransaction", noArg);
				Object inv = getCurrentInvocationMethod.invoke(invmgr, noArg);
				if(inv != null){
					Transaction temp = (Transaction)getTransactionMethod.invoke(inv, noParam);
					setTransactionMethod.invoke(inv, new Object[]{tx});
					result = temp;
				}
			}catch(Throwable t){}
		}
		
		return result;
	}
	
	
	public void beforeCreatingConnection(){
		if(managedConnectionFactory)
			return;
		
		Class switchClass = null;
		Transaction tx = null;
		try {
			switchClass = Class.forName("com.sun.enterprise.Switch");
		} catch (Throwable t) {}
		
		if (switchClass != null) {
			try{
				Object[] noParam = new Object[0];
				Class[] noArg = new Class[0];
				java.lang.reflect.Method getSwitchMethod = switchClass.getMethod(
						"getSwitch", noArg);
				java.lang.reflect.Method getgetTransactionManager = switchClass
						.getMethod("getTransactionManager", noArg);
				Object switchObj = getSwitchMethod.invoke(null, noParam);
				TransactionManager txmgr = (TransactionManager)getgetTransactionManager.invoke(switchObj,
						noParam);
				if(txmgr != null)
					tx = txmgr.getTransaction();

			}catch(Throwable t){}
		}
		invTx = setTransactionInComponentContext(tx);
	}
	public void afterClosingConnection() {
	if (managedConnectionFactory)
	    return;
	Transaction temp = invTx;
	invTx = null;
	setTransactionInComponentContext(temp);
    }

    private Connection createConnection(ConnectionFactory cf) throws JMSException {
	if (cf instanceof TopicConnectionFactory) {
	    return ((TopicConnectionFactory) cf).createTopicConnection();
	} else if (cf instanceof QueueConnectionFactory) {
	    return ((QueueConnectionFactory) cf).createQueueConnection();
	} else {
	    return cf.createConnection();
	}
    }

    private Session createSession(Connection connection, boolean transacted) throws JMSException {
	if (connection instanceof TopicConnection)
	    return ((TopicConnection) connection).createTopicSession(transacted, Session.AUTO_ACKNOWLEDGE);
	else if (connection instanceof QueueConnection)
	    return ((QueueConnection) connection).createQueueSession(transacted, Session.AUTO_ACKNOWLEDGE);
	else
	    return connection.createSession(transacted, Session.AUTO_ACKNOWLEDGE);
    }

    private MessageProducer createProducer(Session session, Destination dest) throws JMSException {
	if (session instanceof TopicSession)
	    return ((TopicSession) session).createPublisher((Topic) dest);
	else if (session instanceof QueueSession)
	    return ((QueueSession) session).createSender((Queue) dest);
	else
	    return session.createProducer(dest);
    }

    private MessageConsumer createConsumer(Session session, Destination dest, String messageSelector) throws JMSException {
	if (messageSelector == null) {
	    if (session instanceof TopicSession && dest instanceof Topic)
		return ((TopicSession) session).createSubscriber((Topic) dest);
	    else if (session instanceof QueueSession && dest instanceof Queue)
		return ((QueueSession) session).createReceiver((Queue) dest);
	    else
		return session.createConsumer(dest);
	} else {
	    if (session instanceof TopicSession)
		return ((TopicSession) session).createSubscriber((Topic) dest, messageSelector, false);
	    else if (session instanceof QueueSession)
		return ((QueueSession) session).createReceiver((Queue) dest, messageSelector);
	    else
		return session.createConsumer(dest, messageSelector);
	}
    }

    private void send(MessageProducer prod, Message msg) throws JMSException {
	if (prod instanceof TopicPublisher)
	    ((TopicPublisher) prod).publish(msg);
	else
	    prod.send(msg);
    }

    private class InvocationHandlerImpl implements InvocationHandler {
	private Object delegate;

	public InvocationHandlerImpl(Object delegate) {
	    this.delegate = delegate;
	}

	public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
	    ClassLoader cls = Util.setContextClassLoader(classloader);
	    try {
		return method.invoke(delegate, args);
	    } finally {
		Util.setContextClassLoader(cls);
	    }
	}

    }

}
