package com.sun.jbi.systemic.quality.propagation.api.experimental.impl;

import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.security.auth.Subject;

import com.sun.jbi.systemic.quality.propagation.api.CorrelatedMessageExchangeException;
import com.sun.jbi.systemic.quality.propagation.api.experimental.CorrelatedMessageExchangeFactory;
import com.sun.jbi.systemic.quality.propagation.api.experimental.MessageExchangeNode;
import com.sun.jbi.systemic.quality.propagation.api.experimental.MutableMessageExchangeNode;

public class CorrelatedMessageExchangeFactoryImpl implements CorrelatedMessageExchangeFactory {

	private static Logger mLogger = Logger.getLogger(CorrelatedMessageExchangeFactory.class.getName());
	
	private ComponentContext mContext;
	
	private DeliveryChannel mChannel;
	
	private MessageExchangeFactory mExchangeFactory;
	
	public CorrelatedMessageExchangeFactoryImpl(ComponentContext context) throws MessagingException {
		this.mContext = context;
		this.mChannel = this.mContext.getDeliveryChannel();
		this.mExchangeFactory = this.mChannel.createExchangeFactory();
	}

	
	public InOnly createInOnlyExchange(MessageExchange parentExchange) throws CorrelatedMessageExchangeException {
		InOnly childExchange = null;
		
		try {
			MessageExchangeFactory  meFactory = this.mExchangeFactory;
			
			childExchange = meFactory.createInOnlyExchange();
			
			createMessageExchangeNode(parentExchange, childExchange);
			if(parentExchange == null) {
				return childExchange;
			}
			
			ExchangeStatus parentMEStatus = parentExchange.getStatus();
						
			if(parentMEStatus == ExchangeStatus.ACTIVE) {
				doTransactionPropogationToChildExchange(parentExchange, childExchange);
			} else {
				Object parentTransaction = parentExchange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
				
				if(parentTransaction != null) {
					throw new CorrelatedMessageExchangeException("Can not create child exchange, parent exchange is not active.");
				}
			
			}
			
		} catch(MessagingException ex) {
			throw new CorrelatedMessageExchangeException("Failed to create child exchange.", ex);
		}
		
		
		return childExchange;
	}

	
	public InOut createInOutExchange(MessageExchange parentExchange) throws CorrelatedMessageExchangeException {
		InOut childExchange = null;
		
		
		
		try {
			MessageExchangeFactory  meFactory = this.mExchangeFactory;
			childExchange = meFactory.createInOutExchange();
			
			createMessageExchangeNode(parentExchange, childExchange);
			if(parentExchange == null) {
				return childExchange;
			}
			
			ExchangeStatus parentMEStatus = parentExchange.getStatus();
			
			if(parentMEStatus == ExchangeStatus.ACTIVE) {
				doTransactionPropogationToChildExchange(parentExchange, childExchange);
			} else {
				
				Object parentTransaction = parentExchange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
				
				if(parentTransaction != null) {
					throw new CorrelatedMessageExchangeException("Can not create child exchange, parent exchange is not active.");
				}
			}
			
		} catch(MessagingException ex) {
			throw new CorrelatedMessageExchangeException("Failed to create child exchange.", ex);
		}
		
		
		return childExchange;
	}
	
	
	public void send(MessageExchange me) throws CorrelatedMessageExchangeException {
		MessageExchangeNode node = (MessageExchangeNode) me.getProperty(MessageExchangeNode.MESSAGE_EXCHANGE_PARENT_NODE);
		if(node != null) {
			MessageExchangeNode parent = node.getParentExchange();
			if(parent != null) {
				MessageExchange parentExchange =  parent.getMessageExchange();
				//if parent message is not live and child exchangee has transaction then we can
				//not send child exchange
				if(parentExchange.getStatus() != ExchangeStatus.ACTIVE && me.isTransacted()) {
					throw new CorrelatedMessageExchangeException("Can not send message exchange, the exchange "+ me + " transacted but parent exchange "+ parentExchange + " is not active.");
				}
				
				doSecurityPropogationToChildExchange(parentExchange, me);
				
			} 
				sendAsyncMessage(me);
			
		} else {
			throw new CorrelatedMessageExchangeException("Can not send message exchange, the exchange "+ me + " is not created by this factory");
		}
	}
	
	
	public void sendSync(MessageExchange me) throws CorrelatedMessageExchangeException {
		MessageExchangeNode node = (MessageExchangeNode) me.getProperty(MessageExchangeNode.MESSAGE_EXCHANGE_PARENT_NODE);
		if(node != null) {
			MessageExchangeNode parent = node.getParentExchange();
			if(parent != null) {
				MessageExchange parentExchange =  parent.getMessageExchange();
				//if parent message is not live and child exchangee has transaction then we can
				//not send child exchange
				if(parentExchange.getStatus() != ExchangeStatus.ACTIVE && me.isTransacted()) {
					throw new CorrelatedMessageExchangeException("Can not send message exchange, the exchange "+ me + " transacted but parent exchange "+ parentExchange + " is not active.");
				}
				
				doSecurityPropogationToChildExchange(parentExchange, me);
				
			} 
			
			sendSyncMessage(me);
		} else {
			throw new CorrelatedMessageExchangeException("Can not send message exchange, the exchange "+ me + " is not created by this factory");
		}
	}
	
	public void sendSync(MessageExchange me, long timeout) throws CorrelatedMessageExchangeException {
		MessageExchangeNode node = (MessageExchangeNode) me.getProperty(MessageExchangeNode.MESSAGE_EXCHANGE_PARENT_NODE);
		if(node != null) {
			MessageExchangeNode parent = node.getParentExchange();
			if(parent != null) {
				MessageExchange parentExchange =  parent.getMessageExchange();
				//if parent message is not live and child exchangee has transaction then we can
				//not send child exchange
				if(parentExchange.getStatus() != ExchangeStatus.ACTIVE && me.isTransacted()) {
					throw new CorrelatedMessageExchangeException("Can not send message exchange, the exchange "+ me + " transacted but parent exchange "+ parentExchange + " is not active.");
				}
				
				doSecurityPropogationToChildExchange(parentExchange, me);
				
			}
			
			sendSyncMessage(me, timeout);
		} else {
			throw new CorrelatedMessageExchangeException("Can not send message exchange, the exchange "+ me + " is not created by this factory");
		}
	}
	
	private void createMessageExchangeNode(MessageExchange parentExchange, MessageExchange childExchange) {
		if(parentExchange == null) {
			MutableMessageExchangeNode root = MessageExchangeNodeFactory.getInstance().createMessageExchangeNode(childExchange);
			childExchange.setProperty(MessageExchangeNode.MESSAGE_EXCHANGE_PARENT_NODE, root);
		} else {
			MutableMessageExchangeNode root = (MutableMessageExchangeNode) parentExchange.getProperty(MessageExchangeNode.MESSAGE_EXCHANGE_PARENT_NODE);
			if(root == null) {
				root = MessageExchangeNodeFactory.getInstance().createMessageExchangeNode(parentExchange);
				MutableMessageExchangeNode child = MessageExchangeNodeFactory.getInstance().createMessageExchangeNode(childExchange);
				root.addChildExchange(child);
				childExchange.setProperty(MessageExchangeNode.MESSAGE_EXCHANGE_PARENT_NODE, child);
				parentExchange.setProperty(MessageExchangeNode.MESSAGE_EXCHANGE_PARENT_NODE, root);
			}
		}
	}
	private void sendAsyncMessage(MessageExchange me)  throws CorrelatedMessageExchangeException {
		try {
			this.mChannel.send(me);
		} catch(MessagingException ex) {
			throw new CorrelatedMessageExchangeException("Can not asynchronous send message exchange, the exchange "+ me + " encountered problem by delivery channel while sending it", ex);
		}
		
	}
	
	private void sendSyncMessage(MessageExchange me)  throws CorrelatedMessageExchangeException {
		try {
			this.mChannel.sendSync(me);
		} catch(MessagingException ex) {
			throw new CorrelatedMessageExchangeException("Can not send synchronous message exchange, the exchange "+ me + " encountered problem by delivery channel while sending it", ex);
		}
		
	}
	
	private void sendSyncMessage(MessageExchange me, long timeout)  throws CorrelatedMessageExchangeException {
		try {
			this.mChannel.sendSync(me, timeout);
		} catch(MessagingException ex) {
			throw new CorrelatedMessageExchangeException("Can not send synchronous message exchange, the exchange "+ me + " encountered problem by delivery channel while sending it", ex);
		}
		
	}
	private void doTransactionPropogationToChildExchange(MessageExchange parentExchange, MessageExchange childExchange) {
		Object parentTransaction = parentExchange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
		if(parentTransaction != null) {
			childExchange.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, parentTransaction);
		}
		
	}
	
	private void doSecurityPropogationToChildExchange(MessageExchange parentExchange, MessageExchange childExchange) {
		NormalizedMessage parentInMessage = null;
		NormalizedMessage childInMessage = null;
		
		if(parentExchange instanceof InOnly) {
			InOnly inOnly = (InOnly) parentExchange;
			parentInMessage = inOnly.getInMessage();
		} else if (parentExchange instanceof InOut) {
			InOut inOut = (InOut) parentExchange;
			parentInMessage = inOut.getInMessage();
		}
		
		if(childExchange instanceof InOnly) {
			InOnly inOnly = (InOnly) childExchange;
			childInMessage = inOnly.getInMessage();
		} else if (childExchange instanceof InOut) {
			InOut inOut = (InOut) childExchange;
			childInMessage = inOut.getInMessage();
		}
		
		
		doSecurityPropogationToChildExchange(parentInMessage, childInMessage);
	}
	
	
	private void doSecurityPropogationToChildExchange(NormalizedMessage parentInMessage, NormalizedMessage childInMessage) {
		if(parentInMessage != null && childInMessage != null) {
			Subject subject = parentInMessage.getSecuritySubject();
			if(subject != null) {
				childInMessage.setSecuritySubject(subject);
			}
		}
	}
}
