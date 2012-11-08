package com.sun.jbi.systemic.quality.propagation.api.impl;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.security.auth.Subject;

import com.sun.jbi.systemic.quality.propagation.api.ConfigManager;
import com.sun.jbi.systemic.quality.propagation.api.CorrelatedMessageExchangeException;
import com.sun.jbi.systemic.quality.propagation.api.ParentChildExchangeCorrelator;

public class ParentChildExchangeCorrelatorImpl implements ParentChildExchangeCorrelator {

	static final String EXCEPTION_CHILD_EXCHANGE_SHOULD_HAVE_INPUT_MESSAGE = "Child Exchange should have input message";
	
	static final String EXCEPTION_PARENT_EXCHANGE_SHOULD_HAVE_INPUT_MESSAGE = "Parent Exchange should have input message";
	
	
	private ConfigManager mManager;
	
	public ParentChildExchangeCorrelatorImpl(ConfigManager manager) {
		this.mManager = manager;
	}
	
	
	public synchronized void assignChildExchange(MessageExchange parentExchange, 
									MessageExchange childExchange) throws CorrelatedMessageExchangeException {
		
		if(childExchange == null) {
			throw new CorrelatedMessageExchangeException("Child Exchange is null");
		}
		
		ConfigManager.TRANSACTIONTYPE tType = this.mManager.getTransactionType(childExchange);
		ConfigManager.SECURITYTYPE sType = this.mManager.getSecurityType(childExchange);
		
		if ((parentExchange == null) && 
				((tType == ConfigManager.TRANSACTIONTYPE.JOIN_PARENT) || (sType == ConfigManager.SECURITYTYPE.ALWAYS))) 
		{
			throw new CorrelatedMessageExchangeException("Parent Exchange cannot be null when transaction type is " 
					+ "'JOIN_PARENT' or when security type is 'ALWAYS'");
		}
       
		if(tType == ConfigManager.TRANSACTIONTYPE.JOIN_PARENT) {
			Object transaction = parentExchange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
			if(parentExchange.getStatus() == ExchangeStatus.ACTIVE) {
				doTransactionPropogationToChildExchange(transaction, 
						childExchange);

			} else {
				if(transaction != null) {
					throw new CorrelatedMessageExchangeException("Can not assign properties to child exchange, " 
							+ "parent exchange is not active and it has transaction.");
				}
			}
		} else if(tType == ConfigManager.TRANSACTIONTYPE.REQUIRES_NEW) {
			Object transaction = this.mManager.createNewTransaction(childExchange);
			doTransactionPropogationToChildExchange(transaction, childExchange);
		}
		
		if (sType == ConfigManager.SECURITYTYPE.ALWAYS) {
			doSecurityPropogationToChildExchange(parentExchange, childExchange);
		}
		
	}
	
	void doTransactionPropogationToChildExchange(Object transaction, 
                                                             MessageExchange childExchange) {
		if(transaction != null) {
			childExchange.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, transaction);
		}
		
	}
	
	void doSecurityPropogationToChildExchange(MessageExchange parentExchange, 
													  MessageExchange childExchange) throws CorrelatedMessageExchangeException {
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
			InOnly inOnlyME = (InOnly) childExchange;
			childInMessage = inOnlyME.getInMessage();
			
		} else if(childExchange instanceof InOut) {
			InOut inOutME = (InOut) childExchange;
			childInMessage = inOutME.getInMessage();
			
		}
		
		if(parentInMessage == null) {
			throw new CorrelatedMessageExchangeException(EXCEPTION_PARENT_EXCHANGE_SHOULD_HAVE_INPUT_MESSAGE);
		}
		
		if(childInMessage == null) {
			throw new CorrelatedMessageExchangeException(EXCEPTION_CHILD_EXCHANGE_SHOULD_HAVE_INPUT_MESSAGE);
		}
		
		ConfigManager.SECURITYTYPE sType = this.mManager.getSecurityType(childExchange);
		
		if(sType == ConfigManager.SECURITYTYPE.ALWAYS) {
			doSecurityPropogationToChildExchange(parentInMessage, childInMessage);
		}
		
	}
	
	
   void doSecurityPropogationToChildExchange(NormalizedMessage parentInMessage, NormalizedMessage childInMessage) {
		if(parentInMessage != null && childInMessage != null) {
			Subject subject = parentInMessage.getSecuritySubject();
			if(subject != null) {
				childInMessage.setSecuritySubject(subject);
			}
		}
	}
}
