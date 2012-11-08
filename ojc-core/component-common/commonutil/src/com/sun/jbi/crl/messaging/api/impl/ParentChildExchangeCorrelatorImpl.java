package com.sun.jbi.crl.messaging.api.impl;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.security.auth.Subject;

import com.sun.jbi.crl.messaging.api.CorrelatedMessageExchangeException;
import com.sun.jbi.crl.messaging.api.ParentChildExchangeCorrelator;

public class ParentChildExchangeCorrelatorImpl implements ParentChildExchangeCorrelator {

	public void assignChildExchange(MessageExchange parentExchange, 
									MessageExchange childExchange) throws CorrelatedMessageExchangeException {
		
		if(parentExchange == null) {
			throw new CorrelatedMessageExchangeException("Parent Exchange is null");
		}
		
		if(childExchange == null) {
			throw new CorrelatedMessageExchangeException("Child Exchange is null");
		}
		
		ExchangeStatus parentMEStatus = parentExchange.getStatus();
		
		Object transaction = parentExchange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
		if(parentMEStatus == ExchangeStatus.ACTIVE) {
			doTransactionPropogationToChildExchange(parentExchange, childExchange);
			
		} else {
			if(transaction != null) {
				throw new CorrelatedMessageExchangeException("Can not assign properties to child exchange, parent exchange is not active and it has transaction.");
			}
		}
		
		doSecurityPropogationToChildExchange(parentExchange, childExchange);
		
	}
	
	private void doTransactionPropogationToChildExchange(MessageExchange parentExchange, MessageExchange childExchange) {
		Object parentTransaction = parentExchange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
		if(parentTransaction != null) {
			childExchange.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, parentTransaction);
		}
		
	}
	
	private void doSecurityPropogationToChildExchange(MessageExchange parentExchange, 
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
			if(childInMessage == null) {
				throw new CorrelatedMessageExchangeException("Child Exchane should have input message");
			}
			
		} else if(childExchange instanceof InOut) {
			InOut inOutME = (InOut) childExchange;
			childInMessage = inOutME.getInMessage();
			if(childInMessage == null) {
				throw new CorrelatedMessageExchangeException("Child Exchane should have input message");
			}
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
