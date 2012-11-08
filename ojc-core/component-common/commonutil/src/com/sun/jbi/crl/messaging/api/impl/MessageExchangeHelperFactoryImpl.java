package com.sun.jbi.crl.messaging.api.impl;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessagingException;

import com.sun.jbi.crl.messaging.api.CorrelatedMessageExchangeException;
import com.sun.jbi.crl.messaging.api.CorrelatedMessageExchangeFactory;
import com.sun.jbi.crl.messaging.api.MessageExchangeHelperFactory;

public class MessageExchangeHelperFactoryImpl extends MessageExchangeHelperFactory {

	@Override
	public synchronized CorrelatedMessageExchangeFactory newCorrelatedMessageExchangeFactory(ComponentContext context) throws CorrelatedMessageExchangeException {
		try {
			return new CorrelatedMessageExchangeFactoryImpl(context);
		} catch (MessagingException ex) {
			throw new CorrelatedMessageExchangeException("Failed to create CorrelatedMessageExchangeFactoryImpl, got MessagingException", ex);
		}
		
	}

	
}
