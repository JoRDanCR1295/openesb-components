package com.sun.jbi.systemic.quality.propagation.api.experimental.impl;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.MessagingException;

import com.sun.jbi.systemic.quality.propagation.api.CorrelatedMessageExchangeException;
import com.sun.jbi.systemic.quality.propagation.api.experimental.CorrelatedMessageExchangeFactory;
import com.sun.jbi.systemic.quality.propagation.api.experimental.MessageExchangeHelperFactory;

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
