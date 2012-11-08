package com.sun.jbi.crl.messaging.api.impl;

import com.sun.jbi.crl.messaging.api.CorrelatedMessageExchangeException;
import com.sun.jbi.crl.messaging.api.ParentChildExchangeCorrelator;
import com.sun.jbi.crl.messaging.api.ParentChildExchangeCorrelatorFactory;

public class ParentChildExchangeCorrelatorFactoryImpl extends ParentChildExchangeCorrelatorFactory {

	public ParentChildExchangeCorrelatorFactoryImpl() {
	}

	@Override
	public ParentChildExchangeCorrelator newParentChildExchangeCorrelator() throws CorrelatedMessageExchangeException {
		return new ParentChildExchangeCorrelatorImpl();
	}
	
	
}
