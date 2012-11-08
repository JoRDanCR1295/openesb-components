package com.sun.jbi.systemic.quality.propagation.api.impl;

import com.sun.jbi.systemic.quality.propagation.api.ConfigManager;
import com.sun.jbi.systemic.quality.propagation.api.CorrelatedMessageExchangeException;
import com.sun.jbi.systemic.quality.propagation.api.ParentChildExchangeCorrelator;
import com.sun.jbi.systemic.quality.propagation.api.ParentChildExchangeCorrelatorFactory;

public class ParentChildExchangeCorrelatorFactoryImpl extends ParentChildExchangeCorrelatorFactory {

	public ParentChildExchangeCorrelatorFactoryImpl() {
	}

	@Override
	public ParentChildExchangeCorrelator newParentChildExchangeCorrelator(ConfigManager manager) throws CorrelatedMessageExchangeException {
		return new ParentChildExchangeCorrelatorImpl(manager);
	}
	
	
}
