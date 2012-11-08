package com.sun.jbi.crl.messaging.api;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;

public abstract class ParentChildExchangeCorrelatorFactory {

	private static final String FACTORY_CLASS_NAME = "com.sun.jbi.crl.messaging.api.ParentChildExchangeCorrelatorFactory";
	
	private static Logger mLogger = Logger.getLogger(MessageExchangeHelperFactory.class.getName());
	
	private static ParentChildExchangeCorrelatorFactory mInstance;
	
	public abstract ParentChildExchangeCorrelator newParentChildExchangeCorrelator() throws CorrelatedMessageExchangeException;
	
	public static synchronized ParentChildExchangeCorrelatorFactory getDefault() {
		
		if(mInstance == null) {
			String className = System.getProperty(FACTORY_CLASS_NAME, "com.sun.jbi.crl.messaging.api.impl.ParentChildExchangeCorrelatorFactoryImpl");
			if(className != null) {
				try {
					Class cls = Class.forName(className);
					mInstance = (ParentChildExchangeCorrelatorFactory) cls.newInstance();
				} catch(Exception ex) {
					mLogger.log(Level.SEVERE, "Failed to get a default implementation of " +  FACTORY_CLASS_NAME);
				}
				
			}
			
		}
		
		return mInstance;
	}
}

