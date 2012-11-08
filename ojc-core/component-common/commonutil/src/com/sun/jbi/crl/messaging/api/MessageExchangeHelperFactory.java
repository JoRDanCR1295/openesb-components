package com.sun.jbi.crl.messaging.api;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;

public abstract class MessageExchangeHelperFactory {

	private static final String FACTORY_CLASS_NAME = "com.sun.jbi.crl.messaging.api.MessageExchangeHelperFactory";
	
	private static Logger mLogger = Logger.getLogger(MessageExchangeHelperFactory.class.getName());
	
	private static MessageExchangeHelperFactory mInstance;
	
	public abstract CorrelatedMessageExchangeFactory newCorrelatedMessageExchangeFactory(ComponentContext context) throws CorrelatedMessageExchangeException;
	
	public static synchronized MessageExchangeHelperFactory getDefault() {
		
		if(mInstance == null) {
			String className = System.getProperty(FACTORY_CLASS_NAME, "com.sun.jbi.crl.messaging.api.impl.MessageExchangeHelperFactoryImpl");
			if(className != null) {
				try {
					Class cls = Class.forName(className);
					mInstance = (MessageExchangeHelperFactory) cls.newInstance();
				} catch(Exception ex) {
					mLogger.log(Level.SEVERE, "Failed to get a default implementation of " +  FACTORY_CLASS_NAME);
				}
				
			}
			
		}
		
		return mInstance;
	}
}
