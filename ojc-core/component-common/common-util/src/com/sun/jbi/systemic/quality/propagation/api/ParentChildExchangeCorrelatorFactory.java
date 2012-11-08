package com.sun.jbi.systemic.quality.propagation.api;

import com.sun.jbi.systemic.quality.propagation.api.experimental.MessageExchangeHelperFactory;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
/**
 * This is the factory class used to get and instance
 * of ParentChildExchangeCorrelator.
 * 
 * One has to pass ConfigManager to get instance of
 * ParentChildExchangeCorrelator. 
 * 
 * ConfigManager
 * @author radval
 *
 */
public abstract class ParentChildExchangeCorrelatorFactory {

	private static final String FACTORY_CLASS_NAME = "com.sun.jbi.systemic.quality.propagation.api.ParentChildExchangeCorrelatorFactory";
	
	private static Logger mLogger = Logger.getLogger(MessageExchangeHelperFactory.class.getName());
	
	private static ParentChildExchangeCorrelatorFactory mInstance;
	
	public abstract ParentChildExchangeCorrelator newParentChildExchangeCorrelator(ConfigManager manager) throws CorrelatedMessageExchangeException;
	
	public static synchronized ParentChildExchangeCorrelatorFactory getDefault() {
		
		if(mInstance == null) {
			String className = System.getProperty(FACTORY_CLASS_NAME, "com.sun.jbi.systemic.quality.propagation.api.impl.ParentChildExchangeCorrelatorFactoryImpl");
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

