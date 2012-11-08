package com.sun.jbi.systemic.quality.propagation.api;

import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.systemic.quality.propagation.api.experimental.MessageExchangeHelperFactory;

/**
 * ConfigManagerFactory is factory while loads
 * declartive XML configuration for transaction propgation
 * and other systematic qualities.
 * 
 * This is a place holder for post sierra work.
 * @author radval
 *
 */
public abstract class ConfigManagerFactory {

	private static final String FACTORY_CLASS_NAME = "com.sun.jbi.systemic.quality.propagation.api.ConfigManagerFactory";
	
	private static Logger mLogger = Logger.getLogger(ConfigManagerFactory.class.getName());
	
	private static ConfigManagerFactory mInstance;
	
	public abstract ConfigManager loadConfig(File configFile);
		
        public static synchronized ConfigManagerFactory getDefault() {
		
		if(mInstance == null) {
			String className = System.getProperty(FACTORY_CLASS_NAME, "com.sun.jbi.systemic.quality.propagation.api.impl.ConfigManagerFactoryImpl");
			if(className != null) {
				try {
					Class cls = Class.forName(className);
					mInstance = (ConfigManagerFactory) cls.newInstance();
				} catch(Exception ex) {
					mLogger.log(Level.SEVERE, "Failed to get a default implementation of " +  FACTORY_CLASS_NAME);
				}
				
			}
			
		}
		
		return mInstance;
	}
}
