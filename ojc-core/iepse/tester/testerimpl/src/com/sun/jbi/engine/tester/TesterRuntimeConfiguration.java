/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.tester;

import java.io.File;
import java.io.FileInputStream;
import java.util.Properties;

/**
 *
 * @author radval
 */
public class TesterRuntimeConfiguration implements TesterRuntimeConfigurationMBean {
    
    private String mConfigFileLocation = "";
      
    private Properties mProp = new Properties();

    private String mConfigSchema;
    
    private String mConfigData;
    
    public TesterRuntimeConfiguration(String propFile, String configSchema, String configData) {
    	this.mConfigSchema = configSchema;
    	this.mConfigData = configData;
        read(propFile);
    }
    
    public boolean read(String propFile) {
        File fin = new File(propFile);

        try {
            FileInputStream fis = new FileInputStream(fin);
            mProp.load(fis);
            mConfigFileLocation = mProp.getProperty(Constants.PROP_CONFIG_FILE_LOCATION);
            
        } catch (Exception ex) {
            return false;
        }

        return true;
    }

    public String getConfigFileLocation() {
        return this.mConfigFileLocation;
    }
    
    /**
     * Retrieves the configuration display schema
     */
    public String retrieveConfigurationDisplaySchema() {
    	return mConfigSchema;
    }
    
    /**
     * Retrieves the configuration display data
     */
    public String retrieveConfigurationDisplayData() {
    	return mConfigData;
    }
}
