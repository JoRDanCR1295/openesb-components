/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.tester;

/**
 *
 * @author radval
 */
public interface TesterRuntimeConfigurationMBean {

    public String getConfigFileLocation();
    
    /**
     * Retrieves the configuration display schema
     */
    public String retrieveConfigurationDisplaySchema();
    
    /**
     * Retrieves the configuration display data
     */
    public String retrieveConfigurationDisplayData();
}
