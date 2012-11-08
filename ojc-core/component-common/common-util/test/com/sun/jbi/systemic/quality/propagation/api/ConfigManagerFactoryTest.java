/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.systemic.quality.propagation.api;

import java.io.File;
import junit.framework.TestCase;

/**
 *
 * @author radval
 */
public class ConfigManagerFactoryTest extends TestCase {
    
    public ConfigManagerFactoryTest(String testName) {
        super(testName);
    }            

    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    /**
     * Test of loadConfig method, of class ConfigManagerFactory.
     */
    public void testLoadConfig() {
        System.out.println("loadConfig");
        File configFile = null;
        ConfigManagerFactory instance = ConfigManagerFactory.getDefault();
        ConfigManager result = instance.loadConfig(configFile);
        //not yet implemented ConfigManager
        assertNull(result);
       
    }

    /**
     * Test of getDefault method, of class ConfigManagerFactory.
     */
    public void testGetDefault() {
        System.out.println("getDefault");
        ConfigManagerFactory result = ConfigManagerFactory.getDefault();
        assertNotNull("ConfigManagerFactory should not be null", result);
    }

}
