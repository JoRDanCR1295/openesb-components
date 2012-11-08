/*
 * @(#)InstallationConfigurationTest.java        $Revision: 1.2 $ $Date: 2009/01/27 21:42:46 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.engine.component;

import org.openesb.components.rules4jbi.shared.logging.Logger;
import org.openesb.components.rules4jbi.shared.logging.LoggerImpl;
import java.io.File;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2009/01/27 21:42:46 $
 * 
 * @since 0.1
 */
public class InstallationConfigurationTest {
    
    private InstallationConfiguration configOne;
    
    private InstallationConfiguration configTwo;

    @Before
    public void setUp() {
        Logger logger =
                new LoggerImpl(java.util.logging.Logger.getLogger(java.util.logging.Logger.GLOBAL_LOGGER_NAME));
        
        configOne = new InstallationConfiguration(5, 7);
        configOne.setLogger(logger);
        
        configTwo = new InstallationConfiguration(12, 23);
        configTwo.setLogger(logger);
    }

    @Test
    public void load() throws InvalidInstallationConfigurationException {
        String testDirectory = System.getProperty("test.dir");
        
        File file = new File(testDirectory, "configA.properties");
        InstallationConfiguration configuration = InstallationConfiguration.load(file);
        
        assertEquals(15, configuration.getPoolSize());
        assertEquals(7, configuration.getMaxServiceUnits());

        
        file = new File(testDirectory, "configB.properties");
        configuration = InstallationConfiguration.load(file);
        
        assertEquals(3, configuration.getPoolSize());
        assertEquals(11, configuration.getMaxServiceUnits());
    }

    @Test(expected=org.openesb.components.rules4jbi.engine.component.InvalidInstallationConfigurationException.class)
    public void loadException() throws InvalidInstallationConfigurationException {
        String testDirectory = System.getProperty("test.dir");
        
        File file = new File(testDirectory, "configC.properties");
        InstallationConfiguration configuration = InstallationConfiguration.load(file);
    }
    
    @Test
    public void save() throws InvalidInstallationConfigurationException {
        /* We assume here, that load() already works properly */
        
        String testDirectory = System.getProperty("test.dir");
        
        File file = new File(testDirectory, "configOne.properties");
        configOne.save(file);
        
        InstallationConfiguration loadedConfig = InstallationConfiguration.load(file);
        
        assertEquals(5, loadedConfig.getPoolSize());
        assertEquals(7, loadedConfig.getMaxServiceUnits());

        file.delete();

        
        file = new File(testDirectory, "configTwo.properties");
        configTwo.save(file);
        
        loadedConfig = InstallationConfiguration.load(file);
        
        assertEquals(12, loadedConfig.getPoolSize());
        assertEquals(23, loadedConfig.getMaxServiceUnits());

        file.delete();
    }
}
