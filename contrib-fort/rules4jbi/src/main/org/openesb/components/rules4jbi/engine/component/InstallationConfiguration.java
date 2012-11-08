/*
 * @(#)InstallationConfiguration.java        $Revision: 1.4 $ $Date: 2008/11/15 01:22:24 $
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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Properties;

import com.google.inject.Inject;
import com.google.inject.name.Named;

import org.openesb.components.rules4jbi.shared.logging.Logger;

/**
 * JMX MBean for configuring component's installation specific parameters.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.4 $ $Date: 2008/11/15 01:22:24 $
 * 
 * @since 0.1
 */
public class InstallationConfiguration implements InstallationConfigurationMBean {

    public static final String CONFIG_FILE_NAME = "config.properties";
    
    static final int DEFAULT_POOL_SIZE = 10;
    
    static final int DEFAULT_MAX_SERVICE_UNITS = 10;
    
    private static final int MIN_POOL_SIZE = 1;
    
    private static final int MAX_POOL_SIZE = 100;
    
    private static final int MIN_MAX_SERVICE_UNITS = 1;
    
    private static final int MAX_MAX_SERVICE_UNITS = 25;
    
    private static final String POOL_SIZE_PROPERTY_KEY = "pool.size";
    
    private static final String MAX_SERVICE_UNITS_PROPERTY_KEY = "max.service.units";
    
    private Logger logger;

    private int poolSize;

    private int maxServiceUnits;

    /** Creates installation configuration with default values. */
    public InstallationConfiguration() {
        this(DEFAULT_POOL_SIZE, DEFAULT_MAX_SERVICE_UNITS);
    }

    InstallationConfiguration(int poolSize, int maxServiceUnits) {
        this.poolSize = poolSize;
        this.maxServiceUnits = maxServiceUnits;
    }

    @Inject 
    public void setLogger(@Named("InstallationConfiguration") Logger logger) {
        this.logger = logger;
    }
    
    public int getPoolSize() {
        return poolSize;
    }
    
    public void setPoolSize(int poolSize) {
        logger.entering(this.getClass(), "setPoolSize", poolSize);

        if (poolSize < MIN_POOL_SIZE || poolSize > MAX_POOL_SIZE) {
            throw new IllegalArgumentException("Invalid pool size: " + poolSize);
        }
        
        this.poolSize = poolSize;
        logger.exiting(this.getClass(), "setPoolSize");
    }

    public int getMaxServiceUnits() {
        return maxServiceUnits;
    }

    public void setMaxServiceUnits(int maxServiceUnits) {
        logger.entering(this.getClass(), "setMaxServiceUnits", maxServiceUnits);

        if (maxServiceUnits < MIN_MAX_SERVICE_UNITS || maxServiceUnits > MAX_MAX_SERVICE_UNITS) {
            throw new IllegalArgumentException("Invalid max service units: " + maxServiceUnits);
        }
        
        this.maxServiceUnits = maxServiceUnits;
        logger.exiting(this.getClass(), "setMaxServiceUnits");
    }
    
    public void save(File file) {
        logger.fine("Saving installation configuration properties: PoolSize=%d, MaxServiceUnits=%d",
                poolSize, maxServiceUnits);
        
        OutputStream outputStream = null;
        try {
            outputStream = new FileOutputStream(file);
            
            Properties properties = new Properties();
            properties.setProperty(POOL_SIZE_PROPERTY_KEY, Integer.toString(poolSize));
            properties.setProperty(MAX_SERVICE_UNITS_PROPERTY_KEY, Integer.toString(maxServiceUnits));
            properties.store(outputStream, "rules4jbi installation configuration properties");
            
        } catch (FileNotFoundException e) {
            logger.severe("Failed to open properties configuration file");
            
        } catch (IOException e) {
            logger.severe("Failed to store properties");
            
        } finally {
            try {
                if (outputStream != null) {
                    outputStream.close();
                }
            } catch (IOException e) {
                logger.severe("Error occured while closing properties file");
            }
        }
    }
    
    public static InstallationConfiguration load(File file)
            throws InvalidInstallationConfigurationException
    {
        InputStream inputStream = null;
        try {
             inputStream = new FileInputStream(file);
            
             Properties properties = new Properties();
             properties.load(inputStream);
             
             if (properties.getProperty(POOL_SIZE_PROPERTY_KEY) == null
                     || properties.getProperty(MAX_SERVICE_UNITS_PROPERTY_KEY) == null)
             {
                 throw new InvalidInstallationConfigurationException("Could not find required properties");
             }

             try {
                 int poolSize = Integer.parseInt(properties.getProperty(POOL_SIZE_PROPERTY_KEY));
                 int maxServiceUnits = Integer.parseInt(properties.getProperty(MAX_SERVICE_UNITS_PROPERTY_KEY));
                 
                 return new InstallationConfiguration(poolSize, maxServiceUnits);
                 
             } catch (NumberFormatException e) {
                 throw new InvalidInstallationConfigurationException(e);
             }
            
        } catch (FileNotFoundException e) {
            throw new InvalidInstallationConfigurationException(e);

        } catch (IOException e) {
            throw new InvalidInstallationConfigurationException(e);
            
        } finally {
            try {
                if (inputStream != null) {
                    inputStream.close();
                }
            } catch (IOException e) {
                // ignore
            }
        }
    }
}
