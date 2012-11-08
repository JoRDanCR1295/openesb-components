/*
 * @(#)Rules4JBIServiceUnitManager.java        $Revision: 1.2 $ $Date: 2008/07/14 16:30:25 $
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

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;

import com.google.inject.Inject;
import com.google.inject.Injector;

import org.openesb.components.rules4jbi.shared.logging.Logger;

import org.openesb.components.rules4jbi.engine.guice.annotations.Manager;
import org.openesb.components.rules4jbi.engine.guice.annotations.MaxServiceUnits;
import org.openesb.components.rules4jbi.engine.util.JBIUtils;

/**
 * This class implements the <code>javax.jbi.component.ServiceUnitManager</code> interface
 * and provides additional methods for managing deployed service units.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/07/14 16:30:25 $
 * 
 * @see ServiceUnit
 * @since 0.1
 */
public class Rules4JBIServiceUnitManager implements ServiceUnitManager {
    
    @Inject @Manager
    private Logger logger;
    
    @Inject
    private Injector injector;
    
    @Inject
    private ComponentContext componentContext;
    
    @Inject
    private ServiceUnitFactory serviceUnitFactory;
    
    @Inject @MaxServiceUnits
    private int maxServiceUnits;
    
    private final Map<String, ServiceUnit> serviceUnits;

    public Rules4JBIServiceUnitManager() {
        serviceUnits = Collections.synchronizedMap(new HashMap<String, ServiceUnit>());
    }
    
    public String deploy(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        logger.entering(this.getClass(), "deploy", serviceUnitName, serviceUnitRootPath);
        
        boolean failed = false;
        
        synchronized (serviceUnits) {
            if (serviceUnits.containsKey(serviceUnitName)) {
                failed = true;
                
            } else if (serviceUnits.size() >= maxServiceUnits) {
                failed = true;
                
            } else {
                ServiceUnit serviceUnit = 
                        serviceUnitFactory.createNewServiceUnit(serviceUnitName, serviceUnitRootPath);
                serviceUnits.put(serviceUnitName, serviceUnit);
            }
        }
        
        logger.fine("Deployment of service unit '%s' %s", serviceUnitName, failed ? "failed" : "successful");
        
        ComponentTaskResult taskResult =
                new ComponentTaskResult(componentContext.getComponentName(), "deploy", failed);
        
        logger.fine("Returning component task result: %s", taskResult);
        return taskResult.toXML();
    }
    
    /*
     * Note that JBI runtime must call init when the service unit is about to be started for
     * the first time, as well as before any other lifecycle method in case of state restoration.
     */
    public void init(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        logger.entering(this.getClass(), "init", serviceUnitName, serviceUnitRootPath);
        
        ServiceUnit serviceUnit = null;
        
        synchronized (serviceUnits) {
            serviceUnit = serviceUnits.get(serviceUnitName);
            
            if (serviceUnit == null) {
                serviceUnit = serviceUnitFactory.createNewServiceUnit(serviceUnitName, serviceUnitRootPath);
                serviceUnits.put(serviceUnitName, serviceUnit);
            }
            
        }
        
        serviceUnit.init();
    }
    
    public void start(String serviceUnitName) throws DeploymentException {
        logger.entering(this.getClass(), "start", serviceUnitName);

        ServiceUnit serviceUnit = findByName(serviceUnitName);
        serviceUnit.start();
    }
    
    public void stop(String serviceUnitName) throws DeploymentException {
        logger.entering(this.getClass(), "stop", serviceUnitName);

        ServiceUnit serviceUnit = findByName(serviceUnitName);
        serviceUnit.stop();
    }

    public void shutDown(String serviceUnitName) throws DeploymentException {
        logger.entering(this.getClass(), "shutDown", serviceUnitName);
        
        ServiceUnit serviceUnit = findByName(serviceUnitName);
        serviceUnit.shutDown();
    }
    
    public String undeploy(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        logger.entering(this.getClass(), "undeploy", serviceUnitName, serviceUnitRootPath);
        
        boolean failed = serviceUnits.remove(serviceUnitName) == null;
            
        logger.fine("Undeployment of service unit '%s' %s", serviceUnitName, failed ? "failed" : "successful");
        
        ComponentTaskResult taskResult =
                new ComponentTaskResult(componentContext.getComponentName(), "undeploy", failed);
        
        logger.fine("Returning component task result: %s", taskResult);
        return taskResult.toXML();
    }
    
    public ServiceUnit findByName(String serviceUnitName) throws DeploymentException {
        ServiceUnit serviceUnit = serviceUnits.get(serviceUnitName);
        
        if (serviceUnit != null) {
            logger.fine("Found service unit '%s'", serviceUnitName);
            
            return serviceUnit;
            
        } else {
            logger.warning("Could not find service unit '%s'", serviceUnitName);
            
            throw new DeploymentException("Service unit '" + serviceUnitName + "' does not exist");
        }
    }
    
    public ServiceUnit findByServiceEndpoint(ServiceEndpoint endpoint) {
        
        synchronized (serviceUnits) {
            Collection<ServiceUnit> values = serviceUnits.values();

            for (ServiceUnit serviceUnit : values) {
                if (JBIUtils.equal(endpoint, serviceUnit.getServiceEndpoint())) {
                    logger.fine("Found service unit with the specified endpoint");
                    
                    return serviceUnit;
                }
            }
        }

        return null;
    }
    
    public void injectDependenciesIntoServiceUnits() {
        synchronized (serviceUnits) {
            Collection<ServiceUnit> values = serviceUnits.values();
            for (ServiceUnit serviceUnit : values) {
                injector.injectMembers(serviceUnit);
            }

        }
    }
}
