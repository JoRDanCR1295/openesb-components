/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)AbstractServiceUnitManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.lifecycle.impl;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;

import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.descriptor.ServicesDescriptor;
import com.sun.jbi.common.util.EntryRegistry;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;
import com.sun.jbi.component.toolkit.util.I18n;
import com.sun.jbi.component.toolkit.util.task.ExceptionInfo;
import com.sun.jbi.component.toolkit.util.task.MsgLocInfo;
import com.sun.jbi.component.toolkit.util.task.TaskResult;
import com.sun.jbi.component.toolkit.util.task.TaskResultDetails;
import com.sun.jbi.component.toolkit.util.task.TaskXmlWriter;
import com.sun.jbi.component.toolkit.util.task.TaskResultDetails.MessageType;

/**
 * Utility base class for {@link ServiceUnitManager} implementations, which
 * maintains a set of deployed {@link ServiceUnit}s.
 * 
 * @author Kevan Simpson
 */
public class DefaultServiceUnitManager implements ServiceUnitManager {
    public static final String DEPLOY_TASK 		= "deploy";  // NOI18N
    public static final String UNDEPLOY_TASK 	= "undeploy";  // NOI18N
    public static final String INIT_TASK 		= "init";  // NOI18N
    public static final String SHUTDOWN_TASK 	= "shutDown";  // NOI18N
    
    /** Maps service units by their names. */
    private EntryRegistry<String, ServiceUnit> mServiceUnits = null;
    /** The component's context. */
    private final ManagerContext mManagerCtx; 
    /** The logger. */
    private Logger mLogger;
    
    protected DefaultServiceUnitManager(ManagerContext mgrCtx) {
        mManagerCtx = mgrCtx;
        mServiceUnits = new EntryRegistry<String, ServiceUnit>();
        mLogger = Util.getLogger(mgrCtx.getComponentContext(), 
                                 this.getClass().getName());
    }
    
    /**
     * This implementation performs the following actions:
     * <ol>
     *      <li>logs a message at <code>Level.INFO</code></li>
     *      <li>tests for duplicate deployment<li>
     *      <li>creates and registers a service unit with this manager</li>
     *      <li>returns a <code>SUCCESS</code> task message string if the 
     *          specified service unit is not already deployed</li>
     * </ol> 
     *  
     * @see javax.jbi.component.ServiceUnitManager#deploy(java.lang.String, java.lang.String) 
     */
    public String deploy(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
    	try {
	        if (log().isLoggable(Level.FINE)) {
	            log().fine("COMPTK-3002: Deploying SU \""+ serviceUnitName +
	            				 "\" at root path: "+ serviceUnitRootPath);
	        }
	        if (isDeployed(serviceUnitName)) {
	            // TODO should this return a FAILED task message instead?
	            throw new DeploymentException(I18n.loc(
	                    "COMPTK-6017: Service unit already deployed: {0}", 
	                    serviceUnitName));
	        }
	
	        installServiceUnit(serviceUnitName, serviceUnitRootPath);
	        
	        return createSuccessMessage(DEPLOY_TASK);
    	}
    	catch (Exception e) {
    	    // remove service unit to avoid issues redeploying later
    	    getServiceUnits().remove(serviceUnitName);
    	    
    		String msg = I18n.loc("COMPTK-6018: Failed to deploy SU \"{0}\": {1}", 
    							serviceUnitName, e.getMessage());
    		log().log(Level.WARNING, msg, e);
    		return createExceptionMessage(DEPLOY_TASK, e, "COMPTK_DEPLOY_1", msg);
    	}
    }

    protected ServiceUnit installServiceUnit(String serviceUnitName, String serviceUnitRootPath) 
            throws DeploymentException {
        ServiceUnit srvcUnit = 
                createServiceUnit(serviceUnitName, serviceUnitRootPath); 
        getServiceUnits().register(serviceUnitName, srvcUnit);
        getManagerContext().getEndpointManager().addServiceUnit(srvcUnit);
        
        return srvcUnit;
    }
    
    protected void uninstallServiceUnit(String serviceUnitName) {
        ServiceUnit srvcUnit = getServiceUnits().remove(serviceUnitName);
        getManagerContext().getEndpointManager().removeServiceUnit(srvcUnit);
    }
    
    /** @see javax.jbi.component.ServiceUnitManager#init(java.lang.String, java.lang.String) */
    public void init(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("COMPTK-3003: Initializing SU \""+ serviceUnitName +
            				 "\" at root path: "+ serviceUnitRootPath);
        }

        ServiceUnit srvcUnit = null;
        if (!isDeployed(serviceUnitName)) {
            /*
             * JBI javadoc says to throw exception. Does not account for AS shutdown,
             * since SUs are stored in memory and lost at that time.
             * For now, adding service unit instance to registry.
             */
            srvcUnit = installServiceUnit(serviceUnitName, serviceUnitRootPath);
        }

        try {
            getManagerContext().getMessagingChannel()
                    .installServiceQualities(serviceUnitName, serviceUnitRootPath);

            if (srvcUnit == null) srvcUnit = getServiceUnits().lookup(serviceUnitName);
            
            /*          START PROVISIONING         */
            if (hasEndpointLifeCycle()) {
                getManagerContext().getEndpointManager()
                        .getLifeCycle().startProvisioning(srvcUnit);
            }
        }
        catch (Exception e) {
            throw new DeploymentException(
                    createFailedMessage(INIT_TASK, "COMPTK_INIT_1", e.getMessage()), e);
        }
        
        if (log().isLoggable(Level.FINE)) {
            log().fine("COMPTK-3004: Initialized SU \""+ serviceUnitName +
            				 "\" successfully!");
        }
    }

    /** @see javax.jbi.component.ServiceUnitManager#shutDown(java.lang.String) */
    public void shutDown(String serviceUnitName) throws DeploymentException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("COMPTK-3005: Shutting down SU \""+ serviceUnitName +"\"");
        }

        if (!isDeployed(serviceUnitName)) {
            throw new DeploymentException(I18n.loc(
                    "COMPTK-6019: shutDown failed for undeployed service unit \"{0}\"", 
                    serviceUnitName));
        }

        try {
            ServiceUnit srvcUnit = getServiceUnits().lookup(serviceUnitName);
            getManagerContext().getMessagingChannel()
                    .uninstallServiceQualities(srvcUnit.getName());
            /*          STOP PROVISIONING         */
            if (hasEndpointLifeCycle()) {
                getManagerContext().getEndpointManager()
                        .getLifeCycle().stopProvisioning(srvcUnit);
            }
        }
        catch (Exception e) {
            throw new DeploymentException(
                    createFailedMessage(INIT_TASK, "COMPTK_INIT_1", e.getMessage()), e);
        }

        if (log().isLoggable(Level.FINE)) {
            log().fine("COMPTK-3006: Shut down successful for SU-"+ serviceUnitName);
        }
    }

    /** @see javax.jbi.component.ServiceUnitManager#start(java.lang.String) */
    public void start(String serviceUnitName) throws DeploymentException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("COMPTK-3007: Starting service unit: "+ serviceUnitName);
        }

        if (!isDeployed(serviceUnitName)) {
            throw new DeploymentException(I18n.loc(
                    "COMPTK-6020: start failed for undeployed service unit \"{0}\"", 
                    serviceUnitName));
        }
        
        /*          START CONSUMING         */
        try {
            if (hasEndpointLifeCycle()) {
                ServiceUnit srvcUnit = getServiceUnits().lookup(serviceUnitName);
                getManagerContext().getEndpointManager()
                        .getLifeCycle().startConsuming(srvcUnit);
            }
        }
        catch (Exception e) {
            String msg = I18n.loc("COMPTK-6021: Service unit \"{0}\" start failed: {1}", 
                                  serviceUnitName, e.getMessage());
            log().log(Level.WARNING, msg, e);
            throw new DeploymentException(msg, e);
        }

        if (log().isLoggable(Level.FINE)) {
            log().fine("COMPTK-3008: Start successful for service unit: "+ serviceUnitName);
        }
    }

    /** @see javax.jbi.component.ServiceUnitManager#stop(java.lang.String) */
    public void stop(String serviceUnitName) throws DeploymentException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("COMPTK-3009: Stopping SU-"+ serviceUnitName);
        }

        if (!isDeployed(serviceUnitName)) {
            throw new DeploymentException(I18n.loc(
                    "COMPTK-6022: stop failed for undeployed service unit \"{0}\"", 
            		serviceUnitName));
        }
        
        /*          STOP CONSUMING         */
        try {
            if (hasEndpointLifeCycle()) {
                ServiceUnit srvcUnit = getServiceUnits().lookup(serviceUnitName);
                getManagerContext().getEndpointManager()
                        .getLifeCycle().stopConsuming(srvcUnit);
            }
        }
        catch (Exception e) {
            String msg = I18n.loc("COMPTK-6023: Service unit \"{0}\" stop failed: {1}", 
                                  serviceUnitName, e.getMessage());
            log().log(Level.WARNING, msg, e);
            throw new DeploymentException(msg, e);
        }

        if (log().isLoggable(Level.FINE)) {
            log().fine("COMPTK-3010: Stop successful for service unit: "+ serviceUnitName);
        }
    }

    /**
     * This implementation logs undeployment at <code>Level.INFO</code> and 
     * returns a <code>SUCCESS</code> task message string.
     *  
     * @see javax.jbi.component.ServiceUnitManager#undeploy(java.lang.String, java.lang.String) 
     */
    public String undeploy(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
    	try {
	        if (log().isLoggable(Level.FINE)) {
	            log().fine("COMPTK-3011: Undeploying service unit: "+ serviceUnitName
	            				 +" at root path: "+ serviceUnitRootPath);
	        }
	        
	        uninstallServiceUnit(serviceUnitName);
	        
	        if (log().isLoggable(Level.FINE)) {
	            log().fine("COMPTK-3012: Undeploy successful for service unit: "+ serviceUnitName);
	        }

	        return createSuccessMessage(UNDEPLOY_TASK);
    	}
    	catch (Exception e) {
    		String msg = I18n.loc("COMPTK-6024: Failed to undeploy service unit \"{0}\": {1}", 
    							  serviceUnitName, e.getMessage());
    		log().log(Level.WARNING, msg, e);
    		return createExceptionMessage(UNDEPLOY_TASK, e, "COMPTK_UNDEPLOY_1", msg);
    	}
    }

    /* **********************   ACCESSORS   ********************************* */

    protected ManagerContext getManagerContext() {
        return mManagerCtx;
    }
    protected Logger log() {
        return mLogger;
    }
    protected EntryRegistry<String, ServiceUnit> getServiceUnits() {
        return mServiceUnits;
    }

    /* **********************   UTILITIES   ********************************* */
    
    protected boolean hasEndpointLifeCycle() {
        return (getManagerContext() != null &&
                getManagerContext().getEndpointManager() != null &&
                getManagerContext().getEndpointManager().getLifeCycle() != null);
    }
    
    /**
     * Builds a standard <code>SUCCESS</code> task message per 
     * the JBI specification.
     * 
     * @param task The task name.
     * @return An XML string representing a task message.
     */
    protected String createSuccessMessage(String task) {
        TaskResultDetails details = 
                new TaskResultDetails(task,     // task id
                                      true,     // succeeded
                                      MessageType.INFO,  
                                      null,     // no task-status-msg elements
                                      null);    // no exception-info elements
        TaskResult taskResult = new TaskResult(
                getManagerContext().getComponentContext().getComponentName(), details);
        return TaskXmlWriter.toXml(taskResult); 
    }

    /**
     * Builds a standard <code>FAILED</code> task message containing 
     * one <code>msg-loc-info</code> element per the JBI specification.
     * 
     * @param task The task name.
     * @param locToken The message key for looking up localized text for the message.
     * @param locMessage The default message, using {@link java.text.MessageFormat} patterns.
     * @param locParams An array of parameters, may be <code>null</code>.
     * @return An XML string representing a task message.
     */
    protected String createExceptionMessage(String task,
                                            Throwable exception,
                                            String locToken,
                                            String locMessage,
                                            Object... locParams) {
        MsgLocInfo locInfo = new MsgLocInfo(locToken, locMessage, locParams);
        ExceptionInfo excInfo = new ExceptionInfo(1, locInfo, exception);
        TaskResultDetails details = 
                new TaskResultDetails(task,     // task id
                                      false,    // failed
                                      MessageType.ERROR,  
                                      null,     // msg-loc-info used for exception-info
                                      new ExceptionInfo[] { excInfo });
        TaskResult taskResult = new TaskResult(
                getManagerContext().getComponentContext().getComponentName(), details);
        return TaskXmlWriter.toXml(taskResult);
    }

    /**
     * Builds a standard <code>FAILED</code> task message containing 
     * one <code>msg-loc-info</code> element per the JBI specification.
     * 
     * @param task The task name.
     * @param locToken The message key for looking up localized text for the message.
     * @param locMessage The default message, using {@link java.text.MessageFormat} patterns.
     * @param locParams An array of parameters, may be <code>null</code>.
     * @return An XML string representing a task message.
     */
    protected String createFailedMessage(String task,
                                         String locToken,
                                         String locMessage,
                                         Object... locParams) {
        MsgLocInfo locInfo = new MsgLocInfo(locToken, locMessage, locParams);
        TaskResultDetails details = 
                new TaskResultDetails(task,     // task id
                                      false,    // failed
                                      MessageType.WARNING,  
                                      new MsgLocInfo[] { locInfo },
                                      null);    // no exception-info elements
        TaskResult taskResult = new TaskResult(
                getManagerContext().getComponentContext().getComponentName(), details);
        return TaskXmlWriter.toXml(taskResult);
    }
    
    protected ServiceUnit createServiceUnit(String name, String rootPath) throws DeploymentException {
        return ServicesDescriptor.parse(name, rootPath);
    }
    
    /**
     * Returns <code>true</code> if the specified service unit is deployed.
     * @param serviceUnitName The name of the service unit.
     * @return <code>true</code> if the specified service unit is deployed.
     */
    protected boolean isDeployed(String serviceUnitName) {
        return getServiceUnits().containsKey(serviceUnitName);
    }
}
