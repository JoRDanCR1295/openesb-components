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

package com.sun.jbi.component.lifecycle.impl;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.descriptor.ServicesDescriptor;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.component.endpoint.Endpoint;
import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.component.mgmt.task.ExceptionInfo;
import com.sun.jbi.component.mgmt.task.MsgLocInfo;
import com.sun.jbi.component.mgmt.task.TaskResult;
import com.sun.jbi.component.mgmt.task.TaskResultDetails;
import com.sun.jbi.component.mgmt.task.TaskXmlWriter;
import com.sun.jbi.component.mgmt.task.TaskResultDetails.MessageType;
import com.sun.jbi.crl.mep.ManagerContext;
import com.sun.jbi.crl.util.EntryRegistry;
import com.sun.jbi.crl.util.I18n;
import com.sun.jbi.crl.util.LogUtil;

/**
 * Utility base class for {@link ServiceUnitManager} implementations, which
 * maintains a set of deployed {@link ServiceUnit}s.
 * 
 * @author Kevan Simpson
 */
public abstract class AbstractServiceUnitManager implements ServiceUnitManager {
    public static final String DEPLOY_TASK 		= "deploy";  // NOI18N
    public static final String UNDEPLOY_TASK 	= "undeploy";  // NOI18N
    public static final String INIT_TASK 		= "init";  // NOI18N
    public static final String SHUTDOWN_TASK 	= "shutDown";  // NOI18N
    
    /** Maps service units by their names. */
    private EntryRegistry<String, ServiceUnit> mServiceUnits = null;
    /** Maps endpoints by their service and interface qualified names. */
    private EndpointManager mEndpoints = null;
    /** The component's context. */
    private ManagerContext mComponentCtx = null;	// 
    /** The logger. */
    private Logger mLogger;
    
    protected AbstractServiceUnitManager(ManagerContext mgrCtx,
                                      	 EndpointManager emgr) {
        mComponentCtx = mgrCtx;
        mServiceUnits = new EntryRegistry<String, ServiceUnit>();
        mEndpoints = emgr;
        mLogger = LogUtil.getLogger(mgrCtx, this.getClass().getName());
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
	            log().fine("CRL-3007: Deploying SU \""+ serviceUnitName +
	            				 "\" at root path: "+ serviceUnitRootPath);
	        }
	        if (isDeployed(serviceUnitName)) {
	            // TODO should this return a FAILED task message instead?
	            throw new DeploymentException(
	            		I18n.loc("CRL-6011: Service unit already deployed: {0}", serviceUnitName));
	        }
	
	        getServiceUnits().register(serviceUnitName, 
	        		createServiceUnit(serviceUnitName, serviceUnitRootPath));
	
	        return createSuccessMessage(DEPLOY_TASK);
    	}
    	catch (Exception e) {
    		String msg = I18n.loc("CRL-6012: Failed to deploy SU \"{0}\": {1}", 
    							serviceUnitName, e.getMessage());
    		log().log(Level.WARNING, msg, e);
    		return createExceptionMessage(DEPLOY_TASK, e, "CRL_DEPLOY_1", msg);
    	}
    }

    /** @see javax.jbi.component.ServiceUnitManager#init(java.lang.String, java.lang.String) */
    public void init(String serviceUnitName, String serviceUnitRootPath) throws DeploymentException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("CRL-3008: Initializing SU \""+ serviceUnitName +
            				 "\" at root path: "+ serviceUnitRootPath);
        }

        ServiceUnit srvcUnit = null;
        if (!isDeployed(serviceUnitName)) {
            /*
             * JBI javadoc says to throw exception. Does not account for AS shutdown,
             * since SUs are stored in memory and lost at that time.
             * For now, adding service unit instance to registry.
             */
            srvcUnit = createServiceUnit(serviceUnitName, serviceUnitRootPath);
            getServiceUnits().register(serviceUnitName, srvcUnit);
//            throw new DeploymentException("init failed for undeployed service unit: "+ serviceUnitName);
        }

        /*          START PROVISIONING         */
        if (srvcUnit == null) srvcUnit = getServiceUnits().lookup(serviceUnitName);
        // parse jbi descriptor
        EndpointInfo[] endpts = srvcUnit.getServices().getEndpoints();
        // initialize service unit
        getEndpointManager().addServiceUnit(srvcUnit);
        Map<EndpointInfo, ServiceEndpoint> activated = 
                new HashMap<EndpointInfo, ServiceEndpoint>();
        Map<EndpointInfo, Endpoint> created = 
                new HashMap<EndpointInfo, Endpoint>();
        try {
        	// QoS: first look up service qualities for SU, not in for loop
        	DeploymentLookup lookup = new DeploymentLookup(getComponentContext());
        	Map<EndpointInfo, List<ServiceQuality>> qos =
        			lookup.lookupServiceQualities(serviceUnitRootPath);
        	
            for (EndpointInfo info : endpts) {
            	// has other SU already created this endpoint
            	Endpoint ept = getEndpointManager().lookupEndpoint(
            			info.getServiceName(), info.getEndpointName(), info.isProvides());
            	if (ept != null) {
            		throw new DeploymentException(
            				I18n.loc("CRL-6013: Duplicate endpoint ({0}) in component: {1}", 
            					   String.valueOf(info),
            					   getComponentContext().getComponentName()));
            	}
            	
                // create endpoint
                ept = getEndpointManager().getEndpointFactory()
                        .createEndpoint(info, serviceUnitName, serviceUnitRootPath);
                
                if (ept == null) {
                    log().info(
                    		I18n.loc("CRL-5001: EndpointFactory created NULL endpoint (info={0}) for service unit: {1}", 
                            	   String.valueOf(info), serviceUnitName));
                    continue;
                }
                else if (log().isLoggable(Level.FINER)) {
                    log().finer("CRL-2004: Endpoint created: "+
                    				  String.valueOf(ept) +" for SU \""+
                    				  serviceUnitName +"\"");
                }
                
                // start provisioning
                if (info.isProvides()) {
                    // activate with component context
                    ServiceEndpoint srvcEPT = 
                            getComponentContext().activateEndpoint(
                                    info.getServiceName(), info.getEndpointName());
                    activated.put(info, srvcEPT);
                    ept.start();
                    if (log().isLoggable(Level.FINER)) {
                        log().finer("CRL-2005: Endpoint activated: "+ 
                        		String.valueOf(srvcEPT));
                    }
                }
                else {
                	// TODO log the QoS added to MessagingChannel
                	List<ServiceQuality> list = qos.get(info);
                	if (list != null && !list.isEmpty()) {
                		getComponentContext().getMessagingChannel()
                				.addServiceQualityToEndpoint(info, (ServiceQuality[]) 
                						list.toArray(new ServiceQuality[list.size()]));
                	}
                }
                
                // check for duplicate endpoint in this SU
                if (created.get(info) != null) {
                	throw new DeploymentException(
                			I18n.loc("CRL-6014: Duplicate endpoint ({0}) in service unit: {1}", 
                				   String.valueOf(info), serviceUnitName));
                }
                created.put(info, ept);
            }
        }
        catch (Exception e) {
            String msg = I18n.loc("CRL-6015: Service unit \"{0}\" init failed: {1}", 
            					serviceUnitName, e.getMessage());
            log().log(Level.WARNING, msg, e);

            // rollback started endpoints - all or nothing
            for (EndpointInfo info : activated.keySet()) {
                try {
                    if (log().isLoggable(Level.FINER)) {
                        log().finer(
                        		"CRL-2006: Deactivating endpoint during rollback: " 
                        		+ String.valueOf(info));
                    }
                    ServiceEndpoint srvcEPT = activated.get(info);
                    getComponentContext().deactivateEndpoint(srvcEPT);
                }
                catch (Exception e2) {
                    log().warning(
                    		I18n.loc("CRL-6016: Failed to deactivate endpoint: {0}", 
                    			     e2.getMessage()));
                }
            }

            throw new DeploymentException(createFailedMessage(INIT_TASK, "CRL_INIT_1", msg), e);
        }
        
        // register created Endpoints with EndpointManager
        for (Entry<EndpointInfo, Endpoint> entry : created.entrySet()) {
            // TODO do we "start" or "setActive" the endpoint here? after register?
            getEndpointManager().registerEndpoint(
                    entry.getKey().getServiceName(), 
                    entry.getKey().getEndpointName(), 
                    entry.getValue());
        }

        if (log().isLoggable(Level.FINE)) {
            log().fine("CRL-3009: Initialized SU \""+ serviceUnitName +
            				 "\" successfully!");
        }
    }

    /** @see javax.jbi.component.ServiceUnitManager#shutDown(java.lang.String) */
    public void shutDown(String serviceUnitName) throws DeploymentException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("CRL-3010: Shutting down SU \""+ serviceUnitName +"\"");
        }

        if (!isDeployed(serviceUnitName)) {
            throw new DeploymentException(
            		I18n.loc("CRL-5002: shutDown failed for undeployed service unit \"{0}\"", 
            			   serviceUnitName));
        }

        /*          STOP PROVISIONING         */
        ServiceUnit srvcUnit = getServiceUnits().lookup(serviceUnitName);
        EndpointInfo[] endpts = srvcUnit.getServices().getProvides();
        
        // QoS: first look up service qualities for SU, not in for loop
        DeploymentLookup lookup = new DeploymentLookup(getComponentContext());
        Map<EndpointInfo, List<ServiceQuality>> qos =
                lookup.lookupServiceQualities(srvcUnit.getRootPath());

        DeploymentException error = null;
        for (EndpointInfo info : endpts) {
            try {
                Endpoint ept = getEndpointManager()
                        .removeEndpoint(info.getServiceName(), 
                                        info.getEndpointName(),
                                        info.isProvides());
                if (ept.getInfo().isProvides()) {
                	ept.stop();
                }
                ServiceEndpoint srvcEPT = 
                        getComponentContext().getEndpoint(info.getServiceName(), 
                                                          info.getEndpointName());
                getComponentContext().deactivateEndpoint(srvcEPT);
                if (log().isLoggable(Level.FINER)) {
                    log().finer("CRL-2007: Endpoint deactivated: "+ 
                    			   	  String.valueOf(srvcEPT));
                }
                
                // TODO log the QoS added to MessagingChannel
                List<ServiceQuality> list = qos.get(info);
                if (list != null && !list.isEmpty()) {
                    getComponentContext().getMessagingChannel()
                            .removeServiceQualityFromEndpoint(info, (ServiceQuality[]) 
                                    list.toArray(new ServiceQuality[list.size()]));
                }

            }
            catch (Exception e) {
                String msg = I18n.loc("CRL-6017: Endpoint \"{0}\" deactivation failed: {1}", 
                					(info == null) ? "NULL" : info.getServiceName(), 
                					e.getMessage());
                error = new DeploymentException(
                		createFailedMessage(SHUTDOWN_TASK, "CRL_SHUTDOWN_1", msg), e);
                log().log(Level.WARNING, msg, e);
            }
        }
        
        // remove service unit
        getEndpointManager().removeServiceUnit(srvcUnit);
        
        if (error != null) {
        	Throwable t = error.getCause();
        	log().log(Level.WARNING,
        					I18n.loc("CRL-6018: Service Unit \"{0}\" failed to shutDown: {1}", 
        						     serviceUnitName, (t == null) ? "???" : t.getMessage()), 
        					error);
        	throw error;
        }
        
        if (log().isLoggable(Level.FINE)) {
            log().fine("CRL-3011: Shut down successful for SU-"+ serviceUnitName);
        }
    }

    /** @see javax.jbi.component.ServiceUnitManager#start(java.lang.String) */
    public void start(String serviceUnitName) throws DeploymentException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("CRL-3012: Starting SU-"+ serviceUnitName);
        }

        if (!isDeployed(serviceUnitName)) {
            throw new DeploymentException(
            		I18n.loc("CRL-5003: start failed for undeployed service unit \"{0}\"", 
            			   serviceUnitName));
        }
        
        /*          START CONSUMING         */
        try {
            ServiceUnit su = getServiceUnits().lookup(serviceUnitName);
            EndpointInfo[] endpts = su.getServices().getConsumes();
            for (EndpointInfo info : endpts) {
                Endpoint ept = 
                        getEndpointManager().lookupEndpoint(
                                info.getServiceName(), 
                                info.getEndpointName(),
                                false);
                ept.start();
            }
        }
        catch (JBIException ex) {
            String msg = I18n.loc("CRL-6019: Service unit \"{0}\" start failed: {1}", 
            					serviceUnitName, ex.getMessage());
            log().log(Level.WARNING, msg, ex);
            throw new DeploymentException(msg, ex);
        }

        if (log().isLoggable(Level.FINE)) {
            log().fine("CRL-3013: Start successful for SU-"+ serviceUnitName);
        }
    }

    /** @see javax.jbi.component.ServiceUnitManager#stop(java.lang.String) */
    public void stop(String serviceUnitName) throws DeploymentException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("CRL-3014: Stopping SU-"+ serviceUnitName);
        }

        if (!isDeployed(serviceUnitName)) {
            throw new DeploymentException(
            		I18n.loc("CRL-5004: stop failed for undeployed service unit \"{0}\"", 
            			   serviceUnitName));
        }
        
        /*          STOP CONSUMING         */
        try {
            ServiceUnit su = getServiceUnits().lookup(serviceUnitName);
            EndpointInfo[] endpts = su.getServices().getConsumes();
            for (EndpointInfo info : endpts) {
                Endpoint ept = 
                        getEndpointManager().lookupEndpoint(
                                info.getServiceName(),
                                info.getEndpointName(),
                                false);
                ept.stop();
            }
        }
        catch (JBIException ex) {
            String msg = I18n.loc("CRL-6020: Service unit \"{0}\" stop failed: {1}", 
            					serviceUnitName, ex.getMessage());
            log().log(Level.WARNING, msg, ex);
            throw new DeploymentException(msg, ex);
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
	            log().fine("CRL-3016: Undeploying SU-"+ serviceUnitName
	            				 +" at root path: "+ serviceUnitRootPath);
	        }
	        
	        ServiceUnit su = getServiceUnits().remove(serviceUnitName);
	        if (su != null) {
	        	// remove consuming endpoints from manager, provisioned already removed during shutDown()
	        	EndpointInfo[] consumes = su.getServices().getConsumes();
	        	for (EndpointInfo info : consumes) {
	        		getEndpointManager().removeEndpoint(info.getServiceName(), info.getEndpointName(), false);
	        	}
	        }

	        if (log().isLoggable(Level.FINE)) {
	            log().fine("CRL-3017: Undeploy successful for SU-"+ serviceUnitName);
	        }

	        return createSuccessMessage(UNDEPLOY_TASK);
    	}
    	catch (Exception e) {
    		String msg = I18n.loc("CRL-6021: Failed to undeploy SU \"{0}\": {1}", 
    							serviceUnitName, e.getMessage());
    		log().log(Level.WARNING, msg, e);
    		return createExceptionMessage(UNDEPLOY_TASK, e, "CRL_UNDEPLOY_1", msg);
    	}
    }

    /* **********************   ACCESSORS   ********************************* */

    protected ManagerContext getComponentContext() {
        return mComponentCtx;
    }
    protected EndpointManager getEndpointManager() {
        return mEndpoints;
    }
    protected Logger log() {
        return mLogger;
    }
    protected EntryRegistry<String, ServiceUnit> getServiceUnits() {
        return mServiceUnits;
    }

//    protected StatusProviderHelper getStatusProvider() {
//        return mStatusProviderHelper;
//    }
//    protected void setStatusProvider(StatusProviderHelper prov) {
//        mStatusProviderHelper = prov;
//    }
    
    /* **********************   UTILITIES   ********************************* */
    
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
        TaskResult taskResult = 
                new TaskResult(getComponentContext().getComponentName(), details);
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
        TaskResult taskResult = 
                new TaskResult(getComponentContext().getComponentName(), details);
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
        TaskResult taskResult = 
                new TaskResult(getComponentContext().getComponentName(), details);
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
