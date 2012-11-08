/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * Copyright 2004-2006 Sun Microsystems, Inc. All Rights Reserved.
 */

/*
 * DefaultServiceUnitManager.java
 *
 */

package it.imolinfo.jbi4ejb.jbi.component.runtime;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;
import it.imolinfo.jbi4ejb.jbi.Messages;

import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.MessagingException;

/**
 * Default Service Unit Manager implementation. Component's supporting the
 * deployment should extend this class to support the service unit deployment.
 *
 * @see javax.jbi.ServiceUnitManager
 *
 * @author Sun Microsystems, Inc.
 */
public class DefaultServiceUnitManager implements ServiceUnitManager {
	/** The logger. */
    private static final Logger LOG = LoggerFactory.getLogger( DefaultServiceUnitManager.class);    
    private static final Messages MESSAGES = Messages.getMessages( DefaultServiceUnitManager.class);   
    /**
     * Component runtime as context
     */
    private ComponentRuntime mContext;
    /** private constructor */
    private DefaultServiceUnitManager() {}
    /**
     * constructor that takes the compoent runtime
     */
    public DefaultServiceUnitManager(ComponentRuntime ctx) {
        this.mContext = ctx;
    }
    ///////////////////////////////////////////////////////////////////////////
    // Service Unit Deployment methods implementation
    ///////////////////////////////////////////////////////////////////////////
    
    /**
     * Deploy a Service Unit to the component.
     * @see javax.jbi.component.ServiceUnitManager#deploy(String, String);
     */
    public String deploy(String suName, String suZipPath) throws DeploymentException {
        boolean isSuccess = false;
        String msg=MESSAGES.getString("EJB000216_Service_unit_deployment_not_supported_for_this_component");
        LOG.error(msg);
        throw new DeploymentException(msg);
        
    }
    /**
     * Undeploy a service unit from the component.
     * @see javax.jbi.component.ServiceUnitManager#undeploy(String, String);
     */
    public String undeploy(String suName, String suZipPath) throws DeploymentException {
        boolean isSuccess = false;
        String msg=MESSAGES.getString("EJB000216_Service_unit_deployment_not_supported_for_this_component");
        LOG.error(msg);
        throw new DeploymentException(msg);
    }
    
    ///////////////////////////////////////////////////////////////////////////
    // Service Unit Lifecycle Management methods implementation
    ///////////////////////////////////////////////////////////////////////////
    /**
     * Initialize the given deployed service unit.
     * @see javax.jbi.component.ServiceUnitManager#init(String, String);     */
    public void init(String serviceUnitName, String serviceUnitRootPath)
    throws javax.jbi.management.DeploymentException {
        boolean isSuccess = false;
        String msg=MESSAGES.getString("EJB000216_Service_unit_deployment_not_supported_for_this_component");
        LOG.error(msg);
        throw new DeploymentException(msg);
    }
    /**
     * Shut down the deployment.
     * @see javax.jbi.component.ServiceUnitManager#shutdown(String);
     */
    public void shutDown(String serviceUnitName)
    throws javax.jbi.management.DeploymentException {
        boolean isSuccess = false;
        String msg=MESSAGES.getString("EJB000216_Service_unit_deployment_not_supported_for_this_component");
        LOG.error(msg);
        throw new DeploymentException(msg);
    }
    /**
     * Start the deployed service unit.
     * @see javax.jbi.component.ServiceUnitManager#start(String);
     */
    public void start(String serviceUnitName)
    throws javax.jbi.management.DeploymentException {
        boolean isSuccess = false;
        String msg=MESSAGES.getString("EJB000216_Service_unit_deployment_not_supported_for_this_component");
        LOG.error(msg);
        throw new DeploymentException(msg);
    }
    /**
     * Stop the deployed service unit.
     * @see javax.jbi.component.ServiceUnitManager#stop(String);
     */
    public void stop(String serviceUnitName)
    throws javax.jbi.management.DeploymentException {
        boolean isSuccess = false;
        String msg=MESSAGES.getString("EJB000216_Service_unit_deployment_not_supported_for_this_component");
        LOG.error(msg);
        throw new DeploymentException(msg);
    }
    
    ///////////////////////////////////////////////////////////////////////////
    // Helper methods
    ///////////////////////////////////////////////////////////////////////////
    
    /**
     * helper method to create result message in management message xml.
     * @param message message string to return
     * @param isSuccess true to format a sucess result, false to format a failed result.
     * @return XML string.
     */
    protected String createComponentTaskResultXML(String message, boolean isSuccess) {
        
        String taskResult = isSuccess ? "SUCCESS" : "FAILED";
        String msgType = isSuccess ? "INFO" : "ERROR";
        String componentName = RuntimeHelper.getComponentName();
        
        String xmlResult =
            "<component-task-result xmlns=\"http://java.sun.com/xml/ns/jbi/management-message\" >" +
            "  <component-name>" + componentName + "</component-name>" +
            "  <component-task-result-details >" +
            "      <task-result-details>" +
            "          <task-id>deployment</task-id>" +
            "          <task-result>" + taskResult + "</task-result>" +
            "          <message-type>" + msgType + "</message-type>" +
            "          <task-status-msg>" +
            "             <msg-loc-info>" +
            "                <loc-token>MSG_ID_000</loc-token>" +
            "                <loc-message>" + message + "</loc-message>" +
            "              </msg-loc-info>" +
            "          </task-status-msg>" +
            "      </task-result-details>" +
            "  </component-task-result-details>" +
            "</component-task-result>";
        
        return xmlResult;
    }
    
}
