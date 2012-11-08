/*
 * ServiceUnitManagerImpl.java
 */
package com.sun.jbi.sample.component.common;

import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;

/**
 * Default Service Unit Manager implementation. Components supporting the
 * deployment can add the service unit deployment implementation support by
 * extending this class.
 *
 * Default runtime implementation ComponentImpl returns creates this class and 
 * return it as a supported ServiceUnitManager from Component.getServiceUnitManager
 *
 * @see javax.jbi.ServiceUnitManager
 * @see com.sun.jbi.sample.component.common.ComponentImpl#createServiceUnitManager
 * @author chikkala
 */
public class ServiceUnitManagerImpl implements ServiceUnitManager {
    private ComponentImpl mCompRuntime;
    /** default constructor */
    public ServiceUnitManagerImpl(ComponentImpl compRuntime) {
        this.mCompRuntime = compRuntime;
    }
    /**
     * Deploy a Service Unit to the component.
     * @see javax.jbi.component.ServiceUnitManager#deploy(String, String);
     */
    public String deploy(String suName, String suZipPath) throws DeploymentException {
        //TODO: process deployment and return the result using component result xml.
        // processDeployment(suName, suZipPath); ...
        String compName = this.mCompRuntime.getComponentContext().getComponentName();
        String resultXml = createComponentTaskResultXML(compName, "deployTask",
            false, "Service unit deployment not supported");
        return resultXml;
    }
    /**
     * Undeploy a service unit from the component.
     * @see javax.jbi.component.ServiceUnitManager#undeploy(String, String);
     */
    public String undeploy(String suName, String suZipPath) throws DeploymentException {
        //TODO: process deployment and return the result using component result xml.
        // processUndeployment(suName, suZipPath); ...
        String compName = this.mCompRuntime.getComponentContext().getComponentName();
        String resultXml = createComponentTaskResultXML(compName, "undeployTask",
            false, "Service unit undeployment not supported");
        return resultXml;
    }
    /**
     * Initialize the given deployed service unit.
     * @see javax.jbi.component.ServiceUnitManager#init(String, String);     */
    public void init(String serviceUnitName, String serviceUnitRootPath)
    throws DeploymentException {
        throw new DeploymentException("Service unit lifecycle not supported");
        //TODO: initialize the service providers and consumers from this service unit
        //System.out.println("ServiceUnitManger.init called for " + serviceUnitName);
    }
    /**
     * Shut down the deployment.
     * @see javax.jbi.component.ServiceUnitManager#shutdown(String);
     */
    public void shutDown(String serviceUnitName) throws DeploymentException {
        throw new DeploymentException("Service unit lifecycle not supported");
        //TODO: remove service providers and consumers belongs to this service unit
        //System.out.println("ServiceUnitManger.shutdown called for " + serviceUnitName);
    }
    /**
     * Start the deployed service unit.
     * @see javax.jbi.component.ServiceUnitManager#start(String);
     */
    public void start(String serviceUnitName) throws DeploymentException {
        throw new DeploymentException("Service unit lifecycle not supported");
        //TODO: activate service providers and consumers belongs to this service unit
        //System.out.println("ServiceUnitManger.start called for " + serviceUnitName);
    }
    /**
     * Stop the deployed service unit.
     * @see javax.jbi.component.ServiceUnitManager#stop(String);
     */
    public void stop(String serviceUnitName) throws DeploymentException {
        throw new DeploymentException("Service unit lifecycle not supported");
        //TODO: deactivate service providers and consumers belongs to this service unit.
        //System.out.println("ServiceUnitManger.stop called for " + serviceUnitName);
    }
    /**
     * helper method to create result message in management message xml.
     * @param componentName name of the component for this xml.
     * @param taskId task id
     * @param message message string to return
     * @param isSuccess true to format a sucess result, false to format a failed result.
     * @return XML string.
     */
    protected static String createComponentTaskResultXML(
        String componentName, String taskId, boolean isSuccess, String message) {
        
        String taskResult = isSuccess ? "SUCCESS" : "FAILED";
        String msgType = isSuccess ? "INFO" : "ERROR";
        
        String xmlResult =
            "<component-task-result xmlns=\"http://java.sun.com/xml/ns/jbi/management-message\" >" +
            "  <component-name>" + componentName + "</component-name>" +
            "  <component-task-result-details >" +
            "      <task-result-details>" +
            "          <task-id>" + taskId + "</task-id>" +
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
