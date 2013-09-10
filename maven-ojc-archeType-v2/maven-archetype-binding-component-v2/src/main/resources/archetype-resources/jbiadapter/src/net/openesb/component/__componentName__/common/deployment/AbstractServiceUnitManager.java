#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * AbstractServiceUnitManager.java
 *
 */

package net.openesb.component.${componentName}.common.deployment;

import net.openesb.component.${componentName}.common.RuntimeHelper;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;

/**
 * This is an abstract class that implements {@link javax.jbi.component.ServiceUnitManager} to provide a
 * service unit deployment support for the component. The implementation supported by this service
 * unit manager and related classes in this package provides WSDL1.1 based service unit deployment
 * in the component.
 *
 * @see javax.jbi.component.ServiceUnitManager
 * @see ServiceUnit
 *
 * @author chikkala
 */
public abstract class AbstractServiceUnitManager implements ServiceUnitManager {
    
    /** Map of ServiceUnit Name to the ServiceUnit Object for all the service units
     * deployed in the component */
    private Map<String,ServiceUnit> mSUMap;
    
    /** Creates a new instance of AbstractSUManager */
    protected AbstractServiceUnitManager() {
        this.mSUMap = Collections.synchronizedMap(new HashMap<String,ServiceUnit>());
    }
    /** returns the component name
     * @return component name.
     */
    protected abstract String getComponentName();
    /**
     * @return Logger
     */
    protected abstract Logger getLogger();
    /**
     * returns the creation of the ServiceUnit implementation specific to the service unit deployment
     * for the component.
     * @param suName service unit name
     * @param suRootPath service unit root path
     * @param concrete implementation of the ServiceUnit class.
     */
    protected abstract ServiceUnit createServiceUnit(String suName, String suRootPath)
    throws DeploymentException;
    /**
     * returns the service unit object deployed by the specified name. or null if not deployed.
     * @param suName service unit name to look for
     * @param ServiceUnit object for the suName. or null if not present.
     */
    private ServiceUnit getServiceUnit(String suName) {
        return this.mSUMap.get(suName);
    }
    /**
     * lookup for the deployed service unit. If not there, throws a deployment exception.
     * @param suName service unit name
     * @return ServiceUnit object
     * @throws DeploymentException if the service unit is not present.
     */
    private ServiceUnit findServiceUnit(String suName) throws DeploymentException {
        ServiceUnit serviceUnit = getServiceUnit(suName);
        if ( serviceUnit == null ) {
            throw new DeploymentException("Service unit " + suName + " does not exist");
        }
        return serviceUnit;
    }
    /**
     * add the service unit object to the list of deployed service units. if there is already a
     * service unit present with the name, throws a deployment exception.
     * @param su service unit to add
     * @return ServiceUnit object that is added.
     * @throws DeploymentException if the service unit already present.
     */
    private ServiceUnit addServiceUnit(ServiceUnit su) throws DeploymentException {
        String suName = su.getName();
        ServiceUnit oldSU = getServiceUnit(suName);
        if ( oldSU != null ) {
            throw new DeploymentException("Service unit " + suName + "already exists");
        }
        this.mSUMap.put(suName, su);
        return su;
    }
    /**
     * removes service unit object from the list of deployed service units. if there is no
     * service unit present with the name, throws a deployment exception.
     * @param su service unit to add
     * @return ServiceUnit object that is being removed.
     * @throws DeploymentException if the service unit already present.
     */
    private ServiceUnit removeServiceUnit(ServiceUnit su) throws DeploymentException {
        String suName = su.getName();
        ServiceUnit oldSU = getServiceUnit(suName);
        if ( oldSU == null ) {
            throw new DeploymentException("Service unit " + suName + " does not exist");
        }
        return this.mSUMap.remove(suName);
    }
    /**
     * creates the concrete service unit implementation and calls the load method on it to initialize
     * the created service unit.
     * @param suName service unit name to create
     * @param suRootPath service unit root path.
     * @return ServiceUnit that is created and loaded.
     */
    private ServiceUnit loadServiceUnit(String suName, String suRootPath) throws DeploymentException {
        ServiceUnit serviceUnit = createServiceUnit(suName, suRootPath);
        serviceUnit.doLoad();
        return serviceUnit;
    }
    /**
     * deploys a service unit. it creates and loads the service unit object for the suName and then call
     * doDeploy on the service unit and adds it to the deployed service unit list
     * @return result as jbi management xml
     * @throws DeploymentException if there is an error deploying.
     */
    private synchronized String deployServiceUnit(String suName, String suRootPath) throws DeploymentException {
        String result = suName;
        ServiceUnit oldSU = getServiceUnit(suName);
        if ( oldSU != null ) {
            throw new DeploymentException("Service unit " + suName + "already exists");
        }
        ServiceUnit serviceUnit = loadServiceUnit(suName, suRootPath);
        serviceUnit.doDeploy();
        addServiceUnit(serviceUnit);
        return result;
    }
    /**
     * Deploy a Service Unit to the component.
     * @see javax.jbi.component.ServiceUnitManager${symbol_pound}deploy(String, String);
     */
    public final String deploy(String suName, String suRootPath) throws DeploymentException {
        this.getLogger().fine("Deploying service unit " + suName + " with suRootPath " + suRootPath);
        String compName = this.getComponentName();
        boolean isSuccess = true;
        String mainMsg = "Successfully deployed service unit " + suName;
        Exception errEx = null;
        try {
            deployServiceUnit(suName, suRootPath);
        } catch (Exception ex) {
            isSuccess = false;
            errEx = ex;
            mainMsg = "Failed to deploy service unit " + suName ;
            this.getLogger().log(Level.FINE, mainMsg, ex);
        }
        return createComponentTaskResultXML(compName, "deployTask", isSuccess, mainMsg, errEx);
    }
    
    /**
     * undeploys the service unit. it looks up the existing deployed service unit and call doUndeploy
     * on it and then removes from the deployed service unit list.
     */
    private synchronized String undeployServiceUnit(String suName, String suRootPath) throws DeploymentException {
        String result = suName;
        ServiceUnit serviceUnit = findServiceUnit(suName);
        serviceUnit.doUndeploy();
        removeServiceUnit(serviceUnit);
        return result;
    }
    /**
     * Undeploy a service unit from the component.
     * @see javax.jbi.component.ServiceUnitManager${symbol_pound}undeploy(String, String);
     */
    public final String undeploy(String suName, String suRootPath) throws DeploymentException {
        this.getLogger().fine("Undeploying service unit " + suName + " with suRootPath " + suRootPath);        
        String compName = this.getComponentName();
        boolean isSuccess = true;
        String mainMsg = "Successfully undeployed service unit " + suName;
        Exception errEx = null;
        try {
            undeployServiceUnit(suName, suRootPath);
        } catch (Exception ex) {
            isSuccess = false;
            errEx = ex;
            mainMsg = "Failed to undeploy service unit " + suName ;
            this.getLogger().log(Level.FINE, mainMsg, errEx);
        }        
        return createComponentTaskResultXML(compName, "undeployTask", isSuccess, mainMsg, errEx);
    }
    /**
     * Initialize the given deployed service unit.
     * @see javax.jbi.component.ServiceUnitManager${symbol_pound}init(String, String);     */
    public final void init(String suName, String suRootPath)
    throws DeploymentException {
        this.getLogger().fine("Initializing service unit " + suName + " with suRootPath " + suRootPath);        
        String result = suName;
        ServiceUnit serviceUnit = getServiceUnit(suName);
        if ( serviceUnit == null ) { // if the service unit not exists, create and add 
            serviceUnit = loadServiceUnit(suName, suRootPath); // create and load service unit
            addServiceUnit(serviceUnit);  // add service unit to existing service units
        }
        serviceUnit.doInit();  // Do Service unit initialization tasks
        this.getLogger().fine("Service unit initialized:" + suName);
    }
    /**
     * Shut down the deployment.
     * @see javax.jbi.component.ServiceUnitManager${symbol_pound}shutdown(String);
     */
    public final void shutDown(String suName) throws DeploymentException {
        ServiceUnit serviceUnit = findServiceUnit(suName); // find service unit
        serviceUnit.doShutdown(); // Do Service unit shutdown tasks
        this.getLogger().fine("Service unit shut down:" + suName);
    }
    /**
     * Start the deployed service unit.
     * @see javax.jbi.component.ServiceUnitManager${symbol_pound}start(String);
     */
    public final void start(String suName) throws DeploymentException {
        ServiceUnit serviceUnit = findServiceUnit(suName); // find service unit
        serviceUnit.doStart();  // Do service unit start tasks.
        this.getLogger().fine("Service unit started:" + suName );
    }
    /**
     * Stop the deployed service unit.
     * @see javax.jbi.component.ServiceUnitManager${symbol_pound}stop(String);
     */
    public final void stop(String suName) throws DeploymentException {
        ServiceUnit serviceUnit = findServiceUnit(suName); // find service unit
        serviceUnit.doStop();  // do service unit stop tasks
        this.getLogger().fine("Service unit stopped: " + suName + " stopped.");
    }
    
    /**
     * helper method to create result message as jbi management message xml.
     * @param componentName name of the component for this xml.
     * @param taskId task id
     * @param isSuccess true to format a success result, false to format a failed result.
     * @param mainMsg main result message
     * @param errEx Exception, null if there is no exception in failure message.
     * @return XML string.
     */
    protected static String createComponentTaskResultXML(
        String componentName, String taskId, boolean isSuccess, String mainMsg, Exception errEx ) {
        
        String exMsgXml = getComponentTaskResultExceptionXML(errEx);
        String mainMsgXmlEsc = RuntimeHelper.replaceXmlEscapeCharsToEntityRefereces(mainMsg);
        String taskResult = isSuccess ? "SUCCESS" : "FAILED";
        String msgType = isSuccess ? "INFO" : "ERROR";
        
        String xmlResult =
            "<component-task-result xmlns=${symbol_escape}"http://java.sun.com/xml/ns/jbi/management-message${symbol_escape}" >" +
            "  <component-name>" + componentName + "</component-name>" +
            "  <component-task-result-details >" +
            "      <task-result-details>" +
            "          <task-id>" + taskId + "</task-id>" +
            "          <task-result>" + taskResult + "</task-result>" +
            "          <message-type>" + msgType + "</message-type>" +
            "          <task-status-msg>" +
            "             <msg-loc-info>" +
            "                <loc-token>SU_MGR_MSG_ID</loc-token>" +
            "                <loc-message>" + mainMsgXmlEsc + "</loc-message>" +
            "              </msg-loc-info>" +
            "          </task-status-msg>" +
            exMsgXml +
            "      </task-result-details>" +
            "  </component-task-result-details>" +
            "</component-task-result>";
        
        return xmlResult;
    }
    /**
     * converts the exception to the jbi management message xml chunk.
     */
    private static String getComponentTaskResultExceptionXML(Exception errEx) {
        StringBuffer exMsgBuff = new StringBuffer();
        if ( errEx == null ) {
            return exMsgBuff.toString(); // empty string.
        }
        
        List<Throwable> exList = new ArrayList<Throwable>();
        int exLevel = 0;
        for ( Throwable cause = errEx ; cause != null ; cause = cause.getCause() ) {
            String causeMsg = RuntimeHelper.replaceXmlEscapeCharsToEntityRefereces(
                cause.getMessage());
            StringWriter stBuff = new StringWriter();
            PrintWriter stOut = new PrintWriter(stBuff);
            StackTraceElement[] stList = cause.getStackTrace();
            for (StackTraceElement stEl : stList) {
                stOut.println(stEl.toString());
            }
            stOut.close();
            String causeStackTrace = RuntimeHelper.replaceXmlEscapeCharsToEntityRefereces(
                stBuff.getBuffer().toString());
            
            exMsgBuff.append("<exception-info>");
            exMsgBuff.append(" <nesting-level>" + exLevel + "</nesting-level>");
            exMsgBuff.append(" <msg-loc-info>" + "<loc-token>SU_MGR_EXP_ID</loc-token>");
            exMsgBuff.append("  <loc-message>" + causeMsg + "</loc-message>");
            exMsgBuff.append(" </msg-loc-info>");
            exMsgBuff.append(" <stack-trace>" + causeStackTrace + "</stack-trace>");
            exMsgBuff.append("</exception-info>");
            ++exLevel;
        }
        
        return exMsgBuff.toString();
    }
}
