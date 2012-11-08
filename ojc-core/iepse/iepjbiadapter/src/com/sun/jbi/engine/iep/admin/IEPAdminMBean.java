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
package com.sun.jbi.engine.iep.admin;

import java.util.Map;

/**
 *
 * @author rdwivedi
 */
public interface IEPAdminMBean {
    /**
     * Get a array of attributes of this service unit.
     * @param serviceUnitName name of the service unit.
     * @return Array of Process Attributes.
     */    
    String[] listProcessAttributes(String serviceUnitName);
    /**
     * List all the operators of the deployed process by name.
     * @param processDeployName process name(plan name).
     * @return Array of operators name.
     */
    public String[] listOperators(String processDeployName);
    /**
     * Get a map of managed attributes for an operator in a plan that is deployed
     * @param processDeployName
     * @param operatorName
     * @return Map of Attribute Name , Value of the attribute as an Object.
     */
    public Map<String, Object> getManagedAttributesForOperator(String processDeployName, String operatorName);
    /**
     * Set a value of a managed attribute of an operator .
     * @param planName
     * @param oprName
     * @param propName same key as that in getManagedAttributesForOperator;
     * @param value
     */
    public String setManageablePropertyValue(String planName, String oprName, 
            String propName, Object value) throws Exception ;
    
    /**
     * Start the process of changes in operators. For example before changing 
     * the properties of operators user need to inform engine by calling this method.
     */
    public String startNewChangeSet();
    
    /**
     * Changes are complete from user end. This method informs IEP engine that changes 
     * are complete and engin can post the complete change set to be applied.
     */
    public String applyChangeSet()  throws Exception ;
    /**
     * Roll back the changes that has been made so far. Has no affect after applyChangeSet();
     */
    public String ignoreChangeSet();
    /**
     * Set and IEP process for step by step processing.
     * @param planName
     */
    public String setIEPForDebugging(String planName);
    /**
     * Unlink an IEP process from step by step process. It will make this process
     * avialbale for Round Robin shceduler and let the procecss run as usual in IEP engine.
     * @param planName
     */
    public String unsetIEPForDebugging(String planName);
    /**
     * Execute next executable operator. setIEPForDebugging is required before this.
     * @param planName
     * @return a message from the execution usually the name of the operator executed.
     * @throws java.lang.Exception
     */
    public String executeNext(String planName) throws Exception;
    /**
     * Get the Debug Information in formatted string.
     * @param planName
     * @param oprName
     * @return
     * @throws java.lang.Exception
     */
    public String getCurrentDebugInfo(String planName, String oprName) throws Exception;
    /**
     * Execute a debug command in step by step processor. Allowed command are
     * next : execute next available operator.
     * completeCycle : completes one cycle of IEP process .

     * nextCompleteCycles noOfTime(int) : complete cycles of IEP process noOfTime times.

     * nextCompleteCyclesForTimePeriod milliseconds (long) : complete the cycles of IEP process for milliseconds time.

     * addPauseBefore operatorName : add a pause (breakpoint) before an IEP process.

     * nextTillPause : execute iep process till a pause is found or cycle is complete.

     * nextOperator : displays the name of the operator that is to be executed next.

     * removePauseBefore operatorName : removes the pause before the operator if there is one available.

     * info : shows the relevent information about the last executed operator.

     * info oprName : shows the debug information about the oprName operator.

     * @param cmd see above for avilable commands.
     * @param planName
     * @param args
     * @return String message as a result of command results.
     * @throws java.lang.Exception
     */
    public String executeDebugCommand(String cmd, String planName, Object args) 
            throws Exception;
    
    /**
     * 
     * @param planName
     * @param operatorName
     * @return
     * @throws java.lang.Exception
     */
    public String setEnableDataAccessObject(String planName, String operatorName
            , String tabName) throws Exception;
    /**
     * 
     * @param planName plan name
     * @param oprName the operator name
     * @return
     */
    public String disableDataAccess(String planName, String oprName);
    /**
     * Set a property for the iep java object that externalize the data.
     * @param planName name of the iep process plan
     * @param operatorName name of the operator
     * @param propertyName property name
     * @param value value of the property must be simple serilaizable java object.
     * @throws java.lang.Exception
     */

    public String setIEPDataAccessProperty(String planName, String operatorName,
            String propertyName , String value ) throws Exception;
    /**
     * 
     * @param planName planName
     * @param operatorName Operator name
     * @param jndi Jndi url
     * @param tableName tableName to be created and used by save stream
     * @return result of the operation
     */
    public String enableSaveStream(String planName, String operatorName , 
            String jndi, String tableName,String isGlobal);
    /**
     * 
     * @param planName plan Name
     * @param saveStreamName name of the save stream operator that gets added and returned 
     * as a call to enableSaveStream
     * @return result of the operation.
     */
    public String removeSaveStream(String planName, String saveStreamName);
    
    /**
     * Export a plan version. 
     * @param planName
     * @param planVersion planVersion can be null in that case latest version
     * content is returned
     * @return content of the plan version
     */
    public String exportPlan(String planName, String planVersion) throws Exception;
   
    /**
     * Delete version history for given plan
     * @param planName
     * @return
     */
    public String purgePlanVersions(String planName) throws Exception;
    
    /**
     * Delete version history for all the plans
     * @return
     */
    public String purgeAllPlanVersions() throws Exception;
}
