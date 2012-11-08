/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.iep.management.api;

import com.sun.esb.management.common.ManagementRemoteException;

import java.io.File;
import java.util.Map;

/**
 *
 * @author rdwivedi
 */
public interface IEPManagementService {
    /**
     * 
     * @param suName service unit name
     * @param targetName  target name , null= no clustering
     * @return list names of deployed IEP process
     * @throws com.sun.esb.management.common.ManagementRemoteException
     */
    public String[] listDeployedIEP(String suName,String targetName)
            throws ManagementRemoteException ;
    /**
     * List the names of operators in an IEP process
     * @param iepName
     * @param targetName
     * @return
     * @throws com.sun.esb.management.common.ManagementRemoteException
     */
     
    public String[] listOperatorsForIEP(String iepName,String targetName)
            throws ManagementRemoteException ;

    /**
     * Get the list of managed attribute for a operator in a process.
     * @param planName
     * @param oprName
     * @param targetName
     * @return
     * @throws com.sun.esb.management.common.ManagementRemoteException
     */
    public Map getManagedAttributeForOperator(String planName,String oprName,
            String targetName) throws ManagementRemoteException ;

    /**
     * Set the value of allowed for a managed properties.
     * @param planName
     * @param oprName
     * @param propName
     * @param value
     * @param targetName
     * @throws com.sun.esb.management.common.ManagementRemoteException
     */
    public String setManagedAttributeForOperator(String planName, String oprName,
            String propName,Object value,String targetName) throws ManagementRemoteException ;

    /**
     * sets an IEP process for step by step processing .
     * @param planName
     * @param targetName
     * @throws com.sun.esb.management.common.ManagementRemoteException
     */
    public String setIEPForDebugging(String planName,String targetName) throws ManagementRemoteException ;
    /**
     * Unlink an IEP process from step by step process. It will make this process
     * avialbale for Round Robin shceduler and let the procecss run as usual in IEP engine.
     * @param planName
     */
    public String unsetIEPForDebugging(String planName,String targetName) throws ManagementRemoteException ;
    
    /**
     * Get the Debug Information in formatted string.
     * @param planName
     * @param oprName
     * @return
     * @throws java.lang.Exception
     */
    public String getCurrentDebugInfo(String planName, String oprName,String targetName) throws Exception;
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
    public String executeDebugCommand(String cmd, String planName, Object args,String targetName) throws Exception;

    /**
     *
     * @param planName
     * @param operatorName
     * @return
     * @throws java.lang.Exception
     */
    public String setEnableDataAccessObject(String planName, String operatorName, String tabName,String targetName) throws Exception;
/**
 * Disable data access for an operator.
 * @param planName
 * @param oprName
 * @param targetName
 * @return
 * @throws java.lang.Exception
 */
    public String disableDataAccess(String planName, String oprName,String targetName) throws Exception;
    /**
     * Set a property for the iep java object that externalize the data.
     * @param planName name of the iep process plan
     * @param operatorName name of the operator
     * @param propertyName property name
     * @param value value of the property must be simple serilaizable java object.
     * @throws java.lang.Exception
     */
    public String setIEPDataAccessProperty(String planName, String operatorName, String propertyName , String value,String targetName ) throws Exception;
    /**
     *
     * @param planName planName
     * @param operatorName Operator name
     * @param jndi Jndi url
     * @param tableName tableName to be created and used by save stream
     * @return result of the operation
     */
    public String enableSaveStream(String planName, String operatorName , String jndi, String tableName,String isGlobal,String targetName)  throws ManagementRemoteException ;
    /**
     *
     * @param planName plan Name
     * @param saveStreamName name of the save stream operator that gets added and returned
     * as a call to enableSaveStream
     * @return result of the operation.
     */
    public String removeSaveStream(String planName, String saveStreamName,String targetName)  throws ManagementRemoteException ;

    /**
     * Start the process of changes in operators. For example before changing 
     * the properties of operators user need to inform engine by calling this method.
     */
    public String startNewChangeSet(String targetName) throws ManagementRemoteException ;
    
    /**
     * Changes are complete from user end. This method informs IEP engine that changes 
     * are complete and engin can post the complete change set to be applied.
     */
    public String applyChangeSet(String targetName) throws ManagementRemoteException ;
    /**
     * Roll back the changes that has been made so far. Has no affect after applyChangeSet();
     */
    public String ignoreChangeSet(String targetName)throws ManagementRemoteException ;

    /**
     * Export a specific version of an iep plan to file system
     * @param planName
     * @param planVersion
     * @param targetName
     * @return status message
     * @throws ManagementRemoteException
     */
    public String exportPlan(String planName, String planVersion, String targetName) throws ManagementRemoteException;
    
    /**
     * Deletes all the version history for a given planName
     * @param planName
     * @param targetName
     * @return
     */
    public String purgePlanVersions(String planName, String targetName) throws ManagementRemoteException ;
    
    /**
     * Deletes all the version history of all the plans
     * @return
     */
    public String purgeAllPlanVersions(String targetName) throws ManagementRemoteException ;
}
