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
 * @(#)$Id: BPELSEManagementMBean.java,v 1.9 2010/02/04 02:52:02 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;


/**
 * The Management Mbean interface to perform managmenet tasks on BPELSE
 * @author Sun Microsystems
 *
 */
public interface BPELSEManagementMBean {

	/**
	 * Change BPEL  variable value, only the leaf node can be changed
	 * Only available when isMonitoringVariableEnabled returns true. If isMonitoringVariableEnabled return false, exception 
	 * will be thrown
	 * @param instanceId		The instance id of the instance on which the variable will be changed
	 * @param varId				The variable Id, can not be null
	 * @param partName		The part name of the variable, if null, the variable is not message type 	
	 * @param xpath			The xpath leading to the leaf node to be changed, if null, the part itself is 
	 * @param value			The new value to change
	 * @throws Exception
	 */
	Boolean changeVariableValue(String instanceId, Long varId, String partName, String xpath,
			String value) throws Exception;
	
	/**
	 * Changes the value of the event generation flag for each process. Method takes in a map of bpel process ids and 
	 * the corresponding events generation flag value for that process. This method can be called even if 
	 * monitoring is turned off on the bpel engine. However the values will not be persisted to the DB if monitoring 
	 * is not enabled. This value will be reset to the value set in the bpel file if the engine is restarted if 
	 * not persisted to the DB.
	 * 
	 * @param configValues Map of BPEL Process Ids to their respective events generation flag value.
	 * @throws Exception
	 */
	void updateProcessEventsGenerationFlag(Map<String, Boolean> configValues) throws Exception;
	
	/**
	 * Gets the value of the process level events generation flag for all processes deployed on the bpel engine. This
	 * method can be called even if monitoring is turned off on the bpel engine. The values returned are the
	 * current runtime configuration on the engine.
	 * 
	 * @return
	 * @throws Exception
	 */
	Map<String, Map<String, Boolean>> getAllProcessEventsGenerationFlag() throws Exception;


	/**
	 * Suspends the bpel instance
	 * @param instanceId
	 * @throws Exception If the instance is not found or any exception occurs and not successfully suspended, the exception will be thrown
	 */
	Boolean suspendInstance (String instanceId) throws Exception;

	/**
	 * Suspend all instances of a bpel process
	 * @param processName  The process name (QName)
	 * @return  The list of suspended instance ids
	 * @throws Exception
	 */
	List<String> suspendAllInstance (String processName) throws Exception;

	/**
	 * Resume the bpel instance
	 * @param instanceId
	 * @throws Exception If the instance is not found or any exception occurs and not successfully suspended, the exception will be thrown
	 */
	Boolean resumeInstance (String instanceId) throws Exception;

	/**
	 * Resume all instances of a bpel process
	 * @param processName  The process name (QName)
	 * @return  The list of suspended instance ids
	 * @throws Exception
	 */
	List<String> resumeAllInstance (String processName) throws Exception;     

	/**
	 * Terminate the bpel instance
	 * @param instanceId
	 * @throws Exception If the instance is not found or any exception occurs and not successfully suspended, the exception will be thrown
	 */
	Boolean terminateInstance (String instanceId) throws Exception;     

	/**
	 * Terminate all instances of a bpel process
	 * @param processName  The process name (QName)
	 * @return  The list of suspended instance ids
	 * @throws Exception
	 */
	List<String> terminateAllInstance (String processName) throws Exception;      
	/**
	 * Get the list of BPEL process QName name as String within an SU.
	 * @param suName The name of the SU.
	 * @return The list of BPELProcess File name Strings, empty list will be returned
	 * @throws Exception
	 */
	List<String> getBPELProcesses(String suName) throws Exception;
	/**
	 * Get the list of ActivityStatus of a BPEL instance <p>
	 * 
	 * Each Map<String, Object> returned in the List is of the following strucutre:
	 * <tt>key: activityId value: Activity Id</tt>
	 * <tt>key: activityXpath value: Activity Xpath</tt>
	 * <tt>key: status value: status</tt>
	 * <tt>key: startTime value: start time of the activity</tt>
	 * <tt>key: endTime value: end time of the activity </tt>
	 * <tt>key: lasted value: the elapse time of the activity </tt>
	 *  	  
	 * @param bpId
	 * @return List of ActivityStatus 
	 * @throws Exception
	 */
	List<Map<String, Object>> getBPELInstanceActivityStatus(String instanceId) throws Exception;

	/**
	 * Get BPEL instances info given optional BPEL process QName and/or an optional BPStatus or an optional instance id.
	 * If instanceid is present, the BPEL process QName and BPStatus are ignored. <p>
	 * 
	 * The maximum instances to return is 1000, user specifies a lower number for <code>maxRecords</code> to limit the number
	 * of the instances returned. <p>
	 * 
	 * The  first Map<String, Object>is a header that contains properties about the result:<p>
	 * <tt>key:total, value:total number of qualifying instances</tt>
	 * <tt>key:returned value:number of instance returned</tt>
	 * <tt>key:overflow, value:true or false,  true if no <code>maxRecords</code> is specified and
	 * 						the number of qualifying instances exceeds 1000</tt>
	 * 
	 * If the header indicates the result is overFlow, no instances are returned in the result list, user should specify a <code>maxRecords</code>
	 * (<=1000) and <code>sortColumn</code> and <code>order</code>
	 * 
	 * 
	 * The rest of  Map<String, Object> returned are of the following structure. <p>
	 * <tt>key: id value: instanceId</tt>
	 * <tt>key: bpelId value: process QName</tt>
	 * <tt>key: status value: instance status </tt>
	 * <tt>key: startTime value: the instance start time </tt>
	 * <tt>key: endTime  value: the instance end time </tt>
	 * <tt>key: lastUpdateTime value: the last updated time on the instance</tt>
	 * <tt>key: lasted value: the elapse time of instance in seconds </tt>
	 * 
	 * @param bpelQName  BPEL process QName as String
	 * @param status   Optional, if present, only the instances of the specific status are returned
	 * @param instanceId  Optional, if present only the specific instance is returned
	 * @param maxRecord 	Optional, must be <= 1000
	 * @param sortColumn	Optional, must be one of {@link #START_TIME}, {@link #END_TIME} and {@link #UPDATED_TIME}
	 * @param order  Optional, must be one of {@link #ASC} or {@link #DESC}
	 * @return  A list of BPEL instance info, see comments of the operation
	 * @throws Exception
	 */
	List<Map<String, Object>> getBPELInstances(String bpelQName, String status, String instanceId, 
			Integer maxRecords, String sortColumn, String order) throws Exception ;
	
	/**
	 * Search for BPEL instances info given BPEL process QName and a SearchString and/or an optional BPStatus.
	 * 
	 * The maximum instances to return is 1000, user specifies a lower number for <code>maxRecords</code> to limit the number
	 * of the instances returned. <p>
	 * 
	 * The  first Map<String, Object>is a header that contains properties about the result:<p>
	 * <tt>key:total, value:total number of qualifying instances</tt>
	 * <tt>key:returned value:number of instance returned</tt>
	 * <tt>key:overflow, value:true or false,  true if no <code>maxRecords</code> is specified and
	 * 						the number of qualifying instances exceeds 1000</tt>
	 * 
	 * If the header indicates the result is overFlow, no instances are returned in the result list, user should specify a <code>maxRecords</code>
	 * (<=1000) and <code>sortColumn</code> and <code>order</code>
	 * 
	 * 
	 * The rest of  Map<String, Object> returned are of the following structure. <p>
	 * <tt>key: id value: instanceId</tt>
	 * <tt>key: bpelId value: process QName</tt>
	 * <tt>key: status value: instance status </tt>
	 * <tt>key: startTime value: the instance start time </tt>
	 * <tt>key: endTime  value: the instance end time </tt>
	 * <tt>key: lastUpdateTime value: the last updated time on the instance</tt>
	 * <tt>key: lasted value: the elapse time of instance in seconds </tt>
	 * 
	 * @param bpelQName  BPEL process QName as String
	 * @param status   Optional, if present, only the instances of the specific status are returned
	 * @param searchString  The searchString
	 * @param maxRecord 	Optional, must be <= 1000
	 * @param sortColumn	Optional, must be one of {@link #START_TIME}, {@link #END_TIME} and {@link #UPDATED_TIME}
	 * @param order  Optional, must be one of {@link #ASC} or {@link #DESC}
	 * @return  A list of BPEL instance info, see comments of the operation
	 * @throws Exception
	 */
	List<Map<String, Object>> searchBPELInstances(String bpelQName, String status, String searchString, 
			Integer maxRecords, String sortColumn, String order) throws Exception ;
	
	

	/**
	 * Get the BPEL variable info for a bpel instance. <p>
	 * Only available when isMonitoringVariableEnabled returns true. If isMonitoringVariableEnabled return false, exception 
	 * will be thrown
	 * 
	 * Each Map<String, Object> returned in the List is of the following strucutre:
	 * <tt>key:varName value:variable Name</tt>
	 * <tt>key:varId value:variable string value</tt>
	 * <tt>key:xpath  value:the xpath of the scope where the variable is defined</tt>
	 * 
	 * @param instanceId The instance id of the instance
	 * @param varName   Optional, if specified, only returns the info for the variable of this name 
	 * @return
	 */ 	
	List<Map<String, Object>> listBPELVaraibles(String instanceId, String varName) throws Exception ;

	/**
	 * Get the fault detail of a faulted bpel instance;
	 * @param  bpid  the faulted bpel instance id
	 * 
	 */
	String getBPELInstanceFault(String bpid) throws Exception;



	/**
	 * Get the list of invoker bpel instance ids for a specific bpel instance <p>
	 * Only available when isMonitoringVariableEnabled returns true. If isMonitoringVariableEnabled return false, exception 
	 * will be thrown
	 * 
	 * Each Map<String, Object> returned in the List is of the following strucutre:
	 * <tt>key: id value: instanceId</tt>
	 * <tt>key: bpelId value: process QName</tt>
	 * <tt>key: status value: instance status </tt>
	 * <tt>key: startTime value: the instance start time </tt>
	 * <tt>key: endTime  value: the instance end time </tt>
	 * <tt>key: lastUpdateTime value: the last updated time on the instance</tt>
	 * <tt>key: lasted value: the elapse time of instance in seconds </tt>
	 *  	  
	 * @param bpid 		The invoked bpel instance id
	 * @param receiveActivityId	Optional receive activity id of the invoked bpel instance, if present, returns only one invoker bpel instance that sends msg to the specific receive
	 * @return   The list of invoker instances
	 * @throws Exception
	 */
	/*List<BPInstanceInfo>*/ List<Map<String, Object>> getInvokerInstance(String bpid, Long receiveActivityId) throws Exception;

	/**
	 * Get the list of invokee bpel instance ids for a specific bpel instance <p>
	 * Only available when isMonitoringVariableEnabled returns true. If isMonitoringVariableEnabled return false, exception 
	 * will be thrown
	 * 
	 * Each Map<String, Object> returned in the List is of the following strucutre:
	 * <tt>key: id value: instanceId</tt>
	 * <tt>key: bpelId value: process QName</tt>
	 * <tt>key: status value: instance status </tt>
	 * <tt>key: startTime value: the instance start time </tt>
	 * <tt>key: endTime  value: the instance end time </tt>
	 * <tt>key: lastUpdateTime value: the last updated time on the instance</tt>
	 * <tt>key: lasted value: the elapse time of instance in seconds </tt>
	 *  	  
	 * @param bpid 		The invoked bpel instance id
	 * @param  invokeActivityId	Optional invoke activity id of the invoker bpel instance, if present, returns the only one invokee bpel instance that receives msg from the specific invoke
	 * @return  The list of invokee instances
	 * @throws Exception
	 */
	/*List<BPInstanceInfo>*/ List<Map<String, Object>> getInvokeeInstance(String bpid, Long invokeActivityId) throws Exception;

	/**
	 * Get the BPEL variable value, the content of BPEL variable will be returned, only available when 
	 * isMonitoringVariableEnabled returns true. If isMonitoringVariableEnabled return false, exception 
	 * will be thrown
	 * @param instanceId  The instance id of the instance 
	 * @param varId		   The variable Id, can not be null
	 * @return					The content of the BPEL variable
	 * @throws Exception
	 */
	String getVariableValue(String instanceId, Long varId) throws Exception;

	/**
	 * Is Monitoring enabled
	 * 
	 * @return true if monitoring is enabled
	 * @throws ManagementRemoteException
	 */
	Boolean isMonitoringEnabled();


	/**
	 * Is Monitoring Variable Enabled. 
	 * @return true if monitoring is enabled
	 */
	Boolean isMonitoringVariableEnabled ();

	/**
	 * Is persistence enabled
	 * 
	 * @return true if persistence is enabled
	 * @throws ManagementRemoteException
	 */
	Boolean isPersistenceEnabled();	 

	
	/**
	 * Returns the timestamp of last successfully completed instance of the business process.
	 * Note, this excludes the instances that were faulted/terminated.
	 * 
	 * @param bpelQName
	 * @return Timestamp of the last completed instance.
	 */
	public Timestamp getLastInstanceProcessedTime(String bpelQName);

    /**
     * Returns total number of active instances for the given 
     * business process
     * 
     * @param bpelQName
     * @return number of live instances or -1 if business process is not deployed on engine.
     */
    public int getLiveInstancesCount(String bpelQName);
    
    /**
     * Returns total number of completed instances for the given 
     * business process
     * 
     * @param bpelQName
     * @return number of live instances or -1 if business process is not deployed on engine.
     */
    public int getCompletedInstancesCount(String bpelQName);
    
    /**
     * Returns total number of suspended instances for given 
     * business process
     * 
     * @param bpelQName
     * @return number of suspended instances or -1 if business process is not deployed on engine.
     */
    public int getSuspendedInstancesCount(String bpelQName);
    
    /**
     * Returns total number of faulted instances for given business process
     * @param bpelQName
     * @return number of faulted instances or -1 if business process is not deployed on engine.
     */
    public int getFaultedInstancesCount(String bpelQName);
    
    /**
     * Returns total number of terminated instances for given business process
     * @param bpelQName
     * @return number of terminated instances or -1 if the business process is not deployed on engine.
     */
    public int getTerminatedInstancesCount(String bpelQName);
    
    /**
     * Sets the time interval (in seconds) that the engine will use to calculate the instance processing rate 
     * for the given business process. Passing null for business process will set the value for all the 
     * processes.
     * 
     * If not set, the default of 1 second is used.
     *  
     * @param bpelQName Business Process Name
     * @param period in seconds
     * @return outcome
     */
    public boolean setInstanceProcessingRatePeriod(String bpelQName, Long period);
    
    /**
     * Returns the Instance Processing Rate for the given process. The default value is one second. Hence the
     * default return value will be number of messages process by the engine for the given process
     * in second which is same as saying Transactions Per Second or Throughput. Use setInstanceProcessingRatePeriod
     * api to change this value.
     * 
     * @param bpelQName
     * @return
     */
    public double getInstanceProcessingRate(String bpelQName);
    
    /**
     * Returns the business process definition for the given business process
     * @param bpelQName
     * @return
     */
    public String getBusinessProcessDefinition(String bpelQName);
    
    /**
     * Return the in memory instances that for the BPEL process identified by the 
     * <code>bpelQName</code>. The optional parameter <code>status</code> is used
     * to filter the returned list of instances based on their state of processing.
     * @param bpelQName, BPEL process QName.
     * @param status, status like <code>COMPLETED</code>, <code>RUNNING</code>, etc.
     * @return Map with the instance id as the key and a String array as the attributes
     * of the instance. For the values in the array refer the method 
     * <code>getMemInstanceInfo</code>
     */
    public Map<String, String[]> getMemInstances(String bpelQName, String status);
    
    /**
     * Return the in memory statistics of the instance identified by the <code>instanceId</code>
     * value. The return value is a String array ex: <code>info[8]</code>, that is defined as follows.
     * info[0]: The engine id. 
     * info[1]: BPEL process name(QName)
     * info[2]: Instance start time (format is yyyy-mm-dd hh:mm:ss.fffffffff)
     * info[3]: Instance end time (format is yyyy-mm-dd hh:mm:ss.fffffffff)
     * info[4]: Instance status (ex: RUNNING, COMPLETED, FAULTED, etc)
     * info[5]: Activity name.
     * info[6]: Activity xpath. (format is yyyy-mm-dd hh:mm:ss.fffffffff)
     * info[7]: Activity start time 
     * @param bpelQName, BPEL process QName.
     * @param instanceId, The particular instance id 
     * @return String array of the attributes of the requested instance.
     */
    public String[] getMemInstanceInfo(String bpelQName, String instanceId);
    	
}
