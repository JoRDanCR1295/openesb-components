package com.sun.caps.management.api.bpel;

import java.sql.Timestamp;
import java.util.List;

import com.sun.caps.management.common.ManagementRemoteException;

/**
 * Defines operations for managing BPEL process instances
 * 
 * @author Sun Microsystems
 */
public interface BPELManagementService {
        
    /**
     * Is Monitoring enabled
     * 
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @return true if monitoring is enabled
     * @throws ManagementRemoteException
     */
    boolean isMonitoringEnabled(String target) throws ManagementRemoteException;
    
    /**
     * Is Monitoring Variable Enabled. 
     * 
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster     
     * @return true if monitoring is enabled
     * @throws ManagementRemoteException
     */
    boolean  isMonitoringVariableEnabled (String target) throws ManagementRemoteException;    
    
    /**
     * Is persistence enabled
     * 
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @return true if persistence is enabled
     * @throws ManagementRemoteException
     */
    boolean isPersistenceEnabled(String target) throws ManagementRemoteException;
    
    /**
     * Get the list of BPEL process QName as String within an SU.
     * @param suName The name of the SU.
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @return The list of BPELProcesses Ids, empty list will be returned
     * @throws Exception
     */
    List<String> getBPELProcessIds (String suName, String target) throws ManagementRemoteException;
    
    /**
     * Get the list of ActivityStatus of a BPEL instance
     * @param instanceId
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @return
     * @throws Exception
     */
    List<ActivityStatus> getBPELInstanceActivityStatus (String instanceId, String target) throws ManagementRemoteException;
    
    
    /**
     * Get BPEL instances given optional BPEL process QName and/or an optional BPStatus or an optional instance id 
     * If instanceid is present, the BPEL process QName and BPStatus are ignored <p>
     * 
     * Without max specified, the maximum instances to return is 1000, user specifies a number for <code>maxRecords</code> to limit the number
     * of the instances returned. <p>
     * 
     * If the <code>BPInstanceQueryResult.overflow</code> is true, it indicates the number of qualifying instances is larger than 1000,
     * no instances are returned in the result list, user should specify a <code>maxRecords</code>
     * and <code>sortColumn</code> and <code>order</code>
     *  
     *  
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @param bpelQName  BPEL process QName as String, not used if instanceId is present
     * @param status   Optional, if present, only the instances of the specific status are returned, not used if instanceId is present
     * @param instanceId  Optional, if present only the specific instance is returned
     * @param maxRecords  Optional
     * @param sortColumn Optional
     * @param SortOrder  Optional
     * @return  A list of BPEL instance ids
     * @throws Exception
     */
    BPInstanceQueryResult getBPELInstances (String bpelQName, BPStatus status, String instanceId, Integer maxRecords,     		SortColumn sortColumn, SortOrder order, String target) throws ManagementRemoteException;           /**     * Search for BPEL instances info given BPEL process QName and a SearchString and/or an optional BPStatus.     *      * Without max specified, the maximum instances to return is 1000, user specifies a number for <code>maxRecords</code> to limit the number     * of the instances returned. <p>     *      * If the <code>BPInstanceQueryResult.overflow</code> is true, it indicates the number of qualifying instances is larger than 1000,     * no instances are returned in the result list, user should specify a <code>maxRecords</code>     * and <code>sortColumn</code> and <code>order</code>     *       *       * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster     * @param bpelQName  BPEL process QName as String, not used if instanceId is present     * @param status   Optional, if present, only the instances of the specific status are returned.     * @param searchString  The searchString     * @param maxRecords  Optional     * @param sortColumn Optional     * @param SortOrder  Optional     * @return  A list of BPEL instance ids     * @throws Exception     */    BPInstanceQueryResult searchBPELInstances (String bpelQName, BPStatus status, String searchString, Integer maxRecords,     		SortColumn sortColumn, SortOrder order, String target) throws ManagementRemoteException;  
    
    
    /**
     * Get the fault detail of a faulted bpel instance;
     * @param  instanceId  the faulted bpel instance id
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @param conn     database connection, when the result is obtained, the conn is closed.
     * 
     */
    String getBPELInstanceFault (String instanceId, String target) throws ManagementRemoteException; 
    
    
    /**
     * Get the list of invoker (parent) bpel instance(s)  that invoked a specific bpel instance.
     * For example: bp1 (invoke) --> bp2 (receive), given bp2's instance id, returns the info for bp1. <p>
     * Only available when isMonitoringVariableEnabled returns true. If isMonitoringVariableEnabled return false, exception 
     * will be thrown     
     * @param instanceId            The invoked bpel instance id
     * @param receiveActivityId     Optional receive activity id of the invoked bpel instance, if present, returns only one invoker bpel instance that sends msg to the specific receive
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @return   The list of invoker instances
     * @throws Exception
     */
    List<BPInstanceInfo> getInvokerInstance(String instanceId, Long receiveActivityId, String target) throws ManagementRemoteException; 
    
    
    /**
     * Get the list of invokee (sub) bpel instance(s)  that a specific bpel instance invoked.
     * For example: bp1 (invoke) --> bp2(receive), given bp1's instance id, return the info for bp2. <p>
     * Only available when isMonitoringVariableEnabled returns true. If isMonitoringVariableEnabled return false, exception 
     * will be thrown
     * @param instanceId            The invoked bpel instance id
     * @param  invokeActivityId     Optional invoke activity id of the invoker bpel instance, if present, 
     *                                                                                          returns the only one invokee bpel instance that receives msg from the specific invoke
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @return  The list of invokee instances
     * @throws Exception
     */
    List<BPInstanceInfo> getInvokeeInstance(String instanceId, Long invokeActivityId, String target) throws ManagementRemoteException; 
    
    
    /**
     * Suspend BPEL instance
     * @param instanceId  The bpel instance to be suspended
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @return  true if the operation is successful, false if no instance of instanceId is found, either no such instance or the instance has completed or falted
     * @throws Exception
     */
    boolean suspendInstance (String instanceId, String target) throws ManagementRemoteException;
    
    /**
     * Suspend all instances of a bpel process
     * @param processName  The process name (QName)
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @return  The list of suspended instance ids
     * @throws Exception
     */
    List<String> suspendAllInstance (String processName, String target) throws ManagementRemoteException;    
    
    /**
     * Resume  a suspended BPEL instance
     * @param instanceId  The bpel instance to be resumed
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @return  true if the operation is successful, false if no instance of instanceId is found, either no such instance or the instance is not suspended
     * @throws Exception
     */
    boolean resumeInstance (String instanceId, String target) throws ManagementRemoteException;
    
    /**
     * Resume all instances of a bpel process
     * @param processName  The process name (QName)
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @return  The list of suspended instance ids
     * @throws Exception
     */
    List<String> resumeAllInstance (String processName, String target) throws ManagementRemoteException;         

    
    /**
     * Terminate  a  BPEL instance
     * @param instanceId  The bpel instance to be terminated
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @return  true if the operation is successful, false if no instance of instanceId is found, either no such instance or the instance has completed or falted
     * @throws Exception
     */
    boolean terminateInstance (String instanceId, String target) throws ManagementRemoteException;    
    
    /**
     * Terminate all instances of a bpel process
     * @param processName  The process name (QName)
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @return  The list of suspended instance ids
     * @throws Exception
     */
    List<String> terminateAllInstance (String processName, String target) throws ManagementRemoteException;          
    
    /**
     * Change BPEL  variable value, only the leaf node can be changed
     * Only available when isMonitoringVariableEnabled returns true. If isMonitoringVariableEnabled return false, exception 
     * will be thrown     
     * @param instanceId            The instance id of the instance on which the variable will be changed
     * @param varId                         The variable Id, can not be null
     * @param partName              The part name of the variable, if null, the variable is not message type        
     * @param xpath                 The xpath leading to the leaf node to be changed, if null, the whole variable part (if message type) or variable (not message type) is to change
     * @param value                 The new value to change
     * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
     * @throws Exception
     */
     boolean changeVariableValue(String instanceId, long varId, String partName, String xpath,
            String value, String target) throws ManagementRemoteException;    
     
     /**
      * Get the BPEL variable value, the content of BPEL variable will be returned
     * Only available when isMonitoringVariableEnabled returns true. If isMonitoringVariableEnabled return false, exception 
     * will be thrown      
      * @param instanceId  The instance id of the instance 
      * @param varId               The variable Id, can not be null
      * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
      * @return                                 The content of the BPEL variable
      * @throws Exception
      */
     String getVariableValue (String instanceId, long varId, String target) throws ManagementRemoteException;    
     
     /**
      * Get the BPEL variable info for a bpel instance
     * Only available when isMonitoringVariableEnabled returns true. If isMonitoringVariableEnabled return false, exception 
     * will be thrown      
      * @param instanceId The instance id of the instance
      * @param varName   Optional, if specified, only returns the info for the variable of this name 
      * @param target  If null, it is not in a clustered, if not null, it is the target name of a cluster
      * @return
      */
     List<VarInfo> listBPELVaraibles(String instanceId, String varName, String target) throws ManagementRemoteException; 
    
     /**
      * The Query Result wrapper class containing the return list and data
      * of the return batch
      * @author Sun Microsystems
      *
      */
     public static final class BPInstanceQueryResult {
          public List<BPInstanceInfo> bpInstnaceList;
          public int total;
          public int returned;
          public boolean overflow = false;
     }
    
     /**
      * Encapsulates the Activity status and runtime information
      * 
      *
      */
    public static class ActivityStatus {
        public static enum Status {
            /**
             * The Activity is started
             */
            STARTED,
            /**
             * The Activity is completed
             */
            COMPLETED,
            /**
             * The Activity incurs a fault
             */
            FAULTED,
            /**
             * The Activity is suspended
             */ 
            SUSPENDED,
            /**
             * The Activity is terminated
             */
            TERMINATED          
        };
        
        /**
         * The unique id assigned to the Activity, the id
         * belongs to the static bpel model and does not change
         * from instance to instance
         * 
         */
        public String activityId;
        /**
         * The xpath of the activity
         */
        public String activityXpath;
        
        /**
         * The iteration number, starts from 0. it is  always 0 if the activity is not within a loop.
         * When the activity is  within the loop, the iteration number shows the iteration of this activity.
         */
        public int iteration;
        
        /**
         * The start time of the activity
         */;       
        public Timestamp startTime;
        /**
         * The end time of the activity
         */
        public Timestamp endTime;
        /**
         * The elapse time of the activity in seconds
         */
        public float lasted;        
        /**
         * The current status of the activity
         */
        public Status status;
    }
    
    /**
     * Encapsulates the BPEL instance status
     * 
     *
     */
    public static enum BPStatus {
        /**
         * The instance is live
         */
        RUNNING,
        /**
         * The instance is completed normally
         */
        COMPLETED,
        /**
         * The instance is suspended
         */
        SUSPENDED,
        /**
         * The instance is terminated 
         */
        TERMINATED,
        /**
         * The instance is ended with an unhandled fault
         */
        FAULTED
    }
    
    /**
     *  An Enum class for valid sort parameter values  
     * 
     *
     */
    public static enum SortColumn {
        /**
         * Sort on StartTime
         */
        STARTTIME("startTime"),
        /**
         * Sort on EndTime
         */
        ENDTIME("endTime"),
        /**
         * Sort on updatedTime
         */
        UPDATEDTIME ("updatedTime");
        
        private String mVal;
        
         SortColumn (String  value) {
            mVal = value;
        }

        /* 
         * @see java.lang.Enum#toString()
         */
        public String toString() {
            return mVal;
        }    
    }
    
   /**
    * An Enum class for valid order parameter values  
    * @author mei
    *
    */
    public static enum SortOrder {
        /**
         * Ascending
         */
        ASC,
        /**
         * Descending
         */
        DESC
    }
    
    /**
     * Encapsulates BPEL instance runtime information
     * 
     *
     */
    public static class BPInstanceInfo {
        /**
         * Unique id of the instance, each instance is assigned a globally unique id
         */
        public String id;
        /**
         * Process QName as String
         */
        public String bpelId;
        /**
         * Instance status
         */
        public BPStatus status;
        /**
         * The started time of the instance
         */
        public Timestamp startTime;
        /**
         * The ended time of the instance
         */
        public Timestamp endTime;
        /**
         * The last updated time of the instance
         */
        public Timestamp lastUpdateTime;
        /**
         * The elapsed time of the instance in seconds
         */
        public float lasted;
    }
    
    /**
     * Encapsulates BPEL variable information
     *
     *
     */
    public static class VarInfo {
        /**
         * BPEL variable name
         */
        public String varName;
        /**
         * Unique id of the BPEL variable, the id
         * belongs to the static bpel model and does not change
         * from instance to instance
         */
        public long varId;
        /**
         * The xpath of the scope/process where the variable is defined
         * 
         */
        public String xpath;                /**         * Additional notes related to the variable         */        public String notes;
    }      
}
