package com.sun.jbi.engine.bpel;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;

import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ThreadManager;

public class BPELSEManagement implements BPELSEManagementMBean {

	private com.sun.jbi.engine.bpel.core.bpel.management.BPELSEManagement bpelseManagement;
	private ThreadManager threadMgr;
	
	public BPELSEManagement(Engine engine, ThreadManager threadMgr) throws Exception {
        bpelseManagement = new com.sun.jbi.engine.bpel.core.bpel.management.BPELSEManagement(engine);
        this.threadMgr = threadMgr;
    }
	
	/*
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#updateProcessEventsGenerationFlag(java.util.Map)
	 */
	public void updateProcessEventsGenerationFlag(Map<String, Boolean> configValues) throws Exception {
		bpelseManagement.setProcessEventsGenerationFlag(configValues);
	}
	
	/*
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#getAllProcessEventsGenerationFlag()
	 */
	public Map<String, Map<String, Boolean>> getAllProcessEventsGenerationFlag() throws Exception {
		return bpelseManagement.getAllProcessEventsGenerationFlag();
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#changeVariableValue(java.lang.String, java.lang.Long, java.lang.String, java.lang.String, java.lang.String)
	 */
	public Boolean changeVariableValue(String instanceId, Long varId,
			String partName, String xpath, String value) throws Exception {
		return bpelseManagement.changeVariableValue(instanceId, varId, partName, xpath, value);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#getBPELInstanceActivityStatus(java.lang.String)
	 */
	public List<Map<String, Object>> getBPELInstanceActivityStatus(
			String instanceId) throws Exception {
		return bpelseManagement.getBPELInstanceActivityStatus(instanceId);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#getBPELInstanceFault(java.lang.String)
	 */
	public String getBPELInstanceFault(String bpid) throws Exception {
		return bpelseManagement.getBPELInstanceFault(bpid);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#getBPELInstances(java.lang.String, java.lang.String, java.lang.String, java.lang.Integer, java.lang.String, java.lang.String)
	 */
	public List<Map<String, Object>> getBPELInstances(String bpelQName,
			String status, String instanceId, Integer maxRecords,
			String sortColumn, String order) throws Exception {
		return bpelseManagement.getBPELInstances(bpelQName, status, instanceId, maxRecords, sortColumn, order);
	}
	
	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#searchBPELInstances(java.lang.String, java.lang.String, java.lang.String, java.lang.Integer, java.lang.String, java.lang.String)
	 */
	public List<Map<String, Object>> searchBPELInstances(String bpelQName,
			String status, String searchString, Integer maxRecords,
			String sortColumn, String order) throws Exception {
		return bpelseManagement.searchBPELInstances(bpelQName, status, searchString, maxRecords, sortColumn, order);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#getBPELProcesses(java.lang.String)
	 */
	public List<String> getBPELProcesses(String suName) throws Exception {
		return bpelseManagement.getBPELProcesses(suName);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#getInvokeeInstance(java.lang.String, java.lang.Long)
	 */
	public List<Map<String, Object>> getInvokeeInstance(String bpid,
			Long invokeActivityId) throws Exception {
		return bpelseManagement.getInvokeeInstance(bpid, invokeActivityId);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#getInvokerInstance(java.lang.String, java.lang.Long)
	 */
	public List<Map<String, Object>> getInvokerInstance(String bpid,
			Long receiveActivityId) throws Exception {
		return bpelseManagement.getInvokerInstance(bpid, receiveActivityId);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#getVariableValue(java.lang.String, java.lang.Long)
	 */
	public String getVariableValue(String instanceId, Long varId)
			throws Exception {
		return bpelseManagement.getVariableValue(instanceId, varId);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#isMonitoringEnabled()
	 */
	public Boolean isMonitoringEnabled() {
		return bpelseManagement.isMonitoringEnabled();
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#isPersistenceEnabled()
	 */
	public Boolean isPersistenceEnabled() {
		return bpelseManagement.isPersistenceEnabled();
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#listBPELVaraibles(java.lang.String, java.lang.String)
	 */
	public List<Map<String, Object>> listBPELVaraibles(String instanceId,
			String varName) throws Exception {
		return bpelseManagement.listBPELVaraibles(instanceId, varName);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#resumeAllInstance(java.lang.String)
	 */
	public List<String> resumeAllInstance(String processName) throws Exception {
        List<String> list = bpelseManagement.resumeAllInstance(processName);
        this.threadMgr.notifyThreads();
        return list;
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#resumeInstance(java.lang.String)
	 */
	public Boolean resumeInstance(String instanceId) throws Exception {
		Boolean result = bpelseManagement.resumeInstance(instanceId);
        this.threadMgr.notifyThreads(); 
        return result;
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#suspendAllInstance(java.lang.String)
	 */
	public List<String> suspendAllInstance(String processName) throws Exception {
		return bpelseManagement.suspendAllInstance(processName);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#suspendInstance(java.lang.String)
	 */
	public Boolean suspendInstance(String instanceId) throws Exception {
		return bpelseManagement.suspendInstance(instanceId);
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#terminateAllInstance(java.lang.String)
	 */
	public List<String> terminateAllInstance(String processName)
			throws Exception {
		List<String> list =  bpelseManagement.terminateAllInstance(processName);
		this.threadMgr.notifyThreads();
		return list;
	}

	/**
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#terminateInstance(java.lang.String)
	 */
	public Boolean terminateInstance(String instanceId) throws Exception {
		boolean status = bpelseManagement.terminateInstance(instanceId);
		this.threadMgr.notifyThreads();
		return status;
	}

    public Boolean isMonitoringVariableEnabled() {
        return  bpelseManagement.isMonitoringVariableEnabled();
    }
    
	public Timestamp getLastInstanceProcessedTime(String bpelQName) {
		return bpelseManagement.getLastInstanceProcessedTime(bpelQName);
	}
    
    public int getLiveInstancesCount(String bpelQName) {
    	return bpelseManagement.getLiveInstancesCount(bpelQName);
    }
    
    public int getCompletedInstancesCount(String bpelQName) {
    	return bpelseManagement.getCompletedInstancesCount(bpelQName);
    }
    
    public int getSuspendedInstancesCount(String bpelQName) {
    	return bpelseManagement.getSuspendedInstancesCount(bpelQName);
    }
    
    public int getFaultedInstancesCount(String bpelQName) {
    	return bpelseManagement.getFaultedInstancesCount(bpelQName);
    }
    
    public int getTerminatedInstancesCount(String bpelQName) {
    	return bpelseManagement.getTerminatedInstancesCount(bpelQName);
    }
    
    public double getInstanceProcessingRate(String bpelQName) {
    	return bpelseManagement.getInstanceProcessingRate(bpelQName);
    }
    
    public String getBusinessProcessDefinition(String bpelQName){
    	return bpelseManagement.getBusinessProcessDefinition(bpelQName);
    }
    
    public boolean setInstanceProcessingRatePeriod(String bpelQName, Long period) {
    	return bpelseManagement.setInstanceProcessingRatePeriod(bpelQName, period);
    }

    public String[] getMemInstanceInfo(String bpelQName, String instanceId) {
        return bpelseManagement.getMemInstanceInfo(bpelQName, instanceId);
    }

    public Map<String, String[]> getMemInstances(String bpelQName, String status) {
        return bpelseManagement.getMemInstances(bpelQName, status);
    }
    
}
