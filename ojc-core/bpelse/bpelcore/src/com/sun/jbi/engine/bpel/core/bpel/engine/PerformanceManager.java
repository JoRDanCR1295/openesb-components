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
 * @(#)PerformanceManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.engine;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.sun.jbi.engine.bpel.core.bpel.event.ActivityEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.BPELInstanceEvent;
import com.sun.jbi.engine.bpel.core.bpel.event.impl.BPELEventPersister;

/**
 * Class used by the monitoring to get key statistics for the business process.
 * 
 * @author mbhasin
 * 
 */
public class PerformanceManager {

	private Timestamp lastProcessedInstanceTS = null;

	private int runningInstances = -1;
	private int completedInstances = 0;
	private int suspendedInstances = 0;
	private int faultedInstances = 0;
	private int terminatedInstances = 0;
	private long processingRateInterval = 1000;
	private long processingRateRunningCount = 0;
	private long samplingWindowStartTS = 0;
	private double currentProcessingRate = 0.0;
	
	private static final int POS_ENGINE_ID = 0;
	private static final int POS_PROCESS_NAME = 1;
    private static final int POS_START_TIME = 2;
    private static final int POS_END_TIME = 3;
    private static final int POS_STATUS = 4;
    private static final int POS_ACT_NAME = 5;
    private static final int POS_ACT_PATH = 6;
    private static final int POS_ACT_START_TIME = 7;
    
    private static final int INFO_SIZE = 8;
    
	private Map<String, String[]> mInstanceInfo = Collections.synchronizedMap(new HashMap<String, String[]>());
	
	public PerformanceManager() {
	    super();
	}

	public synchronized int getRunningInstancesCount() {
		if (runningInstances < 0) {
			return -1;
		}
		return runningInstances;
	}

	public synchronized int getCompletedInstancesCount() {
		return completedInstances;
	}

	public synchronized int getSuspendedInstancesCount() {
		return suspendedInstances;
	}

	public synchronized int getFaultedInstancesCount() {
		return faultedInstances;
	}

	public synchronized int getTerminatedInstancesCount() {
		return terminatedInstances;
	}

	public synchronized Timestamp getLastProcessedInstanceTimestamp() {
		return lastProcessedInstanceTS;
	}

	public synchronized boolean setInstanceProcessingRatePeriod(Long period) {
		long value = period.longValue();
		if (value == 0) {
			return false;
		}
		this.processingRateInterval = value * 1000;
		return true;
	}
	/**
	 * Instance processing rate (for the given process) is calculated by
	 * dividing the total number of completed instances with the total time
	 * elapsed since the start of processing of the first instance.
	 * 
	 * @return instances per second.
	 * 
	 */
	public synchronized double getInstanceProcessingRate() {
		//System.out.println("****** MBEAN CALL ***** current processingRateInterval: " + processingRateInterval);
		return calculateCurrentProcessingRate();
	}

	public synchronized void instanceCreated(BPELEvent event) {
		if (runningInstances < 0) {
			this.runningInstances = 1;
		} else {
			this.runningInstances++;
		}
	}
	
	public synchronized void instanceRecovered(BPELEvent event) {
	    // no need to update the running instance count at this point
	    // as the persistence count will be updated when the first 
	    // activity of the recovered instance is executed. 
	    /*if (runningInstances < 0) {
	        this.runningInstances = 1;
	    } else {
	        this.runningInstances++;
	    }*/
	}

	public synchronized void instanceSuspended(BPELEvent event) {
		this.suspendedInstances++;
		this.runningInstances--;
	}

	public synchronized void instanceResumed(BPELEvent event) {
		this.runningInstances++;
		this.suspendedInstances--;
	}

	public synchronized void instanceCompleted(BPELEvent event) {
		this.runningInstances--;
		this.completedInstances++;
		this.processingRateRunningCount++;
		updateLastProcessedInstanceTS();
		calculateCurrentProcessingRate();
	}

	public synchronized void instanceTerminated(BPELEvent event) {
		this.terminatedInstances++;
		//this.runningInstances--;
	}

	public synchronized void instanceFaulted(BPELEvent event) {
		this.faultedInstances++;
		this.runningInstances--;
	}

	public synchronized void reset(Set<String> states) {
		// NOT IMPLEMENTED
	}
	
	public synchronized void updateSuspendedCountOnTerminate() {
	    this.suspendedInstances--;
	}

	public synchronized void updateRunningCountOnTerminate() {
	    this.runningInstances--;
	}
	/**
	 * Return the In-Memory instance info
	 * @param instanceId
	 * @return String[] for more information on the return array structure see
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#getMemInstanceInfo(java.lang.String, java.lang.String)
	 */
	public String[] getMemInstanceInfo(String instanceId) {
	    synchronized (mInstanceInfo) {
	        return mInstanceInfo.get(instanceId).clone();
	    }
	}
	
	/**
	 * Return the In-Memory instance info. 
	 * If <code>status</code> is present then return only the instances that match the 
	 * status requested else return all the available instances. 
	 * @param status
	 * @return Map<String, String[]> for more information on the return structure see
	 * @see com.sun.jbi.engine.bpel.BPELSEManagementMBean#getMemInstances(java.lang.String, java.lang.String) 
	 */
	public Map<String, String[]> getMemInstantances(String status) {
	    // iterate and return a clone structure. 
	    synchronized (mInstanceInfo) {
	        Map<String, String[]> cloneMap = new HashMap<String, String[]>();
	        // if status is null return all the values. 
	        if (status == null || status.length() == 0) {
	            for (String id: mInstanceInfo.keySet()) {
	                cloneMap.put(id, mInstanceInfo.get(id).clone());
	            }
	            return cloneMap;
	        }
	        // if status is not null iterate and return the matching status values.
	        for (String id: mInstanceInfo.keySet()) {
	            String[] values = mInstanceInfo.get(id);
	            String stat = values[POS_STATUS];
	            if (stat.equals(status)) {
	                String[] cVal = values.clone();
	                cloneMap.put(id, cVal);
	            }
	        }
	        return cloneMap;
	    }
	}
	
	public synchronized void setInstanceInfo(BPELEvent event) {
	    BPELEvent.EventType evtType = event.getEventType();
	    if (evtType.equals(BPELEvent.EventType.BP_START)
	            || evtType.equals(BPELEvent.EventType.BP_RECOVERED)) {
	        BPELInstanceEvent instEvent = (BPELInstanceEvent) event;
	        String[] info = new String[INFO_SIZE];
	        //String[] info = mInstanceInfo.get(instEvent.getInstanceId());
	        info[POS_ENGINE_ID] = instEvent.getEngineId();
	        info[POS_PROCESS_NAME] = instEvent.getBPELName().toString();
	        info[POS_START_TIME] = instEvent.getTimeStamp().toString();
	        info[POS_STATUS] = BPELEventPersister.RUNNING;
	        
	        String instId = instEvent.getInstanceId();
	        mInstanceInfo.put(instId, info);
	    } else if (evtType.equals(BPELEvent.EventType.BP_SUSPEND)) {
	        BPELInstanceEvent instEvent = (BPELInstanceEvent) event;
	        String instId = instEvent.getInstanceId();
	        String[] info = mInstanceInfo.get(instId);
            info[POS_STATUS] = BPELEventPersister.SUSPENDED;
	    }  else if (evtType.equals(BPELEvent.EventType.BP_RESUME)) {
            BPELInstanceEvent instEvent = (BPELInstanceEvent) event;
            String instId = instEvent.getInstanceId();
            String[] info = mInstanceInfo.get(instId);
            info[POS_STATUS] = BPELEventPersister.RUNNING;
        }  else if (evtType.equals(BPELEvent.EventType.BP_COMPLETE)) {
            BPELInstanceEvent instEvent = (BPELInstanceEvent) event;
            String instId = instEvent.getInstanceId();
            /*String[] info = mInstanceInfo.get(instId);
            // check to see if the instance has already been FAULTED
            String status = info[POS_STATUS];
            if (status == null || !status.equals(STR_FAULTED)) {
                info[POS_END_TIME] = instEvent.getTimeStamp().toString();
                info[POS_STATUS] = STR_COMPLETED;
            } */
            // the instance has been completed remove it from memory. 
            mInstanceInfo.remove(instId);
        } else if (evtType.equals(BPELEvent.EventType.BP_TERMINATE)) {
            BPELInstanceEvent instEvent = (BPELInstanceEvent) event;
            String instId = instEvent.getInstanceId();
            mInstanceInfo.remove(instId);
        } else if (evtType.equals(BPELEvent.EventType.BP_FAULT)) {
            BPELInstanceEvent instEvent = (BPELInstanceEvent) event;
            String instId = instEvent.getInstanceId();
            String[] info = mInstanceInfo.get(instId);
            info[POS_END_TIME] = instEvent.getTimeStamp().toString();
            info[POS_STATUS] = BPELEventPersister.FAULTED;
        } else if (evtType.equals(BPELEvent.EventType.ACTIVITY_START)) {
            ActivityEvent actEvent = (ActivityEvent) event;
            String instId = actEvent.getInstanceId();
            String[] info = mInstanceInfo.get(instId);
            info[POS_ACT_NAME] = actEvent.getActivityName();
            info[POS_ACT_PATH] = actEvent.getActivityXpath();
            info[POS_ACT_START_TIME] = actEvent.getTimeStamp().toString();
        }
	}
	
	private void updateLastProcessedInstanceTS() {
		if (lastProcessedInstanceTS == null) {
			lastProcessedInstanceTS = new Timestamp(System.currentTimeMillis());
		} else {
			lastProcessedInstanceTS.setTime(System.currentTimeMillis());
		}
	}
	
	/**
	 * The logic is to keep count of instances completed for the defined interval, and when the 
	 * interval is over, take a reading of the processing rate. If the call happen to be made
	 * from the mbean  (getInstanceProcessingRate()), then if the call happen to be just when the 
	 * configured interval is expired, then return the last calculated value of the processing rate,
	 * otherwise calculate a new value again.
	 * 
	 * Also, it is worth noting that the first value (call made before the expiration of first 
	 * sampling window, will not be correct.
	 * 
	 * @return
	 */
	private double calculateCurrentProcessingRate() {
		long currentTime = System.currentTimeMillis();
		long elapsedTime =  currentTime - samplingWindowStartTS;
		
		if (elapsedTime >= processingRateInterval) {
			// its time to take a reading
			if (processingRateRunningCount != 0 && elapsedTime != 0) {
				currentProcessingRate = (processingRateRunningCount * processingRateInterval)/elapsedTime;
			}
			
			if (processingRateRunningCount == 0) {
				currentProcessingRate = 0.0;
			}
			
			// reset the window and counter..
			processingRateRunningCount = 0;
			samplingWindowStartTS = currentTime;
		}
		return currentProcessingRate;
	}
	
	public synchronized void syncInstanceCountAtStart(Map<String, Integer> smap) {
	    int size = smap.size();
	    if (size > 0) {
	        Set<String> keys = smap.keySet();
	        for (String key: keys) {
	            int count = smap.get(key).intValue();
	            if (key.equals(BPELEventPersister.COMPLETED)) {
	                completedInstances += count;
	            } else if (key.equals(BPELEventPersister.RUNNING)) {
	                runningInstances += count;
	            } else if (key.equals(BPELEventPersister.SUSPENDED)) {
	                suspendedInstances += count;
	            } else if (key.equals(BPELEventPersister.FAULTED)) {
	                faultedInstances += count;
	            } else if (key.equals(BPELEventPersister.TERMINATED)) {
	                terminatedInstances += count;
	            } 	  
	        }
	    }
	}
	
	public synchronized void purgeInstanceCounts() {
	    // reset the values of the COMPLETED, FAULTED AND TERMINATED instances. 
	    completedInstances = 0;
	    faultedInstances = 0;
	    terminatedInstances = 0;
	    lastProcessedInstanceTS = null;
	}
	
}
