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
 * @(#)StateManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist;

import java.sql.SQLException;
import java.util.Collection;
import java.util.List;

import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationVal;


/**
 * StateManager is the interface for the engine to interact with DB. StatManager abstracts what
 * kind of persistence connection is used under the covers.
 *
 * @author Sun Microsystems
 */
/**
 * @author mei
 *
 */
public interface StateManager {
    
    enum TransactionType { Local, XAParticipate, XAStartNew, XAEnd, XACancel};
    
    /**
     * State object is expected to call the stateConnection updateState(State). Upon succesful
     * execution, marks the status of the state as "for update". This enables the subsequent calls
     * to do a DB update and not an Insert. Returns a boolean stating that the execution
     * succeeded.
     *
     * @param state State
     * @param instance TODO
     *
     * @return boolean  true: if execution is successful false: otherwise
     */
    boolean persistState(State state, TransactionInfo transactionInfo, BPELProcessInstance instance);

    /**
     * gets Execution frames
     *
     * @param process runtime bpel process
     * @param bpInstanceId BP instance ID
     * @param eng bpel processor
     *
     * @return Collection collection of callframes
     *
     * @throws Exception Exception
     */
    Collection getCallframe(BPELProcessManager processManager, String bpInstanceId, Engine eng)
        throws Exception;


    /**
     * Passivate the state
     * 
     * @param instanceId
     * @param engineId
     * @param messageActivities
     */
    void passivateInstance(String instanceId, String engineId, Collection messageActivities, MutableState state);

    /**
     * Activate the state and acquire owner lock. Find the instance for the given correlation ids and 
     * also the given start IMA. 
     * @param engId
     * @param eventsList
     * @return
     */
    List activateInstancesAndUpdateOwnership(String engId, List eventsList);
    
    /**
     * To activate the instance. Called by the business process instance thread created for 
     * onAlram in cluster.
     * 
     * @param instanceId
     * @param engineId 
     * @return
     */
    int activateInstance(String instanceId, String engineId);
    
    /**
     * Called by the scalability thread to passivate the dirty variables. Note that
     * if the variable is not dirty, i.e the isPassivated() on runtime variable
     * returns true, then the variable value is not updated since the last update
     * hence no passivation of variable is required.
     * 
     * @param instance
     */
    //public void passivateVariblesForMemoryRelease(BPELProcessInstance instance);
    

	void passivateVariable(String stateId, long uniqueId, char[] chars, String scopeId);
	

	/**
	 * Load the persisted variable. During scalability call if the variable
	 * is not dirty, the variable value in-memory is dereferenced. When needed
	 * this variable will be loaded back from the database. 
	 * 
	 * @param stateId
	 * @param uniqueId
	 * @param scopeId
	 * @return
	 */
	Object loadPersistedVariable(String stateId, long uniqueId, String scopeId);
	
	/**
     * Lazy load the variable data the regular execution, if the variable is needed 
     * and found to be passivated. If the variable is dirty, it would be found in
     * variable table with special SCABALILITYPASSIVATED flag as 'Y' otherwise
     * read the persisted value from the same table with SCALABILITYPASSIVATED
     * flag as 'N'
     * 
     * @param stateId
     * @param uniqueId
     * @param scopeId
     * @return
     */
    Object loadPassivatedVariable(String stateId, long uniqueId, String scopeId);
    

	/**
	 * When the in memory variable is updated, the passivated variable is deleted
	 * from the database.
	 * 
	 * @param stateId
	 * @param uniqueId
	 * @param scopeId
	 */
	void deletePassivatedVariable(String stateId, long uniqueId, String scopeId);

    /**
     * The outstanding message exchange id is persisted 
     * @param msgExId  Message Exchange Id
     * @param id   Instance Id
     * @param bpelid    BPEL process Id
     */
    void persistOutstandingMsgExchID(String msgExId, String id, String bpelid);

    /**
     * returns the instance id for the outstanding message exchange id
     * 
     * @param msgExId
     * @param bpelid
     * @return
     */
    String getInstanceForMsgEx(String msgExId, String bpelid);

    /**
     * Returns list of instance ids that are scalability passivated and waiting on 
     * Outstanding message exchange i.e waiting on response/status of invocation.
     * 
     * @param bpelId
     * @return
     * @throws SQLException
     */
    List<String> getScalabilityPassivatedInstances(String bpelId) throws SQLException;

	void persistCorreations(MutableState state);
    
    /**
     * Get Scalability passivated instance
     * 
     * @param corrIds
     * @return instance id of the passivated instance
     */
    String getScalabilityPassivatedInstance(List<CorrelationVal> corrIds);

	/**
	 * Phase 1 Scalability would passivate the variables, if the variable was dirty 
	 * (and already persisted earlier) it would result in two entries for the variable. 
	 * In the event of a crash and Subsequent recovery, if the same variable is to 
	 * be passivated again the earlier passivated variable would cause the primary key violation. 
	 * Hence prior to running recovery, all of the scalability passivated
	 * variables will be deleted from the variables table.
	 * 
	 * @param bpelProcessId instance id
	 */
	void deleteAllScalabilityPasvtdVars(List bpInstanceIds); 

}
