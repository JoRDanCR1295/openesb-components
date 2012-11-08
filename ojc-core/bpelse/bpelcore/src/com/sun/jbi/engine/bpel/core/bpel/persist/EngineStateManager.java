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
 * @(#)EngineStateManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist;

import java.sql.SQLException;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.dbo.CRMPDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.QueryRespCRMPDBO;



/** @author Sun Microsystems
 * Mar 3, 2005
 */
public interface EngineStateManager {
    /**
     * @param maxcount list of BPs that this engine can handle
     * @return List of BPIds which are incomplete and no engine is processing them
     */
    List getExpiredEngineBPs(int maxcount);

    /**
     * persist in memory state to database
     * @param state engine state
     * @return boolean
     *      true: if operation is successfully executed
     *      false: otherwise
     * @throws Exception Exception
     */
    //boolean persistState(EngineState state) throws Exception;

    //public boolean persistStaticBPEL(String bpelId, byte[] val) throws Exception;
    /**
     * @return Map whose keys are BPEL Ids and whose values are a list of BP
     * instance Ids (state ID). Map<String, List<String>>
     * This should return the following
     * 1. All the running instances with this engine id and which are not passivated 
     *    (i.e. the ownerlock is 'Y')
     * 2. All the running instances which are passivated and in satisfy the 
     *    the condition - The last check point is pick activity with 
     *    onAlarm defined which already expired at the time of this call.
     *    
     * This should NOT return the instances which satisfies any of the following conditions
     * 1. The instance is passivated and the last check point is pick activity for which 
     *    no onAlarm is specified. (Such instances to be recovered only in the event of 
     *    IMA execution for which the instance is passivated)
     * 2. The instance is passivated and the lastcheckpoint is pick activity for which
     *    onAlarm is specified and the alarm has not expired yet. 
     *     
     * 
     * 
     * @throws Exception Exception
     */
    public Map[] getRunningAndSuspendedStates() throws Exception;
    
    /**
     * update engine heartbeat
     * @param mHeartbeatUpdateConfigTime TODO
     * @return TODO
     */
    public boolean updateHeartbeat(long mHeartbeatUpdateConfigTime);
    
    /**
     * get last heartbeat update time
     * @return
     */
    public long getLastHeartbeatUpdateTime();
    
    /**
     * update dangling instances
     * 
     * @param engineExirationInterval
     * @param recoveryBatchSize TODO
     * @return
     */
    public int updateDanglingInstances(long engineExirationInterval, int recoveryBatchSize);
    
  
    /**
     * This method is used query the DB to verify is there are any entries in the CRMP table 
     * for the value of the CRMPInvokeId property of the received InOut message exchange.
     * @param crmpInvId : the value of the CRMPInvokeId property of the InOut message exchange
     * @param isClusteredInvoke TODO
     * @return CRMPDBO object
     * @throws Exception
     */
    public CRMPDBO getCRMPDBOForCRMPInvokeID(String crmpInvId, boolean isClusteredInvoke) throws Exception;
    
    /**
     * This method is used to query for the response obj in the CRMP table for the crmpInvokeId.
     * the response obj is used for SubBP recovery scenario as explained in 
     * com.sun.jbi.engine.bpel.core.bpel.dbo.QueryRespCRMPDBO.java javadoc. 
     * @param crmpInvId : the value of the CRMPInvokeId property of the InOut message exchange.
     * @return QueryCRMPDBO 
     * @throws Exception
     */
    public QueryRespCRMPDBO getResponseObjForCrmpInvokeId(String crmpInvId) throws Exception;

    /**
     * Find the instances which are passivated and for which on alarm is defined
     * and the on alarm is not expired. We don't have to do recovery of these
     * (Don't need to create special lock to indicate that this engine is scheduling in memory special BPIT for
     * on alarm as all the orphaned instances are already moved over to this engine
     * hence no other engine will do this) 
     * will acquire the ownerlock and process the instance.
     * @return List
     * @throws SQLException 
     * @throws Exception 
     */
    Map getNonExpiredOnAlarmInstances() throws Exception;
    
    Map<QName, List<String>> getExpiredOnAlarmInstances() throws SQLException;

}
