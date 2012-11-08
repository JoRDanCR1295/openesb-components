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
 * @(#)DBObjectFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dbo;

import java.sql.SQLException;
import java.sql.Timestamp;

import javax.sql.rowset.serial.SerialException;
import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.CRMPDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.EngineCorrDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.EngineDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.EventHandlerDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.OutstandingMsgExDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.ScopeDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.ForEachDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.InstanceCorrDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.LastCheckPointDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.PartnerLinkDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.QueryCRMPDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.QueryRespCRMPDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.SimpleVariableDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.StateDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.SuspendResumeStateDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.UpdateDanglingInstancesDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.VariableDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.VariableScalabilityDBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.dbo.impl.WaitingIMADBOImpl;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class DBObjectFactory {
    private static DBObjectFactory mSingleton;

    // TODO evaluate the advantage gained if caching of these DB object instances is done.
    private int mDBType;

    private DBObjectFactory(int dbType) {
        mDBType = dbType;
    }

    /**
     * get DBObject factory
     *
     * @param dbType database type
     *
     * @return DBObjectFactory DBObject factory
     */
    public static DBObjectFactory getDBObjectFactory(int dbType) {
        if (mSingleton == null) {
            mSingleton = new DBObjectFactory(dbType);
        }

        return mSingleton;
    }

    /**
     * creates engine DBObject
     *
     * @param id ID
     * @param location engine location
     * @param expiration engine lease expiration
     *
     * @return EngineDBO engine DBObject
     */
/*    public EngineDBO createEngineDBO(String id, String location, long expiration) {
        return new EngineDBOImpl(mDBType, id, location, expiration);
    }
*/
    public EngineDBO createEngineDBO(String id, String location) {
        return new EngineDBOImpl(mDBType, id, location);
    }

    /**
     * creates engine correlation DBObject
     *
     * @param id ID
     * @param bpelId BPELID
     * @param engineId engine ID
     * @param value value
     *
     * @return EngineCorrelationDBO engine correlation DBObject
     */
    //public EngineCorrelationDBO createEngineCorrelationDBO(long id, String bpelId, String engineId, String value) {
    public EngineCorrelationDBO createEngineCorrelationDBO(long id, QName bpelId, String engineId, String value) {
        return new EngineCorrDBOImpl(mDBType, id, bpelId, engineId, value);
    }

    /**
     * creates instance correlation DBObject
     *
     * @param id ID
     * @param bpId BPID
     * @param value value
     *
     * @return InstanceCorrelationDBO instance correlation DBObject
     */
    public InstanceCorrelationDBO createInstanceCorrelationDBO(long id,
        String bpId, String value) {
        return new InstanceCorrDBOImpl(mDBType, id, bpId, value);
    }

    /**
     * creates last checkpoint DBObject
     *
     * @param bpid BPID
     * @param actId activity ID
     * @param oldActId old activity ID
     * @param date timestamp
     * @param pickCompositeActId pick composite activity ID
     * @return LastCheckPointDBO last checkpoint DBObject
     */
    public LastCheckPointDBO createLastCheckPointDBO(String bpid, Long actId,
        Long oldActId, Timestamp date, Long pickCompositeActId, Long branchInvokeCounter) {
        long lPickCompositeActId = LastCheckPointDBO.DEFAULT_PICK_COMPOSITE_ACT_ID;
        long lBranchInvokeCounter = LastCheckPointDBO.DEFAULT_BRANCH_INVOKE_COUNTER;
        
        if (pickCompositeActId != null) {
            lPickCompositeActId = pickCompositeActId.longValue();
        }
        
        if (branchInvokeCounter != null) {
        	lBranchInvokeCounter = branchInvokeCounter.longValue();
        }
        
        if (oldActId == null) {
            return new LastCheckPointDBOImpl(mDBType, bpid, actId.longValue(),
                LastCheckPointDBO.DEFUALT_OLD_PC_VAL, date,
                lPickCompositeActId, lBranchInvokeCounter);
        }

        return new LastCheckPointDBOImpl(mDBType, bpid, actId.longValue(),
            oldActId.longValue(), date, lPickCompositeActId, lBranchInvokeCounter);
    }

    /**
     * creates state DBObject
     *
     * @param bpid BPID
     * @param bpelId BPELID
     * @param engId engine ID
     *
     * @return StateDBO state DBObject
     */
    public StateDBO createStateDBO(String bpid, QName bpelId, String engId) {
        return new StateDBOImpl(mDBType, bpid, bpelId, engId);
    }
    
    /**
     * create suspend resume state dbo object 
     * 
     * @param bpid  BPID
     * @param suspend  If true, the dbo is for suspend, false the dbo is for resume
     * @return StateDBO state DBObject
     */
    public StateDBO createSuspendResumeStateDBO(String bpid, boolean suspend) {
    	return new SuspendResumeStateDBOImpl(mDBType, bpid, suspend);
    }

    /**
     * creates state DBObject
     *
     * @param bpid BPID
     * @param engineId TODO
     * @param mode activation or passivation
     * @return StateDBO state DBObject
     */
    public StateDBO createStateDBO(String bpid, String engineId, State.Mode mode) {
        return new StateDBOImpl(mDBType, bpid, engineId, mode);
    }
    
    /**
     * creates variable DBObject
     *
     * @param bpid BPID
     * @param id ID
     * @param b characters
     * @param isScalabilityPassivated TODO
     * @return VariableDBO variable DBObject
     *
     * @throws SerialException SerialException
     * @throws SQLException SQLException
     */
    public VariableDBO createVariableDBO(String bpid, long id, char[] b, String scopeGuid)
        throws SerialException, SQLException {
        return new VariableDBOImpl(mDBType, bpid, id, b, scopeGuid);
    }
    
    public VariableDBO createVariableDBO(String bpid, long id, String scopeGuid) {
        return new VariableDBOImpl(mDBType, bpid, id, scopeGuid);
    }
    
    public VariableScalabilityDBO createVariableScalabilityDBO(String instanceId) {
    	return new VariableScalabilityDBOImpl(mDBType, instanceId);
    }
    
    /**
     * For passivating the variable during phase 1 scalability call.
     * 
     * @param bpid
     * @param id
     * @param b
     * @param scopeGuid
     * @return
     * @throws SerialException
     * @throws SQLException
     */
    public VariableScalabilityDBO createVariableScalabilityDBO(String bpid, long id, char[] b, String scopeGuid)
		throws SerialException, SQLException {
		return new VariableScalabilityDBOImpl(mDBType, bpid, id, b, scopeGuid);
    }
    
    /**
     * Scalability related, used for following
     * 1 - Loading (reading) the scalability passivated variable from database. 
     * 2 - Deleting (removing) scalability passivated variable from database.
     * 
     * @param bpid
     * @param id
     * @param scopeGuid
     * @return VariableScalabilityDBO
     */
    public VariableScalabilityDBO createVariableScalabilityDBO(String bpid, long id, String scopeGuid) {
    	return new VariableScalabilityDBOImpl(mDBType, bpid, id, scopeGuid);
    }
    
    public VariableDBO createSimpleVariableDBO (String bpid, long id, Object o, String scopeId)
        throws SerialException, SQLException {
        return new SimpleVariableDBOImpl(mDBType, bpid, id, o, scopeId);
    }

    /**
     * Creates foreach DBObject
     * 
     * @param bpid The bpel id
     * @param foreachId The foreach activity id
     * @param counter The current iteration index
     * @param successes The number of successful iterations
     * @return a foreach DBObject
     * @throws SerialException
     * @throws SQLException
     */
    public ForEachDBO createForEachDBO(String bpid, long foreachId, 
                                       int counter, int successes,
                                       int startCount, int finalCount,
                                       int conditionCount) {
        return new ForEachDBOImpl(mDBType, bpid, foreachId,
                                  counter, successes,
                                  startCount, finalCount, conditionCount);
    }
    
    /**
     * Creates foreach DBObject during recovery
     * 
     * @param bpid The bpel id
     * @return a foreach DBObject
     */
    public ForEachDBO createForEachDBO(String bpid) {
        return new ForEachDBOImpl(mDBType, bpid);
    }
    
    /**
     * creates variable DBObject
     *
     * @param bpid BPID
     * @param scopeId ID
     * @param faultName 
     * @param c characters
     *
     * @return FaultHandlerData DBObject
     *
     * @throws SerialException SerialException
     * @throws SQLException SQLException
     */
    public ScopeDBO createScopeDBO(String bpid, long scopeId, 
    		String scopeGuid, String parentScopeGuid, 
    		String scopeState, 
    		long compensateId, long completionOrder,  
    		QName faultName, char[] c, long faultActivityId)
        throws SerialException, SQLException {
        return new ScopeDBOImpl(mDBType, bpid, scopeId, 
        		scopeGuid, parentScopeGuid, scopeState, compensateId,
        		completionOrder, faultName, c, faultActivityId);
    }
    
    /**
     * creates Engine DBObject
     *
     * @return EngineDBO engine DBObject
     */
    public EngineDBO createEngineDBO() {
        return new EngineDBOImpl(mDBType);
    }

    /**
     * creates engine correlation DBObject
     *
     * @return EngineCorrelationDBO engine correlation DBObject
     */
    public EngineCorrelationDBO createEngineCorrelationDBO() {
        return new EngineCorrDBOImpl(mDBType);
    }

    /**
     * creates instance correlation DBObject
     *
     * @param bpId BPID
     *
     * @return InstanceCorrelationDBO instance correlation DBObject
     */
    public InstanceCorrelationDBO createInstanceCorrelationDBO(String bpId) {
        return new InstanceCorrDBOImpl(mDBType, bpId);
    }

    /**
     * creates last check point DBObject
     *
     * @param bpId BPID
     *
     * @return LastCheckPointDBO last check point DBObject
     */
    public LastCheckPointDBO createLastCheckPointDBO(String bpId) {
        return new LastCheckPointDBOImpl(mDBType, bpId);
    }

    /**
     * creates state DBObject
     *
     * @param id state id
     *
     * @return StateDBO state DBObject
     */
    public StateDBO createStateDBO(String id) {
        return new StateDBOImpl(mDBType, id);
    }

    /**
     * creats variable DBObject
     *
     * @param bpId BPID
     *
     * @return VariableDBO variable DBObject
     */
    public VariableDBO createVariableDBO(String bpId) {
        return new VariableDBOImpl(mDBType, bpId);
    }

    /**
     * creats variable DBObject for xsd simple type variables
     *
     * @param bpId BPID
     *
     * @return VariableDBO variable DBObject
     */
    public VariableDBO createSimpleVariableDBO(String bpId) {
        return new SimpleVariableDBOImpl(mDBType, bpId);
    }
    
    /**
     * creates Scope DBObject
     * @param bpId
     * @return
     */
    public ScopeDBO createScopeDBO(String bpId) {
        return new ScopeDBOImpl(mDBType, bpId);
    }
    
    public DBObject createWaitingIMADBO(String id, String partnerLinkName, String operationName) {
        return new WaitingIMADBOImpl(mDBType, id, partnerLinkName, operationName);
    }

    /**
     * Special DBO constructed to update the dangling instances of the failed engine.
     * @param engineId
     * @param engineExirationInterval
     * @param recoveryBatchSize TODO
     * @return
     */
    public DBObject createupdateDanglingInstancesDBO(String engineId, long engineExirationInterval, int recoveryBatchSize) {
        return new UpdateDanglingInstancesDBOImpl(mDBType, engineId, engineExirationInterval, recoveryBatchSize);
    }
    
    public CRMPDBO createCRMPDBO(String crmpInvId, boolean isClusteredInvoke) {
    	return new CRMPDBOImpl(mDBType, crmpInvId, isClusteredInvoke);
    }
    
    public CRMPDBO createCRMPDBO(String bpId, String crmpInvId, String partnerLink,
    			String operation, String bpelMesgExchange) {
    	return new CRMPDBOImpl(mDBType, bpId, crmpInvId, partnerLink, 
    				operation, bpelMesgExchange);
    }
    
    public CRMPDBO createCRMPDBO(String bpId, String partnerLink, String operation,
    			String bpelMesgExchange, long replyVarId, char[] responseObj) throws SQLException {
    		return new CRMPDBOImpl(mDBType, bpId, partnerLink, operation,
    					bpelMesgExchange, replyVarId, responseObj);
    }
    
    /**
     *  
     * @param bpId
     * @return
     */
    public QueryCRMPDBO createQueryCRMPDBO(String bpId) {
    	return new QueryCRMPDBOImpl(mDBType, bpId);
    }
    
    /**
     * return result set of the Response object for the crmpInvokeId.
     * @param crmpInvId
     * @return
     */
    public QueryRespCRMPDBO createQueryRespCRMPDBO(String crmpInvId) {
    	return new QueryRespCRMPDBOImpl(mDBType, crmpInvId);
    }
    
    public EventHandlerDBO createEventHandlerDBO(String bpId) {
        return new EventHandlerDBOImpl(mDBType, bpId);
    }
    
    public EventHandlerDBO createEventHandlerDBO(String bpId, String id, 
            String scopeId, long eventId, String mUpdateStatus, Timestamp date, Long repeatEvery) {
        return new EventHandlerDBOImpl(mDBType, bpId, id, scopeId, eventId, mUpdateStatus, 
                date, repeatEvery);
    }

    /**
     * creats variable DBObject
     *
     * @param bpId BPID
     *
     * @return PartnerLinkDBO variable DBObject
     */
    public PartnerLinkDBO createPartnerLinkDBO(String bpId) {
        return new PartnerLinkDBOImpl(mDBType, bpId);
    }
    
    /**
     * creates variable DBObject
     *
     * @param bpid BPID
     * @param id ID
     * @param scopeGuid TODO
     * @param b characters
     * @return PartnerLinkDBO PartnerLinkDBO DBObject
     *
     * @throws SerialException SerialException
     * @throws SQLException SQLException
     */
    public PartnerLinkDBO createPartnerLinkDBO(String bpid, long id, String scopeGuid, char[] b)
        throws SerialException, SQLException {
        return new PartnerLinkDBOImpl(mDBType, bpid, id, scopeGuid, b);
    }
    
    public OutstandingMsgExDBO createOutstandingMsgExDBO(String msgExchId, String instanceId, String bpelid) {
        return new OutstandingMsgExDBOImpl(mDBType, msgExchId, instanceId, bpelid);
    }

    public DBObject createOutstandingMsgExDBO(String msgExId, String bpelid) {
        return new OutstandingMsgExDBOImpl(mDBType, msgExId, bpelid);
    }
}
