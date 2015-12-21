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
 * @(#)StateManagerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist.impl;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.sql.BatchUpdateException;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.sql.rowset.serial.SerialException;
import javax.transaction.SystemException;
import javax.transaction.TransactionManager;
import javax.xml.namespace.QName;

import com.sun.bpel.model.Activity;
import com.sun.bpel.model.Pick;
import com.sun.bpel.model.meta.RActivityHolder;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RMessagingElement;
import com.sun.bpel.model.meta.RPartnerLink;
import com.sun.bpel.model.meta.RStartElement;
import com.sun.bpel.model.meta.RVariable;
import com.sun.jbi.engine.bpel.core.bpel.connection.AbstractDBConnection;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBUnavailableException;
import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;
import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObjectFactory;
import com.sun.jbi.engine.bpel.core.bpel.dbo.EventHandlerDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.ForEachDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.InstanceCorrelationDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.LastCheckPointDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.OutstandingMsgExDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.PartnerLinkDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.QueryCRMPDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.ScopeDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.StateDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.VariableDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.VariableScalabilityDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.WaitingIMADBO;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessInstance;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELProcessManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.CorrelationManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.ICallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainer;
import com.sun.jbi.engine.bpel.core.bpel.engine.MessageContainerFactory;
import com.sun.jbi.engine.bpel.core.bpel.engine.RecoveredCallFrame;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelatingSAInComingEventKeyImpl;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationDefnValues;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationVal;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.ActivityUnit;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.Fault;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.FaultHandlingContext;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimePartnerLink;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.WSMessage;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.BPELProcessInstanceImpl;
import com.sun.jbi.engine.bpel.core.bpel.model.runtime.impl.PickUnitImpl;
import com.sun.jbi.engine.bpel.core.bpel.persist.MutableState;
import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.RecoveredState;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;
import com.sun.jbi.engine.bpel.core.bpel.persist.StateManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.TransactionInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.State.EventState;
import com.sun.jbi.engine.bpel.core.bpel.util.DOMHelper;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

/**
 * State Manager is not thread safe, and hence should never contain any instance specific
 * information. In other words shouldn't contain any thread unsafe member variables.
 * 
 * @author Sun Microsystems
 */
public class StateManagerImpl implements StateManager {

    private static final Logger LOGGER = Logger.getLogger(StateManagerImpl.class.getName());

    private static final String SCAL_PASSVTED_INSTS_FOR_PROCESS_QUERY = "SELECT a.STATEID FROM  " + //$NON-NLS-1$
            PersistenceDBSchemaCreation.STATE + " a, " + PersistenceDBSchemaCreation.OUTSTANDINGMSGEX + //$NON-NLS-1$ 
            " b WHERE a. status <> '" + StateDBO.COMPLETE_STATUS + "' and a.STATEID = b.STATEID and a.BPELID = ?"; //$NON-NLS-1$
    
    private DBObjectFactory mDBOFactory;

    /** DBConnectionFactory */
    protected DBConnectionFactory mDBFactory;

    /**
     * constructor
     * 
     * @param eng Engine
     * @param factory DBConnectionFactory factory
     */
    public StateManagerImpl(Engine eng, DBConnectionFactory factory) {
        mDBFactory = factory;
        mDBOFactory = DBObjectFactory.getDBObjectFactory(factory.getType());
    }

    class DBOsForRecovery {
        List<VariableDBO> varDBOs = new ArrayList<VariableDBO>();
        List<VariableDBO> varSimpleDBOs = new ArrayList<VariableDBO>();
        List<LastCheckPointDBO> lcpDBOs = new ArrayList<LastCheckPointDBO>();
        List<InstanceCorrelationDBO> corrDBOs = new ArrayList<InstanceCorrelationDBO>();
        List<ScopeDBO> scopeDBOs = new ArrayList<ScopeDBO>();
        List<ForEachDBO> foreachDBOs = new ArrayList<ForEachDBO>();
        List<QueryCRMPDBO> queryCRMPDBOs = new ArrayList<QueryCRMPDBO>();
        List<EventHandlerDBO> evntHndlrDBOs = new ArrayList<EventHandlerDBO>();
        List<PartnerLinkDBO> pLinkDBOs = new ArrayList<PartnerLinkDBO>();

        void getDBOsForRecovery(String stateId, AbstractDBConnection dbConn) throws Exception {
            DBObject tempVarDBO = mDBOFactory.createVariableDBO(stateId);
            DBObject tempSimpleVarDBO = mDBOFactory.createSimpleVariableDBO(stateId);
            DBObject tempLcpDBO = mDBOFactory.createLastCheckPointDBO(stateId);
            DBObject tempInstCorrDBO = mDBOFactory.createInstanceCorrelationDBO(stateId);
            DBObject tempScopeDBOs = mDBOFactory.createScopeDBO(stateId);
            DBObject tempForEachDBO = mDBOFactory.createForEachDBO(stateId);
            DBObject tempQueryCRMPDBO = mDBOFactory.createQueryCRMPDBO(stateId);
            DBObject tempEvntHndlrDBO = mDBOFactory.createEventHandlerDBO(stateId);
            DBObject tempPLinkDBO = mDBOFactory.createPartnerLinkDBO(stateId);

            varDBOs = (List<VariableDBO>) dbConn.getRow(tempVarDBO);
            varSimpleDBOs = (List<VariableDBO>) dbConn.getRow(tempSimpleVarDBO);
            varDBOs.addAll(varSimpleDBOs);
            lcpDBOs = (List<LastCheckPointDBO>) dbConn.getRow(tempLcpDBO);
            corrDBOs = dbConn.getRow(tempInstCorrDBO);
            scopeDBOs = dbConn.getRow(tempScopeDBOs);
            foreachDBOs = dbConn.getRow(tempForEachDBO);
            queryCRMPDBOs = dbConn.getRow(tempQueryCRMPDBO);
            pLinkDBOs = (List<PartnerLinkDBO>) dbConn.getRow(tempPLinkDBO);
            evntHndlrDBOs = (List<EventHandlerDBO>) dbConn.getRow(tempEvntHndlrDBO);
        }
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.StateManager#getCallframe(int)
     */
    public Collection getCallframe(BPELProcessManager processManager,
                                   String bpInstanceId,
                                   Engine eng) throws Exception {
    	if (LOGGER.isLoggable(Level.FINE)) {
    		LOGGER.log(Level.FINE, I18n.loc("BPCOR-3035: trying to retrieve state - Id = {0}", bpInstanceId)); //$NON-NLS-1$
    	}

        // List<ICallFrame>
        List retVal = new ArrayList();
        BPELProcessInstance processInstance = null;

        DBObject stateDBO = mDBOFactory.createStateDBO(bpInstanceId);
        List varDBOs = null;
        List varSimpleDBOs = null;
        List<LastCheckPointDBO> lcpDBOs = null;
        List corrDBOs = null;
        List ssDBOs = null;
        List scopeDBOs = null;        
        List foreachDBOs = null;
        List queryCRMPDBOs = null;
        List<PartnerLinkDBO> pLinkDBOs = null;
        List<EventHandlerDBO> evntHndlrDBOs = null;

        boolean retry;
        do {
        	retry = false;
        	AbstractDBConnection dbConn = null;
        	try {
        		dbConn = mDBFactory.createNonXAConnection();

        		List objs = dbConn.getRow(stateDBO);
        		if ((objs != null) && (objs.size() == 1)) {
        			stateDBO = (DBObject) objs.get(0);
        		} else {
        			throw new RuntimeException(I18n.loc("BPCOR-6071: invalid persistent state")); //$NON-NLS-1$
        		}
                
                DBOsForRecovery dbosForRecovery = new DBOsForRecovery();
                dbosForRecovery.getDBOsForRecovery(bpInstanceId, dbConn);

                varDBOs = dbosForRecovery.varDBOs;

                varSimpleDBOs = dbosForRecovery.varSimpleDBOs;
                // varDBOs can't be null or empty
                varDBOs.addAll(varSimpleDBOs);
                // lcpDBOs can't be null or empty
                lcpDBOs = dbosForRecovery.lcpDBOs;
                corrDBOs = dbosForRecovery.corrDBOs;
                scopeDBOs = dbosForRecovery.scopeDBOs;
                foreachDBOs = dbosForRecovery.foreachDBOs;
                queryCRMPDBOs = dbosForRecovery.queryCRMPDBOs;
                pLinkDBOs = dbosForRecovery.pLinkDBOs;
                evntHndlrDBOs = dbosForRecovery.evntHndlrDBOs;
                
                List<EventHandlerDBO> tmpEvntHndlrDBOs = new ArrayList<EventHandlerDBO>();
                tmpEvntHndlrDBOs.addAll(evntHndlrDBOs);
                
                while (tmpEvntHndlrDBOs.size() > 0) {
                    List<EventHandlerDBO> tmpEvntHndlrDBOsWithinLoop = new ArrayList<EventHandlerDBO>();
                    for (EventHandlerDBO evDBO : tmpEvntHndlrDBOs) {
                        String ehID = evDBO.getId();
                        DBOsForRecovery ehDBOsForRecovery = new DBOsForRecovery();
                        ehDBOsForRecovery.getDBOsForRecovery(ehID, dbConn);

                        varDBOs.addAll(ehDBOsForRecovery.varDBOs);
                        varSimpleDBOs.addAll(ehDBOsForRecovery.varSimpleDBOs);
                        varDBOs.addAll(varSimpleDBOs);
                        lcpDBOs.addAll(ehDBOsForRecovery.lcpDBOs);
                        corrDBOs.addAll(ehDBOsForRecovery.corrDBOs);
                        scopeDBOs.addAll(ehDBOsForRecovery.scopeDBOs);
                        foreachDBOs.addAll(ehDBOsForRecovery.foreachDBOs);
                        queryCRMPDBOs.addAll(ehDBOsForRecovery.queryCRMPDBOs);
                        pLinkDBOs.addAll(ehDBOsForRecovery.pLinkDBOs);
                        evntHndlrDBOs.addAll(ehDBOsForRecovery.evntHndlrDBOs);
                        
                        tmpEvntHndlrDBOsWithinLoop.addAll(ehDBOsForRecovery.evntHndlrDBOs);
                    }
                    tmpEvntHndlrDBOs = new ArrayList<EventHandlerDBO>();
                    tmpEvntHndlrDBOs.addAll(tmpEvntHndlrDBOsWithinLoop);;
                }
                
        	} catch(SQLException sEx) {
    			if (mDBFactory.doConnectionRetry(dbConn, sEx)) {
    				retry = true;
    			} else {
    				throw new RuntimeException(sEx);
    			}

        	} finally {
        		if (dbConn != null) {
        			try {
        				dbConn.close();
        			} catch (SQLException connCloseErr) {
        				LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6072: Exception thrown when closing a connection"), 
        						connCloseErr);
        			}
        		}
        	}
        } while (retry);

        
        ICallFrame rootFrame = null;

        try {
            // populate the state object
            if (stateDBO == null) {
                throw new RuntimeException(I18n.loc("BPCOR-6073: couldn't construct the state state table " + 
                		"didn't provide the required information")); //$NON-NLS-1$
            }
            //Check the state again, since this method could be called by BPELProcessManager#recreateInstance when the 
            //instance is passivated and recreated
            if (!(((StateDBO) stateDBO).getStatus().equals(StateDBO.RUNNING_STATUS) || ((StateDBO) stateDBO).getStatus().equals(StateDBO.SUSPENDED_STATUS)))  {
                return null;                
            }
            
            //Check the state bpId, exit if it is not the same as the process Manager,
            if (!((StateDBO) stateDBO).getBPELId().toString().equals(processManager.getBPELProcess().getBPELId().toString())) {
                return null;
            }

            if (// no checkpoints are persisted, but other process data is...
            ((lcpDBOs == null) || (lcpDBOs.size() == 0))
                    && ((varDBOs == null) || ((varDBOs != null) && (varDBOs.size() > 0)))
                    && ((corrDBOs == null) || ((corrDBOs != null) || (corrDBOs.size() > 0)))
                    && ((ssDBOs == null) || ((ssDBOs != null) || (ssDBOs.size() > 0)))
                    && ((foreachDBOs == null) || ((foreachDBOs != null) || (foreachDBOs.size() > 0)))
                    && ((scopeDBOs == null) || ((scopeDBOs != null) && (scopeDBOs.size() > 0)))
                    && ((evntHndlrDBOs == null) || ((evntHndlrDBOs != null) && (evntHndlrDBOs.size() > 0)))
                    && ((pLinkDBOs == null) || ((pLinkDBOs != null) && (pLinkDBOs.size() > 0)))) {
                throw new RuntimeException(I18n.loc("BPCOR-6074: DB state could have been corrupted, no last " + 
                		"check point found but other state information is present")); //$NON-NLS-1$
            }

            StateDBO sDBO = (StateDBO) stateDBO;
            String engId = sDBO.getEngineId();
            //String bpelId = sDBO.getBPELId();
            QName bpelId = sDBO.getBPELId();
            String status = sDBO.getStatus();

            if (LOGGER.isLoggable(Level.FINER)) {
                LOGGER.log(Level.FINER, I18n.loc("BPCOR-3036: state object - Id = {0} engine Id  = {1} bpel Id = {2}", 
                		bpInstanceId, engId, bpelId.toString()));
            }

            RecoveredState info = null;
            if (lcpDBOs != null) {
                info = new RecoveryStateInfo(processManager, eng, bpInstanceId,
                                             lcpDBOs, scopeDBOs, foreachDBOs, 
                                             varDBOs, evntHndlrDBOs, pLinkDBOs,
                                             status);
                Object[] vals = CallFrameFactory.getInstance().getCallFrames(info);

                if ((vals == null) || (vals.length < 3)) {
                    throw new RuntimeException(I18n.loc("BPCOR-6073: couldn't construct the state state table " + 
                    		"didn't provide the required information")); //$NON-NLS-1$
                }

                rootFrame = (ICallFrame) vals[0];
                retVal = (List) vals[1];
                processInstance = (BPELProcessInstance) vals[2];
            }

            List<CorrelationVal> corrIDs = new ArrayList<CorrelationVal>();
            if (corrDBOs != null) {
                InstanceCorrelationDBO instCorrDBO = null;
                CorrelationVal corrVal;
                for (int i = 0, size = corrDBOs.size(); i < size; i++) {
                    instCorrDBO = (InstanceCorrelationDBO) corrDBOs.get(i);
                    corrVal = new CorrelationVal(instCorrDBO.getId(), instCorrDBO.getValue());
                    corrIDs.add(corrVal);
                }
                processManager.getCorrMgr().associateInstanceDuringRecovery(corrIDs,
                        processInstance);
            }

            // Calculation of correlation values and associating with instances
            // for IMA is done post persistence. There is possibility system
            // crashes before this logic is executed.
            for (Object frame : retVal) {
                if (!(frame instanceof RecoveredCallFrame)) {
                    continue;
                }
                ActivityUnit pc = ((ICallFrame) frame).getProgramCounter();
                Activity activity = pc.getStaticModelActivity();
                RStartElement act = null;
                //Receive or OnEvent
                if(activity instanceof RStartElement){
                    act = (RStartElement) activity;
                } else if(activity instanceof Pick){
                    RActivityHolder ah =((PickUnitImpl)pc).getChildActHolder();
                    //if it is OnAlarm no processing required
                    if(ah instanceof RStartElement){
                        act = (RStartElement)ah;
                    }
                }
                if (act != null) {
                    CorrelationManager corrMgr = processManager.getCorrMgr();
                    WSMessage wsMesg = pc.getContext().getRuntimeVariable(act.getRVariable()).getWSMessage();
                    // calculate the correlation values
                    CorrelationDefnValues requestCorrVals = corrMgr.calculateCorrValues(wsMesg, act.getCorrelationDefnWrapper(),
                            processInstance);
                    List<CorrelationVal> createIDs = getOnlyCreateIdsNotRecovered(requestCorrVals.getCreateIDs(), corrIDs);
                    CorrelationDefnValues requestCorrVals1 = new CorrelationDefnValues(createIDs, requestCorrVals.getJoinIDs());
                    corrMgr.checkAndAssociateCorrValuesWithInstance(requestCorrVals1, processInstance);
                }
            }
            /*
             * the QueryCRMPDBO is recovered for the purpose of populating the CRMPUpdateList in the
             * ProcessInstance this list that is retrieved is the list of entries in the CRMP table
             * that do not have a response object set. This list will be looked up to update the
             * entries when the reply activty is persisted.
             */
            if (queryCRMPDBOs != null) {
                QueryCRMPDBO qCRMPDBO = null;
                for (int i = 0, size = queryCRMPDBOs.size(); i < size; i++) {
                    qCRMPDBO = (QueryCRMPDBO) queryCRMPDBOs.get(i);
                    String bpId = qCRMPDBO.getBPId();
                    String partnerlink = qCRMPDBO.getPartnerLink();
                    String oper = qCRMPDBO.getOperation();
                    String crmpUpdateListValue = bpId + partnerlink + oper;
                    String crmpInvokeId = qCRMPDBO.getCRMPInvokeId();
                    long replyVarId = qCRMPDBO.getReplyVariableId();
                    String msgExch = qCRMPDBO.getBpelMessageExchange();
                    
                    BPELProcessInstanceImpl instImpl = (BPELProcessInstanceImpl) processInstance;
                    instImpl.addToCRMPUpdateList(crmpUpdateListValue);

                    if (replyVarId == -1) {
                        RBPELProcess proc = instImpl.getBPELProcessManager().getBPELProcess();
                        RStartElement actStart = proc.getStartElement(partnerlink, oper, msgExch);
                        RVariable rVar = actStart.getRVariable();
                        RuntimeVariable runVar = instImpl.getRuntimeVariable(rVar);

                        MessageContainer con = MessageContainerFactory.createMessage(msgExch, runVar.getWSMessage(), crmpInvokeId, null);
                        instImpl.getBPELProcessManager().addCRMPReqForRecoveringInsts(crmpUpdateListValue, con);
                    }
                }
            }
        } catch (Throwable t) {
            LOGGER.log(
                    Level.WARNING,
                    I18n.loc("BPCOR-6168: failed during recovery of the instance") 
                    + bpInstanceId,
                    t); //$NON-NLS-1$
            throw new BPELRuntimeException(t);
        }

        // state = rootFrame.getProcessInstance().getState();\
        if (LOGGER.isLoggable(Level.FINE)) {
        	LOGGER.log(Level.FINE, I18n.loc("BPCOR-3036: state object - Id = {0} engine Id  = {1} bpel Id = {2}", 
        			bpInstanceId, processManager.getBPELProcess().getBPELId().toString(), eng.getId()));
        }

        return retVal; // TODO
    }

    private List<CorrelationVal> getOnlyCreateIdsNotRecovered(List<CorrelationVal> createCorrVals,
            List<CorrelationVal> recoveredCorrVals) {
        if (createCorrVals.isEmpty() || recoveredCorrVals.isEmpty()) {
            return createCorrVals;
        }

        List<CorrelationVal> tempList = new ArrayList<CorrelationVal>();
        for (CorrelationVal corrVal : createCorrVals) {
            if (!recoveredCorrVals.contains(corrVal)) {
                tempList.add(corrVal);
            }
        }
        return tempList;
    }
    
    /**
     * @see StateManager#persistState(com.sun.jbi.engine.bpel.core.bpel.persist.State)
     */
    public boolean persistState(State state, TransactionInfo transactionInfo, BPELProcessInstance instance) {
    	
    	boolean returnFlag = false;
    	
    	if (instance != null) {
    		instance.acquirePersistenceLock(false);
    	}
        try {
            
        	if (state.getUpdateMode() == State.Mode.INSERT) {
        		returnFlag =  insertState(state, transactionInfo);
            } else if (state.getUpdateMode() == State.Mode.UPDATE 
            		|| state.getUpdateMode() == State.Mode.SUSPEND || state.getUpdateMode() == State.Mode.RESUME) {
            	returnFlag =  updateState(state, transactionInfo);
            } else if (state.getUpdateMode() == State.Mode.DELETE) {
            	returnFlag = deleteState(state, transactionInfo);
            } 
            
        } catch (IOException ioe) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6075: failed to persist"), ioe); //$NON-NLS-1$
            throw new RuntimeException(ioe);
        } catch (SQLException se) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6075: failed to persist"), se); //$NON-NLS-1$
            throw new RuntimeException(se);
        } catch (SystemException se) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6075: failed to persist"), se); //$NON-NLS-1$
            throw new RuntimeException(se);
        } finally {
        	if (instance != null) {
        		instance.releasePersistenceLock(false);
        	}
        }
        return returnFlag;
    }
   

    private boolean insertState(State state, TransactionInfo transactionInfo) 
    throws SystemException, SerialException, SQLException {

        if (transactionInfo.isOnlyTxOperation()) {
            performDBTransactionForDBOs(state, null, transactionInfo);
            return true;
        } else {

            InsertUpdateDeleteDBOs insertUpdateDeleteDBOs = getDBOsForInsert(state);
            performDBTransactionForDBOs(state, insertUpdateDeleteDBOs, transactionInfo);

            if (LOGGER.isLoggable(Level.FINE)) {
            	LOGGER.log(Level.FINE, I18n.loc("BPCOR-3037: inserted state - Id = {0}  bpelId = {1} engineId = {2}", 
            			state.getId(), state.getBPELId().toString(), state.getEngineId()));
            }

            return true;
        }
    }

    private boolean updateState(State state, TransactionInfo transactionInfo) 
    throws SystemException, SQLException, IOException {

        if (transactionInfo.isOnlyTxOperation()) {
            performDBTransactionForDBOs(state, null, transactionInfo);
            return true;
        } else {

            InsertUpdateDeleteDBOs insertUpdateDeleteDBOs = getDBOsForUpdate(state);
            performDBTransactionForDBOs(state, insertUpdateDeleteDBOs, transactionInfo);
            if (LOGGER.isLoggable(Level.FINE)) {
            	LOGGER.log(Level.FINE, I18n.loc("BPCOR-3038: updated state - Id = {0} bpelId = {1} engineId = {2}", 
            			state.getId(), state.getBPELId().toString(), state.getEngineId()));
            }

            return true;
        }
    }

    private boolean deleteState(State state, TransactionInfo transactionInfo) throws SystemException {

    	if (LOGGER.isLoggable(Level.FINE)) {
    		LOGGER.log(Level.FINE, I18n.loc("BPCOR-3039: deleting state - Id = {0} bpelId = {1} engineId = {2}", 
    				state.getId(), state.getBPELId(), state.getEngineId().toString()));
    	}

        String stateId = state.getId();

        // construct stateDBO
        DBObject stateDBO = mDBOFactory.createStateDBO(stateId, state.getBPELId(),
                state.getEngineId());

        InsertUpdateDeleteDBOs insertUpdateDeleteDBOs = new InsertUpdateDeleteDBOs();
        insertUpdateDeleteDBOs.addDeleteDBO(stateDBO);

        performDBTransactionForDBOs(state, insertUpdateDeleteDBOs, transactionInfo);

        if (LOGGER.isLoggable(Level.FINE)) {
        	LOGGER.log(Level.FINE, I18n.loc("BPCOR-3040: deleted state - Id = {0} bpelId = {1} engineId = {2}", 
        			state.getId(), state.getBPELId(), state.getEngineId()));
        }

        return true;
    }

    /**
     * The various conditions of XA Transaction processing is handled here.
     * 1. TransactionType.XAParticipate: Here the Activity involved (Inbound messaging activities like receive, 
     * OnMesage(pick), etc) participates in the inbound transaction initiated by the calling component.
     * Here we call resume on the TM with the inbound Tranaction obj, do the Activity's unit of work(persistence)
     * and then call suspend on the TM. If there is an error on doing the unit of work, we setRollbackOnly() and 
     * call suspend on the TM. Based on these responses either a DONE or ERROR status will be send back(for InOnly ME)
     * to the calling component. The calling component would either commit or rollback based on the DONE or ERROR.
     * The TxSynchronizer registered for this Tx would get notified and the logical processing continues for this instance.
     * 
     * 2.TransactionType.XACloseConnection: State that is set by the activities that participate in the inbound transaction
     * after the ExcutionState.WaitingForTxComplete processing is complete (TxSynchronizer.afterCompletion() is called by the TM 
     * and the instance is queued for processing). Here the state.finishedPersisting() is called to clear the state obj of all
     * references.
     *  
     * 3.TransactionType.XAStartNew: Called from the activities performing Outbound InOnly ME(one-way invoke activity).
     * Begin is called on TM for starting new TX(the activity associates this new TX with the outbound ME). Suspend is called on
     * TM to suspend the new TX (for intermediate state of waiting for the STATUS of the InOnly ME).
     * 
     * 4.TransactionType.XAEnd: Called if the Outbound InOnly ME partner component returned a DONE status. The activity performs
     * its transactional unit of work and either chooses to commit or rollback based on the outcome of the unit of work, as we are 
     * the initiator and coordinator of the TX.
     * 
     * 5.TransactionType.XACancel: Called if the Outbound InOnly ME partner component returned a ERROR status. In this case 
     * since the partner is in error, the TX is rolled back as we are the initiator and coordinator of the TX
     * 
     * 6.TransactionType.Local: This is used by persistence units of work which do not participate in inter-component XA enabled
     * ME's. TODO: this could very well be changed in future iterations as using a XA connection is not required for these units
     * or work and could potentially have a performance effect.
     * 
     * @param state
     * @param insertUpdateDeleteDBOs
     * @param transactionInfo
     * @throws Exception
     */
    private void performDBTransactionForDBOs(State state,
                                             InsertUpdateDeleteDBOs insertUpdateDeleteDBOs,
                                             TransactionInfo transactionInfo) throws SystemException {

    	/* TM should never be null, Error handling for this should be done in the lifecycle class
    	* where its set in the Registry
    	*/
        String stateId = state.getId();
//    	TransactionManager tm = (TransactionManager) BPELSERegistry.getInstance().lookup(
//                TransactionManager.class.getName());
        
        if (StateManager.TransactionType.XAParticipate.equals(transactionInfo.getTransactionType())) {
        	AbstractDBConnection xadbConnection = null;
        	try {
//        		tm.resume(transactionInfo.getTransaction());
//        		transactionInfo.getTransaction().registerSynchronization(
//        				transactionInfo.getSynchronization());

        		xadbConnection = mDBFactory.createXAConnection();
        		performDBOperationForDBOs(insertUpdateDeleteDBOs, xadbConnection);
                if (LOGGER.isLoggable(Level.FINEST)) {
                	String insertActIds = insertUpdateDeleteDBOs.getActIdForInserts();
                	String updateActIds = insertUpdateDeleteDBOs.getActIdForUpdates();
                	String deleteActIds = insertUpdateDeleteDBOs.getActIdForDeletes();
                	LOGGER.log(Level.FINEST, 
                			I18n.loc("BPCOR-3041: TX type XAParticipates performed activity with stateid = {0} " + 
                					"Inserted = {1} updated = {2}, deleted = {3}", 
                					stateId, insertActIds, updateActIds, deleteActIds));                	
                }
        		
//       			tm.suspend();
        	} catch (Exception pExcep) { 
                printSQLBatchExceptions(pExcep);
        		// Just in case the connection is stale, validate it. This will cause the connection to be removed
        		// from the DataSource (sun impl of DataSource only)
        		mDBFactory.validateXAConnection(xadbConnection);
//        		transactionInfo.getTransaction().setRollbackOnly();
                if (LOGGER.isLoggable(Level.FINEST)) {
                	String insertActIds = insertUpdateDeleteDBOs.getActIdForInserts();
                	String updateActIds = insertUpdateDeleteDBOs.getActIdForUpdates();
                	String deleteActIds = insertUpdateDeleteDBOs.getActIdForDeletes();
                    LOGGER.log(Level.FINEST, 
                    		I18n.loc("BPCOR-3042: TX type XAParticipates performed rollbackonly with stateid = {0} " + 
                    				"Inserted = {1} updated = {2}, deleted = {3}", 
                    				stateId, insertActIds, updateActIds, deleteActIds));
                	
                }
//        		tm.suspend();
        		throw new RuntimeException(
        				I18n.loc("BPCOR-6076: Exception thrown while processing transaction type XAParticipate"), 
        				pExcep);
        	} finally {
                //We should not release lock on State object, it needs to be release from finishPersistence.PersistenceManager()
                //state.clearPC();

        		if (xadbConnection != null) {
        			try {
        				xadbConnection.close();
        			} catch(SQLException ex) {
        				LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6077: TX type XAParticipates, exception " + 
        						"thrown when trying to close connection"), ex);
        			}
        		}
        	}

        } else if (StateManager.TransactionType.XAStartNew.equals(transactionInfo.getTransactionType())) {
/*        	try {
        		tm.begin();
        		transactionInfo.setTransaction(tm.getTransaction());
        		tm.suspend();
        	} catch (Exception startExcep) {
        		throw new RuntimeException(I18n.loc("BPCOR-6078: Exception thrown while processing " + 
        				"transaction type XAStartNew: {0}"), startExcep);
        	}*/

        } else if (StateManager.TransactionType.XAEnd.equals(transactionInfo.getTransactionType())) {
        	AbstractDBConnection xadbConnection = null;
            boolean sucess = false;
            try {
//            	tm.resume(transactionInfo.getTransaction());

            	xadbConnection = mDBFactory.createXAConnection();
            	performDBOperationForDBOs(insertUpdateDeleteDBOs, xadbConnection);
//            	transactionInfo.getTransaction().commit();
                sucess = true;
                if (LOGGER.isLoggable(Level.FINEST)) {
                	String insertActIds = insertUpdateDeleteDBOs.getActIdForInserts();
                	String updateActIds = insertUpdateDeleteDBOs.getActIdForUpdates();
                	String deleteActIds = insertUpdateDeleteDBOs.getActIdForDeletes();
                    LOGGER.log(Level.FINEST, I18n.loc("BPCOR-3043: TX type XAEnd commited with stateid = {0} " + 
                    		"Inserted = {1} updated = {2}, deleted = {3}", 
                    		stateId, insertActIds, updateActIds, deleteActIds));
                	
                }
            } catch (Exception endExcep) {
                printSQLBatchExceptions(endExcep);
            	// Just in case the connection is stale, validate it. This will cause the connection to be removed
        		// from the DataSource (sun impl of DataSource only)
        		boolean connOK = mDBFactory.validateXAConnection(xadbConnection);
            	// Suspend the transaction and then throw the appropriate exception which would call the condition
            	// XACanel that will perform the rollback of the transaction.
//                tm.suspend();
                if (LOGGER.isLoggable(Level.FINEST)) {
                	String insertActIds = insertUpdateDeleteDBOs.getActIdForInserts();
                	String updateActIds = insertUpdateDeleteDBOs.getActIdForUpdates();
                	String deleteActIds = insertUpdateDeleteDBOs.getActIdForDeletes();
                    LOGGER.log(Level.FINEST, I18n.loc("BPCOR-3044: TX type XAEnd rolledback with stateid = " + 
                    		"{0} Inserted = {1} updated = {2}, deleted = {3}", 
                    		stateId, insertActIds, updateActIds, deleteActIds));
                	
                }
                
                // if the connection was bad we hold here and go into a retry pattern until the DB becomes available 
                // again or until the engine is shutdown. After a valid connection is obtained, code throws exception 
                // and the activity that called with TransactionType.XAEnd (invoke for now) will call back with 
                // TransactionType.XACancel. After the XA is cancelled, invoke puts itself back on Ready To Run Q.
                if (!connOK) {
                	if (mDBFactory.doConnectionRetryForXA()) {
                		throw new DBUnavailableException();
                	}
                }
                
            	throw new RuntimeException(
            			I18n.loc("BPCOR-6079: Exception thrown while processing transaction type XAEnd"), endExcep);
            
            } finally {
                if (sucess){ // if not sucessful release the lock in the cancel
                    //We should release the lock on State object we are done with State object
                    state.clearPC();
                }
        		if (xadbConnection != null) {
        			try {
        				xadbConnection.close();
        			} catch(SQLException ex) {
        				LOGGER.log(Level.WARNING, 
        						I18n.loc("BPCOR-6080: TX type XAEnd, exception thrown when trying to close connection"), 
        						ex);
        			}
        		}
        		// This has to be called even if persisting was unsuccessful.
            }

        } else if (StateManager.TransactionType.XACancel.equals(transactionInfo.getTransactionType())) {
        	try {
//        		tm.resume(transactionInfo.getTransaction());
//        		transactionInfo.getTransaction().rollback();
                if (LOGGER.isLoggable(Level.FINEST)) {
                    LOGGER.log(Level.FINEST, 
                    		I18n.loc("BPCOR-3045: TX type XACancel rolledback with stateid = {0}", stateId));
                	
                }
                // Do not need to do any clean up of the state as updateState is never
                // called for the XACancel condition.
                //state.finishedPersisting();
        	} catch (Exception cancelExcep) {
            	throw new RuntimeException(
            			I18n.loc("BPCOR-6081: Exception thrown while processing transaction type XACancel"), 
            			cancelExcep);
            } finally {
                //We should release the lock on State object we are done with State object
                //but it would not work if it is the first check-point after FLOW
                //we need to fix bug id -- 295
                state.clearPC();
            }
        } else if (StateManager.TransactionType.Local.equals(transactionInfo.getTransactionType())) {
        	boolean retry;
        	do {
        		retry = false;
        		Connection conn = null;
            	AbstractDBConnection dbConn = null;
                boolean sucess = false;
        		try {
        			dbConn = mDBFactory.createNonXAConnection();
        			conn = dbConn.getUnderlyingConnection();
        			conn.setAutoCommit(false);
        			performDBOperationForDBOs(insertUpdateDeleteDBOs, dbConn);
                    conn.commit();
                    sucess = true;
        			if (LOGGER.isLoggable(Level.FINEST)) {
        				String insertActIds = insertUpdateDeleteDBOs.getActIdForInserts();
        				String updateActIds = insertUpdateDeleteDBOs.getActIdForUpdates();
        				String deleteActIds = insertUpdateDeleteDBOs.getActIdForDeletes();
        				LOGGER.log(Level.FINEST, I18n.loc("BPCOR-3046: Local non-transactional connection commit " + 
        						"with stateid = {0} Inserted = {1} updated = {2}, deleted = {3}", 
        						stateId, insertActIds, updateActIds, deleteActIds));

        			}
        		} catch (Exception localExcp) {
                    printSQLBatchExceptions(localExcp);
        			if (conn != null) {
        				try {
        					conn.rollback();
        					if (LOGGER.isLoggable(Level.FINEST)) {
        						String insertActIds = insertUpdateDeleteDBOs.getActIdForInserts();
        						String updateActIds = insertUpdateDeleteDBOs.getActIdForUpdates();
        						String deleteActIds = insertUpdateDeleteDBOs.getActIdForDeletes();
        						LOGGER.log(Level.FINEST, I18n.loc("BPCOR-3047: Local non-transactional connection " + 
        								"rollback with stateid = {0} Inserted = {1} updated = {2}, deleted = {3}", 
        								stateId, insertActIds, updateActIds, deleteActIds));

        					}

        				} catch (SQLException rolbackExcp) {
        					LOGGER.log(Level.WARNING, 
        							I18n.loc("BPCOR-6082: Exception occured when trying to rollback on local non-tranactional connection"),
        							rolbackExcp);
        				}
        			}
        			
        			if (mDBFactory.doConnectionRetry(dbConn, localExcp)) {
        				retry = true;
        			} else {
        				throw new RuntimeException(I18n.loc("BPCOR-6083: Exception thrown while processing " + 
        						"local non-transactional connection"), localExcp);
        			}
        			
        		} finally {
        			if (dbConn != null) {
        				try {
        					dbConn.close(); // wrapper will set the initial value of the setAutoCommit field.
        				} catch (SQLException closeExcp) {
        					LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6084: Exception occured when trying to " + 
        							"close the local non-tranactional connection"), 
        							closeExcp);
        				}
                    }
                    if (sucess){
                        //We should release the lock on State object we are done with State object
                        state.clearPC();
        			}
        		}
        	} while (retry);
        }
    }

    private void performDBOperationForDBOs(InsertUpdateDeleteDBOs insertUpdateDeleteDBOs,
                                           AbstractDBConnection dbCon) throws SQLException {
        
        if (LOGGER.isLoggable(Level.FINE)) {
            insertUpdateDeleteDBOs.log();
        }
        
        // Persist to database
        Object object = null;
        
        ScalabilityVariableDBOs scalabilityVariableDBOs = insertUpdateDeleteDBOs.getScalabilityVariableDBOs();
        
        if (scalabilityVariableDBOs != null) {
            List deleteVarsList = scalabilityVariableDBOs.getPersistedVarsForDeleteDBOs();
            if (deleteVarsList != null && deleteVarsList.size() > 0) {
                dbCon.delete((List) deleteVarsList);
            }

            List updateVarsList = scalabilityVariableDBOs.getPassivatedVarsForUpdateDBOs();
            if (updateVarsList != null && updateVarsList.size() > 0) {
                dbCon.update((List) updateVarsList);
            }
        }
        
        
        for (int i = 0, size = insertUpdateDeleteDBOs.getInsertDBOs().size(); i < size; i++) {
            object = insertUpdateDeleteDBOs.getInsertDBOs().get(i);

            if (object instanceof List) {
                dbCon.insert((List) object);
            } else {
                dbCon.insert((DBObject) object);
            }
        }

        object = null;

        for (int i = 0, size = insertUpdateDeleteDBOs.getUpdateDBOs().size(); i < size; i++) {
            object = insertUpdateDeleteDBOs.getUpdateDBOs().get(i);

            if (object instanceof List) {
                dbCon.update((List) object);
            } else {
                dbCon.update((DBObject) object);
            }
        }

        object = null;

        for (int i = 0, size = insertUpdateDeleteDBOs.getDeleteDBOs().size(); i < size; i++) {
            object = insertUpdateDeleteDBOs.getDeleteDBOs().get(i);

            if (object instanceof List) {
                dbCon.delete((List) object);
            } else {
                dbCon.delete((DBObject) object);
            }
        }

    }

    private InsertUpdateDeleteDBOs getDBOsForInsert(State state) throws SerialException, SQLException {
    	if (LOGGER.isLoggable(Level.FINE)) {
    		LOGGER.log(Level.FINE, I18n.loc("BPCOR-3048: inserting state - Id = {0}  bpelId = {1} engineId = {2}", 
    				state.getId(), state.getBPELId(), state.getEngineId()));
    	}

        String stateId = state.getId();

        // construct stateDBO
        DBObject stateOrEveHdlrDBO = getEHDBOForInsert(state, stateId);
        if (stateOrEveHdlrDBO == null) { 
            stateOrEveHdlrDBO = mDBOFactory.createStateDBO(stateId, state.getBPELId(),
                    state.getEngineId());
        }

        // construct LastCheckPointDBO
        Long[] pc = state.getPC();
        Timestamp date = state.getTimerValue();
        Long pickCompositeActId = state.getPickCompositeActId();
        Long branchInvokeCounter = state.getBranchInvokeCounter();
        DBObject lcpDBO = mDBOFactory.createLastCheckPointDBO(stateId, pc[1], pc[0], date,
                pickCompositeActId, branchInvokeCounter);

        if (LOGGER.isLoggable(Level.FINER)) {
            LOGGER.log(Level.FINER, I18n.loc("BPCOR-3049: last check point value - old Activity Id = {0} - " + 
            		"new Activity Id = {1} timer Value = {2}" , 
            		pc[0], pc[1], ((date != null) ? date.toString() : null)));
        }

        // construct variableDBO to insert.
        DBObject varDBO = null;
        Collection<StateVariableWrapper> varsInsert = state.getVariables(false); /* for_update= */
        RuntimeVariable varInsert = null;
        StateVariableWrapper wrapperInsertVariable = null;
        String insertVarStateId;
        List<DBObject> varDBOs;
        List<DBObject> varSimpleDBOs;
        List<DBObject> scalabilityUpdateVarDBOs;

        varDBOs = new ArrayList<DBObject>();
        varSimpleDBOs = new ArrayList<DBObject>();
        scalabilityUpdateVarDBOs = new ArrayList<DBObject>();

        for (Iterator<StateVariableWrapper> itr = varsInsert.iterator(); itr.hasNext();) {
        	wrapperInsertVariable = itr.next();
        	varInsert = wrapperInsertVariable.getVar();
        	String scopeId = varInsert.getScopeGuid();
        	if (scopeId == null) {
        		// check for the forEach counter variable
        		// it should not be in the list to persist
        		// TODO: remove this when the scope persistence is done.
        		continue;
        	}
        	insertVarStateId = wrapperInsertVariable.getStateId();
        	RVariable varDef = varInsert.getVariableDef();
            if (!varInsert.isInserted() && !varInsert.isPersisted() && varInsert.isPassivated()) {
        		/*
        		 * Scenario B.1 (FFT) (refer design document for details)
        		 * Since the mIsPassivated flag is true, and this is first time persistence
        		 * for the variable. Since the variable is scalability passivated, the passivated
        		 * value flag will be updated to 'N'. The isPassivated flag in memory 
        		 * will still remain TRUE. When this variable is needed (during read), the persisted value 
        		 * will be read.
        		 */
        		varDBO = getDBOForPassivatedVariableUpdate(stateId, varDef.getUniqueId(), scopeId);
        		scalabilityUpdateVarDBOs.add(varDBO);
        		continue;
            }
            
            Object value = varInsert.getSerializedValue();
        	if (varInsert.isSimpleType()) {
        		varDBO = mDBOFactory.createSimpleVariableDBO(insertVarStateId, varDef.getUniqueId(),
        				value, scopeId);
        		varSimpleDBOs.add(varDBO);
        	} 
        	else {
        		varDBO = mDBOFactory.createVariableDBO(insertVarStateId, varDef.getUniqueId(),
        				(value == null) ? null : ((String) value).toCharArray(), scopeId);
        		varDBOs.add(varDBO);
        	}
//      	varDBOs.add(varDBO);

        	if (LOGGER.isLoggable(Level.FINER)) {
        		LOGGER.log(Level.FINER, I18n.loc("BPCOR-3050: Variable that needs to be inserted, name = {0} id = {1}", 
        				varInsert.getVariableDef().getName(), new Long(varInsert.getVariableDef().getUniqueId())));
        	}
        }

        List<DBObject> pLinkDBOs = getPLinksForInsert(state);
        
        List<InstanceCorrelationDBO> corrDBOs = getCorrDBOs(state, stateId);
        List<DBObject> foreachDBOs = getForEachDBOs(state, stateId, false);

        DBObject crmpDBO = getCRMPStateDBO(state, stateId);
        
        List<DBObject> insertScopeDBOs = getScopeDBOs(state, stateId, false);

        InsertUpdateDeleteDBOs insertUpdateDeleteDBOs = new InsertUpdateDeleteDBOs();
        
        ScalabilityVariableDBOs scalabilityVariableDBOs = null;
        if (scalabilityUpdateVarDBOs.size() > 0) {
            scalabilityVariableDBOs = new ScalabilityVariableDBOs();
            scalabilityVariableDBOs.addUpdateDBO(scalabilityUpdateVarDBOs);
            insertUpdateDeleteDBOs.setScalabilityVariableDBOs(scalabilityVariableDBOs);
        }
        insertUpdateDeleteDBOs.addInsertDBO(stateOrEveHdlrDBO);
        insertUpdateDeleteDBOs.addInsertDBO(lcpDBO);
        insertUpdateDeleteDBOs.addInsertDBOs(varDBOs);
        insertUpdateDeleteDBOs.addInsertDBOs(varSimpleDBOs);
        insertUpdateDeleteDBOs.addInsertDBOs(pLinkDBOs);
        insertUpdateDeleteDBOs.addInsertDBOs(corrDBOs);
        insertUpdateDeleteDBOs.addInsertDBOs(foreachDBOs);
        if (crmpDBO != null) {
            insertUpdateDeleteDBOs.addInsertDBO(crmpDBO);
        }
        insertUpdateDeleteDBOs.addInsertDBOs(insertScopeDBOs);

        return insertUpdateDeleteDBOs;
    }

    private List<DBObject> getPLinksForUpdate(State state) throws SQLException {
        // construct PartnerLinkDBO for update.
        DBObject pLinkDBO = null;
        Collection<StatePartnerLinkWrapper> pLinksUpdate = state.getPartnerLinks(true); /* for_update= */
        RuntimePartnerLink pLinkUpdate = null;
        StatePartnerLinkWrapper wrapperUpdatePLink = null;
        String updatePLinkStateId;        

        List<DBObject> updatePLinkDBOs = new ArrayList<DBObject>();

        for (Iterator<StatePartnerLinkWrapper> itr = pLinksUpdate.iterator(); itr.hasNext();) {
            wrapperUpdatePLink = itr.next();
            updatePLinkStateId = wrapperUpdatePLink.getStateId();
            pLinkUpdate = wrapperUpdatePLink.getPLink();
            String value = pLinkUpdate.getSerializedValue();
            RPartnerLink pLinkDef = (RPartnerLink) pLinkUpdate.getStaticModel();
            
                pLinkDBO = mDBOFactory.createPartnerLinkDBO(updatePLinkStateId, pLinkDef.getUniqueId(),
                        pLinkUpdate.getScopeGuid(), (value == null) ? null : ((String) value).toCharArray());
                updatePLinkDBOs.add(pLinkDBO);

            if (LOGGER.isLoggable(Level.FINER)) {
                LOGGER.finer(I18n.loc("BPCOR-3051: Variable that needs to be updated, name = {0} id = {1}", 
                		pLinkDef.getName(), pLinkDef.getUniqueId()));
            }
        }
        return updatePLinkDBOs;
    }
    
    private List<DBObject> getPLinksForInsert(State state) throws SerialException, SQLException {
        // construct partnerLinkDBO to insert.
        DBObject pLinkDBO = null;
        Collection<StatePartnerLinkWrapper> pLinksInsert = state.getPartnerLinks(false); /* for_update= */
        RuntimePartnerLink pLinkInsert = null;
        StatePartnerLinkWrapper wrapperInsertPartnerLink = null;
        String insertPLinkStateId;
        List<DBObject> pLinkDBOs = new ArrayList<DBObject>();

        for (Iterator<StatePartnerLinkWrapper> itr = pLinksInsert.iterator(); itr.hasNext();) {
            wrapperInsertPartnerLink = itr.next();
            pLinkInsert = wrapperInsertPartnerLink.getPLink();
            insertPLinkStateId = wrapperInsertPartnerLink.getStateId();
            String value = pLinkInsert.getSerializedValue();
            RPartnerLink pLinkDef = (RPartnerLink) pLinkInsert.getStaticModel();

            pLinkDBO = mDBOFactory.createPartnerLinkDBO(insertPLinkStateId, 
                    pLinkDef.getUniqueId(),
                    pLinkInsert.getScopeGuid(), (value == null) ? null : ((String) value).toCharArray());
            pLinkDBOs.add(pLinkDBO);

            if (LOGGER.isLoggable(Level.FINER)) {
                LOGGER.log(Level.FINER, I18n.loc("BPCOR-3050: Variable that needs to be inserted, name = {0} id = {1}", 
                		pLinkDef.getName(), new Long(pLinkDef.getUniqueId())));
            }
        }
        return pLinkDBOs;
    }

    private List<DBObject> getScopeDBOs (State state, String stateId, boolean for_update)
    	throws SQLException {
        List<DBObject> dbos = new ArrayList<DBObject>();
        Collection<FaultHandlingContext> scopes = state.getScopeStates(for_update);

        for(FaultHandlingContext scope: scopes) {
        	
        	Fault fault = scope.getFault();
        	QName faultName = null;
            WSMessage wsMsg = null;        	
        	if (fault != null) {
                faultName = fault.getName();
                wsMsg = fault.getData();
        	}
        	char[] faultData = 
        		(wsMsg != null) ? DOMHelper.createXmlString(wsMsg.getElement()).toCharArray() 
        				        : null;
        		ScopeDBO scopedbo = mDBOFactory.createScopeDBO(scope.getStateId(), 
        			scope.getScopeId(),	scope.getScopeGuid(),
        			scope.getParentScopeGuid(), scope.getScopeState(), 
        			scope.getCompensateId(), scope.getCompletionOrder(), 
        			faultName, faultData, scope.getFaultActivityId());	
        	dbos.add(scopedbo);	
        }
        return dbos;
    }
    
    /*
     * used to return the list of ScopeDBO objects that have the state as 
     * compensated (FaulthandlingContext.COMPENSATED). These have to be 
     * deleted from the Scope table.
     */
    /*private List<DBObject> getScopeDBOsForDelete (State state, String stateId) 
    	throws SQLException {
        List<DBObject> dbos = new ArrayList<DBObject>();
        
        // get the Compensated Scopes from the update list.
        Collection<FaultHandlingContext> scopes = state.getScopeStates(true);
    	
        //get the Compensated scopes from the insert list.
        Collection<FaultHandlingContext> insertScopes = state.getScopeStates(false);
        if (!insertScopes.isEmpty()) {
        	scopes.addAll(insertScopes); 
        }
        
        for (FaultHandlingContext scope: scopes) {
        	String scopeState = scope.getScopeState();
        	if (scopeState != null && scopeState.equals(FaultHandlingContext.COMPENSATED)) {
            	Fault fault = scope.getFault();
            	QName faultName = null;
                WSMessage wsMsg = null;        	
            	if (fault != null) {
                    faultName = fault.getName();
                    wsMsg = fault.getData();
            	}
            	char[] faultData = 
            		(wsMsg != null) ? DOMHelper.createXmlString(wsMsg.getElement()).toCharArray() 
            				        : null;
            		ScopeDBO scopedbo = mDBOFactory.createScopeDBO(stateId, 
            			scope.getScopeId(),	scope.getScopeGuid(),
            			scope.getParentScopeGuid(), scopeState, 
            			scope.getCompensateId(), scope.getCompletionOrder(), 
            			faultName, faultData, scope.getFaultActivityId());	
            	dbos.add(scopedbo);	
        	}
        }
        return dbos;
    }*/
    

    private InsertUpdateDeleteDBOs getDBOsForUpdateForVariableChangeState(State state) 
    throws SerialException, SQLException, IOException {
    	if (LOGGER.isLoggable(Level.FINE)) {
    		LOGGER.log(Level.FINE, 
    				I18n.loc("BPCOR-3053: updating variable change state - Id = {0} bpelId = {1} engineId = {2}", 
    						state.getId(), state.getBPELId(), state.getEngineId()));
    	}
        String stateId = state.getId();

         // construct variableDBO to insert.
        DBObject varDBO = null;
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        varDBO = null;

        Collection<StateVariableWrapper> varsUpdate = state.getVariables(true); /* for_update= */
        RuntimeVariable varUpdate = null;
        bos = new ByteArrayOutputStream();

        // List<DBObject>
        List<DBObject> updateVarDBOs;
        List<DBObject> updateSimpleVarDBOs;
        List<DBObject> scalabilityDeleteVarDBOs;
        List<DBObject> scalabilityUpdateVarDBOs;

        try {
            updateVarDBOs = new ArrayList<DBObject>();
            updateSimpleVarDBOs = new ArrayList<DBObject>();
            scalabilityDeleteVarDBOs = new ArrayList<DBObject>();
            scalabilityUpdateVarDBOs = new ArrayList<DBObject>();
            
            for (Iterator<StateVariableWrapper> itr = varsUpdate.iterator(); itr.hasNext();) {
                varUpdate = itr.next().getVar();
                String scopeId = varUpdate.getScopeGuid();
                if (scopeId == null) {
                	// check for the forEach counter variable
                	// it should not be in the list to persist
                	// TODO: remove this when the scope persistence is done.
                	continue;
                }

                RVariable varDef = varUpdate.getVariableDef();

                if (varUpdate.isInserted() && !varUpdate.isPersisted() && varUpdate.isPassivated()) {
                	/* Scenario B.3 (TFT) (refer design document for details)
                	 * Since the variable is already inserted in the database
                	 * during last persistence point and since updated (dirty)
                	 * and was later passivated; we need to do the following
					 * DELETE: The persisted variable 
					 * UPDATE: The passivated variable SCALABILITYFLAG changed to 'N' in the database
					 * 
					 * NOTE: mIsInserted and mIsPersisted will be set true in
					 * when the persistence is completed (see
					 * finishedPersisting() in StateImpl)
					 */
                	varDBO = getBDOForPersistedVariableDelete(stateId, varDef.getUniqueId(), scopeId);
                	scalabilityDeleteVarDBOs.add(varDBO);

                	varDBO = getDBOForPassivatedVariableUpdate(stateId, varDef.getUniqueId(), scopeId);
    				scalabilityUpdateVarDBOs.add(varDBO);
    				continue;
                }
                
                Object value = varUpdate.getSerializedValue();
                if (varUpdate.isSimpleType()) {
                    varDBO = mDBOFactory.createSimpleVariableDBO(stateId, varDef.getUniqueId(), 
                    		value, scopeId);
                    updateSimpleVarDBOs.add(varDBO);
                } 
                else {
                    varDBO = mDBOFactory.createVariableDBO(stateId, varDef.getUniqueId(),
                            (value == null) ? null : ((String) value).toCharArray(), scopeId);
                    updateVarDBOs.add(varDBO);
                }

                if (LOGGER.isLoggable(Level.FINER)) {
                	LOGGER.finer(I18n.loc("BPCOR-3051: Variable that needs to be updated, name = {0} id = {1}", 
                			varUpdate.getVariableDef().getName(), varUpdate.getVariableDef().getUniqueId()));
                }

                bos.reset();
            }

            varDBO = null;
        } finally {
            bos.close();
        }

        List<List<DBObject>> updateVals = new ArrayList<List<DBObject>> ();

        updateVals.add(updateVarDBOs);
        updateVals.add(updateSimpleVarDBOs);

        InsertUpdateDeleteDBOs inserUpdateDeleteDBOs = new InsertUpdateDeleteDBOs();
        ScalabilityVariableDBOs scalabilityVariableDBOs = null;
        if (scalabilityDeleteVarDBOs.size() > 0 || scalabilityUpdateVarDBOs.size() > 0) {
            scalabilityVariableDBOs = new ScalabilityVariableDBOs();
            scalabilityVariableDBOs.addDeleteDBO(scalabilityDeleteVarDBOs);
            scalabilityVariableDBOs.addUpdateDBO(scalabilityUpdateVarDBOs);
            inserUpdateDeleteDBOs.setScalabilityVariableDBOs(scalabilityVariableDBOs);
        }
        
        
        inserUpdateDeleteDBOs.setScalabilityVariableDBOs(scalabilityVariableDBOs);
        inserUpdateDeleteDBOs.addUpdateDBOs(updateVals);
 
        return inserUpdateDeleteDBOs;
    }

    
    /**
     * Delete the persisted variable in the database. This is used
     * during update of a variable in the database during regular 
     * persisted when the variable if found passivated. 
     *  
     * @param stateId
     * @param varId
     * @return
     * @throws SerialException
     * @throws SQLException
     */
    private DBObject getBDOForPersistedVariableDelete(String stateId, long varId, String scopeGuid) throws SerialException, SQLException {
        return mDBOFactory.createVariableDBO(stateId, varId, scopeGuid);
    }
    
    /**
     * Refer to design document. This refers to the case wherein 
     * the variable was scalability passivated prior to first 
     * persistence.  
     * The value in memory is null and the isPassivated flag as TRUE. 
     * The passivated variable's  SCALABILITYPASSIVATED in database 
     * changed to 'N'
     * 
     * @param stateId
     * @param varId
     * @param scopeGuid
     * @return
     * @throws SerialException
     * @throws SQLException
     */

    private DBObject getDBOForPassivatedVariableUpdate(String stateId, long varId, String scopeGuid) throws SerialException, SQLException {
    	return mDBOFactory.createVariableScalabilityDBO(stateId, varId, scopeGuid);
    }
    
    private InsertUpdateDeleteDBOs getDBOsForUpdate(State state) throws SQLException, IOException {
    	if (LOGGER.isLoggable(Level.FINE)) {
    		LOGGER.log(Level.FINE, I18n.loc("BPCOR-3054: updating state - Id = {0} bpelId = {1} engineId = {2}", 
    				state.getId(), state.getBPELId(), state.getEngineId()));
    	}
    	if (state instanceof VariableChangeStateImpl) {
    		return getDBOsForUpdateForVariableChangeState (state);
    	}
        String stateId = state.getId();


        // If the suspend/resume occurs, state needs to be updated, only the suspendResumeDBO will be returned
        DBObject suspendResumeDBO = null;
		if (state.getUpdateMode() == State.Mode.SUSPEND) {
			suspendResumeDBO = mDBOFactory.createSuspendResumeStateDBO(stateId,
					true);
		} else if (state.getUpdateMode() == State.Mode.RESUME) {
			suspendResumeDBO = mDBOFactory.createSuspendResumeStateDBO(stateId,
					false);
		}
		if (suspendResumeDBO != null) {
			InsertUpdateDeleteDBOs inserUpdateDeleteDBOs = new InsertUpdateDeleteDBOs();
			inserUpdateDeleteDBOs.addUpdateDBO(suspendResumeDBO);
			return inserUpdateDeleteDBOs;
		}
       
        // construct LastCheckPointDBO
        Long[] pc = state.getPC();
        Timestamp date = state.getTimerValue();
        Long pickCompositeActId = state.getPickCompositeActId();
        Long branchInvokeCounter = state.getBranchInvokeCounter();
        DBObject lcpDBO = mDBOFactory.createLastCheckPointDBO(stateId, pc[1], pc[0], date,
                pickCompositeActId, branchInvokeCounter);

        if (LOGGER.isLoggable(Level.FINER)) {
            LOGGER.log(Level.FINER, I18n.loc("BPCOR-3049: last check point value - old Activity Id = {0} - new " + 
            		"Activity Id = {1} timer Value = {2}", pc[0], pc[1], ((date != null) ? date.toString() : null)));
        }

        Long[] invalidPCs = state.getInvalidPCs();

        // List<DBObject>
        List toDeleteLCPDBOs = new ArrayList();
        DBObject deleteLCPDBO = null;

        for (int i = 0, length = invalidPCs.length; i < length; i++) {
            deleteLCPDBO = mDBOFactory.createLastCheckPointDBO(stateId, invalidPCs[i], null,
                    (Timestamp) null, null, null);
            toDeleteLCPDBOs.add(deleteLCPDBO);
        }

        // construct variableDBO to insert.
        DBObject varDBO = null;

        Collection<StateVariableWrapper> varsInsert = state.getVariables(false); /* for_update= */
        //RuntimeVariable varInsert = null;
        StateVariableWrapper wrapperInsertVariable = null;
        String insertVarStateId;

        // List<DBObject>
        List<DBObject> insertVarDBOs;
        List<DBObject> insertSimpleVarDBOs;
        List<DBObject> scalabilityDeleteVarDBOs;
        List<DBObject> scalabilityUpdateVarDBOs;

        insertVarDBOs = new ArrayList<DBObject>();
        insertSimpleVarDBOs = new ArrayList<DBObject>();
        scalabilityDeleteVarDBOs = new ArrayList<DBObject>();
        scalabilityUpdateVarDBOs = new ArrayList<DBObject>();

        for (Iterator<StateVariableWrapper> itr = varsInsert.iterator(); itr.hasNext();) {
        	wrapperInsertVariable = itr.next();
        	RuntimeVariable varInsert = wrapperInsertVariable.getVar();
        	String scopeId = varInsert.getScopeGuid();
        	if (scopeId == null) {
        		// check for the forEach counter variable
        		// it should not be in the list to persist
        		// TODO: remove this when the scope persistence is done.
        		continue;
        	}

        	insertVarStateId = wrapperInsertVariable.getStateId();

        	RVariable varDef = varInsert.getVariableDef();
            if (!varInsert.isInserted() && !varInsert.isPersisted() && varInsert.isPassivated()) {
            	/* Scenario B.1 (FFT) (refer design document for details)
            	 * Since the mIsPassivated flag is true, and this is first time persistence
            	 * for the variable. Following will be done
            	 * UPDATE: The passivated variable's SCALABILITYFLAG flag changed to 'N'
            	 * NOTE: mIsInserted and mIsPersisted will be set true in
            	 * when the persistence is completed (see finishedPersisting() in StateImpl) 
            	 */
				varDBO = getDBOForPassivatedVariableUpdate(stateId, varDef.getUniqueId(), scopeId);
				scalabilityUpdateVarDBOs.add(varDBO);
				continue;
            }
            
            Object value = varInsert.getSerializedValue();
        	if (varInsert.isSimpleType()) {
        		// TODO: Needs to be converted SimpleVariableDBO
        		varDBO = mDBOFactory.createSimpleVariableDBO(insertVarStateId, 
        				varDef.getUniqueId(), value, scopeId);
        		insertSimpleVarDBOs.add(varDBO);
        	} 
        	else {
        		varDBO = mDBOFactory.createVariableDBO(insertVarStateId, varDef.getUniqueId(),
        				(value == null) ? null : ((String) value).toCharArray(), scopeId);
        		insertVarDBOs.add(varDBO);
        	}


        	if (LOGGER.isLoggable(Level.FINER)) {
        		LOGGER.finer(I18n.loc("BPCOR-3050: Variable that needs to be inserted, name = {0} id = {1}", 
        				varInsert.getVariableDef().getName(), varInsert.getVariableDef().getUniqueId()));
        	}
        }

        varDBO = null;

        // construct variableDBO for update.
        Collection<StateVariableWrapper> varsUpdate = state.getVariables(true); /* for_update= */
        //RuntimeVariable varUpdate = null;
        StateVariableWrapper wrapperUpdateVariable = null;
        String updateVarStateId;        

        // List<DBObject>
        List<DBObject> updateVarDBOs;
        List<DBObject> updateSimpleVarDBOs;

        updateVarDBOs = new ArrayList<DBObject>();
        updateSimpleVarDBOs = new ArrayList<DBObject>();

        for (Iterator<StateVariableWrapper> itr = varsUpdate.iterator(); itr.hasNext();) {
        	wrapperUpdateVariable = itr.next();
        	updateVarStateId = wrapperUpdateVariable.getStateId();
        	RuntimeVariable varUpdate = wrapperUpdateVariable.getVar();
        	String scopeId = varUpdate.getScopeGuid();
        	if (scopeId == null) {
        		// check for the forEach counter variable
        		// it should not be in the list to persist
        		// TODO: remove this when the scope persistence is done.
        		continue;
        	}

            RVariable varDef = varUpdate.getVariableDef();
            
            if (varUpdate.isInserted() && !varUpdate.isPersisted() && varUpdate.isPassivated()) {
            	/* Scenario B.3 (TFT) (refer design document for details)
            	 * Since the variable is already inserted in the database
            	 * during last persistence point and since updated (dirty)
            	 * and was later passivated; we need to do the following
            	 * DELETE: The persisted variable
            	 * UPDATE: The passivated variable SCALABILITYFLAG changed to 'N' 
            	 * in the database
            	 * 
            	 * NOTE: mIsInserted and mIsPersisted will be set true in
            	 * when the persistence is completed (see finishedPersisting() in StateImpl) 
            	 */
            	varDBO = getBDOForPersistedVariableDelete(stateId, varDef.getUniqueId(), scopeId);
            	scalabilityDeleteVarDBOs.add(varDBO);

            	varDBO = getDBOForPassivatedVariableUpdate(stateId, varDef.getUniqueId(), scopeId);
				scalabilityUpdateVarDBOs.add(varDBO);
				continue;
			}

            Object value = varUpdate.getSerializedValue();
        	if (varUpdate.isSimpleType()) {
        		varDBO = mDBOFactory.createSimpleVariableDBO(updateVarStateId, 
        				varDef.getUniqueId(), value, scopeId);
        		updateSimpleVarDBOs.add(varDBO);
        	} else {
        		varDBO = mDBOFactory.createVariableDBO(updateVarStateId, varDef.getUniqueId(),
        				(value == null) ? null : ((String) value).toCharArray(), scopeId);
        		updateVarDBOs.add(varDBO);
        	}
//      	updateVarDBOs.add(varDBO);

        	if (LOGGER.isLoggable(Level.FINER)) {
        		LOGGER.finer(I18n.loc("BPCOR-3051: Variable that needs to be updated, name = {0} id = {1}", 
        				varUpdate.getVariableDef().getName(), varUpdate.getVariableDef().getUniqueId()));
        	}
        }
        
        List<DBObject> updatePLinkDBOs = getPLinksForUpdate(state);
        List<DBObject> insertPLinkDBOs = getPLinksForInsert(state);
        
        List<InstanceCorrelationDBO> corrDBOs = getCorrDBOs(state, stateId);
        List<DBObject> insertScopeDBOs = getScopeDBOs(state, stateId, false);
        List<DBObject> updateScopeDBOs = getScopeDBOs(state, stateId, true);
        
        List<DBObject> insertForeachDBOs = getForEachDBOs(state, stateId, false);
        List<DBObject> updateForeachDBOs = getForEachDBOs(state, stateId, true);

        State.CRMPState crmp = state.getCRMPState();
        DBObject ehUpdateDBO = getEHDBOForUpdate(state, stateId);
        
        List insertVals = new ArrayList();
        List updateVals = new ArrayList();
        List deleteVals = new ArrayList();

        if (pc[0] == null) {
            insertVals.add(lcpDBO);
        } else {
            updateVals.add(lcpDBO);
        }

        insertVals.add(insertVarDBOs);
        insertVals.add(insertSimpleVarDBOs);
        insertVals.add(corrDBOs);
        insertVals.add(insertForeachDBOs);
        insertVals.add(insertPLinkDBOs);
        insertVals.add(insertScopeDBOs);

        if (crmp != null) {
            DBObject crmpDBO = getCRMPStateDBO(state, stateId);
            if (crmp.isUpdateForReply()) {
                updateVals.add(crmpDBO);
            } else {
                insertVals.add(crmpDBO);
            }
        }

        /*
         * if (toUpdateStateDBO != null) { updateVals.add(toUpdateStateDBO); if
         * (toInsertWaitingIMADBOs != null) { insertVals.add(toInsertWaitingIMADBOs); } }
         */
        updateVals.add(updateVarDBOs);
        updateVals.add(updateSimpleVarDBOs);
        updateVals.add(updateForeachDBOs);
        updateVals.add(updatePLinkDBOs);
        if (ehUpdateDBO != null) {
            updateVals.add(ehUpdateDBO);
        }
        updateVals.add(updateScopeDBOs);
        
        deleteVals.add(toDeleteLCPDBOs);
        //deleteVals.add(deleteScopeDBOs);

        InsertUpdateDeleteDBOs inserUpdateDeleteDBOs = new InsertUpdateDeleteDBOs();
        ScalabilityVariableDBOs scalabilityVariableDBOs = null;
        if (scalabilityDeleteVarDBOs.size() > 0 || scalabilityUpdateVarDBOs.size() > 0) {
            scalabilityVariableDBOs = new ScalabilityVariableDBOs();
            scalabilityVariableDBOs.addDeleteDBO(scalabilityDeleteVarDBOs);
            scalabilityVariableDBOs.addUpdateDBO(scalabilityUpdateVarDBOs);
            inserUpdateDeleteDBOs.setScalabilityVariableDBOs(scalabilityVariableDBOs);
        }
        
        inserUpdateDeleteDBOs.addInsertDBOs(insertVals);
        inserUpdateDeleteDBOs.addUpdateDBOs(updateVals);
        inserUpdateDeleteDBOs.addDeleteDBOs(deleteVals);

        return inserUpdateDeleteDBOs;
    }

    public void persistCorreations(MutableState state) {
        String bpId = state.getId();
        List<InstanceCorrelationDBO> corrDbos = getCorrDBOs((State) state, bpId);
        if (corrDbos.size() < 1) {
            return;
        }

        boolean retry;
        do {
            retry = false;
            Connection conn = null;
            AbstractDBConnection dbConn = null;

            try {
                dbConn = mDBFactory.createNonXAConnection();
                conn = dbConn.getUnderlyingConnection();
                conn.setAutoCommit(false);
                dbConn.insertCorrelationValues(corrDbos);
                conn.commit();
                state.removeCorrelations();
            } catch (SQLException ex) {
                printSQLBatchExceptions(ex);
                if (conn != null) {
                    try {
                        conn.rollback();
                    } catch (Exception se) {
                        LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
                    }
                }
                if (mDBFactory.doConnectionRetry(dbConn, ex)) {
                    retry = true;
                } else {
                    throw new RuntimeException(ex);
                }
            } finally {
                if (dbConn != null) {
                    try {
                        dbConn.close();
                    } catch (SQLException ex) {
                        LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
                    }
                }

            }
        } while (retry);
    }
	
    
    private synchronized List<InstanceCorrelationDBO> getCorrDBOs(State state, String stateId) {
        // construct correlationDBO to insert.
        InstanceCorrelationDBO corrDBO = null;
        Set corrsInsert = state.getCorrelations();
        List<InstanceCorrelationDBO> corrDBOs = new ArrayList<InstanceCorrelationDBO>();
        CorrelationVal corrVal = null;

        for (Iterator itr = corrsInsert.iterator(); itr.hasNext();) {
            corrVal = (CorrelationVal) itr.next();

            corrDBO = mDBOFactory.createInstanceCorrelationDBO(corrVal.getSetID(), stateId,
                    corrVal.toString());
            corrDBOs.add(corrDBO);

            if (LOGGER.isLoggable(Level.FINER)) {
                LOGGER.log(Level.FINER, I18n.loc("Correlation that will be inserted: Correlation Id = {0} value = {1}", 
                		new Long(corrVal.getSetID()), corrVal.toString()));
            }

        }
        return corrDBOs;
    }

    private List<DBObject> getForEachDBOs(State state, String stateId, boolean update) {
        List<DBObject> dbos = new ArrayList<DBObject>();
        Collection feds = state.getForEach();
        for (Iterator iter = feds.iterator(); iter.hasNext();) {
            ForEachState foreach = (ForEachState) iter.next();

            if (!foreach.isStarted() || // finished or not yet begun
                    !foreach.isDirty() || // not changed
                    update && !foreach.isInserted() || // don't update if not inserted
                    !update && foreach.isInserted()) { // avoid duplicate insert
                continue;
            }

            foreach.markClean();

            DBObject obj = mDBOFactory.createForEachDBO(stateId, foreach.getForEachId(),
                    foreach.getCounter(), foreach.getSuccesses(), foreach.getStartCount(),
                    foreach.getFinalCount(), foreach.getCompletionCount());
            dbos.add(obj);
        }

        return dbos;
    }

    private DBObject getCRMPStateDBO(State state, String stateId) throws SQLException {

        State.CRMPState crmp = state.getCRMPState();
        DBObject crmpDbo = null;
        if (crmp == null) {
            // happens when the reply doesn't update the state with CRMP info.
            return null;
        } else {
            boolean isUpdateForReply = crmp.isUpdateForReply();
            if (isUpdateForReply) {
                crmpDbo = mDBOFactory.createCRMPDBO(stateId, crmp.getPLink(), crmp.getOper(),
                        crmp.getBPELME(), crmp.getReplyVarId(), crmp.getRespObj());
            } else {
                crmpDbo = mDBOFactory.createCRMPDBO(stateId, crmp.getCrmpInvokeId(),
                        crmp.getPLink(), crmp.getOper(), crmp.getBPELME());
            }
        }
        return crmpDbo;
    }
    
    private DBObject getEHDBOForInsert(State state, String stateId) {
        if (!(state instanceof EventState)) {
            return null;
        }
        EventState eveState = (EventState) state;
        // the following line is for code readability.
        String ehId = stateId;
        String bpId = eveState.getBPStateId();
        String scopeId = eveState.getAncestorScopeId();
        long eventId = eveState.getEventModelId();
        Timestamp timer = eveState.getOnEventTimerValue();
        Long repeatEveryVal = eveState.getRepeatEveryVal();
        DBObject ehDBO = mDBOFactory.createEventHandlerDBO(bpId, ehId, scopeId, 
                eventId, EventHandlerDBO.INSERT_SATUS, timer, repeatEveryVal);
        return ehDBO;
    }
    
    private DBObject getEHDBOForUpdate(State state, String stateId) {
        if (!(state instanceof EventState)) {
            return null;
        }
        EventState eveState = (EventState) state;
        if (eveState.isUpdatedInDB()) {
            return null;
        }
        String ehId = stateId;
        String bpId = eveState.getBPStateId();
        String scopeId = eveState.getAncestorScopeId();
        long eventId = eveState.getEventModelId();
        Timestamp timer = eveState.getOnEventTimerValue();       
        Long repeatEveryVal = eveState.getRepeatEveryVal();
        DBObject ehDBO = mDBOFactory.createEventHandlerDBO(bpId, ehId, scopeId, 
                eventId, EventHandlerDBO.UPDATE_SATUS, timer, repeatEveryVal);
        return ehDBO;
    }
    
    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.StateManager#passivateInstance(java.lang.String, java.lang.String, java.util.Collection, com.sun.jbi.engine.bpel.core.bpel.persist.MutableState)
     */
    public void passivateInstance(String instanceId, String engineId, Collection messageActivities, MutableState state) {
        // construct stateDBO
        DBObject stateDBO = mDBOFactory.createStateDBO(instanceId, engineId, State.Mode.PASSIVATE);

        List<DBObject> startActivitiesDBOs = new ArrayList<DBObject>();
        DBObject waitingIMADBO = null;
        Iterator<RStartElement> iter = messageActivities.iterator();
        

        while (iter.hasNext()) {
            RStartElement activity = iter.next();
            waitingIMADBO = mDBOFactory.createWaitingIMADBO(instanceId,
                    activity.getRPartner().getName(), activity.getWSDLOperation().getName());
            startActivitiesDBOs.add(waitingIMADBO);
        }

        //List<InstanceCorrelationDBO> corrDbos = getCorrDBOs((State)state, state.getId());
        
        // do not move this remove correlations after the database commit, as once committed
        // another correlated receive can active the instance and correlation violation will happen
        // refer Issue https://open-jbi-components.dev.java.net/issues/show_bug.cgi?id=421
        //state.removeCorrelations();

        boolean retry;
        do {
        	retry = false;
        	Connection conn = null;
            AbstractDBConnection dbConn = null;
        	try {
        		dbConn = mDBFactory.createNonXAConnection();
        		conn = dbConn.getUnderlyingConnection();
        		conn.setAutoCommit(false);
        		dbConn.update(stateDBO);
        		//dbConn.insert(corrDbos);
        		dbConn.insert(startActivitiesDBOs);
                conn.commit();
        	} catch (Exception ex) {
                printSQLBatchExceptions(ex);
        		if (conn != null) {
        			try {
        				conn.rollback();
        			} catch (Exception se) {
        				LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
        			}
        		}
        		if (mDBFactory.doConnectionRetry(dbConn, ex)) {
    				retry = true;
    			} else {
    				throw new RuntimeException(ex);
    			}
        	} finally {
        		if (dbConn != null) {
        			try {
        				dbConn.close();
        			} catch (SQLException ex) {
        				LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
        			}
        		}
        	}
        } while (retry);
    }

    /**
     * To activate the instance. Called by upon expiration of timer in the business process instance
     * thread created for onAlram in cluster.
     * 
     * @param instanceId
     * @return
     */
    public int activateInstance(String instanceId, String engineId) {
    	// Timestamp date = new Timestamp(currentTime);
    	int updateCount = 0;
    	DBObject stateDBO = mDBOFactory.createStateDBO(instanceId, engineId, State.Mode.ACTIVATE);
    	boolean retry;
    	do {
    		retry = false;
    		AbstractDBConnection dbConn = null;
    		Connection conn = null;
    		try {
    			dbConn = mDBFactory.createNonXAConnection();
        		conn = dbConn.getUnderlyingConnection();
        		conn.setAutoCommit(false);
        		
    			// updateCount = dbCon.activateInstance(engId, instanceId, date);
    			updateCount = dbConn.update(stateDBO);
    			if (updateCount > 1) {
    				// throw exception
    			}
    			
    			conn.commit();
    			
    		} catch (Exception ex) {
        		if (conn != null) {
        			try {
        				conn.rollback();
        			} catch (Exception se) {
        				LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
        			}
        		}
        		
    			if (mDBFactory.doConnectionRetry(dbConn, ex)) {
    				retry = true;
    			} else {
    				throw new RuntimeException(ex);
    			}
    		} finally {
    			if (dbConn != null) {
    				try {
    					dbConn.close();
    				} catch (SQLException se) {
    					LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
    				}
    			}
    		}
    	} while (retry);

    	return updateCount;
    }


    /*
     * (non-Javadoc)
     * 
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.StateManager#activateInstances(java.lang.String,
     *      java.util.List)
     */
    public List activateInstancesAndUpdateOwnership(String engId, List eventsList) {

        List instancesList = new ArrayList();

        CorrelatingSAInComingEventKeyImpl event = null;
        RStartElement startElement = null;
        List corrIds = null;

        WaitingIMADBO waitingIMADBO = null;
        Iterator corrWaitingEventsIter = eventsList.iterator();

        boolean retry;
        do {
        	retry = false;
        	Connection conn = null;
            AbstractDBConnection dbConn = null;
        	try {
        		dbConn = mDBFactory.createNonXAConnection();
        		conn = dbConn.getUnderlyingConnection();
        		conn.setAutoCommit(false);

        		while (corrWaitingEventsIter.hasNext()) {
        			String instanceId = null;
        			event = (CorrelatingSAInComingEventKeyImpl) corrWaitingEventsIter.next();
        			startElement = event.getEventModel().getStartElement();

        			RMessagingElement messagingElement = (RMessagingElement) startElement;

        			waitingIMADBO = (WaitingIMADBO) mDBOFactory.createWaitingIMADBO(null,
        					messagingElement.getRPartner().getName(),
        					messagingElement.getWSDLOperation().getName());
        			corrIds = event.getCorrIds();

        			//TODO the following call to check the correlated passivated instance
        			// and instance ownership should be combined into one transaction. 
        			
        			instanceId = dbConn.findCorrelatedPassivatedInstance(corrIds, waitingIMADBO);

        			if (instanceId != null) {
        				DBObject stateDBO = mDBOFactory.createStateDBO(instanceId, engId, State.Mode.ACTIVATE);
        				int updateCount = dbConn.update(stateDBO);

        				// This check is done as it is possible that in the interim (while the 
        				// above findCorrelatedPassivatedInstance call might have returned instance,
        				// the instance is already acquired by another engine (for example due to on message)
        				// due to race condition. Hence checking the update count to be sure that
        				// the instance ownership was actually acquired.

        				if (updateCount == 1) {
        					if (LOGGER.isLoggable(Level.FINE)) {
        						LOGGER.log(Level.FINE, 
        								I18n.loc("BPCOR-3006: Instance Id {0} * Activated * by Engine {1}",
        								instanceId, engId));
        					}
        					instancesList.add(instanceId);
        				} else if (updateCount > 1) {
        					// exception condition
        				}
        			}
        		}

                conn.commit();

        	} catch (Exception ex) {
        		if (conn != null) {
        			try {
        				conn.rollback();
        			} catch (Exception se) {
        				LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
        			}
        		}
    			if (mDBFactory.doConnectionRetry(dbConn, ex)) {
    				retry = true;
    			} else {
    				throw new RuntimeException(ex);
    			}
        	} finally {
        		if (dbConn != null) {
        			try {
        				dbConn.close(); // wrapper will set the initial value of the setAutoCommit field.
        			} catch (SQLException se) {
        				LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
        			}
        		}
        	}
        } while(retry);
        return instancesList;
    }    
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.EngineStateManager#persistVariableForMemoryRelease(java.lang.String, java.util.Map)
     */
    public void passivateVariable(String stateId, long uniqueId, char[] chars, String scopeId) {
        try {
        	DBObject dbo = mDBOFactory.createVariableScalabilityDBO(stateId, uniqueId, chars, scopeId);      
            if (true) {
                boolean retry;
                do {
                    retry = false;
                    Connection conn = null;
                    AbstractDBConnection dbConn = null;
                    try {
                        dbConn = mDBFactory.createNonXAConnection();
                        conn = dbConn.getUnderlyingConnection();
                        conn.setAutoCommit(false);
                        dbConn.insert(dbo);
                        conn.commit();
                    } catch (Exception ex) {
                        printSQLBatchExceptions(ex);
                        if (conn != null) {
                            try {
                                conn.rollback();
                            } catch (Exception se) {
                                LOGGER.log(Level.WARNING,
                                		I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
                            }
                        }
                        if (mDBFactory.doConnectionRetry(dbConn, ex)) {
                            retry = true;
                        } else {
                            throw new RuntimeException(ex);
                        }
                    } finally {
                        if (dbConn != null) {
                            try {
                                dbConn.close();
                            } catch (SQLException ex) {
                                LOGGER.log(Level.WARNING,
                                		I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
                            }
                        }
                    }
                } while (retry);
            }
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.StateManager#loadPersistedVariable(java.lang.String, long, java.lang.String)
     */
    public Object loadPersistedVariable(String stateId, long uniqueId, String scopeId) {
    	List<DBObject> objs = null;
    	DBObject tempDbo = mDBOFactory.createVariableDBO(stateId, uniqueId, scopeId);

		boolean retry;
    	do {
    		retry = false;
    		Connection conn = null;
    		AbstractDBConnection dbConn = null;
    		try {
    			dbConn = mDBFactory.createNonXAConnection();
    			conn = dbConn.getUnderlyingConnection();
    			objs = dbConn.getRow(tempDbo);
    			if (objs.size() == 0) {
    				throw new RuntimeException("Errors during scalability. Persistence corrupt. Variable not found in database");
    			} else if (objs.size() > 1) {
    				throw new RuntimeException("Errors during scalability. Mutiple entries for same variable found ");
    			}
    		} catch (Exception ex) {
    			if (conn != null) {
    				try {
    					conn.rollback();
    				} catch (Exception se) {
    					LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
    				}
    			}
    			if (mDBFactory.doConnectionRetry(dbConn, ex)) {
    				retry = true;
    			} else {
    				throw new RuntimeException(ex);
    			}
    		} finally {
    			if (dbConn != null) {
    				try {
    					dbConn.close();
    				} catch (SQLException ex) {
    					LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
    				}
    			}
    		}
    	} while (retry);
    	
    	return ((VariableDBO)objs.get(0)).getValue();
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.StateManager#loadPassivatedVariable(java.lang.String, long, java.lang.String)
     */
    public Object loadPassivatedVariable(String stateId, long uniqueId, String scopeId) {
        List<DBObject> objs = null;
        DBObject tempDbo = mDBOFactory.createVariableScalabilityDBO(stateId, uniqueId, scopeId);
        
        boolean retry;
        do {
            retry = false;
            Connection conn = null;
            AbstractDBConnection dbConn = null;
            try {
                dbConn = mDBFactory.createNonXAConnection();
                conn = dbConn.getUnderlyingConnection();
                conn.setAutoCommit(false);
                objs = dbConn.getRow(tempDbo);
                if (objs.size() == 0) {
                    throw new RuntimeException("Scalability Persistence corrupt. Passivated variable not found ");
                } else if (objs.size() > 1) {
                    throw new RuntimeException("Scalability Persistence corrupt. Mutiple entries for same variable found ");
                }
                dbConn.delete(tempDbo);
                conn.commit();
            } catch (Exception ex) {
                if (conn != null) {
                    try {
                        conn.rollback();
                    } catch (Exception se) {
                        LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
                    }
                }
                if (mDBFactory.doConnectionRetry(dbConn, ex)) {
                    retry = true;
                } else {
                    throw new RuntimeException(ex);
                }
            } finally {
                if (dbConn != null) {
                    try {
                        dbConn.close();
                    } catch (SQLException ex) {
                        LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
                    }
                }
            }
        } while (retry);

        return ((VariableScalabilityDBO)objs.get(0)).getValue();
    }
    
    public void deletePassivatedVariable(String stateId, long uniqueId, String scopeId) {
    	
    	List<DBObject> objs = null;
    	DBObject dbo = null;
    	try {
    		dbo = mDBOFactory.createVariableScalabilityDBO(stateId, uniqueId, scopeId);
    	} catch (Exception ex) {
    		throw new RuntimeException(ex);
    	}
    	
    	boolean retry;
    	do {
    		retry = false;
    		Connection conn = null;
    		AbstractDBConnection dbConn = null;
    		try {
    			dbConn = mDBFactory.createNonXAConnection();
    			conn = dbConn.getUnderlyingConnection();
    			conn.setAutoCommit(false);
				dbConn.delete(dbo);
				conn.commit();
    		} catch (Exception ex) {
    			if (conn != null) {
    				try {
    					conn.rollback();
    				} catch (Exception se) {
    					LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
    				}
    			}
    			if (mDBFactory.doConnectionRetry(dbConn, ex)) {
    				retry = true;
    			} else {
    				throw new RuntimeException(ex);
    			}
    		} finally {
    			if (dbConn != null) {
    				try {
    					dbConn.close();
    				} catch (SQLException ex) {
    					LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
    				}
    			}
    		}
    	} while (retry);
    }

    /**
     * @param msgExchId
     * @param instanceId
     * @param bpelid     
     * @throws SerialException
     * @throws SQLException
     */
  public void persistOutstandingMsgExchID(String msgExchId, String instanceId, String bpelid)  
 {
        DBObject outstandingMsgExDBO = mDBOFactory.createOutstandingMsgExDBO(msgExchId, instanceId, bpelid);
        
        boolean retry;

        do {
            retry = false;
            Connection conn = null;
            AbstractDBConnection dbConn = null;
            try {
                dbConn = mDBFactory.createNonXAConnection();
                conn = dbConn.getUnderlyingConnection();
                conn.setAutoCommit(false);
                dbConn.insert(outstandingMsgExDBO);
                conn.commit();
            } catch (Exception ex) {
                printSQLBatchExceptions(ex);
                if (conn != null) {
                    try {
                        conn.rollback();
                    } catch (Exception se) {
                        LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
                    }
                }
                if (mDBFactory.doConnectionRetry(dbConn, ex)) {
                    retry = true;
                } else {
                    throw new RuntimeException(ex);
                }
            } finally {
                if (dbConn != null) {
                    try {
                        dbConn.close();
                    } catch (SQLException ex) {
                        LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
                    }
                }
            }
        } while (retry);
    }

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.StateManager#getInstanceForMsgEx(java.lang.String)
     */
    public String getInstanceForMsgEx(String msgExId, String bpelId) {
        
        DBObject outstandingMsgExDBO = mDBOFactory.createOutstandingMsgExDBO(msgExId, bpelId);
        
        String instanceId = null;
        boolean retry;
        List<DBObject> objs = null;
        
        do {
            retry = false;
            AbstractDBConnection dbConn = null;
            try {
                dbConn = mDBFactory.createNonXAConnection();
                objs = dbConn.getRow(outstandingMsgExDBO);
                //Remove the MsgEx, so the instance might be passivated on Pending-DONE
                if (objs != null && objs.size() == 1) {
                    dbConn.delete(((OutstandingMsgExDBO) objs.get(0)) );
                }
            } catch (Exception ex) {
                if (mDBFactory.doConnectionRetry(dbConn, ex)) {
                    retry = true;
                } else {
                    throw new RuntimeException(ex);
                }
            } finally {
                if (dbConn != null) {
                    try {
                        dbConn.close();
                    } catch (SQLException ex) {
                        LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
                    }
                }
            }
        } while (retry);

        if (objs != null && objs.size() == 1) {
            instanceId = ((OutstandingMsgExDBO) objs.get(0)).getInstanceId();
        }
        
        return instanceId;
    }
    

    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.StateManager#getScalabilityPassivatedInstances(java.lang.String)
     */
    public List<String> getScalabilityPassivatedInstances(String bpelId) throws SQLException {
        
        List<String> instnceIdList = new ArrayList<String>();
        List<String> queryVals = new ArrayList<String>();
        
        queryVals.add(bpelId);
        ResultSet rs = null;
        
        boolean retry;
        do {
            retry = false;
            AbstractDBConnection dbConn = null;
            try {
                dbConn = mDBFactory.createNonXAConnection();
                rs = dbConn.get(SCAL_PASSVTED_INSTS_FOR_PROCESS_QUERY, queryVals);
                
                String instanceId = null;
                
                while (rs.next()) {
                    instanceId = rs.getString(1);
                    instnceIdList.add(instanceId);
                }
            } catch (Exception ex) {
                if (mDBFactory.doConnectionRetry(dbConn, ex)) {
                    retry = true;
                } else {
                    throw new RuntimeException(ex);
                }
            } finally {
                if (rs != null) {
                    java.sql.Statement stmt = rs.getStatement();
                    try {
                        stmt.close(); // closing the statement object should close its current Resultset object.
                    } catch (SQLException ex){
                        LOGGER.log(Level.WARNING, 
                        		I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
                    }
                }
                if (dbConn != null) {
                    try {
                        dbConn.close();
                    } catch (SQLException ex) {
                        LOGGER.log(Level.WARNING, 
                        		I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
                    }
                }
            }
        } while (retry);
        
        return instnceIdList;
    }
    
    public String getScalabilityPassivatedInstance(List<CorrelationVal> corrIds) {
    	String instanceId = null;
        boolean retry;
        do {
        	retry = false;
            AbstractDBConnection dbConn = null;
        	try {
        		dbConn = mDBFactory.createNonXAConnection();
        		instanceId = dbConn.findCorrelatedPassivatedInstance(corrIds);
        	} catch (Exception ex) {
    			if (mDBFactory.doConnectionRetry(dbConn, ex)) {
    				retry = true;
    			} else {
    				throw new RuntimeException(ex);
    			}
        	} finally {
        		if (dbConn != null) {
        			try {
        				dbConn.close(); // wrapper will set the initial value of the setAutoCommit field.
        			} catch (SQLException se) {
        				LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
        			}
        		}
        	}
        } while(retry);
        
    	return instanceId;
    }
    
	public void deleteAllScalabilityPasvtdVars(List bpInstanceIds) {
		List dbObjs = new ArrayList();
		Iterator iter = bpInstanceIds.iterator();
		String instanceId = null;
		DBObject dbo = null;

		while (iter.hasNext()) {
			instanceId = (String) iter.next();
			dbo = mDBOFactory.createVariableScalabilityDBO(instanceId);
			dbObjs.add(dbo);
		}

		if (true) {
			boolean retry;
                do {
                    retry = false;
                    Connection conn = null;
                    AbstractDBConnection dbConn = null;
                    try {
                        dbConn = mDBFactory.createNonXAConnection();
                        conn = dbConn.getUnderlyingConnection();
                        conn.setAutoCommit(false);
                        dbConn.delete(dbObjs);
                        conn.commit();
                    } catch (Exception ex) {
                        if (conn != null) {
                            try {
                                conn.rollback();
                            } catch (Exception se) {
                                LOGGER.log(Level.WARNING,
                                		I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), se);
                            }
                        }
                        if (mDBFactory.doConnectionRetry(dbConn, ex)) {
                            retry = true;
                        } else {
                            throw new RuntimeException(ex);
                        }
                    } finally {
                        if (dbConn != null) {
                            try {
                                dbConn.close();
                            } catch (SQLException ex) {
                                LOGGER.log(Level.WARNING,
                                		I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
                            }
                        }
                    }
                } while (retry);
            }

	}
    
    private static void printSQLBatchExceptions(Exception excep) {
        if (!(excep instanceof BatchUpdateException)) {
            return;
        }
        BatchUpdateException batchEx = (BatchUpdateException) excep;
        LOGGER.log(Level.FINE,
                I18n.loc("Number of rows persisted successfully in the batch = {0}",  batchEx.getUpdateCounts().length));
        int upperBound = 5; // just to avoid infinitely printing exceptions because of unforeseen issues in JDK or the underlying driver or others.
        int count = 0;
        while ((batchEx.getNextException() != null) && (count < upperBound)) {
            LOGGER.log(Level.WARNING, batchEx.getNextException().getMessage());
            count++;
        }
    }
    
    private class ScalabilityVariableDBOs {
        
        private List persistedVarsForDeleteDBOs = new ArrayList();
        
        private List passivatedVarsForUpdateDBOs = new ArrayList();
        
        private void addDeleteDBO(List dbos) {
            if (dbos != null) {
                persistedVarsForDeleteDBOs.addAll(dbos);
            }
        }
        
        private void addUpdateDBO(List dbos) {
            if (dbos != null) {
                passivatedVarsForUpdateDBOs.addAll(dbos);
            }
        }

        public List getPassivatedVarsForUpdateDBOs() {
            return passivatedVarsForUpdateDBOs;
        }

        public List getPersistedVarsForDeleteDBOs() {
            return persistedVarsForDeleteDBOs;
        }
    }
    
    private class InsertUpdateDeleteDBOs {

        private ScalabilityVariableDBOs mScalabilityVariableDBOs;
        
        private List insertDBOs = new ArrayList();

        private List updateDBOs = new ArrayList();

        private List deleteDBOs = new ArrayList();

        void setScalabilityVariableDBOs(ScalabilityVariableDBOs scalabilityVariableDBOs) {
            this.mScalabilityVariableDBOs = scalabilityVariableDBOs;
        }
        
        ScalabilityVariableDBOs getScalabilityVariableDBOs() {
            return mScalabilityVariableDBOs;
        }
        
        void addInsertDBO(DBObject dbo) {
            if (dbo != null) {
                insertDBOs.add(dbo);
            }
        }

        void addUpdateDBO(DBObject dbo) {
            if (dbo != null) {
                updateDBOs.add(dbo);
            }
        }

        void addDeleteDBO(DBObject dbo) {
            if (dbo != null) {
                deleteDBOs.add(dbo);
            }
        }

        void addInsertDBOs(List moreDBOs) {
            if (moreDBOs != null) {
                insertDBOs.addAll(moreDBOs);
            }
        }

        void addUpdateDBOs(List moreDBOs) {
            if (moreDBOs != null) {
                updateDBOs.addAll(moreDBOs);
            }
        }

        void addDeleteDBOs(List moreDBOs) {
            if (moreDBOs != null) {
                deleteDBOs.addAll(moreDBOs);
            }
        }

        /**
         * @return the deleteDBOs
         */
        public List getDeleteDBOs() {
            return deleteDBOs;
        }

        /**
         * @return the insertDBOs
         */
        public List getInsertDBOs() {
            return insertDBOs;
        }

        /**
         * @return the updateDBOs
         */
        public List getUpdateDBOs() {
            return updateDBOs;
        }
        
        public String getActIdForUpdates() {
        	return getActIdforLcpDbo(updateDBOs);
        }
        
        public String getActIdForInserts() {
        	return getActIdforLcpDbo(insertDBOs);
        }
        
        public String getActIdForDeletes() {
        	return getActIdforLcpDbo(deleteDBOs);
        }
        
        public String getActIdforLcpDbo(List dbos) {
        	String retVal = ":";
        	Iterator iter = dbos.iterator();
        	while (iter.hasNext()) {
        		Object obj = iter.next();
        		if (obj instanceof List) {
        			retVal = retVal.concat(getActIdforLcpDbo((List)obj));
        		} else if (obj instanceof LastCheckPointDBO) {
        			LastCheckPointDBO lcpDbo = (LastCheckPointDBO) obj;
        			String actId = String.valueOf(lcpDbo.getActivityId()) + ":";
        			retVal = retVal.concat(actId);
        		}
        	}
        	return retVal;
        }
        
        /** @see java.lang.Object#toString()
         */
        public void log() {
            if (getInsertDBOs() != null) {
                for (int i = 0; i < getInsertDBOs().size(); i++) {
                    if (getInsertDBOs().get(i) instanceof List) {
                        List insertList = (List) getInsertDBOs().get(i);
                        for (int j = 0; j < insertList.size(); j++) {
                            LOGGER.log(Level.FINE, "Inserting Data - " + insertList.get(j).toString());
                        } 
                    } else {
                        LOGGER.log(Level.FINE, "Inserting Data - " + getInsertDBOs().get(i).toString());
                    }
                }
            }
            if (getUpdateDBOs() != null) {
                for (int i = 0; i < getUpdateDBOs().size(); i++) {
                    if (getUpdateDBOs().get(i) instanceof List) {
                        List updateList = (List) getUpdateDBOs().get(i);
                        for (int j = 0; j < updateList.size(); j++) {
                            LOGGER.log(Level.FINE, "Updating Data - " + updateList.get(j).toString());
                        } 
                    } else {
                        LOGGER.log(Level.FINE, "Updating Data - " + getUpdateDBOs().get(i).toString());
                    }
                }
            }
            if (getDeleteDBOs() != null) {
                for (int i = 0; i < getDeleteDBOs().size(); i++) {
                    if (getDeleteDBOs().get(i) instanceof List) {
                        List deleteList = (List) getDeleteDBOs().get(i);
                        for (int j = 0; j < deleteList.size(); j++) {
                            LOGGER.log(Level.FINE, "Deleting Data - " + deleteList.get(j).toString());
                        } 
                    } else {
                        LOGGER.log(Level.FINE, "Deleting Data - " + getDeleteDBOs().get(i).toString());
                    }
                }
            }
        }
    }
}
