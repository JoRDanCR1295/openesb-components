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
 * @(#)EngineStateManagerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.persist.impl;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;

import com.sun.jbi.engine.bpel.core.bpel.connection.AbstractDBConnection;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.dbo.CRMPDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;
import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObjectFactory;
import com.sun.jbi.engine.bpel.core.bpel.dbo.QueryRespCRMPDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.StateDBO;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.persist.EngineStateManager;
import com.sun.jbi.engine.bpel.core.bpel.persist.MonitorDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.NonExpiredOnAlarmInstanceInfo;
import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.persist.State;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class EngineStateManagerImpl implements EngineStateManager {
    
    // To get instances list for running instances with owner lock
    private static final String RUNNING_STATES_CLUSTER_QUERY = "select bpelid, stateid, status from " + PersistenceDBSchemaCreation.STATE + 
        " where OWNERLOCK = '" + StateDBO.OWNERLOCK_YES + "' and engineid = ? AND (status = '" + StateDBO.RUNNING_STATUS + "'  or status= '" + StateDBO.SUSPENDED_STATUS + "' )";

    // get the running instances for non cluster mode
    private static final String RUNNING_STATES_NONCLUSTER_QUERY = "select bpelid, stateid, status from "
            + PersistenceDBSchemaCreation.STATE + " where (status = '" + StateDBO.RUNNING_STATUS + "'  or status = '" + StateDBO.SUSPENDED_STATUS + "')";
        
    // To get instances list for the passivated and expired on alarms.
    private static final String EXPIRED_ONALARM_STATES_QUERY = " select bpelid, " + PersistenceDBSchemaCreation.STATE + ".stateid from " + PersistenceDBSchemaCreation.STATE + "," + PersistenceDBSchemaCreation.LASTCHECKPOINT +
        " where  " + PersistenceDBSchemaCreation.STATE +".stateid = " + PersistenceDBSchemaCreation.LASTCHECKPOINT + ".stateid " +
        " and OWNERLOCK = '" + StateDBO.OWNERLOCK_NO + "' and engineid = ? AND status = '" + StateDBO.RUNNING_STATUS + "' and TIMEVAL < ?";

    // To get instances list for the passivated and on alarm is not expired
    private static final String NON_EXPIRED_ONALARM_STATES_QUERY = " select BPELID, " + PersistenceDBSchemaCreation.STATE + ".stateid, TIMEVAL from " + PersistenceDBSchemaCreation.STATE + "," + PersistenceDBSchemaCreation.LASTCHECKPOINT +
        " where  " + PersistenceDBSchemaCreation.STATE +".stateid = " + PersistenceDBSchemaCreation.LASTCHECKPOINT + ".stateid " +
        " and OWNERLOCK = '" + StateDBO.OWNERLOCK_NO + "' and engineid = ? AND status = '" + StateDBO.RUNNING_STATUS + "' and TIMEVAL > ?";
    
    private Engine mEng;
    private DBObjectFactory mDBOFactory;

    /** DOCUMENT ME! */
    boolean mInserted = false;
    private DBConnectionFactory mDBFactory;

    private long lastHeartbeatUpdateTime = -1;
    
    private static final Logger LOGGER = Logger.getLogger(EngineStateManagerImpl.class.getName());
    
    private Object mUpdateHeartbeatLock = new Object();
    
    /**
     * constructor
     *
     * @param eng Engine
     * @param factory DBConnectionFactory
     */
    public EngineStateManagerImpl(Engine eng, DBConnectionFactory factory) {
        mDBFactory = factory;
        mEng = eng;
        mDBOFactory = DBObjectFactory.getDBObjectFactory(factory.getType());
        insertHeartbeat();
    }

	/**
     * get expired engine BPs
     *
     * @param maxcount maximum count
     *
     * @return List list of expired engine BPs
     */
    public List getExpiredEngineBPs(int maxcount) {
        return null;
    }

    /**
     * persiste in-memory state to database
     *
     * @param state EngineState
     *
     * @return boolean: on successful execution, it returns true; otherwise, it returns false
     *
     * @throws Exception Exception
     */
   /* public boolean persistState(EngineState state) throws Exception {
        DBObject dbo = mDBOFactory.createEngineDBO(
                mEng.getId(), mEng.getLocation(), mEng.getExpiration().longValue()
            );
        DBObject dbo = mDBOFactory.createEngineDBO(mEng.getId(), mEng.getLocation());
        DBConnection dbCon = null;

        try {
            TransactionManager tm = (TransactionManager) BPELSERegistry.getInstance().lookup(TransactionManager.class.getName());
            tm.begin();
            Transaction tx = tm.getTransaction();
            dbCon = mDBFactory.createConnection();

            if (!isEngineRestarted()) {
                dbCon.insert(dbo);
            } else {
                dbCon.update(dbo);
            }

            tx.commit();

        } finally {
            if (dbCon != null) {
                dbCon.getUnderlyingConnection().close();
            }
        }

        //mConnMgr.releaseConnection(dbCon);
        return true;
    }*/

    /*
       public boolean persistStaticBPEL(String bpelId, byte[] val) throws Exception {
           DBObject dbo = mDBOFactory.createBPELModelDBO(bpelId, val);
           DBConnection dbCon = mConnMgr.getConnection();
           dbCon.beginTx();
           if (!mInserted) {
               dbCon.insert(dbo);
           } else {
               dbCon.update(dbo);
           }
           dbCon.endTx();
           mConnMgr.releaseConnection(dbCon);
           return true;
           return true;
       }
     */

    /**
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.EngineStateManager#getRunningAndSuspendedStates()
     */
    public Map[] getRunningAndSuspendedStates() throws Exception {
        Map<QName, List<String>> runningInstancesMap = new HashMap<QName, List<String>>();
        Map<QName, List<String>> suspendedInstancesMap = new HashMap<QName, List<String>>();

        ResultSet rs = null;
        
        boolean retry;
        do {
        	retry = false;
            AbstractDBConnection dbConn = null;
        	try {

        		dbConn = mDBFactory.createNonXAConnection();
        		Connection conn = dbConn.getUnderlyingConnection();

        		List<String> runningStatesQueryVals = new ArrayList<String>();
        		if (mEng.isClustered()) {
        			// Pick all the orphaned and non-passivated running instances of failed engine(s).
        			runningStatesQueryVals.add(mEng.getId());
        			//TODO: refactor this call to not return resultset, but a data structure so that 
        			// Statement, ResultSet closing can be better handled.
        			rs = dbConn.get(RUNNING_STATES_CLUSTER_QUERY, runningStatesQueryVals);
        		} else {
        			//TODO: refactor this call to not return resultset, but a data structure so that 
        			// Statement, ResultSet closing can be better handled.
        			rs = dbConn.get(RUNNING_STATES_NONCLUSTER_QUERY, runningStatesQueryVals);
        		}


        		List<String> stateIdsList = null;
        		String bpelProcessId = null;
        		QName bpelIdQName = null;
                String status = null;

        		while (rs.next()) {
        			bpelProcessId = rs.getString(1);
                    status = rs.getString(3);
        			bpelIdQName = QName.valueOf(bpelProcessId);
                    
                    if (status.equals(StateDBO.RUNNING_STATUS)) {
                        stateIdsList = runningInstancesMap.get(bpelIdQName);
                        if (stateIdsList == null) {
                            stateIdsList = new ArrayList<String>();
                            runningInstancesMap.put(bpelIdQName, stateIdsList);
                        }
                    } else {
                        stateIdsList = suspendedInstancesMap.get(bpelIdQName);
                        if (stateIdsList == null) {
                            stateIdsList = new ArrayList<String>();
                            suspendedInstancesMap.put(bpelIdQName, stateIdsList);
                        }
                    }
                    
                    stateIdsList.add(rs.getString(2));
        		}
	
        	} catch (Exception e) {
    			if (mDBFactory.doConnectionRetry(dbConn, e)) {
    				retry = true;
    			} else {
    				throw new RuntimeException(e);
    			}
        	} finally {
        		if (rs != null) {
        			java.sql.Statement stmt = rs.getStatement();
        			try {
        				stmt.close(); // closing the statement object should close its current resultset object.
        			} catch (SQLException ex){
        				LOGGER.log(Level.WARNING, 
        						I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
        			}
        		}

        		if (dbConn != null) {
        			try {
        				dbConn.close(); // wrapper will set the intial value of the setAutoCommit field.
        			} catch (SQLException ex) {
        				LOGGER.log(Level.WARNING, 
        						I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
        			}
        		}
        	}
        } while (retry);
        
        return new Map[]{runningInstancesMap, suspendedInstancesMap};
    }

    /**
     * Select all the orphened and cluster passivated instances for the failed engine
     * for which on alarm is defined and already expired. Schedule them for
     * recovery and mark the ownerlock for these instances as acquired in the 
     * state table to prevent other engines also picking them up.
     * 
     * @param rs
     * @param dbConn
     * @param runningInstancesMap
     * @throws SQLException
     */
    public Map<QName, List<String>> getExpiredOnAlarmInstances() throws SQLException {
        
    	Map<QName, List<String>> expiredOnAlarmInstancesMap = new HashMap<QName, List<String>>();
    	
        ResultSet rs = null;
        boolean retry;
        do {
            retry = false;
            Connection conn = null;
            AbstractDBConnection dbConn = null;
            try {

                dbConn = mDBFactory.createNonXAConnection();
                conn = dbConn.getUnderlyingConnection();
                conn.setAutoCommit(false);

                List onAlrmStatesQryVals = new ArrayList();
                onAlrmStatesQryVals.add(mEng.getId());

                Timestamp date = new Timestamp(System.currentTimeMillis());
                onAlrmStatesQryVals.add(date);

                // TODO: refactor this call to not return resultset, but a datatstructure so that
                // Statement, ResultSet closing can be better handled.
                rs = dbConn.get(EXPIRED_ONALARM_STATES_QUERY, onAlrmStatesQryVals);

                // update the expired onAlarm instance ownership, as they should not
                // be picked by the onMessage either by the same or different engine.
                List expiredOnAlarmStateDBOsList = new ArrayList();
                DBObject stateDBO = null;
                String bpId = null;
                List<String> stateIdsList = null;
                String bpelProcessId = null;
                QName bpelIdQName = null;

                while (rs.next()) {
                    bpelProcessId = rs.getString(1);
                    bpelIdQName = QName.valueOf(bpelProcessId);
                    stateIdsList = expiredOnAlarmInstancesMap.get(bpelIdQName);

                    if (stateIdsList == null) {
                        stateIdsList = new ArrayList<String>();
                        expiredOnAlarmInstancesMap.put(bpelIdQName, stateIdsList);
                    }
                    bpId = rs.getString(2);
                    stateIdsList.add(bpId);
                    stateDBO = mDBOFactory.createStateDBO(bpId, mEng.getId(), State.Mode.ACTIVATE);
                    expiredOnAlarmStateDBOsList.add(stateDBO);
                }

/*                //This earlier RS and Statement has to be closed to prevent mem leak
                // if the above TODO is implemented then this code can be removed.
                if (rs != null) {
                    java.sql.Statement stmt = rs.getStatement();
                    stmt.close(); //closing the statement object should close its current resultset object.
                }*/
                
                if (expiredOnAlarmStateDBOsList.size() > 0) {
                    dbConn.update(expiredOnAlarmStateDBOsList);
                }

                conn.commit();
                
            } catch (Exception e) {
                if (conn != null) {
                    try {
                        conn.rollback();
                    } catch (Exception se) {
                        LOGGER.log(Level.WARNING,
                        		I18n.loc("BPCOR-6068: Exception thrown when rollback was called on connection"), se);
                    }
                }
                if (mDBFactory.doConnectionRetry(dbConn, e)) {
                    retry = true;
                } else {
                    throw new RuntimeException(e);
                }
            } finally {
            	try {
					if (rs != null) {
						java.sql.Statement stmt = rs.getStatement();
						stmt.close(); // closing the statement object should close its current resultset object.
					}

					if (dbConn != null) {
						dbConn.close(); // wrapper will set the intial value of the setAutoCommit field.
					}
				} catch (SQLException ex) {
					LOGGER.log(Level.WARNING,I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
				}
            }
        } while (retry);
        
        return expiredOnAlarmInstancesMap;
    }
    

    /*
	 * (non-Javadoc)
	 * 
	 * @see com.sun.jbi.engine.bpel.core.bpel.persist.EngineStateManager#getNonExpiredOnAlarmInstancesList()
	 */
    public Map getNonExpiredOnAlarmInstances() throws Exception {
        ResultSet rs = null;   
        List instancesInfoList = null;
        Map bpelIdInstanceInfoMap = new HashMap();
        Timestamp date = new Timestamp(System.currentTimeMillis());
        
        List onAlrmStatesQryVals = new ArrayList();
        onAlrmStatesQryVals.add(mEng.getId());
        onAlrmStatesQryVals.add(date);
        
        boolean retry;
        do {
        	retry = false;
        	AbstractDBConnection dbConn = null;
        	try {
        		dbConn = mDBFactory.createNonXAConnection();
        		String bpelProcessId = null;            
        		String instanceId = null;
        		Timestamp waitTime;
        		NonExpiredOnAlarmInstanceInfo instanceInfo = null;

        		//TODO: refactor this call to not return resultset, but a datatstructure so that 
        		// Statement, ResultSet closing can be better handled.
        		rs = dbConn.get(NON_EXPIRED_ONALARM_STATES_QUERY, onAlrmStatesQryVals);

        		while (rs.next()) {
        			bpelProcessId = rs.getString(1);
        			instanceId = rs.getString(2);
        			waitTime = rs.getTimestamp(3);
        			instanceInfo = new NonExpiredOnAlarmInstanceInfo(instanceId, waitTime.getTime());
        			instancesInfoList = (List) bpelIdInstanceInfoMap.get(bpelProcessId);

        			if (instancesInfoList == null) {
        				instancesInfoList = new ArrayList();
        				bpelIdInstanceInfoMap.put(bpelProcessId, instancesInfoList);
        			}
        			instancesInfoList.add(instanceInfo);
        		}
        	} catch (Exception e) {
    			if (mDBFactory.doConnectionRetry(dbConn, e)) {
    				retry = true;
    			} else {
    				throw new RuntimeException(e);
    			}
        	} finally {
        		if (rs != null) {
        			java.sql.Statement stmt = rs.getStatement();
        			try {
        				stmt.close(); // closing the statment object should close its current Resultset object.
        			} catch (SQLException ex){
        				LOGGER.log(Level.WARNING, 
        						I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
        			}
        		}

        		if (dbConn != null) {
        			try {
        				dbConn.close(); // wrapper will set the intial value of the setAutoCommit field.
        			} catch (SQLException ex) {
        				LOGGER.log(Level.WARNING, 
        						I18n.loc("BPCOR-6069: Exception thrown when closing DB cursors"), ex);
        			}
        		}
        	}   
        } while (retry);
        return bpelIdInstanceInfoMap;
    }    

    private void insertHeartbeat() {
        DBObject dbo = mDBOFactory.createEngineDBO(mEng.getId(), mEng.getLocation());
        
        boolean retry;
        do {
        	retry = false;
        	AbstractDBConnection dbConn = null;
        	Connection conn = null;
        	this.lastHeartbeatUpdateTime = System.currentTimeMillis();
        	try {
        		dbConn = mDBFactory.createNonXAConnection();

        		conn = dbConn.getUnderlyingConnection();
                conn.setAutoCommit(false);
                
        		int updateCount = dbConn.update(dbo);
        		if (updateCount == 0) {
        			// this indicates that no entry exists for this engine id,
        			// insert new one.
        			dbConn.insert(dbo);
        			if (LOGGER.isLoggable(Level.FINE)) {
        				LOGGER.log(Level.FINE, 
        						I18n.loc("BPCOR-3033: Registering engine and first heart beat for engine id : {0}", 
        								mEng.getId()));
        			}
        		} else if (updateCount == 1) {
        			if (LOGGER.isLoggable(Level.FINE)) {
        				LOGGER.log(Level.FINE, 
        						I18n.loc("BPCOR-3034: Restarting engine with id : {0}", 
        								mEng.getId()));
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
    				throw new RuntimeException(
    						I18n.loc("BPCOR-3033: Registering engine and first heart beat for engine id : {0}", 
    								mEng.getId()), ex);
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
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.EngineStateManager#updateHeartbeat(long)
     */
    public boolean updateHeartbeat(long mHeartbeatUpdateConfigTime) {
        synchronized (mUpdateHeartbeatLock) {
            long elapsedTimeSinceLastUpdate = System.currentTimeMillis() - lastHeartbeatUpdateTime;
            if (elapsedTimeSinceLastUpdate < (0.6 * mHeartbeatUpdateConfigTime)) {
                return false;
            }
            this.lastHeartbeatUpdateTime = System.currentTimeMillis();
        }
        
        DBObject dbo = mDBOFactory.createEngineDBO(mEng.getId(), mEng.getLocation());

        boolean retry;
        do {
        	retry = false;
        	AbstractDBConnection dbConn = null;
        	Connection conn = null;
        	
        	try {
        		dbConn = mDBFactory.createNonXAConnection();
                conn = dbConn.getUnderlyingConnection();
                conn.setAutoCommit(false);
                
        		dbConn.update(dbo);
        		if (LOGGER.isLoggable(Level.FINE)) {
        			LOGGER.log(Level.FINE, 
        					I18n.loc("BPCOR-3077: Updating heart beat for engine id : {0}", mEng.getId()));
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
        			throw new RuntimeException(I18n.loc("BPCOR-3077: Updating heart beat for engine id : {0}", 
        					mEng.getId()), ex);      			
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
        return true;
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.EngineStateManager#updateDanglingInstances(long)
     */
    public int updateDanglingInstances(long engineExirationInterval, int recoveryBatchSize) {
        int count = 0;
        DBObject dbo = mDBOFactory.createupdateDanglingInstancesDBO(mEng.getId(), engineExirationInterval, recoveryBatchSize);
        
        boolean retry;
        do {
        	retry = false;
        	AbstractDBConnection dbConn = null;
        	Connection conn = null;
        	try {
        		dbConn = mDBFactory.createNonXAConnection();

        		conn = dbConn.getUnderlyingConnection();
        		conn.setAutoCommit(false);
        		if (LOGGER.isLoggable(Level.FINE)) {
        			LOGGER.log(Level.FINE, I18n.loc("BPCOR-3011: {0} Engine Id: {1} Expiry Interval : {2}", 
        					dbo.getUpdateStmt(), mEng.getId(), engineExirationInterval));
        		}
        		count = dbConn.update(dbo);
        		
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
        return count;        
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.EngineStateManager#getLastHeartbeatUpdateTime()
     */
    public long getLastHeartbeatUpdateTime() {
        return lastHeartbeatUpdateTime;
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.EngineStateManager#getCRMPDBOForCRMPInvokeID()
     */
    public CRMPDBO getCRMPDBOForCRMPInvokeID(String crmpInvId, boolean isClusteredInvoke) throws Exception {
    	CRMPDBO tempDbo = mDBOFactory.createCRMPDBO(crmpInvId, isClusteredInvoke);
    	CRMPDBO crmpDbo = null;
    	boolean retry;
    	do {
    		retry = false;
    		AbstractDBConnection dbConn = null;
    		try {
    			dbConn = mDBFactory.createNonXAConnection();
    			List objs = dbConn.getRow(tempDbo);

    			if (!objs.isEmpty()) {
    				if (objs.size() > 1) {
    					throw new RuntimeException(
    							I18n.loc("BPCOR-6070: CRMP table has more than one row for the this crmpInvokeId: {0}", 
    									crmpInvId));
    				}
    				crmpDbo = (CRMPDBO) objs.get(0);

    			} 

    		} catch (SQLException ex) {
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

    	return crmpDbo;
    }
    
    /* (non-Javadoc)
     * @see com.sun.jbi.engine.bpel.core.bpel.persist.EngineStateManager#getResponseObjForCrmpInvokeId(java.lang.String)
     */
    public QueryRespCRMPDBO getResponseObjForCrmpInvokeId(String crmpInvId) 
    	throws Exception {
    	QueryRespCRMPDBO tempDbo = mDBOFactory.createQueryRespCRMPDBO(crmpInvId);
    	QueryRespCRMPDBO queryRespCrmpDbo = null;
    	
    	boolean retry;
    	do {
    		retry = false;
    		AbstractDBConnection dbConn = null;
    		try {
    			dbConn = mDBFactory.createNonXAConnection();
    			List objs = dbConn.getRow(tempDbo);

    			if (!objs.isEmpty()) {
    				if (objs.size() > 1) {
    					throw new RuntimeException(
    							I18n.loc("BPCOR-6070: CRMP table has more than one row for the this crmpInvokeId: {0}", 
    									crmpInvId));
    				}
    				queryRespCrmpDbo = (QueryRespCRMPDBO) objs.get(0);

    			} 

    		} catch (SQLException ex) {
    			if (mDBFactory.doConnectionRetry(dbConn, ex)) {
    				retry = true;
    			} else {
    				throw new RuntimeException ("EngineStateManager.getResponseObjForCrmpInvokeId ",ex); //$NON-NLS-1$
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

    	return queryRespCrmpDbo;
    	
    }

}
