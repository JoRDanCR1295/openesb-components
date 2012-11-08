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
 * @(#)AbstractDBConnection.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.connection;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.engine.bpel.core.bpel.dbo.DBObject;
import com.sun.jbi.engine.bpel.core.bpel.dbo.InstanceCorrelationDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.StateDBO;
import com.sun.jbi.engine.bpel.core.bpel.dbo.WaitingIMADBO;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.CorrelationVal;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;
import com.sun.jbi.engine.bpel.core.bpel.exception.StandardException;
import com.sun.jbi.engine.bpel.core.bpel.persist.PersistenceDBSchemaCreation;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;


/**
 * DOCUMENT ME!
 * 
 * @author Sun Microsystems
 */
/**
 * @author pVarghese
 *
 */
/**
 * @author pvarghese
 *
 */
public class AbstractDBConnection {
    
    /** The JDBC connection */
    protected Connection mConn;

    /**
     * Batch Database Manipulation Language error
     */
    protected static String BATCH_DML_ERROR = "DBConnection_BATCH_UPDATE_ERROR"; //$NON-NLS-1$

    /**
     * Database Manipulation Language error
     */
    protected static String DML_ERROR = "DBConnection_UPDATE_ERROR"; //$NON-NLS-1$
    
    private static final Logger LOGGER = Logger.getLogger(AbstractDBConnection.class.getName());

    
    /**
     * constructor
     * 
     * @param conn jdbc connection
     * @throws SQLException SQLException
     */
    public AbstractDBConnection(Connection conn) throws SQLException {
        mConn = conn;

    }

    /**
     * A handle to the undelying JDBC connection
     * 
     * @return
     */
    public Connection getUnderlyingConnection() {
        return mConn;
    }

    /**
     * @param obj DBObject
     * @throws SQLException SQLException
     */
    public void insert(DBObject obj) throws SQLException {
        if (obj == null) {
            return;
        }

        PreparedStatement stmt = null;

        try {
            stmt = constructInsertStmt(obj);

            int updatedRows = stmt.executeUpdate();

            if (updatedRows < 1) {
                throw new BPELRuntimeException(BPELRuntimeException.PERSISTENCE_ERROR, DML_ERROR);
            }
        } catch (SQLException ex) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6149: failed while executing SQL statement {0}, DB oject {1}",
                    obj.getInsertStmt(), obj.toString()), ex);
            throw ex;
        } finally {
            if (stmt != null) {
                stmt.close();
            }
        }
    }

    /**
     * @param objs List<DBObject>. all the DBObjects are expected to be of the same DBObject
     *            instance type. For instance a valid list will have <VariableDBO, VariableDBO,
     *            VariableDBO..> an invalid list may be like <VariableDBO, InstanceCorrelationDBO,
     *            VariableDBO...>. Invalid lists throw SQLException.
     * @throws SQLException SQLException
     */
    public void insert(List objs) throws SQLException {
        if ((objs == null) || (objs.size() < 1)) {
            return;
        }

        if (mConn.getMetaData().supportsBatchUpdates()) {
            batchInsert(objs);
        } else {
            PreparedStatement stmt = null;
            DBObject obj = null;;
            try {
                for (int i = 0, size = objs.size(); i < size; i++) {
                 
                    obj = (DBObject) objs.get(i);
                    stmt = constructInsertStmt(obj);

                    int updatedRows = stmt.executeUpdate();

                    if (updatedRows < 1) {
                        throw new BPELRuntimeException(BPELRuntimeException.PERSISTENCE_ERROR,
                                DML_ERROR);
                    }
                }
            } catch (SQLException ex) {
                LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6149: failed while executing SQL statement {0}, DB oject {1}",
                        obj.getInsertStmt(), obj.toString()), ex);

                throw ex;
            } finally {
                if (stmt != null) {
                    stmt.close();
                }
            }
        }
    }

    private String getCorrelationValuesFromDBOs(List<InstanceCorrelationDBO> corrIds) {
        Iterator<InstanceCorrelationDBO> iter = corrIds.iterator();
        StringBuffer corrValues = new StringBuffer();
        int i = 1;

        if (corrIds.size() > 0) {
            while (iter.hasNext()) {
                if (i > 1)
                    corrValues.append(",");
                corrValues.append("'" + iter.next().getValue() + "'");
                i++;
            }
        } else {
            corrValues.append("''");
        }
        return corrValues.toString();
    }

    /**
     *  It is wrapper on insert(List objs), identifies duplicate instance
     *  "CorrelationViolation". If there is no violation it delegates
     * @param corrDbos
     * @throws SQLException
     */
    public void insertCorrelationValues(List<InstanceCorrelationDBO> corrDbos) throws SQLException {
        String CORR = PersistenceDBSchemaCreation.INSTANCECORRELATION;
        String STATE = PersistenceDBSchemaCreation.STATE;

        String corrValues = getCorrelationValuesFromDBOs(corrDbos);

        String query = " SELECT " + CORR + ".stateid FROM " + CORR + ",  " + STATE + " WHERE " + CORR + ".stateid = " + STATE
                + ".stateid " + " AND " + STATE + ".STATUS = '" + StateDBO.RUNNING_STATUS + "' " + " AND " + CORR + ".VALUE IN ("
                + corrValues + ")";
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            stmt = mConn.prepareStatement(query.toString());
            rs = stmt.executeQuery();
            if (rs.next()) {
                String message = I18n.loc("BPCOR-6192: Another BP instance {0} associated with correlation values: '{1}'", 
                        rs.getString("stateid"), corrValues);
                throw new StandardException(StandardException.Fault.CorrelationViolation, message);
            }
        } finally {
            if (rs != null) {
                rs.close();
            }

            if (stmt != null) {
                stmt.close();
            }
        }
        insert(corrDbos);
    }

    /**
     * @param obj DBObject
     * @throws SQLException SQLException
     */
    public int update(DBObject obj) throws SQLException {
        if (obj == null) {
            return -1;
        }
        int updatedRows = -1;

        PreparedStatement stmt = null;
        try {
            stmt = constructUpdateStmt(obj);

            updatedRows = stmt.executeUpdate();

            // The following check can fail for update dangling instances 
            // and also for update of state dbo object for activation and passivation
            // of the instance. For example, when the activation is called by the 
            // businessprocessinstance thread for clustered pick upon expiration of 
            // the timer, the instance might already have been picked by another
            // thread during recovery (when kicked in) due to new deployment. Hence
            // masking the following check.
            /*if (updatedRows < 1 && !(obj instanceof UpdateDanglingInstancesDBO)) {
                throw new BPELRuntimeException(BPELRuntimeException.PERSISTENCE_ERROR, DML_ERROR
                        + " " + obj.getUpdateStmt());
            }*/
        
        } catch (SQLException ex) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6149: failed while executing SQL statement {0}, DB oject {1}",
                    obj.getUpdateStmt(), obj.toString()), ex);

            throw ex;
        } finally {
            if (stmt != null) {
                stmt.close();
            }
        }

        return updatedRows;
    }

    /**
     * @param objs List<DBObject>. all the DBObjects are expected to be of the same DBObject
     *            instance type. For instance a valid list will have <VariableDBO, VariableDBO,
     *            VariableDBO..> an invalid list may be like <VariableDBO, InstanceCorrelationDBO,
     *            VariableDBO...>. Invalid lists throw SQLException.
     * @throws SQLException SQLException
     */
    public void update(List objs) throws SQLException {
        if ((objs == null) || (objs.size() < 1)) {
            return;
        }

        if (mConn.getMetaData().supportsBatchUpdates()) {
            batchUpdate(objs);
        } else {
            PreparedStatement stmt = null;
            DBObject obj = null;

            try {
                for (int i = 0, size = objs.size(); i < size; i++) {
                    obj = (DBObject) objs.get(i);
                    stmt = constructUpdateStmt(obj);

                    int updatedRows = stmt.executeUpdate();

                    if (updatedRows < 1) {
                        throw new BPELRuntimeException(BPELRuntimeException.PERSISTENCE_ERROR,
                                DML_ERROR);
                    }
                }
            } catch (SQLException ex) {
                LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6149: failed while executing SQL statement {0}, DB oject {1}",
                        obj.getUpdateStmt(), obj.toString()), ex);

                throw ex;
            } finally {
                if (stmt != null) {
                    stmt.close();
                }
            }
        }
    }

    /**
     * @param obj DBObject
     * @throws SQLException SQLException
     */
    public void delete(DBObject obj) throws SQLException {
        if (obj == null) {
            return;
        }

        PreparedStatement stmt = null;

        try {
            stmt = constructDeleteStmt(obj);

            int updatedRows = stmt.executeUpdate();

            if (updatedRows < 1) {
                throw new BPELRuntimeException(BPELRuntimeException.PERSISTENCE_ERROR,
                        BATCH_DML_ERROR);
            }
        } catch (SQLException ex) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6149: failed while executing SQL statement {0}, DB oject {1}",
                    obj.getDeleteStmt(), obj.toString()), ex);

            throw ex;
        } finally {
            if (stmt != null) {
                stmt.close();
            }
        }
    }

    /**
     * @param objs List<DBObject> all the DBObjects are expected to be of the same DBObject
     *            instance type. For instance a valid list will have <VariableDBO, VariableDBO,
     *            VariableDBO..> an invalid list may be like <VariableDBO, InstanceCorrelationDBO,
     *            VariableDBO...>. Invalid lists throw SQLException.
     * @throws SQLException SQLException
     */
    public void delete(List objs) throws SQLException {
        if ((objs == null) || (objs.size() < 1)) {
            return;
        }

        if (mConn.getMetaData().supportsBatchUpdates()) {
            batchDelete(objs);
        } else {
            PreparedStatement stmt = null;
            DBObject obj = null;
            try {
                for (int i = 0, size = objs.size(); i < size; i++) {
                    stmt = constructDeleteStmt((DBObject) objs.get(i));

                    int updatedRows = stmt.executeUpdate();

                    if (updatedRows < 1) {
                        throw new BPELRuntimeException(BPELRuntimeException.PERSISTENCE_ERROR,
                                DML_ERROR);
                    }
                }
            } catch (SQLException ex) {
                LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6149: failed while executing SQL statement {0}, DB oject {1}",
                        obj.getDeleteStmt(), obj.toString()), ex);

                throw ex;
            } finally {
                if (stmt != null) {
                    stmt.close();
                }
            }
        }
    }

    /**
     * executes query
     * 
     * @param obj DBObject
     * @return ResultSet ResultSet
     * @throws SQLException SQLException
     */
    public List getRow(DBObject dbo) throws SQLException {
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            stmt = mConn.prepareStatement(dbo.getQueryStmt());
            dbo.fillQueryStmt(stmt);
            rs = stmt.executeQuery();
            return populateDBO(dbo, rs);
        } finally {
        	if (rs != null) {
        		rs.close();
        	}
        	//closing the Statement object closes its current ResultSet object.
        	if (stmt != null) {
        		stmt.close();
        	}
        }
    }
    
    private List populateDBO(DBObject tempObj, ResultSet rs)
    	throws SQLException {
    	List retVals = new ArrayList();
    	DBObject obj = null;

    	while (rs.next()) {
    		obj = tempObj.createNew();
    		obj.populateDBO(rs);
    		retVals.add(obj);
    	}

    	return retVals;
    }    

    /**
     * executes query
     * 
     * @param query query preparedstatement string
     * @param vals values to be bound to query string
     * @return ResultSet result set
     * @throws SQLException SQLException
     */
    public ResultSet get(String query, List vals) throws SQLException {
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            stmt = mConn.prepareStatement(query);

            for (int i = 0, size = vals.size(); i < size; i++) {
                stmt.setObject(i + 1, vals.get(i));
            }

            rs = stmt.executeQuery();
        } catch (SQLException ex) {
            if (rs != null) {
                rs.close();
            }

            if (stmt != null) {
                stmt.close();
            }

            throw ex;
        }
        
        return rs;
    }

    private PreparedStatement constructInsertStmt(DBObject obj) throws SQLException {
        String stmtStr = obj.getInsertStmt();
        PreparedStatement stmt = mConn.prepareStatement(stmtStr);
        obj.fillInsertStmt(stmt);
        return stmt;
    }

    private PreparedStatement constructUpdateStmt(DBObject obj) throws SQLException {
        String stmtStr = obj.getUpdateStmt();
        PreparedStatement stmt = mConn.prepareStatement(stmtStr);
        obj.fillUpdateStmt(stmt);
        return stmt;
    }

    private PreparedStatement constructDeleteStmt(DBObject obj) throws SQLException {
        String stmtStr = obj.getDeleteStmt();
        PreparedStatement stmt = mConn.prepareStatement(stmtStr);
        obj.fillDeleteStmt(stmt);
        return stmt;
    }

    protected void batchInsert(List objs) throws SQLException {
        PreparedStatement stmt = null;

        DBObject obj = (DBObject) objs.get(0);
        try {
            stmt = constructInsertStmt(obj);
            stmt.addBatch();

            for (int i = 1, size = objs.size(); i < size; i++) {
                obj = (DBObject) objs.get(i);
                obj.fillInsertStmt(stmt);
                stmt.addBatch();
            }

            int[] executedRows = stmt.executeBatch();

            for (int i = 0; i < executedRows.length; i++) {
                if (executedRows[i] == Statement.EXECUTE_FAILED) {
                    throw new BPELRuntimeException(BPELRuntimeException.PERSISTENCE_ERROR,
                            BATCH_DML_ERROR);
                }
            }
        } catch (SQLException ex) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6149: failed while executing SQL statement {0}, DB oject {1}",
                    obj.getInsertStmt(), obj.toString()), ex);

            throw ex;
        } finally {
            if (stmt != null) {
                stmt.close();
            }
        }
    }

    protected void batchUpdate(List objs) throws SQLException {
        PreparedStatement stmt = null;

        DBObject obj = (DBObject) objs.get(0);
        try {
            stmt = constructUpdateStmt(obj);
            stmt.addBatch();

            for (int i = 1, size = objs.size(); i < size; i++) {
                obj = (DBObject) objs.get(i);
                obj.fillUpdateStmt(stmt);
                stmt.addBatch();
            }

            int[] executedRows = stmt.executeBatch();

            for (int i = 0; i < executedRows.length; i++) {
                if (executedRows[i] == Statement.EXECUTE_FAILED) {
                    throw new BPELRuntimeException(BPELRuntimeException.PERSISTENCE_ERROR,
                            BATCH_DML_ERROR);
                }

                ;
            }
        } catch (SQLException ex) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6149: failed while executing SQL statement {0}, DB oject {1}",
                    obj.getUpdateStmt(), obj.toString()), ex);

            throw ex;
        } finally {
            if (stmt != null) {
                stmt.close();
            }
        }
    }

    protected void batchDelete(List objs) throws SQLException {
        PreparedStatement stmt = null;

        DBObject obj = (DBObject) objs.get(0);
        try {
            stmt = constructDeleteStmt(obj);
            stmt.addBatch();

            for (int i = 1, size = objs.size(); i < size; i++) {
                obj = (DBObject) objs.get(i);
                obj.fillDeleteStmt(stmt);
                stmt.addBatch();
            }

            int[] executedRows = stmt.executeBatch();

            for (int i = 0; i < executedRows.length; i++) {
                if (executedRows[i] == Statement.EXECUTE_FAILED) {
                    throw new BPELRuntimeException(BPELRuntimeException.PERSISTENCE_ERROR,
                            BATCH_DML_ERROR);
                }

                ;
            }
        } catch (SQLException ex) {
            LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6149: failed while executing SQL statement {0}, DB oject {1}",
                    obj.getDeleteStmt(), obj.toString()), ex);

            throw ex;
        } finally {
            if (stmt != null) {
                stmt.close();
            }
        }
    }


    /**
     * activate instance. This could be called by the messaging event on the other engine than the
     * engine that created the instance thus the engine id of the instance and the ownerlock need to
     * be updated.
     * 
     * @param engId
     * @param corrIds
     * @param waitingIMADBO
     * @return
     * @throws SQLException
     */
    public String findCorrelatedPassivatedInstance(List corrIds, WaitingIMADBO waitingIMADBO)
            throws SQLException {
        String partnerLinkName = waitingIMADBO.getPartnerLinkName();
        String operationName = waitingIMADBO.getOperationName();

        Iterator iter = corrIds.iterator();
        StringBuffer corrValues = new StringBuffer();
        int i = 1;

        if (corrIds.size() > 0) {
            while (iter.hasNext()) {
                if (i > 1)
                    corrValues.append(",");
                corrValues.append("'" + (CorrelationVal) iter.next() + "'");
                i++;
            }
        } else {
            corrValues.append("''");
        }

    	String CORR = PersistenceDBSchemaCreation.INSTANCECORRELATION;
    	String STATE = PersistenceDBSchemaCreation.STATE;
    	String WIMA = PersistenceDBSchemaCreation.WAITINGIMA;
    	
        String query = " select " + CORR + ".stateid from " + CORR + " , " + WIMA + " , " + STATE +
                " where " + STATE + ".stateid = " + CORR + ".stateid " +
                " and " + CORR + ".stateid = " + WIMA + ".stateid " +
                " and " + STATE + ".STATUS = '" + StateDBO.RUNNING_STATUS +
                "'  and " + STATE + ".OWNERLOCK = '" + StateDBO.OWNERLOCK_NO + "'" +
                " and PARTNERLINK = '" + partnerLinkName + "' and OPERATION = '" + operationName + "'" +
                " and VALUE IN (" + corrValues + ")";

        String instance = null;

        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            stmt = mConn.prepareStatement(query.toString());
            rs = stmt.executeQuery();
            if (rs.next()) {
                instance = rs.getString("stateid");
            }

        } finally {
            if (rs != null) {
                rs.close();
            }

            if (stmt != null) {
                stmt.close();
            }
        }
        return instance;
    }
    
    public String findCorrelatedPassivatedInstance(List<CorrelationVal> corrIds) throws SQLException {
    	String instanceId = null;
    	String corrValues = getCorrelationValues(corrIds);
    	
    	String CORR = PersistenceDBSchemaCreation.INSTANCECORRELATION;
    	String STATE = PersistenceDBSchemaCreation.STATE;
    	
        String query =  " SELECT " + CORR + ".stateid FROM " + CORR + ",  " + STATE + 
        				" WHERE " + CORR + ".stateid = " + STATE + ".stateid " +
        				" AND " + STATE + ".STATUS = '" + StateDBO.RUNNING_STATUS + "' " +
        				" AND " + CORR + ".VALUE IN (" + corrValues + ")";

        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            stmt = mConn.prepareStatement(query.toString());
            rs = stmt.executeQuery();
            if (rs.next()) {
            	instanceId = rs.getString("stateid");
            }

        } finally {
            if (rs != null) {
                rs.close();
            }

            if (stmt != null) {
                stmt.close();
            }
        }
        
    	return instanceId;
    }
    
    private String getCorrelationValues(List<CorrelationVal> corrIds) {
        Iterator<CorrelationVal> iter = corrIds.iterator();
        StringBuffer corrValues = new StringBuffer();
        int i = 1;

        if (corrIds.size() > 0) {
            while (iter.hasNext()) {
                if (i > 1)
                    corrValues.append(",");
                corrValues.append("'" + iter.next() + "'");
                i++;
            }
        } else {
            corrValues.append("''");
        }
        return corrValues.toString();
    }
    
    /**
     * wrapper to set the initial value of the autoCommit value 
     * and close the underlying connection.
     * @throws SQLException
     */
    public void close() throws SQLException {
    	mConn.close();
    }    
}
