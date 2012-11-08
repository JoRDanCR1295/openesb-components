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
 * @(#)ConnectionUnavailableException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.hl7bc.extservice.persist.connection;

import com.sun.jbi.hl7bc.extservice.persist.dbo.DBObject;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;


import com.sun.jbi.hl7bc.I18n;

/**
 * DOCUMENT ME!
 * 
 * @author Sun Microsystems
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
     * Closes the underlying connection
     */
    public void close() throws SQLException{
    	mConn.close();
    }
    
    public DatabaseMetaData getMetaData() throws SQLException{
    	return mConn.getMetaData();
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
                throw new SQLException(I18n.msg("E0296: Didn't update any rows, Should have updated atleast one row"));
            }
        } finally {
            if (stmt != null) {
                stmt.close();
            }
        }
    }

    public void insert(List objs) throws SQLException {
        if ((objs == null) || (objs.size() < 1)) {
            return;
        }

        if (mConn.getMetaData().supportsBatchUpdates()) {
            batchInsert(objs);
        } else {
            PreparedStatement stmt = null;

            try {
                DBObject obj;

                for (int i = 0, size = objs.size(); i < size; i++) {
                    obj = (DBObject) objs.get(i);
                    stmt = constructInsertStmt(obj);

                    int updatedRows = stmt.executeUpdate();

                    if (updatedRows < 1) {
                        throw new SQLException(I18n.msg("E0296: Didn't update any rows, Should have updated atleast one row"));
                    }
                }
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
    public int update(DBObject obj) throws SQLException {
        if (obj == null) {
            return -1;
        }
        int updatedRows = -1;

        PreparedStatement stmt = null;
        try {
            stmt = constructUpdateStmt(obj);

            updatedRows = stmt.executeUpdate();

        } finally {
            if (stmt != null) {
                stmt.close();
            }
        }

        return updatedRows;
    }

    public void update(List objs) throws SQLException {
        if ((objs == null) || (objs.size() < 1)) {
            return;
        }

        if (mConn.getMetaData().supportsBatchUpdates()) {
            batchUpdate(objs);
        } else {
            PreparedStatement stmt = null;

            try {
                for (int i = 0, size = objs.size(); i < size; i++) {
                    stmt = constructUpdateStmt((DBObject) objs.get(i));

                    int updatedRows = stmt.executeUpdate();

                    if (updatedRows < 1) {
                        throw new SQLException(I18n.msg("E0296: Didn't update any rows, Should have updated atleast one row"));
                    }
                }
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
                throw new SQLException(I18n.msg("E0296: Didn't update any rows, Should have updated atleast one row"));
            }
        } finally {
            if (stmt != null) {
                stmt.close();
            }
        }
    }

    public void delete(List objs) throws SQLException {
        if ((objs == null) || (objs.size() < 1)) {
            return;
        }

        if (mConn.getMetaData().supportsBatchUpdates()) {
            batchDelete(objs);
        } else {
            PreparedStatement stmt = null;

            try {
                for (int i = 0, size = objs.size(); i < size; i++) {
                    stmt = constructDeleteStmt((DBObject) objs.get(i));

                    int updatedRows = stmt.executeUpdate();

                    if (updatedRows < 1) {
                        throw new SQLException(I18n.msg("E0296: Didn't update any rows, Should have updated atleast one row"));
                    }
                }
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
    public ResultSet getRow(DBObject dbo) throws SQLException {
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            stmt = mConn.prepareStatement(dbo.getQueryStmt());
            dbo.fillQueryStmt(stmt);
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

        try {
            obj.fillInsertStmt(stmt);
        } catch (Exception ex) {
            if (stmt != null) {
                stmt.close();
            }
        }

        return stmt;
    }

    private PreparedStatement constructUpdateStmt(DBObject obj) throws SQLException {
        String stmtStr = obj.getUpdateStmt();
        PreparedStatement stmt = mConn.prepareStatement(stmtStr);

        try {
            obj.fillUpdateStmt(stmt);
        } catch (Exception ex) {
            if (stmt != null) {
                stmt.close();
            }
        }

        return stmt;
    }

    private PreparedStatement constructDeleteStmt(DBObject obj) throws SQLException {
        String stmtStr = obj.getDeleteStmt();
        PreparedStatement stmt = mConn.prepareStatement(stmtStr);

        try {
            obj.fillDeleteStmt(stmt);
        } catch (Exception ex) {
            if (stmt != null) {
                stmt.close();
            }
        }

        return stmt;
    }

    protected void batchInsert(List objs) throws SQLException {
        PreparedStatement stmt = null;

        try {
            stmt = constructInsertStmt((DBObject) objs.get(0));
            stmt.addBatch();

            DBObject obj = null;

            for (int i = 1, size = objs.size(); i < size; i++) {
                obj = (DBObject) objs.get(i);
                obj.fillInsertStmt(stmt);
                stmt.addBatch();
            }

            int[] executedRows = stmt.executeBatch();

            for (int i = 0; i < executedRows.length; i++) {
                if (executedRows[i] == Statement.EXECUTE_FAILED) {
                    throw new SQLException(I18n.msg("E0297: One of the Queries in the batch didn't update any rows, Should have updated atleast one row"));
                }

                ;
            }
        } finally {
            if (stmt != null) {
                stmt.close();
            }
        }
    }

    protected void batchUpdate(List objs) throws SQLException {
        PreparedStatement stmt = null;

        try {
            stmt = constructUpdateStmt((DBObject) objs.get(0));
            stmt.addBatch();

            DBObject obj = null;

            for (int i = 1, size = objs.size(); i < size; i++) {
                obj = (DBObject) objs.get(i);
                obj.fillUpdateStmt(stmt);
                stmt.addBatch();
            }

            int[] executedRows = stmt.executeBatch();

            for (int i = 0; i < executedRows.length; i++) {
                if (executedRows[i] == Statement.EXECUTE_FAILED) {
                    throw new SQLException(I18n.msg("E0297: One of the Queries in the batch didn't update any rows, Should have updated atleast one row"));
                }

                ;
            }
        } finally {
            if (stmt != null) {
                stmt.close();
            }
        }
    }

    protected void batchDelete(List objs) throws SQLException {
        PreparedStatement stmt = null;

        try {
            stmt = constructDeleteStmt((DBObject) objs.get(0));
            stmt.addBatch();

            DBObject obj = null;

            for (int i = 1, size = objs.size(); i < size; i++) {
                obj = (DBObject) objs.get(i);
                obj.fillDeleteStmt(stmt);
                stmt.addBatch();
            }

            int[] executedRows = stmt.executeBatch();

            for (int i = 0; i < executedRows.length; i++) {
                if (executedRows[i] == Statement.EXECUTE_FAILED) {
                    throw new SQLException(I18n.msg("E0297: One of the Queries in the batch didn't update any rows, Should have updated atleast one row"));
                }

                ;
            }
        } finally {
            if (stmt != null) {
                stmt.close();
            }
        }
    }

}
