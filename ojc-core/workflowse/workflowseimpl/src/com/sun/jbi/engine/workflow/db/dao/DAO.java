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
 * @(#)DAO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.dao;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.List;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.engine.workflow.db.connection.Connection;
import com.sun.jbi.engine.workflow.db.connection.ConnectionManager;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.internationalization.Messages;

/**
 * This class is the base database class which contains
 * methods that all inherited classes will
 * need.
 *
 * @author SeeBeyond Technology Corporation
 * @version 
 *
 * @since eGate50
 */
public abstract class DAO {

    private static Logger LOGGER = Logger.getLogger(DAO.class.getName());       

    /** ASIS */
    public static final String ASIS = "AsIs";
    
    /** Derby database **/
    public static final String DERBY = "Derby";
    
    /** Oracle database **/
    public static final String ORACLE = "Oracle";
    
    public static final String SQLSERVER = "SQL Server";
    
    public static final String SYBASE = "Sybase";
    
    public static final String DB2 = "DB2";
    
    public static final String MYSQL = "MySQL";
    
    public static final DBType DERBY_TYPE = new DBType (DERBY);
    public static final DBType ORACLE_TYPE = new DBType (ORACLE);
    public static final DBType SQLSERVER_TYPE = new DBType (SQLSERVER);
    public static final DBType SYBASE_TYPE = new DBType (SYBASE);
    public static final DBType DB2_TYPE = new DBType (DB2);
    public static final DBType MYSQL_TYPE = new DBType (MYSQL);
    

    /** date function */
    protected String dateFunction = "";

    /** time function */
    protected String timeFunction = "";

    /** no lock function */
    protected String noLockFunction = "";

    /** row lock function */
    protected String rowLockFunction = "";

    /** DB Connection Used to free the connection*/
    protected Connection dbConn;

	protected static boolean debug = false;

    /**
     * Constructor. 
     *
     * @param connProp The db connection properties.
     *
     * @exception DAOException Problem with initializing database.
     */
    public DAO() throws DAOException {
		debug = LOGGER.isLoggable(Level.FINE);
        initialize();
    }

    /**
     * Reads the database meta data and database type.  Initializes database date/time functions
     * and sql functions according to the type of database.
     *
     * @exception SQLException Problem with reading the database meta data.
     */
    private void initialize() throws DAOException {
        try {
            dbConn = ConnectionManager.getInstance().getConnection(null);
        } catch (Exception se) {
            throw new DAOException (se);
        }
    }

    /**
     * Get the db connection.
     *
     * @return Connection the db connection.
     *
     * @throws SQLException SQLException
     */
    private Connection getDBConnection() throws SQLException {
        if (dbConn == null) {
            // TO_DO : This should never happen. If this happens the connection
            // pool need to know. Hopefully the connection should have been returned 
            // to the pool prior to this, otherwise U get into connection "Accounting" issues
            try {
                dbConn = ConnectionManager.getInstance().getConnection();
            } catch (SQLException ex) {
                throw ex;
            }
        }
        return dbConn;
    }
    
    /**
     * gets the db type pertaining to the connection
     */
    public abstract DBType getDBType();




    /**
     * Returns the database time function according to the type of database.
     *
     * @return The database time function.  Returns "" if database type is DB_TYPE_UNKNOWN.
     */
    public String getTimeFunction() {
        return timeFunction;
    }

    /**
     * Returns the database date function according to the type of database.
     *
     * @return The database date function.  Returns "" if database type is DB_TYPE_UNKNOWN.
     */
    public String getDateFunction() {
        return dateFunction;
    }

    /**
     * Returns the database sql NO LOCK function according to the type of database.
     *
     * @return The database sql NO LOCK function.  Returns "" if database type is  DB_TYPE_UNKNOWN.
     */
    public String getNoLockFunction() {
        return noLockFunction;
    }

    /**
     * Returns the database sql ROW LOCK function according to the type of database.
     *
     * @return The database sql ROW LOCK function.  Returns "" if database type is DB_TYPE_UNKNOWN.
     */
    public String getRowLockFunction() {
        return rowLockFunction;
    }

    /**
     * Gets the maximum varchar length size based on the database type.
     *
     * @return The maximum varchar length size.
     */
    public abstract int getMaxVarcharLength() ;
 
	/**
	 * Close db connection.
	 *
	 * @throws SQLException SQLException
	 */
	public void close() throws SQLException {
        if (debug) {
            LOGGER.fine("-->> returning connection to pool");
        }
        if (dbConn != null) {
            ConnectionManager.getInstance().freeConnection(dbConn);
            dbConn = null;
        }
		// TO-DO: The connection variable has to be set to null. For, the con is 
		// released to pool and could get allocated to any other DAO. This DAO
		// should not refer to the same connection any more. This could happen
		// in the case of Monitor/Reporting type of usages where the DAO is 
		// exposed outside the Adapter/Connector framework
	}

    /**
     * gets the current auto commit flag
     *
     * @return boolean
     *
     * @throws SQLException SQLException
     */
    public boolean getAutoCommit() throws SQLException {
        if (dbConn != null) {
            return dbConn.getConnection().getAutoCommit();
        }
        return false;
    }

    /**
     * sets the auto commit flag
     *
     * @param autoCommit boolean
     *
     * @throws SQLException SQLException
     */
    public void setAutoCommit(boolean autoCommit) throws SQLException {
        if (dbConn != null) {
            dbConn.setAutoCommit(autoCommit);
        }
    }

    /**
     * commits the transaction
     *
     * @throws SQLException SQLException
     */
    public void commit() throws SQLException {
        if (dbConn != null) {
            dbConn.commit();
        }
    }

    /**
     * rollbacks the transaction
     *
     * @throws SQLException SQLException
     */
    public void rollback() throws SQLException {
        if (dbConn != null) {
            dbConn.rollback();
        }
    }


    protected abstract void fillInParms(PreparedStatement ps, List columns, List columnTypes)
    throws SQLException;
    
    /**
     * Generate a WHERE SQL clause based on the where table values/column names
     *
     * @param where Hashtable
     * @param command StringBuffer
     * @param columns Vector
     *
     * @return command
     */
    public String whereClause(Hashtable where, StringBuffer command, Vector columns) {
        if ((where == null) || (where.size() == 0)) {
            return null;
        }

        int amount = 0;
        command.append(" where ");

        for (Enumeration e = where.keys(); e.hasMoreElements(); amount++) {
            String columnName = (String) e.nextElement();
            Object value = where.get(columnName);

            if (amount > 0) {
                command.append(" AND ");
            }

            if (columnName.equals(ASIS)) {
                command.append(value.toString());
            } else {
                command.append(columnName);
                command.append(" = ?");
                columns.addElement(value);
            }
        }

        return command.toString();
    }

    /**
     * Generate an update prepared set SQL clause based on the where table values/column names
     *
     * @param command StringBuffer
     * @param modified Hashtable
     *
     * @return command
     */
    public String setPreparedClause(StringBuffer command, Hashtable modified) {
        // Construct the Sql statement
        // Use each entry from the modified hashtable for set elements
        command.append(" set ");

        int count = 0;

        for (Enumeration e = modified.keys(); e.hasMoreElements(); count++) {
            String key = (String) e.nextElement();

            if (count > 0) {
                command.append(", ");
            }

            if (key.equals(ASIS)) {
                command.append((String) modified.get(key));
            } else {
                command.append(key);
                command.append(" = ?");
            }
        }

        return command.toString();
    }

    /**
     * Execute the sql statement.
     *
     * @param command The sql statement.
     *
     * @return RowSet RowSet object.
     *
     * @throws SQLException SQLException
     */
    public ResultSet executeQuery(String command) throws SQLException {
        return executeQuery(command, null, null);
    }

    /**
     * Executes a common sql query and returns a RowSet. This will execute a straight sql
     * statement or create a prepared statement
     *
     * @param command the sql command
     * @param columns list of bind values
     * @param columnTypes arrary of column types (java.sql.Type) for nulls
     *
     * @return a RowSet
     *
     * @exception SQLException if there is a db sql problem
     */
    public ResultSet executeQuery(String command, List columns, List columnTypes)
                                    throws SQLException {
        Statement st = null;
        PreparedStatement ps = null;
        ResultSet result = null;

        try {
            if (dbConn == null) getDBConnection();
            if (columns != null) {
                ps = dbConn.prepareStatement(command);
                fillInParms(ps, columns, columnTypes);
                result = ps.executeQuery();
            } else {
                st = dbConn.createStatement();
                result = st.executeQuery(command);
            }

            return result;
        } catch (SQLException sqe) {
            if (isDBConnectionBroken(sqe)) {
                return executeQuery(command, columns, columnTypes);
            }
            throw sqe;
        } finally {
        	// Since we are using the result set we cannot close
       		// any of these. These objects would be garbage collected, which is OK
            //These are closed by the caller of this method.

        }
    }

    /**
     * Executes a common sql query and returns a RowSet. This will execute a straight sql
     * statement or create a prepared statement
     *
     * @param command the sql command
     * @param columns list of bind values
     * @param columnTypes arrary of column types (java.sql.Type) for nulls
     *
     * @return a RowSet
     *
     * @exception SQLException if there is a db sql problem
     */
    public ResultSet executeQuery(String command, List columns, List columnTypes, int maxRows)
                                    throws SQLException {
        Statement st = null;
        PreparedStatement ps = null;
        ResultSet result = null;

        try {
            if (dbConn == null) getDBConnection();           
            if (columns != null) {
                ps = dbConn.prepareStatement(command);
                ps.setMaxRows(maxRows);
                fillInParms(ps, columns, columnTypes);
                result = ps.executeQuery();
            } else {
                st = dbConn.createStatement();
                st.setMaxRows(maxRows);
                result = st.executeQuery(command);
            }

            return result;
        } catch (SQLException sqe) {
            if (isDBConnectionBroken(sqe)) {
                return executeQuery(command, columns, columnTypes, maxRows);
            }
            throw sqe;
        } finally {
            // Since we are using the result set we cannot close
            // any of these. These objects would be garbage collected, which is OK
            //These are closed by the caller of this method.

        }
    }
    /**
     * Executes a sql update and returns row count
     *
     * @param command the sql command
     * @param columns list of bind values
     * @param columnTypes arrary of column types (java.sql.Type) for nulls
     *
     * @return list of return values rowcount and lob
     *
     * @exception SQLException if there is a db sql problem
     */
    public int executeUpdate(String command, List columns, List columnTypes)
                      throws SQLException {
        Statement st = null;
        PreparedStatement ps = null;
        int rowCount = 0;
        try {
            if (dbConn == null) getDBConnection();
            dbConn.setAutoCommit(true);
            if (columns != null) {
                ps = dbConn.prepareStatement(command);
                fillInParms(ps, columns, columnTypes);
                rowCount = ps.executeUpdate();
            } else {
                st = dbConn.createStatement();
                rowCount = st.executeUpdate(command);
            }
            return rowCount;
        } catch (SQLException sqe) {
            if (isDBConnectionBroken(sqe)) {
                return executeUpdate(command, columns, columnTypes);
            }
            throw sqe;
        } finally {
            if (st != null) {
                st.close();
                st = null;
            }
            dbConn.closePreparedStatement(ps);
        }
    }

    /**
     * Update a SQL command.
     *
     * @param command The SQL command.
     *
     * @return int The row count.
     *
     * @throws SQLException SQLException
     */
    protected int executeUpdate(String command) throws SQLException {
        return executeUpdate(command, null, null);
    }

    /**
     * Executes a sql and returns 1 for successful execution.
     *
     * @param command the sql command
     * @param columns list of bind values
     * @param columnTypes arrary of column types (java.sql.Type) for nulls
     *
     * @return 1 - successful execution otherwise execution failed.
     *
     * @exception SQLException if there is a db sql problem
     */
    protected List execute(String command, List columns, List columnTypes)
                   throws SQLException {
        Statement st = null;
        PreparedStatement ps = null;
        int result = -1;

        try {
            if (dbConn == null) getDBConnection();
            if (columns != null) {
                ps = dbConn.prepareStatement(command);
                fillInParms(ps, columns, columnTypes);
                ps.execute();
            } else {
                st = dbConn.createStatement();
                st.execute(command);
            }

            ArrayList retList = new ArrayList();
            retList.add(new Integer(result));
            retList.add(new Boolean(false));
            return retList;
        } catch (SQLException sqe) {
            if (isDBConnectionBroken(sqe)) {
                execute(command, columns, columnTypes);
            }
            throw sqe;
        } finally {
            if (st != null) {
                st.close();
                st = null;
            }
            dbConn.closePreparedStatement(ps);
        }
    }

    /**
     * Executes a sql with large data.
     *
     * @param command The sql command
     * @param seqId The sequence id.
     * @param data The large data in String.
     *
     * @return true if success otherwise false
     *
     * @throws SQLException SQLException
     */
    protected boolean insertCharacterStream(String command, long seqId, String data)
                                     throws SQLException {
        PreparedStatement ps = null;

        try {
            if (dbConn == null) getDBConnection();
            ps = dbConn.prepareStatement(command);
            ps.setLong(1, seqId);

            byte[] b = data.getBytes("UTF-8");

            ps.setCharacterStream(2, new StringReader(data), b.length);

            return ps.execute();
        } catch (SQLException sqe) {
            if (isDBConnectionBroken(sqe)) {
                return insertCharacterStream(command, seqId, data);
            }
            throw sqe;
        } catch (UnsupportedEncodingException uee) {
            uee.printStackTrace();
            throw new SQLException(I18n.loc("WLM-6036: Error with encoding setting") + ":" + uee.getMessage());
        } finally {
            dbConn.closePreparedStatement(ps);
        }
    }
    
    
    /**
     * Checks the exception thrown and verifies the exception type. If the execption
     * type is one of the listed sql states, invalidates the connection and gets new
     * connection from the connection pool.
     *
     * @param e SQLException
     * @return boolean
     * @throws DAOException
     */
    private boolean isDBConnectionBroken(SQLException e) throws SQLException {
        if (ConnectionManager.getInstance().isDBConnectionBroken(e, dbConn)) {
            this.dbConn = null;
            try {
                getDBConnection();
            } catch (SQLException se) {
                LOGGER.log(Level.SEVERE, "Giving up.. no connection recieved..");
                throw se;
            }
            return true;
        }
        return false;
    }

    /**
     * Set the various functions syntax for the specific Database
     *
     * @param dbType the type of database (Oracle, SqlServer, Sybase, DB2)
     */
     public  abstract void setDBFunctions() ;

    /**
     * This method takes the SQLException error code and finds out
     * if the errorcode is a unique constraint error code
     *
     * @param errorCode DOCUMENT ME!
     *
     * @return true if errocode is unique constraint violation, false otherwise
     */
    public abstract boolean isErrorCodeUnique(int errorCode) ;

    /**
     * This gets a String field stored in a BLOB column in the database
     *
     * @param columnName the column name
     * @param cs the RowSet
     *
     * @return the extracted String
     */
    public abstract byte[] getBlob(String columnName, ResultSet cs) throws SQLException ;

    /**
     * This gets a String field stored in a CLOB column in the database
     *
     * @param columnName the column name
     * @param cs the RowSet
     *
     * @return the extracted String
     */
    public abstract String getClob(String columnName, ResultSet cs) throws SQLException, IOException;
    
    

}
