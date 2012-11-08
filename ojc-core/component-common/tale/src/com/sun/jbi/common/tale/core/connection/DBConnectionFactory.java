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
 * @(#)DBConnectionFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.connection;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import org.apache.derby.jdbc.ClientConnectionPoolDataSource;

import com.sun.jbi.common.tale.core.util.TaleConfigurationException;
import com.sun.jbi.common.tale.core.util.I18n;
import com.sun.jbi.common.util.Util;

/**
 * @author Sun Microsystems
 */
public class DBConnectionFactory {
    public enum DBType {
        /** derby */
        DERBY_DB("Apache Derby"),

        /** mysql */
        MYSQL_DB("MySQL"),

        /** Oracle */
        ORCL_DB("Oracle"),

        /** sybase */
        SYBS_DB("Sybase"),  // TODO verify correct name

        /** DB2 */
        DB2_DB("DB2");      // TODO verify correct name
        
        private String mName;
        
        private DBType(String name) {
            mName = name;
        }
        
        public String getName() {
            return mName;
        }
    }
	
	/* DB Type */
    private DBType mDbType;

    /* The datasource for DB connections */
    private DataSource mDataSourceInstance;
    
    private long mIdentityColumnValue = 100;

    /* Log handle */
    private static final Logger mLogger = Logger.getLogger(DBConnectionFactory.class.getName());
    
    protected DBConnectionFactory(){
        //constructor added for supporting test class
        //TODO: needs to be removed.
    }
    
    /**
     * Creates a connection factory.
     * <p>
     * The following properties are expected to be defined in the specified configuration:
     * <ul>
     *      <li>{@link ConnectionProperties#DB_TYPE}</li>
     *      <li>{@link ConnectionProperties#DB_INSTANCE}</li>
     *      <li>{@link ConnectionProperties#DB_HOST}</li>
     *      <li>{@link ConnectionProperties#DB_PORT}</li>
     *      <li>{@link ConnectionProperties#DB_USERNAME}</li>
     *      <li>{@link ConnectionProperties#DB_PASSWORD}</li>
     * </ul></p>
     * 
     * @param prop The database configuration.
     */
    public DBConnectionFactory(Properties prop) {
        this(prop, null);
    }
    
    /**
     * <p>
     * The following properties are expected to be defined in the specified configuration:
     * <ul>
     *      <li>{@link ConnectionProperties#DB_TYPE}</li>
     *      <li>{@link ConnectionProperties#DatabaseJNDIName}</li>
     * </ul></p>
     * <p>
     * If an <code>InitialContext</code> is not specified, the following properties
     * are also required <b>in place of</b> {@link ConnectionProperties#DatabaseJNDIName}:
     * <ul>
     *      <li>{@link ConnectionProperties#DB_INSTANCE}</li>
     *      <li>{@link ConnectionProperties#DB_HOST}</li>
     *      <li>{@link ConnectionProperties#DB_PORT}</li>
     *      <li>{@link ConnectionProperties#DB_USERNAME}</li>
     *      <li>{@link ConnectionProperties#DB_PASSWORD}</li>
     * </ul></p>
     * 
     * @param prop The database configuration.
     * @param initialContext
     */
    public DBConnectionFactory(Properties prop, InitialContext initialContext) {    	
        String datasourceJndiName = prop.getProperty(ConnectionProperties.DatabaseJNDIName);
        try {
            if (initialContext == null) {
                mDbType = initDBType(prop.getProperty(ConnectionProperties.DB_TYPE));
                initDataSource(prop);
            }
            else {
                try {
                    setDataSource((DataSource) initialContext.lookup(datasourceJndiName));
                } 
                catch (NamingException ex) {
                    String msg = I18n.loc("TALE-7001: Exception while looking up ALESE Schema JDBC Resource with the JNDI name {0}. " +
                            "Exception Detals : {1} ", datasourceJndiName, ex.getMessage());
        		    mLogger.log(Level.SEVERE, msg);  
                    throw new TaleConfigurationException(msg);
                } 
         
                mDbType = initDBType(null); // look up from datasource
    		}
        }
        catch (SQLException e) {
            // TODO is this correct message for both scenarios above? should be "Cannot identify DB"?
        	String msg = I18n.loc("TALE-7003: Could not get Database Connection Meta Data from JDBC resource with the JNDI name {0}. " +
        			"\nException Details : {1} ", datasourceJndiName, e.getMessage());
			mLogger.log(Level.SEVERE, msg);  
        	throw new TaleConfigurationException(msg);
		}
    }
    
    public void readIdentity() {
        Connection conn = null;
        ResultSet rs = null;
        try {
            conn = mDataSourceInstance.getConnection();
            String sqlQueryIdentity = "select MAX(MESG_ID) from ALESE_USER.CSF_LOGGER_LOG";
            rs = conn.createStatement().executeQuery(sqlQueryIdentity);
            if (rs.next()) {
                String s = rs.getString(1);
                if ((s != null) && (s.trim().length() > 0)) {
                    mIdentityColumnValue = Long.parseLong(s) + 1;
                }
            }
        } 
        catch (SQLException e) {
            String msg = I18n.loc(
                            "TALE-7010: Not able to read identity value due to DB error, Exception Details : {0} ",
                            e.getMessage());
            mLogger.log(Level.SEVERE, msg);
            throw new TaleConfigurationException(msg);
        } 
        finally {
            try {
                rs.close();
                conn.close();
            } 
            catch (SQLException e) { // ignore
            }
        }
    }

    protected void initDataSource(Properties prop) {
        String dbName = prop.getProperty(ConnectionProperties.DB_INSTANCE);
        String server = prop.getProperty(ConnectionProperties.DB_HOST);
        String port = prop.getProperty(ConnectionProperties.DB_PORT);
        String user = prop.getProperty(ConnectionProperties.DB_USERNAME);
        String pswd = prop.getProperty(ConnectionProperties.DB_PASSWORD);
        // TODO log these values at FINE? or maybe CONFIG?

        switch (getDBType()) {
            case DERBY_DB: {
                ClientConnectionPoolDataSource ccpds = new ClientConnectionPoolDataSource();
                ccpds.setDatabaseName(dbName);
                ccpds.setServerName(server);
                ccpds.setPortNumber(Util.parseInt(port, 1527));
                ccpds.setUser(user);
                ccpds.setPassword(pswd);
                setDataSource(ccpds);
                break;
            }
            default: {
                String msg = I18n.loc(
                        "TALE-7016: Failed to initialize DataSource - Invalid Configuration!");
                mLogger.severe(msg);
                throw new TaleConfigurationException(msg);
            }
        }
    }
    
	/**
     * The following will also determine if the configured database is one of
     * the supported types.
     * 
     * @param prop
     * @return
     * @throws SQLException
     */
    private DBType initDBType(String databaseProductName) throws SQLException {
        Connection conn = null;
        DBType dbType = null;

        try {
            if (Util.isEmpty(databaseProductName)) {
                conn = mDataSourceInstance.getConnection();
                DatabaseMetaData meta = conn.getMetaData();
                databaseProductName = meta.getDatabaseProductName();
            }
            
            boolean identifiedDB = false;
            try {
                for (DBType type : DBType.values()) {
                    if (databaseProductName.equals(type.getName())) {
                        dbType = type;
                        break;
                    }
                }
                identifiedDB = (dbType != null);
            }
            catch (Exception e) { /* ignore */ }
            
            if (!identifiedDB) {
                // TODO update list of supported DBs
                String msg = I18n.loc(
                        "TALE-7002: Database Not Supported: {0} Supported Database: {1}",
                        databaseProductName, DBType.DERBY_DB); 
                throw new TaleConfigurationException(msg);
            }
        } 
        finally {
            if (conn != null) {
                try {
                    conn.close();
                } 
                catch (SQLException ex) {
                    mLogger.log(
                          Level.WARNING,
                          I18n.loc("TALE-6001: Error when trying to close DB cursors"),
                          ex);
                }
            }
        }
        
        return dbType;
    }
    
    /**
	 * return database type
	 * 
	 * @return int database type
	 */
    public DBType getDBType() {
        return mDbType;
    }

    protected void setDBType(DBType type) {
        mDbType = type;
    }
    
    /**
     * Creates and returns a DBConnection that has a non transactional Connection object as its underlying 
     * connection.
     * Created using the resource type javax.sql.DataSource.
     * @return com.sun.jbi.engine.ale.connection.DBConnection
     * @throws SQLException 
     * @throws Exception throws Exception if a connection could not be obtained.
     */
    public DBConnection createDBConnection() throws SQLException {
        Connection conn = mDataSourceInstance.getConnection();
        DBConnection dbConn = new DBConnection(conn);
        return dbConn;
    }
    
    /**
     * Returns dataSource
     * @return DataSource
     */
    public DataSource getDataSource () {
        return mDataSourceInstance;
    }

    protected void setDataSource(DataSource ds) {
        mDataSourceInstance = ds;
    }
    
    public synchronized String getNextIdentityColumnValue() {
        return String.valueOf(mIdentityColumnValue++);
    }
}
