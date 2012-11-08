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
 * @(#)DBSchemaCreation.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.persist;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.common.tale.core.connection.ConnectionProperties;
import com.sun.jbi.common.tale.core.connection.DBConnection;
import com.sun.jbi.common.tale.core.connection.DBConnectionFactory;
import com.sun.jbi.common.tale.core.connection.DBConnectionFactory.DBType;
import com.sun.jbi.common.tale.core.util.TaleConfigurationException;
import com.sun.jbi.common.tale.core.util.I18n;

/**
 * @author Sun Microsystems
 */
public class  ALEDBSchemaCreation {

    private static final Logger LOGGER = Logger.getLogger(ALEDBSchemaCreation.class.getName());
    private static String DELIMITER = ";"; //$NON-NLS-1$
    /** CSF_REP_USERS table name */
    public static final String CSF_REP_USERS = "CSF_REP_USERS "; //$NON-NLS-1$
    /** CSF_JMS_CHANNEL table name */
    public static final String CSF_JMS_CHANNEL = "CSF_JMS_CHANNEL "; //$NON-NLS-1$
    /** CSF_PAYLOAD_LOG table name */
    public static final String CSF_PAYLOAD_LOG = "CSF_PAYLOAD_LOG "; //$NON-NLS-1$
    /** CSF_PAYLOAD_STORE table name */
    public static final String CSF_PAYLOAD_STORE = "CSF_PAYLOAD_STORE "; //$NON-NLS-1$
    /** CSF_CME_LOG table name */
    public static final String CSF_CME_LOG = "CSF_CME_LOG "; //$NON-NLS-1$
    /** CSF_USER_FIELDS table name */
    public static final String CSF_USER_FIELDS = "CSF_USER_FIELDS "; //$NON-NLS-1$
    /** CSF_ALERTER_GROUPS table name */
    public static final String CSF_ALERTER_GROUPS = "CSF_ALERTER_GROUPS "; //$NON-NLS-1$
    /** CSF_ALERTER_CHANNELS table name */
    public static final String CSF_ALERTER_CHANNELS = "CSF_ALERTER_CHANNELS "; //$NON-NLS-1$
    /** CSF_ALERTER_CODES table name */
    public static final String CSF_ALERTER_CODES = "CSF_ALERTER_CODES "; //$NON-NLS-1$
    /** CSF_LOGGER_CHANNELS table name */
    public static final String CSF_LOGGER_CHANNELS = "CSF_LOGGER_CHANNELS "; //$NON-NLS-1$
    /** CSF_LOGGER_CODES table name */
    public static final String CSF_LOGGER_CODES = "CSF_LOGGER_CODES "; //$NON-NLS-1$
    /** CSF_ERROR_CODES table name */
    public static final String CSF_ERROR_CODES = "CSF_ERROR_CODES "; //$NON-NLS-1$
    /** CSF_ALERTER_LOG table name */
    public static final String CSF_ALERTER_LOG = "CSF_ALERTER_LOG "; //$NON-NLS-1$
    /** CSF_ALERT_RESOLUTION table name */
    public static final String CSF_ALERT_RESOLUTION = "CSF_ALERT_RESOLUTION "; //$NON-NLS-1$
    /** CSF_LOGGER_LOG table name */
    public static final String CSF_LOGGER_LOG = "CSF_LOGGER_LOG "; //$NON-NLS-1$
    /** CSF_ERROR_LOG table name */
    public static final String CSF_ERROR_LOG = "CSF_ERROR_LOG "; //$NON-NLS-1$
    /** CSF_ERROR_RESOLUTION table name */
    public static final String CSF_ERROR_RESOLUTION = "CSF_ERROR_RESOLUTION "; //$NON-NLS-1$
    /** CSF_RECONCILIATION_APPS table name */
    public static final String CSF_RECONCILIATION_APPS = "CSF_RECONCILIATION_APPS "; //$NON-NLS-1$
    /** CSF_RECONCILIATION_LOG table name */
    public static final String CSF_RECONCILIATION_LOG = "CSF_RECONCILIATION_LOG "; //$NON-NLS-1$
    /** CSF_USER_AUDIT_LOG table name */
    public static final String CSF_USER_AUDIT_LOG = "CSF_USER_AUDIT_LOG "; //$NON-NLS-1$
    
    private static String [] PERSISTENCE_TABLES = new String [] {CSF_REP_USERS.trim(), 
        CSF_JMS_CHANNEL.trim(), CSF_PAYLOAD_LOG.trim(), CSF_PAYLOAD_STORE.trim(), CSF_CME_LOG.trim(),
        CSF_USER_FIELDS.trim(), CSF_ALERTER_GROUPS.trim(), CSF_ALERTER_CHANNELS.trim(), 
        CSF_ALERTER_CODES.trim(), CSF_LOGGER_CHANNELS.trim(), CSF_LOGGER_CODES.trim(), 
        CSF_ERROR_CODES.trim(), CSF_ALERTER_LOG.trim(), CSF_ALERT_RESOLUTION.trim(),
        CSF_LOGGER_LOG.trim(), CSF_ERROR_LOG.trim(), CSF_ERROR_RESOLUTION.trim(),
        CSF_RECONCILIATION_APPS.trim(), CSF_RECONCILIATION_LOG.trim(), CSF_USER_AUDIT_LOG.trim()};
    
    private static String CREATE_TABLES_SCRIPT = "create_alese_tables.sql"; //$NON-NLS-1$
    private static String DROP_TABLES_SCRIPT = "drop_alese_tables.sql"; //$NON-NLS-1$
    private static String TRUNCATE_TABLES_SCRIPT = "truncate_alese_tables.sql"; //$NON-NLS-1$	
    private static String POPULATE_TABLES_SCRIPT = "populate_alese_tables.sql";
    
    private static ALEDBSchemaCreation mSingleton = new ALEDBSchemaCreation();
    
    /**
     * Creates the tables required for ALE-SE
     * @param prop database properties
     * @param connFac DBConnectionFactory
     */
    public void createTables(Properties prop, DBConnectionFactory connFac) {
        StringTokenizer tokens = createScriptPath(connFac.getDBType(), getCreateScriptName());
        executeScript(prop, tokens, connFac);
    }
    
    /**
     * Drops the ALE-SE tables
     * @param prop database properties
     * @param connFac DBConnectionFactory
     */
    public void dropTables(Properties prop, DBConnectionFactory connFac) {
        StringTokenizer tokens = createScriptPath(connFac.getDBType(), getDropScriptName());
        executeScript(prop, tokens, connFac);
    }
    
    /**
     * Truncates the ALE-SE tables
     * @param prop database properties
     * @param connFac DBConnectionFactory
     */
    public void truncateTables (Properties prop, DBConnectionFactory connFac) {
        StringTokenizer tokens = createScriptPath(connFac.getDBType(), getTruncateScriptName());
        executeScript(prop, tokens, connFac);
    }
    
    /**
     * Populates the ALE-SE tables with sample data
     * @param prop database properties
     * @param connFac DBConnectionFactory
     */
    public void populateTables (Properties prop, DBConnectionFactory connFac) {
        StringTokenizer tokens = createScriptPath(connFac.getDBType(), getPopulateScriptName());
        executeScript(prop, tokens, connFac);
    }

    /**
     * Check to see if all the tables required by ALE-SE are present
     * @param prop database properties
     * @param connFac DBConnectionFactory
     * @return boolean true: if valid schema exists in database false: otherwise
     */
    public boolean checkTablesIntegrity(Properties prop, DBConnectionFactory connFac) {
        
        boolean allTablesPresent = false;
        String dbUserName = null;
        ResultSet resultSet = null;
        DBConnection dbConn = null;
        
        // DEVNOTE: Do not try again if there is a SQLException, since this is a startup method. If the method
        // fails due to DBUnavailability (or anything else for that matter) the ALE SE will not start.
        //Check for all tables present
        int numTablesFound = 0;

        try {
            dbConn = connFac.createDBConnection();
            Connection conn = dbConn.getUnderlyingConnection();
            dbUserName = conn.getMetaData().getUserName();            
            
            String [] objectsToSearchFor = {"TABLE"};
            resultSet = conn.getMetaData().getTables(null, "%", "%", objectsToSearchFor); //$NON-NLS-1$
            while (resultSet.next() && (numTablesFound != getTabels().length)) {                
                 String rsTableName = resultSet.getString("TABLE_NAME");                
                for (int i = 0; i < getTabels().length; i++) {
                    String tableItem = getTabels()[i];
                    if (rsTableName.equalsIgnoreCase(tableItem)) { //$NON-NLS-1$
                        //Found a table, increment the counter
                        numTablesFound++;
                        break;
                    }
                }
            }
        } catch (Exception e) {        	
            String msg = I18n.loc("TALE-7004: Exception occured while verifying that all the " + 
                    "tables required for ALE-SE are present in the schema User {0}. " +
                    "\nException Details : ", dbUserName, e.getMessage());        	
            throw new TaleConfigurationException (msg);
        } finally {
            if (resultSet != null) {
                try {
                    resultSet.close();
                } catch (SQLException resCloseExcp) {
                    LOGGER.log(Level.WARNING, 
                        I18n.loc("TALE-6002: Exception occured while closing a JDBC Resultset"), resCloseExcp);
                }
            }
            if (dbConn != null) {
                try {
                    dbConn.close(); // this wrapper takes care of setting the initial value of setAutoCommit
                } catch (SQLException ex) {
                    LOGGER.log(Level.WARNING, I18n.loc("TALE-6003: Exception while closing a JDBC connection"), ex);
                }
            }
        }
        
        if (numTablesFound == getTabels().length) { // found all tables
            allTablesPresent = true;
            return allTablesPresent;
        } else if (numTablesFound != 0) { // some tables missing. ALE-SE schema corrupt, should not attempt to create tables. 
            String datasourceJndiName = prop.getProperty(ConnectionProperties.DatabaseJNDIName);
        	String msg = I18n.loc("TALE-7008: ALESE Schema corrupt for the configured JDBC Resource {0}. " +
        			"Please verify the schema and that it contains all the required tables.", datasourceJndiName);
        	throw new TaleConfigurationException(msg);
        }        
        return false;
    }
    
    private StringTokenizer createScriptPath(DBType dbType, String scriptName) {
        String scriptsPath = "impl/scripts/"; //$NON-NLS-1$
        switch (dbType) {
            case DERBY_DB:
                scriptsPath = scriptsPath + "derby/" + scriptName;
                break;
            case DB2_DB:    
                scriptsPath = scriptsPath + "db2/" + scriptName; //$NON-NLS-1$
                break;
        }
        
        // we want : scriptsPath=scriptsPath+"derby/create_ALE_schema.sql";
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        InputStream is = null;
        
        try {
            is = ALEDBSchemaCreation.class.getResourceAsStream(scriptsPath);
            
            int c;
            
            while ((c = is.read()) != -1) {
                os.write(c);
            }
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, e.getMessage());
            LOGGER.log(Level.SEVERE, I18n.loc("TALE-7005: Failed to load the schema scripts"), e);
        } finally {
            try {
                if (is != null) {
                    is.close();
                }
                
                if (os != null) {
                    os.close();
                }
            } catch (IOException ex) {
                LOGGER.log(Level.WARNING,
                		I18n.loc("TALE-6004: Error closing the streams"), ex); //$NON-NLS-1$
                throw new RuntimeException(ex);
            }
        }
        
        String script = new String(os.toByteArray());
        StringTokenizer tokens = new StringTokenizer(script, DELIMITER);
        
        return tokens;
    }

    private static void executeScript(
    		Properties prop, StringTokenizer tokens, DBConnectionFactory connFactory) {
        Connection conn = null;
        Statement stmt = null;
        DBConnection dbConn = null;
        
        // DEVNOTE: Do not try again if there is a SQLException, since this is a startup method. If the method
        // fails due to DBUnavailability (or anything else for that matter) the ALE SE will not start.
        try {
            dbConn = connFactory.createDBConnection();
            conn = dbConn.getUnderlyingConnection();
            conn.setAutoCommit(false);
            stmt = conn.createStatement();

            String element = null;
            for (int i = 0, size = tokens.countTokens(); i < (size - 1); i++) {
                // last token may just be empty.
                element = tokens.nextToken();
                stmt.execute(element);
            }
            conn.commit();
        } catch (Exception ex) {
            if (conn != null) {
                try {
                    conn.rollback();
                } catch (SQLException e) {
                    LOGGER.log(Level.WARNING, 
                            I18n.loc("TALE-6005: Exception occured while rollback called on a JDBC connection"), e);
                }
            }            
            throw new RuntimeException(I18n.loc("TALE-7006: Exception occured while trying to create database " + 
            		"tables required by for ALE-SE"), ex);
        } finally {
            if (stmt != null) {
                try {
                    stmt.close();
                } catch (SQLException e) {
                    LOGGER.log(Level.WARNING, I18n.loc("TALE-6006: Exception while closing a JDBC statement"), e);                        
                }
            }
            if (dbConn != null) {
                try {
                    dbConn.close();
                } catch (SQLException e) {
                    LOGGER.log(Level.WARNING, I18n.loc("TALE-6003: Exception while closing a JDBC connection"), e);
                }
            }
        }
    }
    
    /**
     * Get singleton instance of DBSchemaCreation
     * @return DBSchemaCreation DSchemaCreation
     */
    public static ALEDBSchemaCreation getInstance() {
        return mSingleton;
    }

    public String getCreateScriptName() {
        return CREATE_TABLES_SCRIPT;
    }

    public String getDropScriptName() {
        return DROP_TABLES_SCRIPT;
    }

    public String[] getTabels() {
        return PERSISTENCE_TABLES;
    }

    public String getTruncateScriptName() {
        return TRUNCATE_TABLES_SCRIPT;
    }
    
    public String getPopulateScriptName() {
        return POPULATE_TABLES_SCRIPT;
    }

}
