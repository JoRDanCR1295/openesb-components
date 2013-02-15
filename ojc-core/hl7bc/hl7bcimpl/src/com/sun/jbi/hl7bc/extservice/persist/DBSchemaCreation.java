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

package com.sun.jbi.hl7bc.extservice.persist;

import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnection;
import com.sun.jbi.hl7bc.extservice.persist.connection.ConnectionProperties;
import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnectionFactory;
import com.sun.jbi.hl7bc.HL7RuntimeException;
import com.sun.jbi.hl7bc.I18n;

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

/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 */
public class DBSchemaCreation {

    private static final Logger mLog = Logger.getLogger(DBSchemaCreation.class.getName());

    /** HL7 Binding Component persistence database schema name */
    public static final String SCHEMA_NAME = "HL7BC_SCHEMA"; //$NON-NLS-1$

    /** ACKNOWLEDGMENT table name */
    public static final String ACKNOWLEDGMENT = SCHEMA_NAME + ".ACKNOWLEDGMENT "; //$NON-NLS-1$

    /** EXPSEQUENCENO table name */
    public static final String EXPSEQUENCENO = SCHEMA_NAME + ".EXPSEQUENCENO "; //$NON-NLS-1$

    private static boolean m_schemaAlreadyPresent;

    private static DBSchemaCreation mSingleton = new DBSchemaCreation();

    private static String CREATE_SCRIPT = "create_hl7bc_schema.sql"; //$NON-NLS-1$

    private static String DROP_SCRIPT = "drop_hl7bc_schema.sql"; //$NON-NLS-1$

    private static String DELIMITER = ";"; //$NON-NLS-1$

    //private static String[] TABLES_LIST = new String[] { "ACKNOWLEDGMENT", "EXPSEQUENCENO" };
	private static String[] TABLES_LIST = new String[] { "EXPSEQUENCENO" , "HL7MESSAGELOG", "JOURNALHL7MESSAGELOG"};

    /**
     * get singleton instance of DBSchemaCreation
     *
     * @return DBSchemaCreation DSchemaCreation
     */
    public static DBSchemaCreation getInstance() {
        return mSingleton;
    }

    /**
     * Check to see if all the tables required by HL7 BC are present. 
     * If not present, create the tables
     * 
     * @param prop database properties
     */
    public void checkAndCreateDatabaseObjects(Properties prop, DBConnectionFactory connFac) throws HL7RuntimeException {
        //check to see if there is no schema and proper expected tables
        //there then execute the script to create stuff
        if (!checkTablesIntegrity(prop, connFac)) {
            createDatabaseObjects(prop, connFac);
        }
    }

    /**
     * Creates the tables required by HL7 BC
     * @param prop database properties
     */
    public void createDatabaseObjects(Properties prop, DBConnectionFactory connFac) throws HL7RuntimeException {
        StringTokenizer tokens = populateCreateScript(connFac.getType(), CREATE_SCRIPT);
        executeScript(prop, tokens, connFac);
    }

    /**
     * remove tables and schema from database
     *
     * @param prop database properties
     */
    public void dropDatabaseObjects(Properties prop, DBConnectionFactory connFac) throws HL7RuntimeException {
        StringTokenizer tokens = populateCreateScript(connFac.getType(), DROP_SCRIPT);
        executeScript(prop, tokens, connFac);
    }

    private StringTokenizer populateCreateScript(int dbType, String scriptName) throws HL7RuntimeException {
        String scriptsPath = "scripts/"; //$NON-NLS-1$

        if (Integer.valueOf(dbType).equals(ConnectionProperties.DERBY_DB)) {
            scriptsPath = scriptsPath + "derby/" + scriptName; //$NON-NLS-1$
        } else if(Integer.valueOf(dbType).equals(ConnectionProperties.MYSQL_DB)){
        	scriptsPath = scriptsPath + "mysql/" + scriptName; //$NON-NLS-1$
        } else if(Integer.valueOf(dbType).equals(ConnectionProperties.AXION_DB)){
        	scriptsPath = scriptsPath + "axiondb/" + scriptName; //$NON-NLS-1$
        } else if(Integer.valueOf(dbType).equals(ConnectionProperties.ORCL_DB)){
        	scriptsPath = scriptsPath + "oracle/" + scriptName; //$NON-NLS-1$
        }

        // we want : scriptsPath=scriptsPath+"derby/create_hl7bc_schema.sql";
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        InputStream is = null;

        try {
            is = DBSchemaCreation.class.getResourceAsStream(scriptsPath);

            int c;

            while ((c = is.read()) != -1) {
                os.write(c);
            }
        } catch (IOException e) {
            mLog.log(Level.SEVERE, I18n.msg("E0308: failed to load the schema scripts"), e);
        } finally {
            try {
                if (is != null) {
                    is.close();
                }

                if (os != null) {
                    os.close();
                }
            } catch (IOException ex) {
                mLog.log(Level.SEVERE, ex.getMessage());
                mLog.log(Level.SEVERE, I18n.msg("E0309: Error closing the streams"), ex); //$NON-NLS-1$
                throw new HL7RuntimeException(ex);
            }
        }

        String script = new String(os.toByteArray());
        StringTokenizer tokens = new StringTokenizer(script, DELIMITER);

        return tokens;
    }

    private static void executeScript(Properties prop, StringTokenizer tokens, DBConnectionFactory connFac)
            throws HL7RuntimeException {
        Connection conn = null;
        Statement stmt = null;
        try {
            conn = ((DBConnection) connFac.createConnection()).getUnderlyingConnection();
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
            if (stmt != null) {
                try {
                    stmt.close();
                } catch (SQLException e) {
                    mLog.log(Level.SEVERE, I18n.msg("E0310: Exception occured while closing a JDBC statement"), e);
                }
            }

            if (conn != null) {
                try {
                    conn.rollback();
                    conn.close();
                } catch (SQLException e) {
                    mLog.log(Level.SEVERE, I18n.msg("E0311: Exception occured while closing a JDBC connection"), e);
                }
            }
            mLog.log(Level.WARNING, I18n.msg("E0312: Unable to create database tables, which are already created"));
            //throw new HL7RuntimeException(I18n.msg("E0312: Exception occured while trying to create database tables required by for persistence"), ex);
        }finally {
            if (stmt != null) {
                try {
                    stmt.close();
                } catch (SQLException e) {
                    mLog.log(Level.SEVERE, I18n.msg("E0310: Exception occured while closing a JDBC statement"), e);
                }
            }

            if (conn != null) {
                try {
                    //conn.rollback();
                    conn.close();
                } catch (SQLException e) {
                    mLog.log(Level.SEVERE, I18n.msg("E0311: Exception occured while closing a JDBC connection"), e);
                }
            }
		
		}
    }

    /**
     * Check to see if all the tables required by HL7 BC are present
     *
     * @param prop database properties
     * @return boolean true: if valid schema exists in database false: otherwise
     *
     * @throws HL7RuntimeException HL7RuntimeException
     */
    public static boolean checkTablesIntegrity(Properties prop, DBConnectionFactory connFac) throws HL7RuntimeException {
        String targetSchema = "HL7BC_SCHEMA";
        boolean allTablesPresent = false;
        DBConnection dbConn = null;
        ResultSet resultSet = null;
		Connection conn = null;
        try {
            dbConn = connFac.createConnection();
            conn = dbConn.getUnderlyingConnection();

            int tableCount = TABLES_LIST.length;
            int numTablesFound = 0;

            String[] objectsToSearchFor = { "TABLE" }; //$NON-NLS-1$
            resultSet = conn.getMetaData().getTables(null, "%", "%", objectsToSearchFor); //$NON-NLS-1$ 

            while (resultSet.next() && (numTablesFound != tableCount)) {
                String rsTableName = resultSet.getString("TABLE_NAME");
                //if the table is not in a schema same as the user name, go to next
                String rsTableSchemaName = resultSet.getString("TABLE_SCHEM");
                if (!"".equals(rsTableSchemaName) && !(targetSchema.equalsIgnoreCase(rsTableSchemaName))) {
                    continue;
                }
                String tableItem = null;
                for (int i = 0; i < tableCount; i++) {
                    tableItem = TABLES_LIST[i];
                    if (rsTableName.equalsIgnoreCase(tableItem)) { //$NON-NLS-1$
                        //Found a table, increment the counter
                        numTablesFound++;
                        break;
                    }
                }
            }
            resultSet.close();
            conn.close();
            if (numTablesFound == tableCount) { // found all tables
                allTablesPresent = true;
                return allTablesPresent;
            }
            return false;
        } catch (Exception ex) {
            if (dbConn != null) {
                try {
                    dbConn.getUnderlyingConnection().close();
                } catch (Exception e) {
                    mLog.log(Level.SEVERE, I18n.msg("E0311: Exception occured while closing a JDBC connection"), e);
                }
            }
            throw new HL7RuntimeException(I18n.msg("E0313: Exception occured while verifying that all the tables required by HL7 BC are present in the schema {0}",
                    targetSchema ), ex);
        }finally {
            if (dbConn != null) {
                try {
                    dbConn.getUnderlyingConnection().close();
                } catch (Exception e) {
                    mLog.log(Level.SEVERE, I18n.msg("E0311: Exception occured while closing a JDBC connection"), e);
                }
            }
            if (conn != null) {
                try {
                    conn.close();
                } catch (Exception e) {
                    mLog.log(Level.SEVERE, I18n.msg("E0311: Exception occured while closing a JDBC connection"), e);
                }
            }
		
		}
    }
}
