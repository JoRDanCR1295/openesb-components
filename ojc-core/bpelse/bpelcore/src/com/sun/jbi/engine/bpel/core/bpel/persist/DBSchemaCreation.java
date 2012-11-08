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

package com.sun.jbi.engine.bpel.core.bpel.persist;

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

import com.sun.jbi.engine.bpel.core.bpel.connection.AbstractDBConnection;
import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnectionFactory;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;

/**
 * @author Sun Microsystems
 */
public abstract class  DBSchemaCreation {

    private static final Logger LOGGER = Logger.getLogger(DBSchemaCreation.class.getName());
    
 
    private static String DELIMITER = ";"; //$NON-NLS-1$
    
    public enum STATUS {
        EMPTY, CORRUPT, VALID
    };

    /**
     * Check to see if all the tables required by BPEL-SE for persistence are present. 
     * If not present, create the tables
     * @param prop database properties
     * @return TODO
     */
    public boolean checkAndCreateTables(Properties prop, DBConnectionFactory connFac) {
        // Check to see if all the tables required by BPEL-SE for persistence
        // are present
        if (checkTablesIntegrity(connFac) == STATUS.VALID) {
            return true;
        }

        // If not present, create the tables
        if (checkTablesIntegrity(connFac) == STATUS.EMPTY) {
            createTables(connFac);
            return (checkTablesIntegrity(connFac) == STATUS.VALID);
        }

        return false;
    }

    
    public void createTables(DBConnectionFactory connFac) {
        AbstractDBConnection dbConn = null;
        Connection conn = null;
        try {
            int dbType = connFac.getType();
            dbConn = connFac.createNonXAConnection();
            conn = dbConn.getUnderlyingConnection();
            conn.setAutoCommit(false);
            createTables(conn, dbType);
            conn.commit();
        } catch (Exception ex) {
            if (conn != null) {
                try {
                    conn.rollback();
                } catch (SQLException e) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6064: Exception occured while rollback called on a JDBC connection"), e);
                }
            }
            throw new RuntimeException(I18n.loc("BPCOR-7008: Exception occured while trying to create database "
                    + "tables required by for persistence"), ex);
        } finally {
            if (dbConn != null) {
                try {
                    dbConn.close();
                } catch (SQLException e) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6062: Exception while closing a JDBC connection"), e);
                }
            }
        }
    }
    public void createTables(Connection conn, int dbType) throws SQLException {
    	StringTokenizer tokens = populateCreateScript(dbType, getCreateScriptName());
        executeScript(tokens, conn);
    }
    
    /**
     * Drops the tables created for persistence
     * @param prop database properties
     */
    public void dropTables(Properties prop, DBConnectionFactory connFac) {
        int dbType = connFac.getType();
        StringTokenizer tokens = populateCreateScript(dbType, getDropScriptName());
        AbstractDBConnection dbConn = null;
        Connection conn = null;
        try {
            dbConn = connFac.createNonXAConnection();
            conn = dbConn.getUnderlyingConnection();
            conn.setAutoCommit(false);
            executeScript(tokens, conn);
            conn.commit();
        } catch (Exception ex) {
            throw new RuntimeException(I18n.loc("BPCOR-7033: Exception occured while droping database tables"), ex);
        } finally {
            if (dbConn != null) {
                try {
                    dbConn.close();
                } catch (SQLException e) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6062: Exception while closing a JDBC connection"), e);
                }
            }
        }
    }
    
    public void truncateTables (Properties prop, DBConnectionFactory connFac) {
        int dbType = connFac.getType();
        StringTokenizer tokens = populateCreateScript(dbType, getTruncateScriptName());
        AbstractDBConnection dbConn = null;
        Connection conn = null;
        try {
            dbConn = connFac.createNonXAConnection();
            conn = dbConn.getUnderlyingConnection();
            executeScript(tokens, conn);
        } catch (Exception ex) {
            throw new RuntimeException(I18n.loc("BPCOR-7035: Exception occured while truncating database tables"), ex);
        } finally {
            if (dbConn != null) {
                try {
                    dbConn.close();
                } catch (SQLException e) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6062: Exception while closing a JDBC connection"), e);
                }
            }
        }
    }

    public STATUS checkTablesIntegrity(DBConnectionFactory connFac) {
        AbstractDBConnection dbConn = null;
        Connection conn = null;
        try {
            int dbType = connFac.getType();
            dbConn = connFac.createNonXAConnection();
            conn = dbConn.getUnderlyingConnection();
            return checkTablesIntegrity(conn, dbType);
        } catch (Exception ex) {
            String msg = I18n.loc("BPCOR-7006: Exception occured while verifying that all the "
                    + "tables required for persistence are present in the schema");
            throw new RuntimeException(msg, ex);
        } finally {
            if (dbConn != null) {
                try {
                    dbConn.close();
                } catch (SQLException e) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6062: Exception while closing a JDBC connection"), e);
                }
            }
        }
    }

    public STATUS checkTablesIntegrity(Connection conn, int dbType) throws SQLException {
        String dbUserName = null;
        ResultSet resultSet = null;
        // Check for all tables present
        int numTablesFound = 0;

        // DEVNOTE: Do not try again if there is a SQLException, since this is a
        // startup method. If the method
        // fails due to DBUnavailability (or anything else for that matter) the
        // BPEL SE will not start.
        try {
            dbUserName = conn.getMetaData().getUserName();
            String[] objectsToSearchFor = { "TABLE" };
            resultSet = conn.getMetaData().getTables(null, "%", "%", objectsToSearchFor); //$NON-NLS-1$
            while (resultSet.next() && (numTablesFound != getTabels().length)) {

                String rsTableName = resultSet.getString("TABLE_NAME");

                // Do the check against the schema only if Derby or Oracle.
                // MySql always returns null
                // in this scenario.
                if (dbType != ConnectionProperties.MYSQL_DB.intValue()) {
                    // if the table is not in a schema same as the user name, go
                    // to next
                    String rsTableSchemaName = resultSet.getString("TABLE_SCHEM");
                    if (!(dbUserName.equalsIgnoreCase(rsTableSchemaName))) {
                        continue;
                    }
                }

                for (int i = 0; i < getTabels().length; i++) {
                    String tableItem = getTabels()[i];
                    if (rsTableName.equalsIgnoreCase(tableItem)) { //$NON-NLS-1$
                        // Found a table, increment the counter
                        numTablesFound++;
                        break;
                    }
                }
            }
        } finally {
            if (resultSet != null) {
                try {
                    resultSet.close();
                } catch (SQLException resCloseExcp) {
                    LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6061: Exception occured while closing a JDBC Resultset"),
                            resCloseExcp);
                }
            }
        }

        if (numTablesFound == getTabels().length) { // found all tables
            return STATUS.VALID;
        }
        if (numTablesFound > 0) {
            return STATUS.CORRUPT;
        }
        return STATUS.EMPTY;
    }
    
    private StringTokenizer populateCreateScript(int dbType, String scriptName) {
        String scriptsPath = "impl/scripts/"; //$NON-NLS-1$
        
        if (Integer.valueOf(dbType).equals(ConnectionProperties.DB2_DB)) {
            scriptsPath = scriptsPath + "db2/" + scriptName; //$NON-NLS-1$
        } else if (Integer.valueOf(dbType).equals(ConnectionProperties.ORCL_DB)) {
            scriptsPath = scriptsPath + "oracle/" + scriptName; //$NON-NLS-1$
        } else if (Integer.valueOf(dbType).equals(ConnectionProperties.MYSQL_DB)) {
            scriptsPath = scriptsPath + "mysql/" + scriptName; //$NON-NLS-1$
        } else if (Integer.valueOf(dbType).equals(ConnectionProperties.PBASE_DB)) {
            scriptsPath = scriptsPath + "pointbase/" + scriptName; //$NON-NLS-1$
        } else if (Integer.valueOf(dbType).equals(ConnectionProperties.SYBS_DB)) {
            scriptsPath = scriptsPath + "sybase/" + scriptName; //$NON-NLS-1$
        } else if (Integer.valueOf(dbType).equals(ConnectionProperties.DERBY_DB)) {
            scriptsPath = scriptsPath + "derby/" + scriptName; //$NON-NLS-1$
        } else if (Integer.valueOf(dbType).equals(ConnectionProperties.POSTGRES_DB)) {
            scriptsPath = scriptsPath + "postgres/" + scriptName; //$NON-NLS-1$
        }
        
        // we want : scriptsPath=scriptsPath+"derby/create_bpel_schema.sql";
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        InputStream is = null;
        
        try {
            is = DBSchemaCreation.class.getResourceAsStream(scriptsPath);
            
            int c;
            
            while ((c = is.read()) != -1) {
                os.write(c);
            }
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, e.getMessage());
            LOGGER.log(Level.SEVERE, I18n.loc("BPCOR-7007: failed_to_load_the_schema_scripts"), e);
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
                		I18n.loc("BPCOR-6063: Error closing the streams"), ex); //$NON-NLS-1$
                throw new BPELRuntimeException(ex);
            }
        }
        
        String script = new String(os.toByteArray());
        StringTokenizer tokens = new StringTokenizer(script, DELIMITER);
        
        return tokens;
    }

    private static void executeScript(StringTokenizer tokens, Connection conn)
			throws SQLException {

    	Statement stmt = null;
    	// DEVNOTE: Do not try again if there is a SQLException, since this is a
    	// startup method. If the method
    	// fails due to DBUnavailability (or anything else for that matter) the
    	// BPEL SE will not start.
    	try {

    		stmt = conn.createStatement();

    		String element = null;
    		for (int i = 0, size = tokens.countTokens(); i < (size - 1); i++) {
    			// last token may just be empty.
    			element = tokens.nextToken();
    			stmt.execute(element);
    		}
    	} finally {
    		if (stmt != null) {
    			try {
    				stmt.close();
    			} catch (SQLException e) {
    				LOGGER.log(Level.WARNING, I18n.loc("BPCOR-6065: Exception while closing a JDBC statement"), e);
    			}
    		}
    	}
    }
    
    protected abstract String [] getTabels ();
    
    protected abstract String getCreateScriptName ();
    
    protected abstract String getDropScriptName ();
    
    protected abstract String getTruncateScriptName ();
}
