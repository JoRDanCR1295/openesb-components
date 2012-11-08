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
 * @(#)DBSchemaCreator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.schema;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.ResultSet;

import java.sql.SQLException;
import java.sql.Statement;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.sql.DataSource;

import com.sun.jbi.engine.workflow.db.dao.DAO;
import com.sun.jbi.engine.workflow.db.dao.DAOFactory;
import com.sun.jbi.engine.workflow.db.dao.DBType;
import com.sun.jbi.engine.workflow.util.I18n;


/**
 * The class to populate schema and drop schema
 * 
 * @author Sun Microsystems
 *
 */
public class DBSchemaCreator {
    

    private static final Logger LOGGER = 
        Logger.getLogger(DBSchemaCreator.class.getName());    
    
    private static final String[] SCHEMA_TABLES = {
        "TASK_ASSIGNEE", "TASK_TIMER", "TASK_INSTANCE",   "TASK_EXCLUDED_ASSIGNEE"
    };
    private static String CREATE_SCRIPT = "create_workflow_schema.sql"; 
    private static String DROP_SCRIPT = "drop_workflow_schema.sql";   
    private static String DELIMITER = ";";
    
    private DBType mDBType;
    private DataSource mDataSource;
  
    

    
    
    public DBSchemaCreator (String dbTypeStr, DataSource dataSource, String schemaName) {
        mDBType = DAOFactory.getDBType(dbTypeStr);
        mDataSource = dataSource;
    }
        
    

    /**
     * checks if the schema is already installed, if not, creats new schema in database
     *
     * @param prop database properties
     */
    public void createSchema() {

        //check to see if there is no schema and proper expected tables
        //there then execute the script to create stuff
        if (!checkSchemaAndTablesIntegrity()) {
            StringTokenizer tokens = populateCreateScript(CREATE_SCRIPT);
            try {
                String err =    executeScript(tokens);
                if (err != null) {
                    LOGGER.log(Level.WARNING, 
                            I18n.loc("WLM-6044: SQL Exception in executing the script"), err);                    
                }
            } catch (Exception ex) {
                LOGGER.log(Level.WARNING, 
                        I18n.loc("WLM-6044: SQL Exception in executing the script"), ex.getMessage());
            }
        }
    }

    /**
     * remove schema from database
     *
     * @param prop database properties
     */
    public void dropSchema() {
        StringTokenizer tokens = populateCreateScript(DROP_SCRIPT);

        //check to see if there is schema and proper expected tables
        //there that can be dropped then execute the script to create stuff
        if (checkSchemaAndTablesIntegrity()) {
            try {
                executeScript(tokens);
            }catch (Exception ex) {
                //Ignore ...
            }
        }
    }    

    
    
    
    public String  executeScript (String scriptName) throws  DBScheamException {
        StringTokenizer tokens = populateCreateScript(scriptName);
        return executeScript(tokens); 
    }
  
    
    private String executeScript(StringTokenizer tokens) throws DBScheamException {
        
            boolean hasException = false;
            String errMsg = "";
            try {
                java.sql.Connection conn = null;
                Statement stmt = null;
                boolean defaultAutoCommit = false;
                try { 
                    conn = mDataSource.getConnection();

                    if (conn == null) {
                        throw new DBScheamException(
                                I18n.loc("WLM-7006: Failed to get a connection")                       
                            );
                    }
                    
                    defaultAutoCommit = conn.getAutoCommit();
                    conn.setAutoCommit(false);
                    stmt = conn.createStatement();

                    String element = null;

                    for (int i = 0, size = tokens.countTokens(); i <size; i++) {
                        // last token may just be empty.
                        element = tokens.nextToken();
                       element = element.trim();
                       if (element.length() > 0) {
                           try {
                            stmt.execute(element);
                        } catch (Exception e) {
                            // TODO Auto-generated catch block
                            hasException = true;
                            errMsg = errMsg + " " + e.getMessage();
                        }
                       }
                    }
                    conn.commit();
                    
                } catch (SQLException ex) {
                        LOGGER.log(Level.WARNING, 
                                I18n.loc("WLM-6044: SQL Exception in executing the script"), ex.getMessage());
                }
                    finally {
                    if (stmt != null) {
                        stmt.close();
                    }                    
                    if (conn != null) {
                        conn.setAutoCommit(defaultAutoCommit);                        
                    }

                }
            } catch (Exception ex) {
                throw new DBScheamException(ex);
            }
            if (hasException) {
                return errMsg;
            }else {
                return null;
            }
        }
    
    /**
     * check if schema exists and is valid
     *
     * @param prop database properties
     * @return boolean true: if valid schema exists in database false: otherwise
     *
     * @throws BPELRuntimeException BPELRuntimeException
     * @throws RuntimeException DOCUMENT ME!
     */
    public boolean checkSchemaAndTablesIntegrity()
        throws DBScheamException {
         boolean tablesPresent = false;
        ResultSet rs1;
        ResultSet rs2;
        java.sql.Connection conn = null;

        try {
             conn = mDataSource.getConnection();

            if (conn == null) {
                throw new DBScheamException(
                    I18n.loc("WLM-7006: Failed to get a connection")                       
                );
            }

            String schemaName = conn.getMetaData().getUserName();

            // now check for all tables present
                int tableCount = SCHEMA_TABLES.length;

                String[] names = {"TABLE"}; //$NON-NLS-1$
                rs2 = conn.getMetaData().getTables(null, schemaName, "%", names); //$NON-NLS-1$ //$NON-NLS-2$

                while (rs2.next() && (tableCount != 0)) {
                    String tablename = rs2.getString("TABLE_NAME"); //$NON-NLS-1$

                    for (int i = 0; i < SCHEMA_TABLES.length; i++) {
                        if (tablename.equalsIgnoreCase(SCHEMA_TABLES[i])) {
                            tableCount--;

                            break;
                        }
                    }
                }

                if (tableCount == 0) { // found all tables
                    tablesPresent = true;
                }
                
                rs2.close();

        } catch (Exception ex) {
            ex.printStackTrace();
        } finally {
            if (conn != null) {
                try {
                    conn.close();
                } catch (SQLException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
        return tablesPresent;
    }
    
    public StringTokenizer populateCreateScript(String scriptName) {
        String scriptsPath = "scripts/"; //$NON-NLS-1$

        if (mDBType == DAO.DERBY_TYPE) {
            scriptsPath = scriptsPath + "derby/" + scriptName; //$NON-NLS-1$
        } else if (mDBType == DAO.ORACLE_TYPE) {
            scriptsPath = scriptsPath + "oracle/" + scriptName; //$NON-NLS-1$
        } else if (mDBType == DAO.DB2_TYPE) {
            scriptsPath = scriptsPath + "db2/" + scriptName; //$NON-NLS-1$
        } else if (mDBType == DAO.SYBASE_TYPE) {
            scriptsPath = scriptsPath + "sybase/" + scriptName; //$NON-NLS-1$
        } else if (mDBType == DAO.SQLSERVER_TYPE) 
        {
            scriptsPath = scriptsPath + "sqlserver/" + scriptName; //$NON-NLS-1$
        }
        else if (mDBType == DAO.MYSQL_TYPE) 
        {
            scriptsPath = scriptsPath + "mysql/" + scriptName; //$NON-NLS-1$
        }

        // we want : scriptsPath=scriptsPath+"derby/create_bpel_schema.sql";
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        InputStream is = null;

        try {
            is = getClass().getResourceAsStream(scriptsPath);

            int c;

            while ((c = is.read()) != -1) {
                os.write(c);
            }
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, e.getMessage());
            LOGGER.log(Level.SEVERE, 
                    I18n.loc("WLM-7007: Failed to load the script: {0}", scriptsPath), e);
        } finally {
            try {
                if (is != null) {
                    is.close();
                }

                if (os != null) {
                    os.close();
                }
            } catch (IOException ex) {
                LOGGER.log(Level.SEVERE, ex.getMessage());
                LOGGER.log(Level.SEVERE, 
                        I18n.loc("WLM-7008: Error close the stream"), ex); //$NON-NLS-1$
                throw new DBScheamException(ex);
            }
        }

        String script = new String(os.toByteArray());
        StringTokenizer tokens = new StringTokenizer(script, DELIMITER);

        return tokens;
    }
    

}
