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

package com.sun.jbi.hl7bc.extservice.persist.connection;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.sql.DataSource;

import com.sun.jbi.hl7bc.HL7RuntimeException;
import com.sun.jbi.hl7bc.extservice.persist.axiondb.AxionDBConnection;
import com.sun.jbi.hl7bc.extservice.persist.axiondb.AxionDBConnectionPool;
import com.sun.jbi.hl7bc.I18n;

/**
 * DOCUMENT ME!
 * 
 * @author Sun Microsystems
 */
public class DBConnectionFactory {

    private DataSource dataSourceInstance;

    private int dbType;

	private String installRoot;

    private static final String DERBY_DATABSE = "Apache Derby";
    
    private static final String AXION_DATABASE = "AxionDB";

	private static final String MYSQL_DATABASE = "MySQL";

	private static final String ORACLE_DATABASE = "ORACLE";
	private static Logger mLogger = Logger.getLogger(DBConnectionFactory.class.getName());
    
    /**
     * constructor
     * 
     * @param prop Properties
     * @param context InitialContext
     */
    public DBConnectionFactory(Properties prop, InitialContext initialContext, String installRoot) throws HL7RuntimeException {
    	this.installRoot = installRoot;
        String datasourceJndiName = prop.getProperty(ConnectionProperties.DatabaseJNDIName);
        
        DBConnection con = null;
        DatabaseMetaData meta = null;
        
        // If application server datasource is not configured, Axion DB will be used.
        if(datasourceJndiName == null || "".equals(datasourceJndiName.trim())){ 
        	meta = getAxionDBMetaData();
        }else {
        	
	        try {
	            dataSourceInstance = (DataSource) initialContext.lookup(datasourceJndiName);
	            Connection conn = dataSourceInstance.getConnection();
	            meta = conn.getMetaData();
	        } catch (NamingException e) {
	            mLogger.log(Level.WARNING,I18n.msg("W0128: Could not find a JDBC resource with the JNDI name : {0} . Will be using Local DB for persistence.", datasourceJndiName));
	        }catch (SQLException e){
	        	mLogger.log(Level.WARNING,I18n.msg("W0129: Could not get connection from Datasource : {0}. Will be using Local DB for persistence.", datasourceJndiName ));
	        }
	        
        }
        
        if(dataSourceInstance == null){
        	meta = getAxionDBMetaData();
        }
        
        //Set the dbType
        try {

            String databaseProductName = meta.getDatabaseProductName();

            if (databaseProductName.equalsIgnoreCase(DERBY_DATABSE)) {
                dbType = ConnectionProperties.DERBY_DB.intValue();
            }else if(databaseProductName.equalsIgnoreCase(AXION_DATABASE)){
            	dbType = ConnectionProperties.AXION_DB.intValue();
            }else if(databaseProductName.equalsIgnoreCase(MYSQL_DATABASE)){
            	dbType = ConnectionProperties.MYSQL_DB.intValue();			
			}else if(databaseProductName.equalsIgnoreCase(ORACLE_DATABASE)){
            	dbType = ConnectionProperties.ORCL_DB.intValue();
			
            }else{
                throw new HL7RuntimeException(I18n.msg("E0298: Database Not Supported : {0} "
						+ " Supported Databases : {1} only", databaseProductName, DERBY_DATABSE));
            }
        } catch (SQLException e) {
            throw new HL7RuntimeException(
                    I18n.msg("E0299: Could not get Database Connection Meta Data from JDBC resource with the JNDI name : {0} ",
								datasourceJndiName), e);
        }
    }

	private DatabaseMetaData getAxionDBMetaData() throws HL7RuntimeException{
		//AxionDBConnection axionConn = AxionDBConnectionPool.getInstance(this.installRoot).getConnection();
		
		try {
			DBConnection axionConn = createConnection();
			DatabaseMetaData dbMeta = axionConn.getMetaData();
			return dbMeta;
		} catch (SQLException e) {
			mLogger.log(Level.SEVERE,I18n.msg("E0300: Could not get DatabaseMetaData from Axion DB ") ,e);
			throw new HL7RuntimeException(I18n.msg("E0300: Could not get DatabaseMetaData from Axion DB ") ,e) ;
		}catch (Exception e) {
			throw new HL7RuntimeException(I18n.msg("E0300: Could not get DatabaseMetaData from Axion DB ") ,e);
		}
	}

    /**
     * Creates and returns a connection /** Creates and returns a connection
     * 
     * @return DBConnection
     * @throws Exception Throws exception if a counnection could not be obtained
     */
    public DBConnection createConnection() throws Exception {
        DBConnection dbConn = null;

        if(dataSourceInstance != null){
        	Connection conn = dataSourceInstance.getConnection();
        	dbConn = new DBConnection(conn);
        } else{
        	dbConn = AxionDBConnectionPool.getInstance(this.installRoot).getConnection();
        }
        dbConn.getUnderlyingConnection().setAutoCommit(false);
        return dbConn;
    }

    /**
     * return database type
     * 
     * @return int database type
     */
    public int getType() {
        return dbType;
    }
    
    
}


