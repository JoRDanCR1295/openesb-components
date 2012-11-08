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
 * @(#)DBConnectionInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.model.runtime;

import java.sql.SQLException;
import javax.sql.DataSource;
import javax.sql.XADataSource;
import javax.jbi.component.ComponentContext;
import javax.xml.transform.*;
import com.sun.jbi.jdbcbc.EndpointBean;
/*
 * This class encapsulates the DBConnection parameters
 * while obtaining the connection and will help in identifying
 * the
 */
public class DBConnectionInfo {
    private String dbUrl;
    private String serverName;
    private int portNumber;
    private String databaseOrSchemaName;
    private String username;
    private String password;
    private int dbType;
    private String driverName;
	private String mjndiName;
    public static final String JDBCDEF_TAG = "connectiondef";
	DBConnectionInfo dbConnectionInfo;
	private String XAEnabled = null;

	public void buildConnectionInfo(String dbUrl) {
        //Url format: jdbc:derby://localhost:1527/testDB;create=true
        if (dbUrl.startsWith("jdbc:derby://")) {
            dbType = DataBases.DERBY.ordinal();
            dbUrl = dbUrl.substring("jdbc:derby://".length(), dbUrl.length());

            final String[] array1 = dbUrl.split("/");
            final String[] array2 = array1[0].split(":");
            serverName = array2[0];
            portNumber = Integer.valueOf(array2[1]).intValue();
            databaseOrSchemaName = array1[1];
        } else if (dbUrl.startsWith("jdbc:oracle:thin:@")) {
            //jdbc:oracle:thin:@<server>[:<1521>]:<database_name>
            dbType = DataBases.ORACLE.ordinal();
            
        }else if (dbUrl.startsWith("jdbc:Seebeyond:db2://")) {
            //jdbc:Seebeyond:db2://localhost:1433;DatabaseName=db2
            dbType = DataBases.DB2.ordinal();
            dbUrl = dbUrl.substring("jdbc:Seebeyond:db2://".length(), dbUrl.length());

            final String[] array1 = dbUrl.split(";");
            final String[] array2 = array1[0].split(":");
            serverName = array2[0];
            portNumber = Integer.valueOf(array2[1]).intValue();
            databaseOrSchemaName = array1[1];
            
        }else if (dbUrl.startsWith("jdbc:SeeBeyond:sqlserver://")) {
        	//jdbc:SeeBeyond:sqlserver://rpoon-xp.stc.com:1433;DatabaseName=dgdb
            dbType = DataBases.SQLSERVER.ordinal();
            dbUrl = dbUrl.substring("jdbc:SeeBeyond:sqlserver://".length(), dbUrl.length());

            final String[] array1 = dbUrl.split(";");
            final String[] array2 = array1[0].split(":");
            serverName = array2[0];
            portNumber = Integer.valueOf(array2[1]).intValue();
			final String[] array3 = array1[1].split("=");
            databaseOrSchemaName = array3[1];            
        }
    }

    /*
    * This gets the database name based on either the dbUrl or XADataSource or DataSource
    *  that is available
    *  @param dbUrl
    *  @param xads
    *  @param ds
    *  @return String
    *  @throws SQLException
    */
	public String getDataBaseName(final String dbUrl, final XADataSource xads, final DataSource ds)
        throws SQLException {
        if (xads != null) {
            return xads.getXAConnection().getConnection().getMetaData()
                       .getDatabaseProductName();
        }

        if (ds != null) {
            return ds.getConnection().getMetaData().getDatabaseProductName();
        }

        if (dbUrl.contains("derby")) {
            return "DERBY";
        } else if (dbUrl.contains("oracle")) {
            return "ORACLE";
        }else if (dbUrl.contains("sqlserver")) {
            return "SQLSERVER";
        } else if (dbUrl.contains("db2")) {
            return "DB2";
        }  
        return null;
    }

	public String getDatabaseOrSchemaName() {
        return databaseOrSchemaName;
    }

	public void setDatabaseOrSchemaName(final String databaseOrSchemaName) {
        this.databaseOrSchemaName = databaseOrSchemaName;
    }

	public int getDbType() {
        return dbType;
    }

	public void setDbType(final int dbType) {
        this.dbType = dbType;
    }

	public String getDbUrl() {
        return dbUrl;
    }

	public void setDbUrl(final String dbUrl) {
        this.dbUrl = dbUrl;
    }

	public String getPassword() {
        return password;
    }

	public void setPassword(final String password) {
        this.password = password;
    }

	public int getPortNumber() {
        return portNumber;
    }

	public void setPortNumber(final int portNumber) {
        this.portNumber = portNumber;
    }

	public String getServerName() {
        return serverName;
    }

	public void setServerName(final String serverName) {
        this.serverName = serverName;
    }

	public String getUsername() {
        return username;
    }

	public void setUsername(final String username) {
        this.username = username;
    }

	public String getDriverName() {
        return driverName;
    }

	public void setDriverName(final String driverName) {
        this.driverName = driverName;
    }
	
	public String getJNDIName(){
		return this.mjndiName;
	}

	public Object CreateDataSource(final ComponentContext componentContext,
		final String jndiname, final EndpointBean epb, final String XAEnabled) throws Exception {
	
		dbConnectionInfo = epb.getDBInfo();
		final String dbURL = dbConnectionInfo.getDbUrl();
		final String driverClass = dbConnectionInfo.getDriverName();

        dbConnectionInfo.buildConnectionInfo(dbURL);
        dbConnectionInfo.getDataBaseName(dbURL, null, null); 
     
        final String dbName = dbConnectionInfo.getDataBaseName(dbURL, null, null);
        final DataBases dataBases = DataBases.valueOf(dbName);
        dataBases.setDBConnectionInfo(dbConnectionInfo);
		
		Object objDataSource = dataBases.buildDataSource();
		this.mjndiName = dataBases.createAndBindJNDIDS(componentContext,objDataSource,jndiname,XAEnabled);

		return objDataSource;
    }

	public String getXAEnabled() {
		return XAEnabled;
	}

	public void setXAEnabled(String enabled) {
		XAEnabled = enabled;
	} 
}
