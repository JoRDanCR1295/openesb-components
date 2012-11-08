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
 * @(#)Util.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.engine.iep.core.runtime.util;

// Package containing JDBC classes
import java.sql.Blob;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.sql.DriverManager;
import java.sql.Types;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.HashSet;
import java.util.Properties;
import com.sun.jbi.engine.iep.core.runtime.operator.OperatorConstants;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlan;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlanFactory;
import com.sun.jbi.engine.iep.core.runtime.operator.Schema;
import com.sun.jbi.engine.iep.core.runtime.operator.QueryPlanInfo;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.DbSpecial;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.db2.DB2Special;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.derby.DerbySpecial;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.ExternalTablePollingStream;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.mysql.MySqlSpecial;
import com.sun.jbi.engine.iep.core.runtime.operator.impl.oracle.OracleSpecial;
import com.sun.jbi.engine.iep.core.runtime.util.alert.IEPSEAlertSender;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.rmi.server.UID;
import java.sql.SQLWarning;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.sql.DataSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Util.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class Util implements OperatorConstants {
    private static final Messages mMessages = Messages.getMessages(Util.class);

    private static Map<String, TimeUnit> mTimeUnitNameToValueMap = new HashMap<String, TimeUnit>(); 

    static {
        mTimeUnitNameToValueMap.put("nanosecond", TimeUnit.NANOSECONDS);
        mTimeUnitNameToValueMap.put("microsecond", TimeUnit.MICROSECONDS);
        mTimeUnitNameToValueMap.put("millisecond", TimeUnit.MILLISECONDS);
    }

    private static String mLocalIPAddress;
    static {
        try {
            mLocalIPAddress = InetAddress.getLocalHost().getHostAddress();
        } catch (UnknownHostException unknownhostexception) {
            mLocalIPAddress = "_localhost_";
        }
    }

    private static String[] _SQL_TYPE_NAMES = new String[]{SQL_TYPE_BIT, SQL_TYPE_TINYINT, SQL_TYPE_SMALLINT, SQL_TYPE_INTEGER, SQL_TYPE_BIGINT, SQL_TYPE_REAL, SQL_TYPE_FLOAT, SQL_TYPE_DOUBLE, SQL_TYPE_DECIMAL, SQL_TYPE_NUMERIC, SQL_TYPE_CHAR, SQL_TYPE_VARCHAR, SQL_TYPE_LONGVARCHAR, SQL_TYPE_DATE, SQL_TYPE_TIME, SQL_TYPE_TIMESTAMP, SQL_TYPE_BINARY, SQL_TYPE_VARBINARY, SQL_TYPE_LONGVARBINARY, SQL_TYPE_BLOB, SQL_TYPE_CLOB};

    private static int[] _SQL_TYPES = new int[]{Types.BIT, Types.TINYINT, Types.SMALLINT, Types.INTEGER, Types.BIGINT, Types.REAL, Types.FLOAT, Types.DOUBLE, Types.DECIMAL, Types.NUMERIC, Types.CHAR, Types.VARCHAR, Types.LONGVARCHAR, Types.DATE, Types.TIME, Types.TIMESTAMP, Types.BINARY, Types.VARBINARY, Types.LONGVARBINARY, Types.BLOB, Types.CLOB};

    private static QueryPlanFactory mQueryPlanFactory;
    static {
        try {
            mQueryPlanFactory = (QueryPlanFactory) Class.forName("com.sun.jbi.engine.iep.core.runtime.operator.QueryPlanFactoryImpl").newInstance();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_load_QueryPlanFactory", "com.sun.jbi.engine.iep.core.runtime.operator.QueryPlanFactoryImpl", e);
        }
    }

    private static SimpleDateFormat mSDF = new SimpleDateFormat("yyyy.MM.dd HH:mm:ss z");
    
    private static DbSpecial mDbSpecial;
    private static void setDbSpecial(Connection con, Properties prop) {
        mDbSpecial = getDbSpecial(con, prop);
    }
    
    public static DbSpecial getDbSpecial(Connection con, Properties prop) {
        String dbProductName = "unknown database product";
        try {
            DatabaseMetaData dbmd = con.getMetaData();
            dbProductName = dbmd.getDatabaseProductName();
            if (dbProductName.contains("Derby")) {
                String iepDerbyJarPath = prop.getProperty(PROP_IEP_DERBY_JAR_PATH);
                String dbSchema = prop.getProperty(PROP_DB_SCHEMA);
                if (RUNTIME_STYLE_EMBEDDED.equalsIgnoreCase(prop.getProperty(PROP_RUNTIME_STYLE))) {
                   dbSchema = "APP";
                }
                return new DerbySpecial(iepDerbyJarPath, dbSchema);
            }
            if (dbProductName.contains("Oracle")) {
                return new OracleSpecial();
            }
            // Fortent (jtaylor) 2008-11-21
            if (dbProductName.contains("DB2")) {
                return new DB2Special();
            }
            if (dbProductName.contains("MySQL")) {
                return new MySqlSpecial();
            }
            mMessages.log(Level.SEVERE, "Util.Fail_to_set_DbSpecial_for_database_product", dbProductName);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_set_DbSpecial", e);
        }
        
        return null;
    }
    
    public static void setDbSpecial(Properties configProp) {
        Connection con = null;
        try {
            con = getConnection(configProp);
            setDbSpecial(con, configProp);
        } catch (Exception e) {
        } finally {
            close(con);
        }
    }
    public static DbSpecial getDbSpecial() {
        return mDbSpecial;
    }
    public static Connection getConnection() throws Exception {
        Properties prop = new Properties(System.getProperties());
        return getConnection(prop);
    }

    public static Connection getXaConnection(Properties prop) throws Exception {
        Connection conn = null;
        String runtimeStyle = prop.getProperty(PROP_RUNTIME_STYLE);
        if (RUNTIME_STYLE_EMBEDDED.equalsIgnoreCase(runtimeStyle)) {
            return getConnection(prop);
        }

        InitialContext jndiContext = (InitialContext) prop.get(PROP_DB_JNDI_CONTEXT);
        String jndiName = prop.getProperty(PROP_DB_XA_JNDI_NAME);
        if (jndiContext != null && jndiName != null) {
            try {
                DataSource jds = (DataSource) jndiContext.lookup(jndiName);
                conn = jds.getConnection();
            } catch (Exception e) {
                mMessages.log(Level.SEVERE, "Util.DataSource_not_found_in_the_context", jndiName, e);
                //Alert
                IEPSEAlertSender.getInstance().alertUnableToConnectToDB("IEPSE", e.getMessage());
                //
                conn = null;
            }
            if (conn != null) {
                return conn;
            }
        }
        return null;
    }

    public static Connection getConnection(InitialContext jndiContext, String jndiName) throws Exception {
        Connection conn = null;
        
        if (jndiContext != null && jndiName != null) {
            try {
                DataSource jds = (DataSource) jndiContext.lookup(jndiName);
                conn = jds.getConnection();
            } catch (Exception e) {
                mMessages.log(Level.SEVERE, "Util.DataSource_not_found_in_the_context", jndiName, e);
                conn = null;
                throw new Exception(mMessages.getString("Util.Fail_to_connect_to_database"), e);
            }
            if (conn != null) {
                return conn;
            }
        }
        return null;
    }
    
    private static DataSource mDS = null;
    public static Connection getConnection(Properties prop) throws Exception {
        Connection conn = null;
        try {
            String runtimeStyle = prop.getProperty(PROP_RUNTIME_STYLE);
            if (RUNTIME_STYLE_EMBEDDED.equalsIgnoreCase(runtimeStyle)) {
                String dbSchema = "APP";
                if (mDS == null) {
                    // Create a OracleDataSource instance
                    Class c = null;
                    try {
                        c = Class.forName("org.apache.derby.jdbc.EmbeddedDataSource");
                    } catch (ClassNotFoundException cnfe) {
                        throw new Exception(mMessages.getString("Util.Class_cannot_be_found", "org.apache.derby.jdbc.EmbeddedDataSource"), cnfe);
                    }
                    mDS = (DataSource) c.newInstance();

                    // Sets to create a new database.
                    ReflectionUtil.call(mDS, "setCreateDatabase", "create");

                    // Sets the database name
                    ReflectionUtil.call(mDS, "setDatabaseName", dbSchema);
                }
                try {
                    conn = mDS.getConnection();
                } catch (SQLWarning e) {
                    // Database already exists -- we keep going
                    // Sets the port number
                    ReflectionUtil.call(mDS, "setCreateDatabase", null);
                } catch (Exception e) {
                    // Something wrong happened
                    conn = null;
                    throw new Exception(mMessages.getString("Util.Fail_to_connect_to_database"), e);
                }

                return conn;
            }
            InitialContext jndiContext = (InitialContext) prop.get(PROP_DB_JNDI_CONTEXT);
            String jndiName = prop.getProperty(PROP_DB_NON_XA_JNDI_NAME);
            if (jndiContext != null && jndiName != null) {
                try {
                    DataSource jds = (DataSource) jndiContext.lookup(jndiName);
                    conn = jds.getConnection();
                } catch (Exception e) {
                    mMessages.log(Level.SEVERE, "Util.DataSource_not_found_in_the_context", jndiName);
                    conn = null;
                }
                if (conn != null) {
                    return conn;
                }
            }
            String username = prop.getProperty(PROP_DB_USERNAME);
            String password = prop.getProperty(PROP_DB_PASSWORD);
            String dbType = prop.getProperty(PROP_DB_TYPE);
            String hostname = prop.getProperty(PROP_DB_HOSTNAME);
            int port = new Integer(prop.getProperty(PROP_DB_PORT)).intValue();
            String sid = prop.getProperty(PROP_DB_SID);
            if (dbType.equalsIgnoreCase(DB_TYPE_ORACLE)) {
                // Create a OracleDataSource instance
                Class c = null;
                try {
                    c = Class.forName("oracle.jdbc.pool.OracleDataSource");
                } catch (ClassNotFoundException cnfe) {
                    throw new Exception(mMessages.getString("Util.Class_cannot_be_found", "oracle.jdbc.pool.OracleDataSource"), cnfe);
                }
                DataSource ods = (DataSource) c.newInstance();

                // Sets the driver type
                ReflectionUtil.call(ods, "setDriverType", "thin");

                // Sets the database server name
                ReflectionUtil.call(ods, "setServerName", hostname);

                // Sets the port number
                ReflectionUtil.call(ods, "setPortNumber", port);

                // Sets the user name
                ReflectionUtil.call(ods, "setUser", username);

                // Sets the password
                ReflectionUtil.call(ods, "setPassword", password);

                // Sets the database name
                ReflectionUtil.call(ods, "setDatabaseName", sid);

                // Create a connection  object
                conn = ods.getConnection();

                return conn;
            }
            
            //-----------------------------------------------
            // Fortent (jtaylor) 2008-11-21 : DB2 connection
            //-----------------------------------------------
            if (dbType.equalsIgnoreCase(DB_TYPE_DB2)) {

            	// Extra definitions defined in OperatorConstants
                String db2_databaseName = prop.getProperty(            PROP_DB2_DATABASENAME);
            	int    db2_driverType   = new Integer(prop.getProperty(PROP_DB2_DRIVER_TYPE)).intValue();
                String db2_user         = prop.getProperty(            PROP_DB2_USER);
                String db2_password     = prop.getProperty(            PROP_DB2_PASSWORD);
                int    db2_portNumber   = new Integer(prop.getProperty(PROP_DB2_PORTNUMBER)).intValue();
                String db2_serverName   = prop.getProperty(            PROP_DB2_SERVERNAME);

                // Create a DB2ConnectionPoolDataSource instance
                Class c = null;
                try {
                    c = Class.forName("com.ibm.db2.jcc.DB2ConnectionPoolDataSource");
                } catch (ClassNotFoundException cnfe) {
                    throw new Exception(mMessages.getString("Util.Class_cannot_be_found", "com.ibm.db2.jcc.DB2ConnectionPoolDataSource"), cnfe);
                }
                DataSource dds = (DataSource) c.newInstance();

                // Sets the database name
                ReflectionUtil.call(dds, "setDatabaseName", db2_databaseName);

                // Sets the driver type
                ReflectionUtil.call(dds, "setDriverType",   db2_driverType);

                // Sets the user name
                ReflectionUtil.call(dds, "setUser",         db2_user);

                // Sets the password
                ReflectionUtil.call(dds, "setPassword",     db2_password);

                // Sets the port number
                ReflectionUtil.call(dds, "setPortNumber",   db2_portNumber);

                // Sets the database server name
                ReflectionUtil.call(dds, "setServerName",   db2_serverName);

                // Create a connection  object
                conn = dds.getConnection();

                return conn;
            }
            
            if (dbType.equalsIgnoreCase(DB_TYPE_DERBY)) {
                try {
                    Class.forName("org.apache.derby.jdbc.ClientDriver");
                } catch (ClassNotFoundException cnfe) {
                    throw new Exception(mMessages.getString("Util.Class_cannot_be_found", "org.apache.derby.jdbc.ClientDriver"), cnfe);
                }
                StringBuffer url = new StringBuffer();
                url.append("jdbc:derby://");
                url.append(hostname + ":" + port + "/");
                url.append(sid);
                url.append(";create=true");
                url.append(";user=" + username);
                url.append(";password=" + password);

                conn = DriverManager.getConnection(url.toString());
                return conn;
            }
            if (dbType.equalsIgnoreCase(DB_TYPE_MYSQL)) {
               try {
                    // The newInstance() call is a work around for some
                    // broken Java implementations
                    Class.forName("com.mysql.jdbc.Driver").newInstance();
                } catch (ClassNotFoundException cnfe) {
                    throw new Exception(mMessages.getString("Util.Class_cannot_be_found", "com.mysql.jdbc.Driver"), cnfe);
                }
                StringBuffer url = new StringBuffer();
                url.append("jdbc:mysql://");
                url.append(hostname + ":" + port + "/");
                url.append(sid);
                url.append("?user=" + username);
                url.append("&password=" + password);

                conn = DriverManager.getConnection(url.toString());
                return conn;
            }
        } catch (Exception e) {
            
            // Trap SQL errors
            StringBuffer sb = new StringBuffer();
            sb.append(mMessages.getString("Util.Fail_to_connect_to_database_with_given_properties"));
            sb.append("\n");
            sb.append(getConfigPropAsString(prop));
            sb.append("\n");
            sb.append(mMessages.getString("Util.Database_may_not_be_started_or_the_connection_properties_are_incorrect"));
            //Alert
            IEPSEAlertSender.getInstance().alertUnableToConnectToDB("IEPSE", sb.toString());
            //
            
            throw new Exception(sb.toString(), e);
        }
        return conn;
    }

    public static String getConfigPropAsString(Properties p) {
        StringBuffer sb = new StringBuffer();
        Context jndiContext = (Context) p.get(PROP_DB_JNDI_CONTEXT);
        String jndiName = p.getProperty(PROP_DB_NON_XA_JNDI_NAME);
        if (jndiContext != null && jndiName != null) {
            sb.append(PROP_DB_NON_XA_JNDI_NAME + "=" + p.getProperty(PROP_DB_NON_XA_JNDI_NAME) + "\n");
            sb.append(PROP_DB_SCHEMA + "=" + p.getProperty(PROP_DB_SCHEMA) + "\n");
            sb.append(PROP_RUNTIME_STYLE + "=" + p.getProperty(PROP_RUNTIME_STYLE) + "\n");
        } else {
            sb.append(PROP_DB_TYPE + "=" + p.getProperty(PROP_DB_TYPE) + "\n");
            sb.append(PROP_DB_HOSTNAME + "=" + p.getProperty(PROP_DB_HOSTNAME) + "\n");
            sb.append(PROP_DB_PORT + "=" + p.getProperty(PROP_DB_PORT) + "\n");
            sb.append(PROP_DB_SID + "=" + p.getProperty(PROP_DB_SID) + "\n");
            sb.append(PROP_DB_USERNAME + "=" + p.getProperty(PROP_DB_USERNAME) + "\n");
            sb.append(PROP_DB_PASSWORD + "=" + p.getProperty(PROP_DB_PASSWORD) + "\n");
            sb.append(PROP_RUNTIME_STYLE + "=" + p.getProperty(PROP_RUNTIME_STYLE) + "\n");
        }
        return sb.toString();
    }

    public static int getSqlType(String sqlTypeName) {
        for (int i = 0; i < _SQL_TYPE_NAMES.length; i++) {
            if (_SQL_TYPE_NAMES[i].equalsIgnoreCase(sqlTypeName)) {
                return _SQL_TYPES[i];
            }
        }
        return java.sql.Types.OTHER;
    }

    public static String getSqlTypeName(int sqlType) {
        for (int i = 0; i < _SQL_TYPE_NAMES.length; i++) {
            if (_SQL_TYPES[i] == sqlType) {
                return _SQL_TYPE_NAMES[i];
            }
        }
        return "Unknown type: " + sqlType;
    }

    private static final String[] mEMS_TABLES = new String[]{TABLE_EMS_ENGINE, TABLE_EMS_PLAN, TABLE_EMS_PLAN_VERSIONS, TABLE_EMS_OUTPUT};

    //ignore tables which should not be deleted
    private static final String[] mEMS_IGNORE_DROP_TABLES = new String[]{TABLE_EMS_PLAN_VERSIONS};
    
    public static boolean isDbInitialized(Properties configProp) {
        Connection con = null;
        try {
            con = getConnection(configProp);
            con.setAutoCommit(false);
            String dbSchema = configProp.getProperty(PROP_DB_SCHEMA);
            if (RUNTIME_STYLE_EMBEDDED.equalsIgnoreCase(configProp.getProperty(PROP_RUNTIME_STYLE))) {
                dbSchema = "APP";
            }
            Statement s = null;
            try {
                s = con.createStatement();
                s.execute("CREATE SCHEMA " + dbSchema);
                return false;
            } catch (Exception e) {
                mMessages.log(Level.SEVERE, "Util.IEP_database_has_not_been_initialized");
            } finally {
                close(s);
            }
            int found = 0;
            for (String emsTableName : mEMS_TABLES) {
                if (getDbSpecial().hasTable(con, dbSchema, emsTableName)) {
                    found++;
                }
            }
            if (found != mEMS_TABLES.length) {
                mMessages.log(Level.SEVERE, "Util.IEP_database_has_not_been_initialized");
                return false;
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_initialize_IEP_database", e);
        } finally {
            rollback(con);
            close(con);
        }
        return true;
    }

    // id (1) -------- (1) instaneId
    // instanceId -------
    // id (unique in the database) generated for each instanceId. Oracle
    // table names cannot be longer than 30 letters, using id instead of instanceId
    // allows us create more tables.  The instanceId allows better eligibility.
    // Think id as an encoded instanceId. id is also called planId
    private static final String QUERY_TOKEN = "SELECT 'x' FROM " + TABLE_EMS_TOKEN;
    private static final String INSERT_TOKEN = "INSERT INTO " + TABLE_EMS_TOKEN + " (" + COL_ID + ", " + COL_NAME + ") VALUES (?, ?)";
    private static final String QUERY_ENGINE = "SELECT 'x' FROM " + TABLE_EMS_ENGINE + " WHERE " + COL_ID + " = ?";
    private static final String INSERT_ENGINE = "INSERT INTO " + TABLE_EMS_ENGINE + " ("
            + COL_ID + "," 
            + COL_INSTANCE_LOCATION + ","
            + COL_LEASE_EXPIRATION + ") VALUES (?, ?, ?)";
    public static boolean initializeDB(Properties configProp) {
        Connection con = null;
        try {
            con = getConnection(configProp);
            con.setAutoCommit(false);
            setDbSpecial(con, configProp);
            
            // Create schema
            String dbSchema = configProp.getProperty(PROP_DB_SCHEMA);
            if (dbSchema == null || dbSchema.trim().equals("")) {
                dbSchema = "iepseDB";
            }
            if (RUNTIME_STYLE_EMBEDDED.equals(configProp.getProperty(PROP_RUNTIME_STYLE))) {
                dbSchema = "APP";
            }
            Statement s = null;
            try {
                s = con.createStatement();
                s.execute("CREATE SCHEMA " + dbSchema);
                con.commit();
                if (mMessages.isLoggable(Level.FINE)) {
                    mMessages.log(Level.FINE, "Util.Schema_is_created_successfully", dbSchema);
                }    
            } catch (Exception e) {
                rollback(con);
                if (mMessages.isLoggable(Level.FINE)) {
                    mMessages.log(Level.FINE, "Util.Schema_already_exists", dbSchema);
                }    
            } finally {
                close(s);
            }

            // Create TABLE_EMS_TOKEN table
            Schema tokenSchema = new Schema(TABLE_EMS_TOKEN);
            ArrayList<String> columnList = new ArrayList<String>();
            columnList.add(COL_ID);
            // Fortent (mo) 2008-11-21 : DB2 primary key columns must be defined “NOT NULL” 
            // columnList.add(SQL_TYPE_INTEGER + " PRIMARY KEY");
            columnList.add(SQL_TYPE_INTEGER + " NOT NULL PRIMARY KEY");
            columnList.add("");
            columnList.add("");
            columnList.add(COL_NAME);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("20");
            columnList.add("");
            tokenSchema.setColumnMetadataAsList(columnList);
            try {
                getDbSpecial().createTable(con, TABLE_EMS_TOKEN, tokenSchema);
                con.commit();
                if (mMessages.isLoggable(Level.FINE)) {
                    mMessages.log(Level.FINE, "Util.Table_is_created_successfully", TABLE_EMS_TOKEN);
                }    
            } catch (Exception e) {
                rollback(con);
                if (mMessages.isLoggable(Level.FINE)) {
                    mMessages.log(Level.FINE, "Util.Table_already_exists", TABLE_EMS_TOKEN);
                }    
            }

            // Insert a row into TABLE_EMS_TOKEN table if no such row yet
            PreparedStatement queryTokenPs = null;
            ResultSet rsToken = null;
            boolean hasToken = false;
            try {
                queryTokenPs = con.prepareStatement(QUERY_TOKEN);
                rsToken = queryTokenPs.executeQuery();
                if (rsToken.next()) {
                    hasToken = true;
                }
            } catch (Exception e) {
                mMessages.log(Level.SEVERE, "Util.Fail_to_check_if_any_token_already_exists_in_table", TABLE_EMS_TOKEN, e);
            } finally {
                close(rsToken);
                close(queryTokenPs);
            }
            if (!hasToken) {
                PreparedStatement insertTokenPs = null;
                try {
                    insertTokenPs = con.prepareStatement(INSERT_TOKEN);
                    insertTokenPs.setInt(1, 0);
                    insertTokenPs.setString(2, "token");
                    insertTokenPs.execute();
                    con.commit();
                    if (mMessages.isLoggable(Level.FINE)) {
                        mMessages.log(Level.FINE, "Util.Table_is_initialized_successfully", TABLE_EMS_TOKEN);
                    }    
                } catch (Exception e) {
                    rollback(con);
                    mMessages.log(Level.SEVERE, "Util.Fail_to_initialize_table", TABLE_EMS_TOKEN, e);
                } finally {
                    close(insertTokenPs);
                }
            }
            
            Schema[] emsSchemas = new Schema[mEMS_TABLES.length];
            // TABLE_EMS_ENGINE
            columnList = new ArrayList<String>();
            columnList.add(COL_ID);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("200");
            columnList.add("");

            columnList.add(COL_INSTANCE_LOCATION);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("200");
            columnList.add("");

            columnList.add(COL_LEASE_EXPIRATION);
            columnList.add(SQL_TYPE_TIMESTAMP);
            columnList.add("");
            columnList.add("");
            emsSchemas[0] = new Schema(TABLE_EMS_PLAN);
            emsSchemas[0].setColumnMetadataAsList(columnList);

            // TABLE_EMS_PLAN
            columnList = new ArrayList<String>();
            columnList.add(COL_ID);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("200");
            columnList.add("");

            columnList.add(COL_INSTANCE_ID);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("200");
            columnList.add("");

            columnList.add(COL_NAME);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("200");
            columnList.add("");

            columnList.add(COL_CONTENT);
            columnList.add("blob");
            columnList.add("");
            columnList.add("");

            columnList.add(COL_LAST_ACTIVATION);
            columnList.add(SQL_TYPE_TIMESTAMP);
            columnList.add("");
            columnList.add("");

            columnList.add(COL_STATUS);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("20");
            columnList.add("");

            columnList.add(COL_ENGINE_ID);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("200");
            columnList.add("");
            emsSchemas[1] = new Schema(TABLE_EMS_PLAN);
            emsSchemas[1].setColumnMetadataAsList(columnList);
            
            //TABLE_EMS_PLAN_VERSIONS
            columnList = new ArrayList<String>();
            columnList.add(COL_INSTANCE_ID);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("200");
            columnList.add("");
            
            columnList.add(COL_NAME);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("200");
            columnList.add("");
            
            columnList.add(COL_VERSION_ID);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("200");
            columnList.add("");
            
            columnList.add(COL_CONTENT);
            columnList.add("blob");
            columnList.add("");
            columnList.add("");
            
            columnList.add(COL_LAST_MODIFIED);
            columnList.add(SQL_TYPE_TIMESTAMP);
            columnList.add("");
            columnList.add("");

            emsSchemas[2] = new Schema(TABLE_EMS_PLAN_VERSIONS);
            emsSchemas[2].setColumnMetadataAsList(columnList);
            
            // TABLE_EMS_OUTPUT
            columnList = new ArrayList<String>();
            columnList.add(COL_PLAN_INSTANCE_ID);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("200");
            columnList.add("");

            columnList.add(COL_OUTPUT_NAME);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("200");
            columnList.add("");

            columnList.add(COL_OUTPUT_DESCRIPTION);
            columnList.add(SQL_TYPE_VARCHAR);
            columnList.add("200");
            columnList.add("");

            columnList.add(COL_OUTPUT_LAST_UPDATED);
            columnList.add(SQL_TYPE_TIMESTAMP);
            columnList.add("");
            columnList.add("");
            emsSchemas[3] = new Schema(TABLE_EMS_OUTPUT);
            emsSchemas[3].setColumnMetadataAsList(columnList);
            
            
            String engineId = configProp.getProperty(PROP_ENGINE_ID, "0");
            int engineExpiryInterval;
            try {
                engineExpiryInterval = Integer.parseInt(configProp.getProperty(PROP_ENGINE_EXPIRY_INTERVAL));
            } catch (Exception e) {
                engineExpiryInterval = ENGINE_EXPIRY_INTERVAL_FACTORY_DEFAULT;
            }

            Token token = null;
            try {
                // ACQUIRE TOKEN
                token = new Token(con);
                List<String> foundEmsTableList = new ArrayList<String>();
                // Create IEPSE system table if not created yet
                for (int i = 0; i < mEMS_TABLES.length; i++) {
                    String tableName = mEMS_TABLES[i];
                    if (getDbSpecial().hasTable(con, dbSchema, tableName)) {
                        foundEmsTableList.add(tableName);
                    } else {
                        getDbSpecial().createTable(con, tableName, emsSchemas[i]);
                    }
                }
                
                //create procedure if tableList is empty, this will be empty pre TABLE_EMS_PLAN_VERSIONS
                //version of iepse running
                //or if tableList has one table which is TABLE_EMS_PLAN_VERSIONS
                //which remains global during install/reinstall of iepse
                boolean createProcedure = foundEmsTableList.isEmpty() ||
                    (foundEmsTableList.size() == 1 && foundEmsTableList.get(0).equalsIgnoreCase(TABLE_EMS_PLAN_VERSIONS));
                
                if (createProcedure) {
                    getDbSpecial().createStoredProcedures(con);
                }
                
                // Register this engine at the engine table if not yet
                PreparedStatement queryEnginePs = null;
                ResultSet rsEnginePs = null;
                boolean hasEngine = false;
                try {
                    queryEnginePs = con.prepareStatement(QUERY_ENGINE);
                    queryEnginePs.setString(1, engineId);
                    rsEnginePs = queryEnginePs.executeQuery();
                    if (rsEnginePs.next()) {
                        hasEngine = true;
                    }
                } catch (Exception e) {
                    mMessages.log(Level.SEVERE, "Util.Fail_to_check_if_engine_is_already_registered", engineId, e);
                } finally {
                    close(rsEnginePs);
                    close(queryEnginePs);
                }
                Timestamp expiration = new Timestamp(System.currentTimeMillis() + 1000*engineExpiryInterval);
                if (hasEngine) {
                    updateEngineLeaseExpiration(con, engineId, expiration);
                } else {
                    PreparedStatement insertEnginePs = null;
                    try {
                        insertEnginePs = con.prepareStatement(INSERT_ENGINE);
                        insertEnginePs.setString(1, engineId);
                        insertEnginePs.setString(2, mLocalIPAddress);
                        insertEnginePs.setTimestamp(3, expiration);
                        insertEnginePs.executeUpdate();
                    } catch (Exception e) {
                        mMessages.log(Level.SEVERE, "Util.Fail_to_add_engine", engineId, e);
                    } finally {
                        close(insertEnginePs);
                    }
                }    
                con.commit();
            } catch (Exception e) {
                rollback(con);
                throw e;
            } finally {
                if (token != null) {
                    token.close();
                }    
            }
            // TOKEN RELEASED
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "Util.IEP_database_initialization_succeed");
            }    
            return true;
        } catch (Exception e) {
            rollback(con);
            mMessages.log(Level.SEVERE, "Util.Fail_to_initialize_IEP_database", e);
        } finally {
            close(con);
        }
        return false;
    }

    public static void cleanDB(Properties configProp) {
        Connection con = null;
        Token token = null;
        try {
            String dbSchema = configProp.getProperty(PROP_DB_SCHEMA);
            if (RUNTIME_STYLE_EMBEDDED.equals(configProp.getProperty(PROP_RUNTIME_STYLE))) {
                dbSchema = "APP";
            }
            con = getConnection(configProp);
            con.setAutoCommit(false);
            // ACQUIRE TOKEN
            token = new Token(con);
            
            //we do not want to drop tables which are in ignoreTableList
            List<String> ignoreTableList = new ArrayList<String>();
            for(int i=0; i < mEMS_IGNORE_DROP_TABLES.length; i++) {
                ignoreTableList.add(mEMS_IGNORE_DROP_TABLES[i]);
            }
            
            boolean clean = true;
            for (String tableName : mEMS_TABLES) {
                if (getDbSpecial().hasTable(con, dbSchema, tableName) && !ignoreTableList.contains(tableName.toUpperCase())) {
                    dropTable(con, tableName);
                    //cleanTable(con, tableName);
                    clean = false;
                }
            }
            if (!clean) {
                getDbSpecial().dropStoredProcedures(con);
            }    
            cleanTable(con, TABLE_EMS_TOKEN);
            con.commit();
            if (mMessages.isLoggable(Level.FINE)) {
                mMessages.log(Level.FINE, "Util.EMS_tables_are_successfully_removed");
            }    
        } catch (Exception e) {
            rollback(con);
            mMessages.log(Level.SEVERE, "Util.Fail_to_remove_EMS_tables", e);
        } finally {
            if (token != null) {
                token.close();
            }
            close(con);
        }
        // TOKEN RELEASED
    }

    public static void dropTable(Connection con, String tableName) throws Exception {
        Statement stmt = null;
        try {
            StringBuffer sql = new StringBuffer();
            sql.append("DROP TABLE ");
            sql.append(tableName);
            stmt = con.createStatement();
            stmt.execute(sql.toString());
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_drop_table", tableName), e);
        } finally {
            close(stmt);
        }
    }

    public static void cleanTable(Connection con, String tableName) throws Exception {
        Statement stmt = null;
        try {
            StringBuffer sql = new StringBuffer();
            sql.append("DELETE FROM ");
            sql.append(tableName);
            stmt = con.createStatement();
            stmt.execute(sql.toString());
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_clean_table", tableName), e);
        } finally {
            close(stmt);
        }
    }

    //======================== TABLE_EMS_PLAN =======================================================
    // We could use A-Z, a-z, 0-9 so that total number of plans can be (26 + 26 + 10) ^ 9 = 56700 billion
    // For now, we just use digits and total number of plans is 1,000,000,000: 0 - 999,999,999
    private static final String QUERY_PLAN_IDS = "SELECT " + COL_ID + " FROM " + TABLE_EMS_PLAN;
    private static final int MAX_PLAN_COUNT = 999999999;

    public static String getNextPlanId(Connection con) throws Exception {
        Statement getStmt = null;
        ResultSet getRs = null;
        try {
            getStmt = con.createStatement();
            getRs = getStmt.executeQuery(QUERY_PLAN_IDS);

            if (!getRs.next()) {
                return "0";
            }
            List<String> idList = new ArrayList<String>();
            do {
                idList.add(getRs.getString(1));
            } while (getRs.next());
            if (idList.size() >= MAX_PLAN_COUNT) {
                // EMS_PLAN table is full
                return null;
            }
            Collections.sort(idList);
            for (int i = 0; i <= MAX_PLAN_COUNT; i++) {
                String id = "" + i;
                int idx = Collections.binarySearch(idList, id);
                if (idx < 0) {
                    return id;
                }
            }
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_create_next_plan_id"));
        } finally {
            close(getRs);
            close(getStmt);
        }
        return null;
    }

    private static final String QUERY_UNDEPLOYED_ORPHAN_PLAN_LIST = "SELECT " + COL_INSTANCE_ID + " FROM " + TABLE_EMS_PLAN
            + " WHERE " + COL_STATUS + " = '" + STATUS_UNDEPLOYED + "' AND "  + COL_ENGINE_ID + " <> ?" ;
    public static List<String> getUndeployedOrphanPlanList(Connection con, String engineId) throws Exception {
        PreparedStatement getStmt = null;
        ResultSet getRs = null;
        List<String> list = new ArrayList<String>();
        try {
            getStmt = con.prepareStatement(QUERY_UNDEPLOYED_ORPHAN_PLAN_LIST);
            getStmt.setString(1, engineId);
            getRs = getStmt.executeQuery();

            if (getRs.next()) {
                do {
                    list.add(getRs.getString(1));
                } while (getRs.next());
            }
            return list;
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_retrieve_undeployed_event_processors"), e);            
        } finally {
            close(getRs);
            close(getStmt);
        }
    }

    private static final String QUERY_STARTED_ORPHAN_PLAN_LIST = "SELECT " + COL_INSTANCE_ID + " FROM " + TABLE_EMS_PLAN
            + " p WHERE " + COL_STATUS + " = '" + STATUS_STARTED + "' AND " + COL_ENGINE_ID + " <> ? AND (SELECT " 
            + COL_LEASE_EXPIRATION + " FROM " + TABLE_EMS_ENGINE + " e WHERE e." + COL_ID + " = p." + COL_ENGINE_ID 
            + ") < CURRENT_TIMESTAMP";
    public static List<String> getStartedOrphanPlanList(Connection con, String myEngineId) throws Exception {
        PreparedStatement pstmt = null;
        ResultSet getRs = null;
        List<String> list = new ArrayList<String>();
        try {
            pstmt = con.prepareStatement(QUERY_STARTED_ORPHAN_PLAN_LIST);
            pstmt.setString(1, myEngineId);
            getRs = pstmt.executeQuery();

            if (getRs.next()) {
                do {
                    list.add(getRs.getString(1));
                } while (getRs.next());
            }
            return list;
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_retrieve_started_orphan_event_processors"), e);
        } finally {
            close(getRs);
            close(pstmt);
        }
    }
    
    private static final String QUERY_PLAN_LIST = "SELECT " + COL_ID + "," + COL_INSTANCE_ID + "," + COL_NAME + " FROM " + TABLE_EMS_PLAN;
    public static List<String[]> getPlanList(Connection con) throws Exception {
        Statement getStmt = null;
        ResultSet getRs = null;
        List<String[]> list = new ArrayList<String[]>();
        try {
            getStmt = con.createStatement();
            getRs = getStmt.executeQuery(QUERY_PLAN_LIST);

            if (getRs.next()) {
                do {
                    list.add(new String[]{getRs.getString(1), getRs.getString(2), getRs.getString(3)});
                } while (getRs.next());
            }
            return list;
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_retrieve_deployed_event_processors"), e);
        } finally {
            close(getRs);
            close(getStmt);
        }
    }

    private static final String QUERY_PLAN_ID = "SELECT " + COL_ID + " FROM " + TABLE_EMS_PLAN + " WHERE " + COL_INSTANCE_ID + " = ?";
    public static String getPlanId(Connection con, String instanceId) throws Exception {
        PreparedStatement pstmt = null;
        ResultSet rs = null;
        try {
            pstmt = con.prepareStatement(QUERY_PLAN_ID);
            pstmt.setString(1, instanceId);
            rs = pstmt.executeQuery();
            if (rs.next()) {
                return rs.getString(1);
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_retrieve_id_given_instance_id", instanceId, e);
        } finally {
            close(rs);
            close(pstmt);
        }
        return null;
    }

    public static String getPlanId(String instanceId) {
        String ret = null;
        Connection con = null;
        try {
            con = getConnection();
            ret = getPlanId(con, instanceId);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_retrieve_id_given_instance_id", instanceId, e);
        } finally {
            close(con);
        }
        return ret;
    }

    private static final String QUERY_PLAN_NAME = "SELECT " + COL_NAME + " FROM " + TABLE_EMS_PLAN + " WHERE " + COL_INSTANCE_ID + " = ?";
    public static String getPlanName(Connection con, String instanceId) throws Exception {
        PreparedStatement pstmt = null;
        ResultSet rs = null;
        try {
            pstmt = con.prepareStatement(QUERY_PLAN_NAME);
            pstmt.setString(1, instanceId);
            rs = pstmt.executeQuery();
            if (rs.next()) {
                return rs.getString(1);
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_retrieve_name_given_instance_id", instanceId, e);
        } finally {
            close(rs);
            close(pstmt);
        }
        return null;
    }

    private static final String QUERY_PLAN_CONTENT = "SELECT " + COL_CONTENT + " FROM " + TABLE_EMS_PLAN + " WHERE " + COL_INSTANCE_ID + " = ?";
    public static String getPlanContent(Connection con, String instanceId) throws Exception {
        PreparedStatement pstmt = null;
        ResultSet rs = null;
        try {
            pstmt = con.prepareStatement(QUERY_PLAN_CONTENT);
            pstmt.setString(1, instanceId);
            rs = pstmt.executeQuery();
            if (rs.next()) {
                Blob b = rs.getBlob(1);
                // note that: Blob's first byte starts at position 1
                byte[] bytes = b.getBytes(1, (int) b.length());
                String ret = new String(bytes, "UTF-8");
                return ret;
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_retrieve_content_given_instance_id", instanceId, e);
        } finally {
            close(rs);
            close(pstmt);
        }
        return null;
    }
    private static final String UPDATE_PLAN_CONTENT = "UPDATE " + TABLE_EMS_PLAN + " SET " + COL_CONTENT + " = ? WHERE " + COL_INSTANCE_ID + " = ?";
    public static void setPlanContent(Connection con, String instanceId, String content) {
        PreparedStatement pstmt = null;
        try {
            byte[] bytes = content.getBytes("UTF8");

            pstmt = con.prepareStatement(UPDATE_PLAN_CONTENT);
            pstmt.setObject(1, bytes);
            pstmt.setString(2, instanceId);
            pstmt.executeUpdate();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_update_status_given_instance_id", instanceId, e);
        } finally {
            close(pstmt);
        }
    }
    
    private static final String QUERY_EMS_PLAN_VERSIONS = "SELECT " + COL_VERSION_ID + "," + COL_CONTENT +" FROM " + TABLE_EMS_PLAN_VERSIONS + " WHERE " + COL_INSTANCE_ID + " = ? AND " + COL_NAME + " = ?" + " ORDER BY " + COL_VERSION_ID + " DESC" ;
    private static final String INSERT_EMS_PLAN_VERSIONS = "INSERT INTO " + TABLE_EMS_PLAN_VERSIONS + "(" + COL_INSTANCE_ID + "," + COL_NAME + "," + COL_VERSION_ID + "," + COL_CONTENT + "," + COL_LAST_MODIFIED +  ") VALUES(?,?,?,?,?)";
    public static void insertNewEMSPlanVersion(Connection con, String instanceId, String planName, byte[] contentBytes) {
        PreparedStatement pstmt = null;
        try {
            String newContent = new String(contentBytes, "UTF-8");
            //first find out if a version
            //already exist for given plan
            //(a)if it exists check if the plan contents are different
            //if so then insert it as a new version
            //(b) if no version exists then insert the first version
            //the last version id so that we can
            //set new content with new version id
            pstmt = con.prepareStatement(QUERY_EMS_PLAN_VERSIONS);
            pstmt.setString(1, instanceId);
            pstmt.setString(2, planName);
            
            ResultSet rs = pstmt.executeQuery();
            if(rs.next()) {
                String currentVersionId = rs.getString(1);
                Blob b = rs.getBlob(2);
                // note that: Blob's first byte starts at position 1
                byte[] existingBytes = b.getBytes(1, (int) b.length());
                String existingContent = new String(existingBytes, "UTF-8");
                
                close(pstmt);
                //if we have new content then only create a new version
                if(existingContent.hashCode() != newContent.hashCode()) {
                    int currentVersionIdInt = Integer.parseInt(currentVersionId);
                    pstmt = con.prepareStatement(INSERT_EMS_PLAN_VERSIONS);
                    pstmt.setString(1, instanceId);
                    pstmt.setString(2, planName);
                    pstmt.setString(3, ""+(currentVersionIdInt +1));
                    pstmt.setObject(4, contentBytes);
                    pstmt.setTimestamp(5,  new Timestamp(System.currentTimeMillis()));
                    
                    pstmt.executeUpdate();
                }
            } else {
                //no version exists for given plan
                //insert first version
                int currentVersionIdInt = 0;
                pstmt = con.prepareStatement(INSERT_EMS_PLAN_VERSIONS);
                pstmt.setString(1, instanceId);
                pstmt.setString(2, planName);
                pstmt.setString(3, ""+(currentVersionIdInt +1));
                pstmt.setObject(4, contentBytes);
                pstmt.setTimestamp(5,  new Timestamp(System.currentTimeMillis()));
                
                pstmt.executeUpdate();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_insert_new_plan_version_given_instance_id", instanceId, e);
        } finally {
            close(pstmt);
        }
    }
    
    private static final String QUERY_VERSION_SPECIFIED_EMS_PLAN_VERSIONS_CONTENT = "SELECT " + COL_CONTENT +" FROM " + TABLE_EMS_PLAN_VERSIONS + " WHERE " + COL_INSTANCE_ID + " = ? AND " + COL_NAME + " = ? AND " + COL_VERSION_ID + " =?" ;
    private static final String QUERY_VERSION_NOT_SPECIFIED_EMS_PLAN_VERSIONS_CONTENT = "SELECT "  + COL_CONTENT +" FROM " + TABLE_EMS_PLAN_VERSIONS + " WHERE " + COL_INSTANCE_ID + " = ? AND " + COL_NAME + " = ?" + " ORDER BY " + COL_VERSION_ID + " DESC" ;
    public static String getPlanContent(Connection con, String planName, String instanceId, String planVersion) throws Exception {
        String content = null;
        PreparedStatement pstmt = null;
        try {
            //if version number is specified we use it
            if(planVersion != null) {
                pstmt = con.prepareStatement(QUERY_VERSION_SPECIFIED_EMS_PLAN_VERSIONS_CONTENT);
                pstmt.setString(1, instanceId);
                pstmt.setString(2, planName);
                pstmt.setString(3, planVersion);
                
                ResultSet rs = pstmt.executeQuery();
                if(rs.next()) {
                    Blob b = rs.getBlob(1);
                    // note that: Blob's first byte starts at position 1
                    byte[] existingBytes = b.getBytes(1, (int) b.length());
                    content = new String(existingBytes, "UTF-8");
                }
            } else {
                //version number is not specified, by default use
                //the latest version
                pstmt = con.prepareStatement(QUERY_VERSION_NOT_SPECIFIED_EMS_PLAN_VERSIONS_CONTENT);
                pstmt.setString(1, instanceId);
                pstmt.setString(2, planName);
                
                ResultSet rs = pstmt.executeQuery();
                if(rs.next()) {
                    Blob b = rs.getBlob(1);
                    // note that: Blob's first byte starts at position 1
                    byte[] existingBytes = b.getBytes(1, (int) b.length());
                    content = new String(existingBytes, "UTF-8");
                }
                
            }
        }catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_retrieve_plan_content_given_version_and_instance_id", new Object[] {planName, planVersion, instanceId}, e);
            throw e;
        } finally {
            close(pstmt);
        }
        return content;
    }
    
    private static final String DELETE_FROM_EMS_PLAN_VERSIONS = "DELETE FROM " + TABLE_EMS_PLAN_VERSIONS + " WHERE " + COL_INSTANCE_ID + " = ? AND " + COL_NAME + " = ?" ;
    public static void deletePlanVersions(Connection con, String planName, String instanceId) throws Exception {
        PreparedStatement pstmt = null;
        try {
            pstmt = con.prepareStatement(DELETE_FROM_EMS_PLAN_VERSIONS);
            pstmt.setString(1, instanceId);
            pstmt.setString(2, planName);
            
            pstmt.executeUpdate();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_delete_plan_versions_given_planName_and_instance_id", new Object[] {planName, instanceId}, e);
            throw e;
        } finally {
            close(pstmt);
        }
    }
    
    public static void deleteAllPlanVersions(Connection con) throws Exception {
            cleanTable(con, TABLE_EMS_PLAN_VERSIONS);
    }
    
    private static final String QUERY_PLAN_STATUS = "SELECT " + COL_STATUS + " FROM " + TABLE_EMS_PLAN + " WHERE " + COL_INSTANCE_ID + " = ?";
    public static String getPlanStatus(Connection con, String instanceId) {
        PreparedStatement pstmt = null;
        ResultSet rs = null;
        try {
            pstmt = con.prepareStatement(QUERY_PLAN_STATUS);
            pstmt.setString(1, instanceId);
            rs = pstmt.executeQuery();
            if (rs.next()) {
                String ret = rs.getString(1);
                return ret;
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_retrieve_status_given_instance_id", instanceId, e);
        } finally {
            close(rs);
            close(pstmt);
        }
        return null;
    }

    private static final String UPDATE_PLAN_STATUS = "UPDATE " + TABLE_EMS_PLAN + " SET " + COL_STATUS + " = ? WHERE " + COL_INSTANCE_ID + " = ?";
    public static void setPlanStatus(Connection con, String instanceId, String status) {
        PreparedStatement pstmt = null;
        try {
            pstmt = con.prepareStatement(UPDATE_PLAN_STATUS);
            pstmt.setString(1, status);
            pstmt.setString(2, instanceId);
            pstmt.executeUpdate();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_update_status_given_instance_id", instanceId, e);
        } finally {
            close(pstmt);
        }
    }
    
    private static final String UPDATE_PLAN_STATUS_AND_ENGINE_ID = "UPDATE " + TABLE_EMS_PLAN + " SET " + COL_STATUS + " = ?, " + COL_ENGINE_ID + " = ? WHERE " + COL_INSTANCE_ID + " = ?";
    public static void setPlanStatusAndEngineId(Connection con, String instanceId, String status, String engineId) {
        PreparedStatement pstmt = null;
        try {
            pstmt = con.prepareStatement(UPDATE_PLAN_STATUS_AND_ENGINE_ID);
            pstmt.setString(1, status);
            pstmt.setString(2, engineId);
            pstmt.setString(3, instanceId);
            pstmt.executeUpdate();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_update_status_and_engine_id_given_instance_id", instanceId, e);
        } finally {
            close(pstmt);
        }
    }
    
    public static final String QUERY_ENGINE_ID = "SELECT " + COL_ENGINE_ID + " FROM " + TABLE_EMS_PLAN + " WHERE " + COL_INSTANCE_ID + " = ?";
    public static boolean isPlanOwner(Connection con, String engineId, String instanceId) {
        PreparedStatement pstmt = null;
        ResultSet rs = null;
        try {
            pstmt = con.prepareStatement(QUERY_ENGINE_ID);
            pstmt.setString(1, instanceId);
            rs = pstmt.executeQuery();
            if (rs.next()) {
                String ret = rs.getString(1);
                return engineId.equals(ret);
            } else {
                return false;
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_check_if_engine_owns_plan", engineId, instanceId, e);
        } finally {
            close(pstmt);
        }
        return false;
    }
    
    private static final String INSERT_PLAN = "INSERT INTO " + TABLE_EMS_PLAN + " (" 
            + COL_ID + ", " 
            + COL_INSTANCE_ID + ", " 
            + COL_NAME + ", " 
            + COL_CONTENT + ", " 
            + COL_LAST_ACTIVATION + ", " 
            + COL_STATUS + ", "
            + COL_ENGINE_ID + ") VALUES (?, ?, ?, ?, ?, ?, ?)";
    private static final String UPDATE_PLAN = "UPDATE " + TABLE_EMS_PLAN + " SET " + COL_NAME + " = ?, " + COL_CONTENT + " = ? WHERE " + COL_INSTANCE_ID + " = ?";
    public static void savePlanToDatabase(Connection con, List<QueryPlanInfo> queryPlanInfoList) throws Exception {
        PreparedStatement insertStmt = null;
        PreparedStatement updateStmt = null;
        try {
            insertStmt = con.prepareStatement(INSERT_PLAN);
            updateStmt = con.prepareStatement(UPDATE_PLAN);
            for (int i = 0; i < queryPlanInfoList.size(); i++) {
                QueryPlanInfo qpi = queryPlanInfoList.get(i);
                String instanceId = qpi.getInstanceId();
                String planName = qpi.getFullName();
                String planFilePath = qpi.getPlanFilePath();

                try {
                    byte[] planContent = IOUtil.getBytes(planFilePath);

                    String planId = getPlanId(con, instanceId);
                    if (planId == null) {
                        planId = getNextPlanId(con);
                        insertStmt.setString(1, planId);
                        insertStmt.setString(2, instanceId);
                        insertStmt.setString(3, planName);
                        insertStmt.setObject(4, planContent);
                        insertStmt.setTimestamp(5, new Timestamp(0L));
                        insertStmt.setString(6, STATUS_SAVED);
                        insertStmt.setString(7, "");
                        insertStmt.executeUpdate();
                    } else {
                        updateStmt.setString(1, planName);
                        updateStmt.setObject(2, planContent);
                        updateStmt.setString(3, instanceId);
                        updateStmt.executeUpdate();
                    }

                    //always insert this plan to version table
                    insertNewEMSPlanVersion(con, instanceId, planName, planContent);
                } catch (Exception e) {
                    throw new Exception(mMessages.getString("Util.Error_saving_event_processor_to_database", 
                    		new Object[]{instanceId, planFilePath, e.getMessage()}), e);
                }
            }
        } finally {
            close(insertStmt);
            close(updateStmt);
        }
    }

    private static final String DELETE_PLAN = "DELETE FROM " + TABLE_EMS_PLAN + " WHERE " + COL_INSTANCE_ID + " = ?";
    public static void removePlanFromDatabase(Connection con, List<String> instanceIdList) throws Exception {
        PreparedStatement pstmt = null;
        try {
            pstmt = con.prepareStatement(DELETE_PLAN);
            for (int i = 0; i < instanceIdList.size(); i++) {
                String instanceId = instanceIdList.get(i);
                pstmt.setString(1, instanceId);
                pstmt.addBatch();
            }
            pstmt.executeBatch();
        } catch (Exception e) {
            throw e;
        } finally {
            close(pstmt);
        }
    }
    public static void removePlanFromDatabase(List<String> instanceIdList, Properties configProp) {
        Connection con = null;
        Token token = null;
        try {
            con = getConnection(configProp);
            con.setAutoCommit(false);
            // ACQUIRE TOKEN
            token = new Token(con);
            removePlanFromDatabase(con, instanceIdList);
            con.commit();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_undeploy_event_processors", instanceIdList, e);
            rollback(con);
        } finally {
            close(con);
        }
        // RELEASE TOKEN
    }
    public static void removePlanFromDatabase(String instanceId, Properties configProp) {
        ArrayList<String> instanceIdList = new ArrayList<String>();
        instanceIdList.add(instanceId);
        removePlanFromDatabase(instanceIdList, configProp);
    }

    public static QueryPlan getPlanByInstanceId(Connection con, Properties configProp, String instanceId) throws Exception {
        String id = null;
        String planName = null;
        String content = null;
        try {
            id = getPlanId(con, instanceId);
            if (id == null) {
                mMessages.log(Level.WARNING, "Util.Event_processor_is_not_deployed_yet", instanceId);
                return null;
            }
            planName = getPlanName(con, instanceId);
            content = getPlanContent(con, instanceId);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_retrieve_event_processor", instanceId, e);
            throw e;
        }
        Map<String, Object> queryPlanProp = new HashMap<String, Object>();
        queryPlanProp.put(PROP_PLAN_ID, id);
        queryPlanProp.put(PROP_INSTANCE_ID, instanceId);
        queryPlanProp.put(PROP_PLAN_NAME, planName);
        queryPlanProp.put(PROP_PLAN_CONTENT, content);
        queryPlanProp.put(PROP_CONFIG_PROPERTIES, configProp);
        return mQueryPlanFactory.createQueryPlan(queryPlanProp);
    }
    
    public static QueryPlan getPlanByInstanceId(Properties configProp, String instanceId) {
        Connection con = null;
        try {
            con = getConnection(configProp);
            return getPlanByInstanceId(con, configProp, instanceId);
        } catch (Exception e) {
        } finally {
            close(con);
        }
        return null;
    }


    //======================== TABLE_EMS_PROCESSING_STATE =======================================================
    public static String getProcessingStateTableName(String planId) {
        return TABLE_EMS_PROCESSING_STATE + "_" + planId;
    }

    /**
     * @return the status of the table before this method is called
     */
    public static int createProcessingStateTable(Connection con, String schemaName, String planId) throws Exception {
        String tableName = getProcessingStateTableName(planId);
        Schema schema = new Schema(tableName);
        List<String> columnList = new ArrayList<String>();
        columnList.add(COL_PROCESSING_STATE);
        columnList.add("blob");
        columnList.add("");
        columnList.add("");
        columnList.add(COL_PREV_TIMESTAMP_TO_CHECK);
        columnList.add(SQL_TYPE_TIMESTAMP);
        columnList.add("");
        columnList.add("");
        columnList.add(COL_PREV_TIMESTAMP_TO_PROCESS);
        columnList.add(SQL_TYPE_TIMESTAMP);
        columnList.add("");
        columnList.add("");
        schema.setColumnMetadataAsList(columnList);
        int status = getDbSpecial().checkTableStatus(con, schemaName, tableName, schema, new HashSet());
        switch (status) {
            case TS_UNKNOWN:
            case TS_NAME_NOT_EXIST:
                getDbSpecial().createTable(con, tableName, schema);
                break;
            case TS_TABLE_EXIST:
                break;
            case TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA:
                throw new Exception(mMessages.getString("Util.Table_already_exists_but_with_different_schema", tableName));
        }
        return status;
    }

    public static void dropProcessingStateTable(Connection con, String planId) throws Exception {
        String tableName = getProcessingStateTableName(planId);
        dropTable(con, tableName);
    }

    public static void initProcessingState(Connection con, String planId, Map<String, Object> processingState) throws Exception {
        String tableName = getProcessingStateTableName(planId);
        PreparedStatement insertStmt = null;
        try {
            Map ps = getProcessingState(con, planId);
            String s = ((Properties)processingState.get(PS_PLAN_STATE)).getProperty(PS_PREV_TIMESTAMP_TO_CHECK);
            Timestamp tsC = new Timestamp(Long.parseLong(s));
            s = ((Properties)processingState.get(PS_PLAN_STATE)).getProperty(PS_PREV_TIMESTAMP_TO_PROCESS);
            Timestamp tsP = new Timestamp(Long.parseLong(s));
            if (ps == null) {// insert a record if nothing there
                String state = marshallProcessingState(processingState);
                StringBuffer sb = new StringBuffer();
                sb.append("INSERT INTO ");
                sb.append(tableName + "(");
                sb.append(COL_PROCESSING_STATE + "," + COL_PREV_TIMESTAMP_TO_CHECK + "," + COL_PREV_TIMESTAMP_TO_PROCESS);
                sb.append(") VALUES (?,?,?)");
                insertStmt = con.prepareStatement(sb.toString());
                insertStmt.setObject(1, state.getBytes("UTF-8"));
                insertStmt.setTimestamp(2, tsC);
                insertStmt.setTimestamp(3, tsP);
                insertStmt.executeUpdate();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_initialize_processing_state_of_event_processor", planId, e);
            throw e;
        } finally {
            close(insertStmt);
        }
    }

    public static Map<String, Object> getProcessingState(Connection con, String planId) throws Exception {
        String tableName = getProcessingStateTableName(planId);
        Statement getStmt = null;
        ResultSet getRs = null;
        try {
            StringBuffer sb = new StringBuffer();
            sb.append("SELECT " + COL_PROCESSING_STATE + " FROM ");
            sb.append(tableName);
            getStmt = con.createStatement();
            getRs = getStmt.executeQuery(sb.toString());
            if (getRs.next()) {
                Blob b = getRs.getBlob(1);
                // note that: Blob's first byte starts at position 1
                byte[] bytes = b.getBytes(1, (int) b.length());
                String processingState = new String(bytes, "UTF-8");
                return unmarshallProcessingState(processingState);
            }
            return null;
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_retrieve_processing_state_of_event_processor", planId, e);
            throw e;
        } finally {
            close(getRs);
            close(getStmt);
        }
    }

    public static void updateProcessingState(Connection con, String planId, Map<String, Object> processingState) throws Exception {
        String tableName = getProcessingStateTableName(planId);
        PreparedStatement updateStmt = null;
        String s = ((Properties)processingState.get(PS_PLAN_STATE)).getProperty(PS_PREV_TIMESTAMP_TO_CHECK);
        Timestamp tsC = new Timestamp(Long.parseLong(s));
        s = ((Properties)processingState.get(PS_PLAN_STATE)).getProperty(PS_PREV_TIMESTAMP_TO_PROCESS);
        Timestamp tsP = new Timestamp(Long.parseLong(s));
        try {
            String state = marshallProcessingState(processingState);
            StringBuffer sb = new StringBuffer();
            sb.append("UPDATE ");
            sb.append(tableName);
            sb.append(" SET " + COL_PROCESSING_STATE + " = ?, "+ COL_PREV_TIMESTAMP_TO_CHECK 
                    + " = ?, " + COL_PREV_TIMESTAMP_TO_PROCESS + " = ? ");
            updateStmt = con.prepareStatement(sb.toString());
            updateStmt.setObject(1, state.getBytes("UTF-8"));
            updateStmt.setTimestamp(2, tsC);
            updateStmt.setTimestamp(3, tsP);
            updateStmt.executeUpdate();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_update_processing_state_of_event_processor", planId, e);
            throw e;
        } finally {
            close(updateStmt);
        }
    }

    // processingState:
    //  PS_PREV_TIMESTAMP_TO_PROCESS (String) -> 1291029109 (String)
    //  PS_PS_OPERATOR_STATE (String) -> Map
    //                                      opId -> Properties
    //                                                   prop -> value
    //
    // <processingState>
    //      <plan>
    //          <propNode name="" value=""/>
    //      <operator id = ""> *
    //          <propNode name="" value=""/>
    //
    public static String marshallProcessingState(Map<String, Object> processingState) {
        String ret = null;
        try {
            Document doc = XmlUtil.createDocument(true);
            Element rootNode = doc.createElement("processing-state");
            doc.appendChild(rootNode);

            // Plan section
            Element planNode = doc.createElement("plan");
            rootNode.appendChild(planNode);

            Properties planState = (Properties) processingState.get(PS_PLAN_STATE);
            Iterator planPropIt = planState.entrySet().iterator();
            while (planPropIt.hasNext()) {
                Map.Entry e = (Map.Entry) planPropIt.next();
                Element propNode = doc.createElement("property");
                propNode.setAttribute("name", e.getKey().toString());
                propNode.setAttribute("value", e.getValue().toString());
                planNode.appendChild(propNode);
            }

            // Operator section
            Map operatorState = (Map) processingState.get(PS_OPERATOR_STATE);
            Iterator operatorIt = operatorState.entrySet().iterator();
            while (operatorIt.hasNext()) {
                Map.Entry opEntry = (Map.Entry) operatorIt.next();
                // <operator>
                Element opNode = doc.createElement("operator");
                opNode.setAttribute("name", opEntry.getKey().toString());
                rootNode.appendChild(opNode);

                Properties prop = (Properties) opEntry.getValue();
                Iterator opPropIt = prop.entrySet().iterator();
                while (opPropIt.hasNext()) {
                    Map.Entry e = (Map.Entry) opPropIt.next();
                    Element propNode = doc.createElement("property");
                    propNode.setAttribute("name", e.getKey().toString());
                    propNode.setAttribute("value", e.getValue().toString());
                    opNode.appendChild(propNode);
                }
            }
            boolean omitXMLDeclaration = false;
            ret = XmlUtil.toXml(doc, "UTF-8", omitXMLDeclaration);
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_marshall_the_processing_state_of_event_processor", e);
        }
        return ret;
    }

    public static Map<String, Object> unmarshallProcessingState(String processingState) {
        Map<String, Object> ret = new HashMap<String, Object>();
        try {
            boolean namespaceAware = false;
            Document doc = XmlUtil.createDocumentFromXML(namespaceAware, processingState);
            Element rootNode = doc.getDocumentElement();
            // plan section
            Properties planState = new Properties();
            ret.put(PS_PLAN_STATE, planState);
            Node planNode = rootNode.getElementsByTagName("plan").item(0);
            NodeList planPropNodeList = planNode.getChildNodes();
            if (planPropNodeList != null) {
                for (int i = 0, iMax = planPropNodeList.getLength(); i < iMax; i++) {
                    Node node = planPropNodeList.item(i);
                    if (node.getNodeType() != Node.ELEMENT_NODE || !node.getNodeName().equalsIgnoreCase("property")) {
                        continue;
                    }
                    NamedNodeMap attribs = node.getAttributes();
                    if (attribs == null) {
                        continue;
                    }
                    try {
                        String key = attribs.getNamedItem("name").getNodeValue();
                        String value = attribs.getNamedItem("value").getNodeValue();
                        planState.setProperty(key, value);
                    } catch (Exception e) {
                        mMessages.log(Level.SEVERE, "Util.Fail_to_unmarshall_the_processing_state_of_event_processor", e);
                        continue;
                    }
                }
            }

            // operator section
            Map<String, Object> operatorState = new HashMap<String, Object>();
            ret.put(PS_OPERATOR_STATE, operatorState);
            NodeList opNodeList = rootNode.getElementsByTagName("operator");
            if (opNodeList != null) {
                for (int i = 0, iMax = opNodeList.getLength(); i < iMax; i++) {
                    Properties opProp = new Properties();
                    Element opNode = (Element) opNodeList.item(i);
                    NamedNodeMap opAttribs = opNode.getAttributes();
                    if (opAttribs == null) {
                        continue;
                    }
                    try {
                        String name = opAttribs.getNamedItem("name").getNodeValue();
                        operatorState.put(name, opProp);
                    } catch (Exception e) {
                        mMessages.log(Level.SEVERE, "Util.Fail_to_unmarshall_the_processing_state_of_event_processor", e);
                        continue;
                    }

                    NodeList opPropNodeList = opNode.getElementsByTagName("property");
                    if (opPropNodeList == null) {
                        continue;
                    }
                    for (int j = 0, jMax = opPropNodeList.getLength(); j < jMax; j++) {
                        Node node = opPropNodeList.item(j);
                        NamedNodeMap attribs = node.getAttributes();
                        if (attribs == null) {
                            continue;
                        }
                        try {
                            String key = attribs.getNamedItem("name").getNodeValue();
                            String value = attribs.getNamedItem("value").getNodeValue();
                            opProp.setProperty(key, value);
                        } catch (Exception e) {
                            mMessages.log(Level.SEVERE, "Util.Fail_to_unmarshall_the_processing_state_of_event_processor", e);
                            continue;
                        }
                    }
                }
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_unmarshall_the_processing_state_of_event_processor", e);
        }
        return ret;
    }

    public static Timestamp getPrevTimestampToCheck(Connection con, String planId) {
        Timestamp ts = null;
        String tableName = getProcessingStateTableName(planId);
        PreparedStatement getStmt = null;
        ResultSet getRs = null;
        try {
            StringBuffer sb = new StringBuffer();
            sb.append("SELECT " + COL_PREV_TIMESTAMP_TO_CHECK +" FROM ");
            sb.append(tableName);
            getStmt = con.prepareStatement(sb.toString());
            getRs =  getStmt.executeQuery();
            if (getRs.next()) {
                ts = getRs.getTimestamp(1);
            }
            //return null;
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_retrieve_processing_state_of_event_processor", planId, e);
        } finally {
            close(getRs);
            close(getStmt);
        }
        return ts;
    }

    public static Timestamp getPrevTimestampToProcess(Connection con, String planId) {
        String tableName = getProcessingStateTableName(planId);
        Statement getStmt = null;
        ResultSet getRs = null;
        try {
            StringBuffer sb = new StringBuffer();
            sb.append("SELECT " + COL_PREV_TIMESTAMP_TO_PROCESS +" FROM ");
            sb.append(tableName);
            getStmt = con.createStatement();
            getRs = getStmt.executeQuery(sb.toString());
            if (getRs.next()) {
                return getRs.getTimestamp(1);
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_retrieve_processing_state_of_event_processor", planId, e);
        } finally {
            close(getRs);
            close(getStmt);
        }
        return null;
    }

    private static final String Q_CURRENT_TIME = "SELECT CURRENT_TIMESTAMP FROM " + TABLE_EMS_PLAN ;
    public static String currentDBTimestampQueryString() throws Exception {
        return Q_CURRENT_TIME;
        
    }

    public static Timestamp nextTimestampToCheck(Connection con, QueryPlan plan) throws Exception {
        List<String> dependencyIdList = plan.getDependencyIdList();
        if (dependencyIdList.isEmpty()) {
            return nextTimestampToCheck(con, plan.getId());
        }
        Timestamp ret = nextTimestampToCheck(con, plan.getId());
        for (String planId : dependencyIdList) {
            Timestamp prevCheckTs = getPrevTimestampToCheck(con, planId);
            if (prevCheckTs == null) {
                return new Timestamp(0);
            }
            if (ret.after(prevCheckTs)) {
                ret = prevCheckTs;
            }
        }
        return ret;
    }

    private static Timestamp nextTimestampToCheck(Connection con, String planId) throws Exception {
        String tableName = getProcessingStateTableName(planId);
        Timestamp timestamp = null;
        Statement getStmt = null;
        ResultSet getRs = null;
        try {
            StringBuffer sb = new StringBuffer();
            sb.append("SELECT CURRENT_TIMESTAMP FROM ");
            sb.append(tableName);
            getStmt = con.createStatement();
            getRs = getStmt.executeQuery(sb.toString());
            getRs.next();
            timestamp = getRs.getTimestamp(1);
            timestamp = new Timestamp(timestamp.getTime() - 2000); //process records posted two seconds ago
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_retrieve_new_timstamp_to_process_of_event_processor", planId), e);
        } finally {
            close(getRs);
            close(getStmt);
        }
        return timestamp;
    }


    //======================== TABLE_EMS_TABLE_USAGE =======================================================
    public static String getTableUsageTableName(String planId) {
        return TABLE_EMS_TABLE_USAGE + "_" + planId;
    }

    public static void createTableUsageTable(Connection con, String schemaName, String planId) throws Exception {
        String tableName = getTableUsageTableName(planId);
        Schema schema = new Schema(tableName);
        List<String> columnList = new ArrayList<String>();
        columnList.add(COL_TABLE_NAME);
        columnList.add(SQL_TYPE_VARCHAR);
        columnList.add("200");
        columnList.add("");

        columnList.add(COL_USER_ID);
        columnList.add(SQL_TYPE_VARCHAR);
        columnList.add("200");
        columnList.add("");

        columnList.add(COL_TIMESTAMP);
        columnList.add(SQL_TYPE_TIMESTAMP);
        columnList.add("");
        columnList.add("");
        schema.setColumnMetadataAsList(columnList);
        int status = getDbSpecial().checkTableStatus(con, schemaName, tableName, schema, new HashSet());
        switch (status) {
            case TS_UNKNOWN:
            case TS_NAME_NOT_EXIST:
                getDbSpecial().createTable(con, tableName, schema);
                break;
            case TS_NAME_EXIST_BUT_DIFFERENT_SCHEMA:
                throw new Exception(mMessages.getString("Util.Table_already_exists_but_with_different_schema", tableName));
        }
    }

    public static void dropTableUsageTable(Connection con, String planId) throws Exception {
        String tableName = getTableUsageTableName(planId);
        dropTable(con, tableName);
    }

    public static void initializeTableUsage(Connection con, String planId, String table, String user) throws Exception {
        String tableName = getTableUsageTableName(planId);
        PreparedStatement stmt = null;
        try {
            Timestamp timestamp = getTableUsage(con, planId, table, user);
            if (timestamp == null) {
                StringBuffer sb = new StringBuffer();
                sb.append("INSERT INTO ");
                sb.append(tableName);
                sb.append(" VALUES (?, ?, ?)");
                stmt = con.prepareStatement(sb.toString());
                stmt.setString(1, table);
                stmt.setString(2, user);
                stmt.setTimestamp(3, new Timestamp(0)); // time origin
                stmt.executeUpdate();
            }
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_initialize_the_usage_of_table_by_event_processor", new Object[]{table, planId}), e);
        } finally {
            close(stmt);
        }
    }

    private static Timestamp getTableUsage(Connection con, String planId, String table, String user) throws Exception {
        String tableName = getTableUsageTableName(planId);
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            StringBuffer sb = new StringBuffer();
            sb.append("SELECT " + COL_TIMESTAMP + " FROM ");
            sb.append(tableName);
            sb.append(" WHERE " + COL_TABLE_NAME + " = ? AND " + COL_USER_ID + " = ?");
            stmt = con.prepareStatement(sb.toString());
            stmt.setString(1, table);
            stmt.setString(2, user);
            rs = stmt.executeQuery();
            if (rs.next()) {
                return rs.getTimestamp(1);
            }
            return null;
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_retrieve_the_usage_of_table_by_user", new Object[]{table, user}), e);
        } finally {
            close(rs);
            close(stmt);
        }
    }

    public static Timestamp getTableUsage(Connection con, String planId, String table) throws Exception {
        String tableName = getTableUsageTableName(planId);
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            StringBuffer sb = new StringBuffer();
            sb.append("SELECT MIN( " + COL_TIMESTAMP + ") FROM ");
            sb.append(tableName);
            sb.append(" WHERE " + COL_TABLE_NAME + " = ? ");
            stmt = con.prepareStatement(sb.toString());
            stmt.setString(1, table);
            rs = stmt.executeQuery();
            if (rs.next()) {
                return rs.getTimestamp(1);
            }
            return null;
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_retrieve_the_minimum_usage_of_table", table), e);
        } finally {
            close(rs);
            close(stmt);
        }
    }

    public static void updateTableUsage(Connection con, String planId, String table, String user, Timestamp timestamp) throws Exception {
        PreparedStatement stmt = null;
        try {
            stmt = createUpdateTableUsageStmt(con, planId, table, user);
            stmt.setTimestamp(1, timestamp);
            stmt.executeUpdate();
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_update_the_usage_of_table_by_user", new Object[]{table, user}), e);
        } finally {
            close(stmt);
        }
    }

    public static PreparedStatement createUpdateTableUsageStmt(Connection con, String planId, String table, String userId) throws Exception {
        // UPDATE tableName SET COL_TIMESTAMP  = ? WHERE COL_TABLE_NAME = R1 AND COL_USER_ID = mId;
        String tableName = getTableUsageTableName(planId);
        StringBuffer sb = new StringBuffer();
        sb.append("UPDATE ");
        sb.append(tableName);
        sb.append(" SET " + COL_TIMESTAMP + " = ? WHERE " + COL_TABLE_NAME + " = '");
        sb.append(table);
        sb.append("' AND " + COL_USER_ID + " = '");
        sb.append(userId);
        sb.append("'");
        if (mMessages.isLoggable(Level.FINE)) {
            mMessages.log(Level.FINE, "Util.UpdateTableUsageStmt", sb.toString());
        }    
        return con.prepareStatement(sb.toString());
    }

    public static void deleteTableUsage(Connection con, String planId, String table, String user) throws Exception {
        String tableName = getTableUsageTableName(planId);
        PreparedStatement stmt = null;
        try {
            StringBuffer sb = new StringBuffer();
            sb.append("DELETE FROM ");

            sb.append(tableName);
            sb.append(" WHERE " + COL_TABLE_NAME + " = ? AND " + COL_USER_ID + " = ?");
            stmt = con.prepareStatement(sb.toString());
            stmt.setString(1, table);
            stmt.setString(2, user);
            stmt.executeUpdate();
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_delete_the_usage_of_table_by_user", new Object[]{table, user}), e);
        } finally {
            close(stmt);
        }
    }

    //========================End of TABLE_EMS_TABLE_USAGE =======================================================

    public static boolean hasRowsByTimestamp(Connection con, String tableName, Timestamp timestamp) throws Exception {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = createHasRowsBetweenTimestampStmt(con, tableName);
            //stmt.setFetchSize(1);
            stmt.setMaxRows(1);
            stmt.setTimestamp(1, new Timestamp(0));
            stmt.setTimestamp(2, timestamp);
            rs = stmt.executeQuery();
            return rs.next();
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_check_existence_of_rows_from_table_where", new Object[]{tableName, COL_TIMESTAMP + " < " + mSDF.format(timestamp)}), e);
        } finally {
            close(rs);
            close(stmt);
        }
    }

    public static PreparedStatement createHasRowsBetweenTimestampStmt(Connection con, String tableName) throws Exception {
        StringBuffer sql = new StringBuffer();
        sql.append("SELECT 'x' FROM ");
        sql.append(tableName);
        sql.append(" WHERE ");
        sql.append(" ? < ");
        sql.append(COL_TIMESTAMP);
        sql.append(" AND ");
        sql.append(COL_TIMESTAMP);
        sql.append(" <= ?");
        return con.prepareStatement(sql.toString());
    }
    public static PreparedStatement createModHasRowsAfterTimestampStmt(Connection con, String tableName) throws Exception {
        StringBuffer sql = new StringBuffer();
        sql.append("SELECT 'x' FROM ");
        sql.append(tableName);
        sql.append(" WHERE ");
        sql.append(" ? < ");
        sql.append(COL_TIMESTAMP);
        //sql.append(" ");
        
        return con.prepareStatement(sql.toString());
    }

    private static int getColumnIndex(ResultSetMetaData rmd, String columnName) throws Exception {
        for (int i = 1, I = rmd.getColumnCount(); i <= I; i++) {
            if (columnName.equalsIgnoreCase(rmd.getColumnName(i))) {
                return i;
            }
        }
        return -1;
    }

    public static int getRowCount(Connection con, String tableName, Timestamp timestamp) throws Exception {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        try {
            stmt = con.prepareStatement("SELECT COUNT(*) FROM " + tableName + " WHERE " + COL_TIMESTAMP + " <= ?");
            stmt.setTimestamp(1, timestamp);
            rs = stmt.executeQuery();
            if (rs.next()) {
                return rs.getInt(1);
            }
            return 0;
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_retrieve_row_count_from_table_where", new Object[]{tableName, COL_TIMESTAMP + " < " + mSDF.format(timestamp)}, e);
            throw e;
        } finally {
            close(rs);
            close(stmt);
        }
    }


    // map: partition -> (a list of rows ordered by timestamp)
    // note that timestamp can be a partition column
    public static Map<RowSection, List<Object[]>> getPartitionedRows(Connection con, String tableName, List<String> columnList, Timestamp timestamp) throws Exception {
        Map<RowSection, List<Object[]>> map = new HashMap<RowSection, List<Object[]>>();
        PreparedStatement stmt = null;
        Statement riStmt = null;
        ResultSet rs = null;
        ResultSet riRs = null;
        try {
            ResultSetMetaData metaData = null;
            StringBuffer sql = new StringBuffer();
            sql.append("SELECT * FROM ");
            sql.append(tableName);
            if (timestamp != null) {
                sql.append(" WHERE ");
                sql.append(COL_TIMESTAMP);
                sql.append(" <= ? ORDER BY ");
                sql.append(COL_TIMESTAMP);
            } else {
                sql.append(" ORDER BY ");
                sql.append(COL_TIMESTAMP);
            }
            stmt = con.prepareStatement(sql.toString());
            if (timestamp != null) {
                stmt.setTimestamp(1, timestamp);
            }
            rs = stmt.executeQuery();
            metaData = rs.getMetaData();
            int colCnt = metaData.getColumnCount();
            int tsIdx = getColumnIndex(metaData, COL_TIMESTAMP) - 1;
            int partCols = columnList.size();
            int[] colIdx = new int[partCols];
            for (int i = 0; i < partCols; i++) {
                colIdx[i] = getColumnIndex(metaData, columnList.get(i)) - 1;
            }
            Arrays.sort(colIdx); // sort colIdx into asscending order
            while (rs.next()) {
                Object[] part = new Object[partCols];
                Object[] row = new Object[colCnt];
                for (int i = 0, j = 0; i < colCnt; i++) {
                    row[i] = i == tsIdx ? rs.getTimestamp(i + 1) : rs.getObject(i + 1);
                    if (i == colIdx[j]) {
                        part[j] = row[i];
                        if (j < partCols - 1) {
                            j++;
                        }
                    }
                }
                RowSection partition = new RowSection(part);
                List<Object[]> list = map.get(partition);
                if (list == null) {
                    list = new ArrayList<Object[]>();
                    map.put(partition, list);
                }
                list.add(row);
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_retrieve_from_table_where", new Object[]{tableName, COL_TIMESTAMP + " < " + mSDF.format(timestamp)}, e);
            throw e;
        } finally {
            close(rs);
            close(riRs);
            close(stmt);
            close(riStmt);
        }
        return map;
    }


    // map: partition -> (a list of rows ordered by timestamp)
    // note that timestamp can be a partition column
    public static Map getPartitionedRows(Connection con, String tableName, List<String> columnList) throws Exception {
        Timestamp timestamp = null;
        return getPartitionedRows(con, tableName, columnList, timestamp);
    }

    public static void insertRows(Connection con, String tableName, List<Object[]> rowList) throws Exception {
        ArrayList<Object> tailColumns = new ArrayList<Object>();
        insertRowsPlusTailColumns(con, tableName, rowList, tailColumns);
    }


    public static void insertRowsPlusTailColumns(Connection con, String tableName, List<Object[]> rowList, List<Object> tailColumns) throws Exception {
        insertRowsPlusTailColumns(con, tableName, null, rowList, null, tailColumns);
    }

    public static void insertRowsPlusTailColumns(Connection con, String tableName, List<String> columnNameList, List<Object[]> rowList, List<String> tailColumnNameList, List<Object> tailColumns) throws Exception {
        PreparedStatement stmt = null;
        try {
            if (rowList.size() == 0) {
                return;
            }
            Object[] row = rowList.get(0);
            int colCnt = row.length;
            StringBuffer sql = new StringBuffer("INSERT INTO ");
            sql.append(tableName);
            if (columnNameList != null) {
                sql.append("(");
                for (int i = 0, I = columnNameList.size(); i < I; i++) {
                    sql.append(columnNameList.get(i));
                    sql.append(",");
                }
                for (int i = 0, I = tailColumnNameList.size(); i < I; i++) {
                    sql.append(tailColumnNameList.get(i));
                    sql.append(",");
                }
                sql.setCharAt(sql.length() - 1, ')'); // replace last ',' with ')'
            }
            sql.append(" VALUES(");
            for (int i = 0; i < colCnt; i++) {
                sql.append("?,");
            }
            for (int i = 0, I = tailColumns.size(); i < I; i++) {
                sql.append("?,");
            }
            sql.setCharAt(sql.length() - 1, ')'); // replace last ',' with ')'
            stmt = con.prepareStatement(sql.toString());
            for (int i = 0, I = rowList.size(); i < I; i++) {
                row = (Object[]) rowList.get(i);
                for (int j = 0; j < colCnt; j++) {
                    stmt.setObject(j + 1, row[j]);
                }
                for (int j = 0, J = tailColumns.size(); j < J; j++) {
                    stmt.setObject(colCnt + j + 1, tailColumns.get(j));
                }
                stmt.addBatch();
            }
            stmt.executeBatch();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Fail to insert rows into table " + tableName, e);
            throw e;
        } finally {
            close(stmt);
        }
    }

    public static void deleteRowBySeqId(Connection con, String tableName, Object seqId) throws Exception {
        PreparedStatement stmt = null;
        try {
            stmt = con.prepareStatement("DELETE FROM " + tableName + " WHERE " + COL_SEQID + " = ?");
            stmt.setObject(1, seqId);
            stmt.executeUpdate();
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Fail_to_delete_from_table_where", new Object[]{tableName, COL_SEQID + " = " + seqId}, e);
            throw e;
        } finally {
            close(stmt);
        }
    }

    public static PreparedStatement createCleanStreamByMinUsageTimeStmt(Connection con, String planId, String tableName) throws Exception {
        // DELETE FROM S1
        // WHERE Timestamp <= (SELECT Timestamp FROM TABLE_EMS_TABLE_USAGE
        //                     WHERE COL_TABLE_NAME = S1)
        StringBuffer sb = new StringBuffer();
        sb.append("DELETE FROM ");
        sb.append(tableName);
        sb.append(" WHERE " + COL_TIMESTAMP + " <= (SELECT MIN(" + COL_TIMESTAMP + ") FROM ");
        sb.append(getTableUsageTableName(planId));
        sb.append(" WHERE " + COL_TABLE_NAME + " = '" + tableName + "')");
        return con.prepareStatement(sb.toString());
    }

    public static void close(Statement stmt) {
        try {
            if (stmt != null) {
                stmt.close();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_close_statement", e);
        }
    }

    public static void close(Connection con) {
        try {
            if (con != null) {
                con.close();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_close_connection", e);
        }
    }

    public static void close(ResultSet rs) {
        try {
            if (rs != null) {
                rs.close();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_close_resultset", e);
        }
    }

    public static void rollback(Connection con) {
        try {
            if (con != null) {
                con.rollback();
            }
        } catch (Exception e) {
            mMessages.log(Level.SEVERE, "Util.Fail_to_rollback_transaction_on_connection", e);
        }
    }

    // in oracle: database object names cannot be longer than 30 letters and must begin with a letter.
    public static String getQueueName(Properties configProp, String instanceId, String opId) {
        QueryPlan plan = getPlanByInstanceId(configProp, instanceId);
        return plan.getOperatorById(opId).getQueueName();
    }

    public static String getSequenceName(String planId, String opId) {
        return "seq_" + planId + "_" + opId;
    }

    /**
     * get GUID
     *
     * @return String GUID
     */
    public static String getGUID() {
        UID uid = new UID();
        StringBuffer stringbuffer = new StringBuffer();
        stringbuffer.append(mLocalIPAddress);
        stringbuffer.append(":"); // NO I18N
        stringbuffer.append(uid.toString());
        String guid = stringbuffer.toString();

        return guid;
    }
    
    private static final String UPDATE_ENGINE_LEASE_EXPIRATION = "UPDATE " + TABLE_EMS_ENGINE + " SET " 
            + COL_LEASE_EXPIRATION + " = ? WHERE " + COL_ID + " = ?";
    public static void updateEngineLeaseExpiration(Connection con, String engineId, Timestamp expiration) throws Exception {
        PreparedStatement updateStmt = null;
        try {
            updateStmt = con.prepareStatement(UPDATE_ENGINE_LEASE_EXPIRATION);
            updateStmt.setTimestamp(1, expiration);
            updateStmt.setString(2, engineId);
            updateStmt.executeUpdate();
        } catch (Exception e) {
            throw new Exception(mMessages.getString("Util.Fail_to_update_lease_expiration_for_engine", engineId), e);
        } finally {
            close(updateStmt);
        }
    }
    
    
        
    public static TimeUnit getPollingTimeUnit(String pollingIntervalTimeUnit) {
        return mTimeUnitNameToValueMap.get(pollingIntervalTimeUnit);
    }
    
    public static long getAdjustedPollingIntervalInSeconds(int pollingInterval, String pollingIntervalTimeUnit) {
        long pIntervalInSeconds = pollingInterval;
        if(ExternalTablePollingStream.INTERVAL_SECOND.equals(pollingIntervalTimeUnit)) {
            pIntervalInSeconds = pollingInterval;
        }if(ExternalTablePollingStream.INTERVAL_MINUTE.equals(pollingIntervalTimeUnit)) {
            pIntervalInSeconds = pollingInterval * 60;
        } else if(ExternalTablePollingStream.INTERVAL_HOUR.equals(pollingIntervalTimeUnit)) {
            pIntervalInSeconds = pollingInterval * 60 * 60;
        } else if(ExternalTablePollingStream.INTERVAL_DAY.equals(pollingIntervalTimeUnit)) {
            pIntervalInSeconds = pollingInterval * 60 * 60 * 24;
        } else if(ExternalTablePollingStream.INTERVAL_WEEK.equals(pollingIntervalTimeUnit)) {
            pIntervalInSeconds = pollingInterval * 60 * 60 * 24 * 7;
        } 
        
        return pIntervalInSeconds;
        
    }
    
    //for oracle rs.getObject does not written
    //an object which extends java.sql.Timestamp
    //so we need to explicitly call rs.getTimestam
    //so it is better to call type specific api
    public static Object getTypeSpecificValue(ResultSet rs, int columnIndex) throws SQLException {
        Object val = null;
        
        int type = rs.getMetaData().getColumnType(columnIndex);
        switch(type) {
            case Types.TIMESTAMP:
                val = rs.getTimestamp(columnIndex);
                break;
            case Types.TIME:
                val = rs.getTime(columnIndex);
                break;
            case Types.DATE:
                val = rs.getDate(columnIndex);
                break;
            default:
                val = rs.getObject(columnIndex);
        }
         
    
        
        return val;
    }
}
