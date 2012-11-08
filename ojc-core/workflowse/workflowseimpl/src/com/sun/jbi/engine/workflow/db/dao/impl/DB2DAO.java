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
 * @(#)DB2DAO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.dao.impl;

import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Logger;

import javax.sql.DataSource;
import javax.sql.XADataSource;

import com.sun.jbi.engine.workflow.db.connection.ConnectionProperties;
import com.sun.jbi.engine.workflow.db.dao.DAO;
import com.sun.jbi.engine.workflow.db.dao.DAOException;
import com.sun.jbi.engine.workflow.db.dao.DBType;
import com.sun.jbi.engine.workflow.util.I18n;


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
 // QA 59436
public class DB2DAO extends DAO {


    private static Logger LOGGER = Logger.getLogger(DB2DAO.class.getName());
    
    private static final String DB2_CLIENT_DS = "com.ibm.db2.jcc.DB2DataSource";

    private static final String DB2_CLIENTXA_DS = "com.ibm.db2.jcc.DB2XADataSource";
    
    /** MAX_VARCHAR_LENGTH for DB2 Data Type */
    protected static final int MAX_VARCHAR_LENGTH_DERBY = 4000;
 
   
    /**
     * Constructor. 
     *
     * @param connProp The db connection properties.
     *
     * @exception DAOException Problem with initializing database.
     */
    public DB2DAO() throws DAOException, Exception {
        super();
    }

    /**
     * gets the db type pertaining to the connection
     */
    public DBType getDBType() {
        return DAO.DB2_TYPE;
    }

    /**
     * This method takes the SQLException error code and finds out
     * if the errorcode is a unique constraint error code
     *
     * @param errorCode DOCUMENT ME!
     *
     * @return true if errocode is unique constraint violation, false otherwise
     */
    public boolean isErrorCodeUnique(int errorCode) {
        if (errorCode == 1) {
            return true;
        }
        return false;
    }

   /**
     * Set the various functions syntax for the specific Database
     *
     */
    public void setDBFunctions() {
        // determine the database date/time and sql functions
        timeFunction = " VALUES CURRENT TIME ";
        dateFunction = " VALUES CURRENT DATE ";
        rowLockFunction = "";
        noLockFunction = "";
    }

    /**
     * Gets the maximum varchar length size based on the database type.
     *
     * @return The maximum varchar length size.
     */
    public int getMaxVarcharLength() {
        return MAX_VARCHAR_LENGTH_DERBY;
    }

    

    /**
     * This gets a String field stored in a BLOB column in the database
     *
     * @param columnName the column name
     * @param cs the CachedRowSet
     *
     * @return the extracted String
     */
    public byte[] getBlob(String columnName, ResultSet rs) {
		byte[] byteArray = null;
		try {
			byteArray = rs.getBytes(columnName);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return byteArray;
    }



    
    /**
     * This gets a String field stored in a CLOB column in the database
     *
     * @param columnName the column name
     * @param cs the CachedRowSet
     *
     * @return the extracted String
     */
    public String getClob(String columnName, ResultSet rs) {
    char[] cbuf = new char[8192];
      StringBuffer buf = new StringBuffer();

      try {
          InputStreamReader isr = new InputStreamReader(
                                   rs.getAsciiStream(columnName));
          int count = -1;
          while ((count = isr.read(cbuf)) != -1) {
              buf.append(new String(cbuf, 0, count));
          }
          isr.close();
          isr = null;
      } catch (Exception e) {
          e.printStackTrace();
      }
      return buf.toString();
  }



 
 // executeUpdate 
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
            //sqe.printStackTrace();
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
     * Fill in the preparded statement params
     *
     * @param ps the preparded statement to add the bind variables to
     * @param columns the values to use for the colmns
     * @param columnTypes the type to use when the columns are null NOTE - use the java.sql.Types
     *        value woth the statement position as key
     *
     * @return whether the columnTypes have any lob fields
     * @throws SQLException SQLException
     */
    public void fillInParms(PreparedStatement ps, List columns, List columnTypes)
                        throws SQLException {
        int index;
      
        // Add each of the colmns in the correct type
        for (index = 0; index < columns.size(); index++) {
            Object obj = columns.get(index);
  
   
       
            // Id parameter is null then try to look up column type to set null
            if ((obj == null) && (columnTypes.size() > index)) {
                int sqlType = ((Integer) columnTypes.get(index)).intValue();
                if (sqlType != java.sql.Types.CLOB && sqlType != java.sql.Types.BLOB) {
                    ps.setNull(index + 1, sqlType);
                }
            } else if (obj instanceof Integer) {
                ps.setInt(index + 1, ((Integer) obj).intValue());
            } else if (obj instanceof String) {
        
 /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////               
                 int colType = ((Integer) columnTypes.get(index)).intValue();
                 
                 if (colType == java.sql.Types.CLOB ) {
                     // DB2 will insert here now
                     ps.setBytes(index + 1, ((String) obj).getBytes());
                 } else if (colType == java.sql.Types.BLOB ) {
                     ps.setBytes(index + 1, ((String) obj).getBytes());
                 } else {
                     ps.setString(index + 1, (String) obj);
                 }
 /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////               
                
            } else if (obj instanceof java.sql.Timestamp) {
                ps.setTimestamp(index + 1, (java.sql.Timestamp) obj);
            } else if (obj instanceof java.sql.Date || obj instanceof java.util.Date) {
                ps.setDate(index + 1, (java.sql.Date) obj);
            } else if (obj instanceof Long) {
                ps.setLong(index + 1, ((Long) obj).longValue());
			} else if (obj instanceof Float) {
      			ps.setFloat(index + 1, ((Float) obj).floatValue());
			} else if (obj instanceof Short) {
      			ps.setShort(index + 1, ((Short) obj).shortValue());
			} else if (obj instanceof BigDecimal) {
      			ps.setBigDecimal(index + 1, ((BigDecimal) obj));				
            } else if (obj instanceof Double) {
                        ps.setDouble(index + 1, ((Double) obj).doubleValue());
			} else if (obj instanceof Byte) {
      			ps.setByte(index + 1, ((Byte) obj).byteValue());
			} else if (obj instanceof byte[] ) {
                        // some null values on LOB's are on need of this setting  QA 59436 
                        ps.setBytes(index + 1, (byte []) obj);
            } else if (obj instanceof Boolean) {
                ps.setInt(index + 1, (((Boolean) obj).booleanValue()) ? 1 : 0);
            } else {
            	
                if (obj == null) {
                    // do nothing
                } else if ((obj instanceof ArrayList) && (((Integer) columnTypes.get(index)).intValue() == java.sql.Types.OTHER)){
                    // This is a contained objects
                    // would be handled after the parent record
                } else {
                    throw new SQLException("Cannot handle type " + obj.getClass().getName());
                }
            }
        }
        
    }
    /**
     * Creates a client data source and sets all the necessary properties in order to connect to
     * Derby Network Server The server is assumed to be running on 1527 and on the localhost
     * @param database
     *            database name; can include Derby URL attributes
     * @param user
     *            database user
     * @param password
     * 
     * @param 
     * @return returns DataSource
     * @throws Exception
     *             if there is any error
     */
    private static javax.sql.DataSource getClientDataSource(String jdbcDataSource, String database,
            String user, String password, String serverName, int port) throws SQLException,
            ClassNotFoundException, InstantiationException, IllegalAccessException,
            NoSuchMethodException, InvocationTargetException {
        Class nsDataSource = Class.forName(jdbcDataSource);
        DataSource ds = (DataSource) nsDataSource.newInstance();

        // can also include Derby URL attributes along with the database name
        Class[] methodParams = new Class[] { String.class };
        Method dbname = nsDataSource.getMethod("setDatabaseName", methodParams);
        if (database.indexOf("create=true") == -1) {
            database = database + ";create=true";
        }
        Object[] args = new Object[] { database };
        dbname.invoke(ds, args);

        if (user != null) {
            Method setuser = nsDataSource.getMethod("setUser", methodParams);
            args = new Object[] { user };
            setuser.invoke(ds, args);
        }
        if (password != null) {
            Method setpw = nsDataSource.getMethod("setPassword", methodParams);
            args = new Object[] { password };
            setpw.invoke(ds, args);
        }
        // host on which network server is running
        Method servername = nsDataSource.getMethod("setServerName", methodParams);
        args = new Object[] { serverName };
        servername.invoke(ds, args);

        // port on which Network Server is listening
        methodParams = new Class[] { int.class };
        Method portnumber = nsDataSource.getMethod("setPortNumber", methodParams);
        args = new Object[] { new Integer(port) };
        portnumber.invoke(ds, args);

        return ds;

    }
    /**
     * Get XADataSource
     * 
     * @param connProp
     * @return
     * @throws SQLException
     */
    public static XADataSource getXADataSource(Properties connProp) throws SQLException {
        // TODO Auto-generated method stub
        XADataSource xds = null;
        try {

            xds = (XADataSource) getClientDataSource(DB2_CLIENTXA_DS, connProp
                    .getProperty(ConnectionProperties.DB_DATABASE_NAME), connProp
                    .getProperty(ConnectionProperties.DB_USERNAME), connProp
                    .getProperty(ConnectionProperties.DB_PASSWORD), connProp
                    .getProperty(ConnectionProperties.DB_SERVER_NAME), Integer.parseInt(connProp
                    .getProperty(ConnectionProperties.DB_PORT)));
        } catch (SQLException ex) {
            throw ex;
        } catch (Exception ex) {
            throw new RuntimeException(I18n.loc("WLM-6037: RuntimeException occurred"), ex);
        }
        return xds;

    }
    
    /**
     * Get DataSource
     * @param connProp
     * @return
     * @throws SQLException
     */
    public static DataSource getDataSource(Properties connProp) throws SQLException {
        // TODO Auto-generated method stub
        DataSource ds = null;
        try {

            ds = getClientDataSource(DB2_CLIENT_DS, connProp
                    .getProperty(ConnectionProperties.DB_DATABASE_NAME), connProp
                    .getProperty(ConnectionProperties.DB_USERNAME), connProp
                    .getProperty(ConnectionProperties.DB_PASSWORD), connProp
                    .getProperty(ConnectionProperties.DB_SERVER_NAME), Integer.parseInt(connProp
                    .getProperty(ConnectionProperties.DB_PORT)));
        } catch (SQLException ex) {
            throw ex;
        } catch (Exception ex) {
            throw new RuntimeException(I18n.loc("WLM-6037: RuntimeException occurred"), ex);
        }
        return ds;
    }


}// end of class
