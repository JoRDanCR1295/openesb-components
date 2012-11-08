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
 * @(#)SQLServerDAO.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.dao.impl;

import java.io.ByteArrayInputStream;
import java.io.InputStreamReader;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import java.util.ResourceBundle;

import javax.sql.DataSource;
import javax.sql.XADataSource;

import com.sun.jbi.engine.workflow.db.dao.DAO;
import com.sun.jbi.engine.workflow.db.dao.DAOException;
import com.sun.jbi.engine.workflow.db.dao.DBType;
/**
 * This class is the base database class which contains
 * methods that all inherited classes will need.
 *
 * @author SeeBeyond Technology Corporation
 * @version 
 *
 * @since eGate50
 */
public class SQLServerDAO extends DAO {


    /** MAX_VARCHAR_LENGTH for SQLServer Data Type */
    protected static final int MAX_VARCHAR_LENGTH_SQLSERVER = 255;

    /** SQLServer error code for unique constraint. */
    public static final int ERROR_CODE_UNIQUE_CONSTRAINT = 2627;

    /** DB TYPE */
    protected static final String SQLSERVER = "_SQLSERVER";
    ResourceBundle rb = ResourceBundle.getBundle( "com.stc.bpms.bpelImpl.persistence.adapter.Bundle");

    /**
     * Constructor.
     *
     * @param connProp The db connection properties.
     *
     * @exception DAOException Problem with initializing database.
     */
    public SQLServerDAO() throws DAOException, Exception {
         super();
    }

    /**
     * Gets the db type pertaining to the connection
     */
    public DBType getDBType() {
        return DAO.SQLSERVER_TYPE;
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
    protected void fillInParms(PreparedStatement ps, List columns,
                                  List columnTypes) throws SQLException {
        int index;

        // Add each of the colmns in the correct type
        for (index = 0; index < columns.size(); index++) {
            Object obj = columns.get(index);

            int colType = -999;
            try {
                colType = ((Integer)columnTypes.get(index)).intValue();
            } catch (Exception ex) {
                // does not have a value for colType.
                // set to -999
            }

            // Id parameter is null then try to look up column type to set null
            if ( (obj == null) && (columnTypes.size() > index)) {

                String data = "null";  // default data for SQLServer image/text.

                if (colType == java.sql.Types.CLOB) {
                    // CLOB type is text for SQLServer.
                    ps.setString(index + 1, data);
                } else if (colType == java.sql.Types.BLOB) {
                    // BLOB type is image for SQLServer.
                    ByteArrayInputStream bin = new ByteArrayInputStream(data.getBytes());
                    ps.setBinaryStream(index + 1, bin, bin.available());
                } else if (colType == java.sql.Types.LONGVARCHAR) {
                    // LONGVARCHAR type is text for SQLServer.
                     ps.setString(index + 1, data);

                } else {
                    ps.setNull(index + 1, colType);
                }

            } else if (obj instanceof Integer) {
                ps.setInt(index + 1, ( (Integer) obj).intValue());

            } else if (obj instanceof String) {
                String data = (String) obj;
                data.trim();
                if (data.length() == 0) {
                    // if data is empty
                    ps.setNull(index + 1, colType);

                } else if (colType == java.sql.Types.CLOB) {
                    ByteArrayInputStream bin = new ByteArrayInputStream(data.getBytes());
                    ps.setAsciiStream(index + 1, bin, bin.available());

                } else if (colType == java.sql.Types.BLOB) {
                     ByteArrayInputStream bin = new ByteArrayInputStream(data.getBytes());
                    ps.setBinaryStream(index + 1, bin, bin.available());

                } else if (colType == java.sql.Types.LONGVARCHAR) {
                    ps.setString(index + 1, data);

                } else if (colType == java.sql.Types.OTHER) {
                     // skip and do nothing
                     // OTHER type is ArrayList

                } else {
                     ps.setString(index + 1, (String) obj);
                }

            } else if (obj instanceof java.sql.Timestamp) {
                ps.setTimestamp(index + 1, (java.sql.Timestamp) obj);

            } else if (obj instanceof java.sql.Date ||
                     obj instanceof java.util.Date) {
                ps.setDate(index + 1, (java.sql.Date) obj);

            } else if (obj instanceof Long) {
                ps.setLong(index + 1, ( (Long) obj).longValue());

            } else if (obj instanceof Double) {
                ps.setDouble(index + 1, ( (Double) obj).doubleValue());

            } else if (obj instanceof Boolean) {
                ps.setInt(index + 1, ( ( (Boolean) obj).booleanValue()) ? 1 : 0);
            } else if (obj instanceof byte[]) {
                byte[] data = (byte[]) obj;
                if (data.length == 0) {
                    // if data is empty
                    ps.setNull(index + 1, colType);
                } else if (colType == java.sql.Types.CLOB) {
                    ByteArrayInputStream bin = new ByteArrayInputStream(data);
                    ps.setAsciiStream(index + 1, bin, bin.available());
                } else if (colType == java.sql.Types.BLOB) {
                     ByteArrayInputStream bin = new ByteArrayInputStream(data);
                    ps.setBinaryStream(index + 1, bin, bin.available());
                }

            } else {
                if (colType != -999) {
                    if (obj == null) {
                        // do nothing
                    } else if ( (obj instanceof ArrayList) &&
                             (colType == java.sql.Types.OTHER)) {
                        // This is a contained objects
                        // would be handled after the parent record
                    } else {
                        throw new SQLException(rb.getString("STR_CANNOT_HANDLE_TYPE") + " " +
                                               obj.getClass().getName());
                    }
                }
            }
        }
    }

    /**
     * This method takes the SQLException error code and finds out
     * if the errorcode is a unique constraint error code for SQLServer.
     *
     * @param errorCode DOCUMENT ME!
     *
     * @return true if errocode is unique constraint violation, false otherwise
     */
    public boolean isErrorCodeUnique(int errorCode) {
         if (errorCode == ERROR_CODE_UNIQUE_CONSTRAINT) {
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
        timeFunction = " TO_DATE(SYSDATE) ";
        dateFunction = " SYSDATE ";
        rowLockFunction = "";
        noLockFunction = "";
    }

    /**
     * Gets the maximum varchar length size based on the database type.
     *
     * @return The maximum varchar length size.
     */
    public int getMaxVarcharLength() {
        return MAX_VARCHAR_LENGTH_SQLSERVER;
    }

    /**
     * This gets a String field stored in a IMAGE column in the database
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
     * This gets a String field stored in a TEXT column in the database
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

    public static DataSource getDataSource(Properties connProp) {
        // TODO Auto-generated method stub
        return null;
    }

    public static XADataSource getXADataSource(Properties connProp) {
        // TODO Auto-generated method stub
        return null;
    }

}
