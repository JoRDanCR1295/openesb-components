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
 * @(#)DAOFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.dao;

import java.sql.SQLException;
import java.util.Properties;

import javax.sql.DataSource;
import javax.sql.XADataSource;

import com.sun.jbi.engine.workflow.db.dao.impl.DB2DAO;
import com.sun.jbi.engine.workflow.db.dao.impl.DerbyDAO;
import com.sun.jbi.engine.workflow.db.dao.impl.MySQLDAO;
import com.sun.jbi.engine.workflow.db.dao.impl.OracleDAO;
import com.sun.jbi.engine.workflow.db.dao.impl.SQLServerDAO;
import com.sun.jbi.engine.workflow.db.dao.impl.SybaseDAO;


/**
 * A factory class to create DAO object.
 *
 * @author SeeBeyond Technology Corporation
 * @version 
 *
 * @since eInsight5.0
 */
public final class DAOFactory {
  /**
   * Create an instance of DAO object (ie: OracleDAO, SybaseDAO, etc).
   *
   * @param prop
   * @return
   * @throws DAOException
   */
  public static DAO getDAOInstance(String dbType) throws DAOException {
      try {
          DAO dao = null;
          //String dbType = prop.getProperty("dbType", "none");
          if (dbType == null) {
              dbType = "none";
          }
          if (dbType.startsWith("Sybase")) {
              // System.out.println("xxxxxxxx creating SybaseDAO()....");
              dao = new SybaseDAO();
          } else if (dbType.startsWith("Oracle")) {
              // System.out.println("xxxxxxxx creating OracleDAO()....");
              dao = new OracleDAO();
          }  else if (dbType.startsWith("DB2")) {
               // System.out.println("xxxxxxxx creating DB2DAO()....");
              dao = new DB2DAO();
          } else if (dbType.startsWith("SQL Server")) {
              // System.out.println("xxxxxxxx creating SQLServerDAO()....");
              dao = new SQLServerDAO();
          } else {
              // default is Oracle.
              throw new DAOException("DB Not supported" + dbType);
          }
          return dao;
      }
      catch (DAOException daoe) {
          throw daoe;
      }
      catch (Exception e) {
          throw new DAOException(e);
      }
  }
  
  public static DBType getDBType(String dbType) {
      if (dbType.startsWith(DAO.DERBY)) {
          return DAO.DERBY_TYPE;
      } else if (dbType.startsWith(DAO.SYBASE)) {
          return DAO.SYBASE_TYPE;
      } else if (dbType.startsWith(DAO.ORACLE)) {
          return DAO.ORACLE_TYPE;
      }  else if (dbType.startsWith(DAO.DB2)) {
          return DAO.DB2_TYPE;
      } else if (dbType.startsWith(DAO.SQLSERVER)) {
          return DAO.SQLSERVER_TYPE;
      } else if (dbType.startsWith(DAO.MYSQL)) {
          return DAO.MYSQL_TYPE;
      } else {
          return null;
      }
  }
  
  public static DataSource getDataSource (Properties connProp, DBType dbType) throws SQLException {
      if (dbType == DAO.DERBY_TYPE) {
          return DerbyDAO.getDataSource (connProp);
      } else if (dbType == DAO.ORACLE_TYPE) {
          return OracleDAO.getDataSource (connProp);
      } else if (dbType == DAO.SYBASE_TYPE) {
          return SybaseDAO.getDataSource (connProp);
      } else if (dbType == DAO.SQLSERVER_TYPE) {
          return SQLServerDAO.getDataSource (connProp);
      } else if (dbType == DAO.DB2_TYPE) 
      {
          return DB2DAO.getDataSource (connProp);
      } else if (dbType == DAO.MYSQL_TYPE) {
          return MySQLDAO.getDataSource (connProp);
      }
      return null;
  }
  
  public static XADataSource getXADataSource (Properties connProp, DBType dbType) throws SQLException {
      if (dbType == DAO.DERBY_TYPE) {
          return DerbyDAO.getXADataSource (connProp);
      } else if (dbType == DAO.ORACLE_TYPE) {
          return OracleDAO.getXADataSource (connProp);
      } else if (dbType == DAO.SYBASE_TYPE) {
          return SybaseDAO.getXADataSource (connProp);
      } else if (dbType == DAO.SQLSERVER_TYPE) {
          return SQLServerDAO.getXADataSource (connProp);
      } else if (dbType == DAO.DB2_TYPE) {
          return DB2DAO.getXADataSource (connProp);
      } else if (dbType == DAO.MYSQL_TYPE) {
          return MySQLDAO.getXADataSource (connProp);
      }
      return null;
  }  
}
