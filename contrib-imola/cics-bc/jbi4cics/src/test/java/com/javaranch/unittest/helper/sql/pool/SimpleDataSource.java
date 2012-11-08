package com.javaranch.unittest.helper.sql.pool;


import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.DriverManager;

import java.sql.SQLException;
import javax.naming.Reference;
import javax.sql.DataSource;

/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
class SimpleDataSource extends Reference implements DataSource
{

    String dbDriver;
    String dbServer;
    String dbLogin;
    String dbPassword;
    SimpleDataSource()
    {
        super( SimpleDataSource.class.getName() );
    }

    /**
     * Method getConnection creates Connection to the database.
     *
     *
     * @return New Connection each time.
     *
     * @throws java.sql.SQLException
     *
     */
    public Connection getConnection()
    throws java.sql.SQLException
    {
        return getConnection( dbLogin, dbPassword );
    }

    /**
     * Method getConnection
     *
     *
     * @param parm1
     * @param parm2
     *
     * @return
     *
     * @throws java.sql.SQLException
     *
     */
    public Connection getConnection( String dbLogin, String dbPassword )
    throws java.sql.SQLException
    {
      try
      {
          Class.forName( dbDriver );
      }
      catch ( ClassNotFoundException cnfe )
      {
          throw new java.sql.SQLException( cnfe.getMessage() );
      }
      return DriverManager.getConnection( dbServer, dbLogin, dbPassword );
    }

    /**
     * Method getLogWriter not yet implemented.
     *
     *
     * @return
     *
     * @throws java.sql.SQLException
     *
     */
    public PrintWriter getLogWriter()
    throws java.sql.SQLException
    {

        /**@todo: Implement this javax.sql.DataSource method*/
        throw new java.lang.UnsupportedOperationException(
            "Method getLogWriter() not yet implemented." );
    }

    /**
     * Method getLoginTimeout not yet implemented.
     *
     *
     * @return
     *
     * @throws java.sql.SQLException
     *
     */
    public int getLoginTimeout()
    throws java.sql.SQLException
    {

        /**@todo: Implement this javax.sql.DataSource method*/
        throw new java.lang.UnsupportedOperationException(
            "Method getLoginTimeout() not yet implemented." );
    }

    /**
     * Method setLogWriter not yet implemented.
     *
     *
     * @param parm1
     *
     * @throws java.sql.SQLException
     *
     */
    public void setLogWriter( PrintWriter parm1 )
    throws java.sql.SQLException
    {

        /**@todo: Implement this javax.sql.DataSource method*/
        throw new java.lang.UnsupportedOperationException(
            "Method setLogWriter() not yet implemented." );
    }

    /**
     * Method setLoginTimeout not yet implemented.
     *
     *
     * @param parm1
     *
     * @throws java.sql.SQLException
     *
     */
    public void setLoginTimeout( int parm1 )
    throws java.sql.SQLException
    {

        /**@todo: Implement this javax.sql.DataSource method*/
        throw new java.lang.UnsupportedOperationException(
            "Method setLoginTimeout() not yet implemented." );
    }

    public <T> T unwrap(Class<T> iface) throws SQLException {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public boolean isWrapperFor(Class<?> iface) throws SQLException {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
