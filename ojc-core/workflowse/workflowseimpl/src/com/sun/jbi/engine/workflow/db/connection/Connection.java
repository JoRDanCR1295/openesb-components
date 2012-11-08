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
 * @(#)Connection.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.connection;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;

import javax.sql.XAConnection;
import javax.transaction.xa.XAResource;

/**
 * Connection interface 
 * @author Sun Microsystems
 *
 */
public interface Connection {
    public abstract XAConnection getXaConnection();
    
    public abstract java.sql.Connection getConnection();
   
    public abstract Object getProperty(Object key);
    
    public abstract void setProperty(Object key, Object val);
    
    public abstract void commit() throws SQLException;
    
    public abstract void rollback() throws SQLException;
    
    public abstract void setAutoCommit(boolean flag) throws SQLException;
    
    /**
     * @param query
     */
    public abstract PreparedStatement prepareStatement(String query) throws SQLException;
    
    /**
     * @param preparedStatement
     * @throws SQLException
     */
    public abstract void closePreparedStatement(PreparedStatement preparedStatement);
    
    /**
     * @param query
     * @return
     */
    public abstract Statement createStatement() throws SQLException;

    /**
     * Used by the XA connection implementation of this interface to return the XAResource with this XA
     * Connection. Non XA implementation should throw UnsupportedOperationException.
     *
     * @return a <code>XAResource</code> value
     * @exception SQLException if an error occurs
     */
    public XAResource getXAResource() throws SQLException;

    /**
     * Used to close the underlying physical connection. Returns the connection to the pool
     *
     * @exception SQLException if an error occurs
     */
    public void close();
    
    public void invalidate() throws SQLException;
}
