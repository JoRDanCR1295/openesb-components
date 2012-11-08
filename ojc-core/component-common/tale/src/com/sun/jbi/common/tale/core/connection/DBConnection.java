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
 * @(#)AbstractDBConnection.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.tale.core.connection;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;

import com.sun.jbi.common.tale.core.util.I18n;

/**
 * @author Sun Microsystems
 */

public class DBConnection {
    
    /** The JDBC connection */
    protected Connection mConn;
    
    /**
     * constructor
     * 
     * @param conn jdbc connection
     * @throws SQLException SQLException
     */
    public DBConnection(Connection conn) throws SQLException {
    	if (conn == null) {
    		throw new SQLException(I18n.loc("TALE-7007: Cannot create a DBConnection with a Null JDBC Connection"));
    	}
    	mConn = conn;
    }

    /**
     * A handle to the undelying JDBC connection
     * 
     * @return
     */
    public Connection getUnderlyingConnection() {
        return mConn;
    }
    
    /**
     * wrapper to set the initial value of the autoCommit value 
     * and close the underlying connection.
     * @throws SQLException
     */
    public void close() throws SQLException {
    	mConn.close();
    }
    
    public PreparedStatement prepareStatement(String query) throws SQLException {
        PreparedStatement ps = mConn.prepareStatement(query);
        return ps;
    }
    
    public final void setAutoCommit(boolean flag) throws SQLException {
        mConn.setAutoCommit(flag);
    }

    public final void commit() throws SQLException {
        mConn.commit();
    }
    
    public final void rollback() throws SQLException {
        mConn.rollback();
    }
}
