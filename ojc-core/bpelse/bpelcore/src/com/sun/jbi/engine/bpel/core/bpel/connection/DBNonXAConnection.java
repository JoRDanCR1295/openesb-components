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
 * @(#)DBNonXAConnection.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.connection;

import java.sql.Connection;
import java.sql.SQLException;

public class DBNonXAConnection extends AbstractDBConnection {

    /**
     * Initial value of the setAutoCommit field can vary across the driver versions and hence is stored
     * here, so that it can be restored before calling close to return the connection to the pool.
     */
    private boolean mOrigValOfAutoCommit = false;
	
    /**
     * @param conn A JDBC connection that is not enlisted with a TX context.
     * @throws SQLException SQLException
     */
    public DBNonXAConnection(Connection conn) throws SQLException {
        super(conn);
        mOrigValOfAutoCommit = conn.getAutoCommit();

        /* this step is required to address driver version behavior, where the initial value maybe 'false'.
         * In such a case a connection got by the api getNonTxConnection may do unit of work and not commit(like a single read operation)
         * and return the connection to the pool and when this connection is returned from the pool for a getConnection api call that 
         * has to be associated with a TX context the api will return a Exception like 'Resource manager is doing work outside a transaction'.
         * This is set back to the original value on close of the connection(see the close method).
         */
        conn.setAutoCommit(true);
        
    }	

    /**
     * wrapper to set the initial value of the autoCommit value 
     * and close the underlying connection.
     * @throws SQLException
     */
    public void close() throws SQLException {
    	// set the initial value of the conn.autoCommit.
        try {
        	// DEVNOTE: If the database connection has some problem, such as the DB being down, then this 
        	// auto commit statement will throw an exception and as a result the statement which closes the 
        	// connection is skipped. However we do want to attempt to close the connection even if setAutoCommit()
        	// threw an exception, hence the try catch block which basically ignores the exception.
            getUnderlyingConnection().setAutoCommit(mOrigValOfAutoCommit);
        } catch (Exception e) {}
    	// close the connection to return it to the pool.
        super.close();
    }
}
