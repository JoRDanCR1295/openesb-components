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
 * @(#)AxionDBConnection.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.persist.axiondb;

import java.sql.Connection;
import java.sql.SQLException;

import com.sun.jbi.hl7bc.extservice.persist.connection.DBConnection;

public class AxionDBConnection extends DBConnection{

	private AxionDBConnectionPool connPool = null;
	boolean inUse = false;
    private long timestamp;

	private Connection conn = null;
	public AxionDBConnection(Connection conn, AxionDBConnectionPool connPool) throws SQLException {
		super(conn);
		this.conn = conn;
		this.connPool = connPool;
	}
	
    public synchronized boolean lease() {
        if(inUse)  {
            return false;
        } else {
           inUse=true;
           timestamp=System.currentTimeMillis();
           return true;
        }
     }
  
    public boolean inUse() {
        return inUse;
    }
    
    public long getLastUse() {
        return timestamp;
    }

    public void close() throws SQLException {
        this.connPool.returnConnection(this);
    }

    protected void expireLease() {
        inUse=false;
    }

    public boolean validate() {
    	try {
            conn.getMetaData();
        }catch (Exception e) {
        	return false;
        }
    	return true;
    }



}
