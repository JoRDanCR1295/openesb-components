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
 * @(#)ConnectionImplXA.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2004, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.engine.workflow.db.connection.impl;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.sql.XAConnection;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import com.sun.jbi.engine.workflow.db.connection.Connection;
import com.sun.jbi.engine.workflow.db.connection.ConnectionManager;

/**
 * Class implementing connection interface for the XA case.
 * 
 * @author mbhasin
 * @version 
 * @since eInsight 5.1
 */
public class ConnectionImplXA implements Connection {
    

    private static Logger LOGGER = Logger.getLogger(ConnectionImplXA.class.getName());       

    
    private java.sql.Connection connection;
    ConnectionManager connMgr;
    private XAConnection xaConnection;
    private HashMap sessionLobMap = new HashMap();
    private HashMap pstmtCache = new HashMap();    
    
    private Xid xid = null;
    
    
    public ConnectionImplXA(ConnectionManager connMgr, XAConnection xaConnection, Xid xid) throws SQLException {
        this.connMgr = connMgr;
        this.xaConnection = xaConnection;
        this.connection = xaConnection.getConnection();
        this.xid = xid;
    }

    /* (non-Javadoc)
     * @see com.stc.bpms.bpel.runtime.persistence.Connection#getXaConnection()
     */
    public XAConnection getXaConnection() {
        return xaConnection;
    }
    
    /* (non-Javadoc)
     * @see com.stc.bpms.bpel.runtime.persistence.Connection#getConnection()
     */
    public final java.sql.Connection getConnection() {
        return connection;
    }
    
    /* (non-Javadoc)
     * @see com.stc.bpms.bpel.runtime.persistence.Connection#getProperty(java.lang.Object)
     */
    public final Object getProperty(Object key) {
        return sessionLobMap.get(key);
    }
    
    /* (non-Javadoc)
     * @see com.stc.bpms.bpel.runtime.persistence.Connection#setProperty(java.lang.Object, java.lang.Object)
     */
    public final void setProperty(Object key, Object val) {
        sessionLobMap.put(key, val);
    }
    
    /* (non-Javadoc)
     * @see com.stc.bpms.bpel.runtime.persistence.Connection#commit()
     */
    public final void commit() throws SQLException {
    }
    
    /* (non-Javadoc)
     * @see com.stc.bpms.bpel.runtime.persistence.Connection#rollback()
     */
    public final void rollback() throws SQLException {
    }
    
    /* (non-Javadoc)
     * @see com.stc.bpms.bpel.runtime.persistence.Connection#setAutoCommit(boolean)
     */
    public final void setAutoCommit(boolean flag) throws SQLException {
    }
    /**
     * @param query
     */
    public final PreparedStatement prepareStatement(String query) throws SQLException {
        PreparedStatement ps = (PreparedStatement)this.pstmtCache.get(query);
        if(ps == null) {
            ps = connection.prepareStatement(query);
            pstmtCache.put(query, ps);
        }
        return ps;
    }
    
    
    
    /* (non-Javadoc)
     * @see com.stc.bpms.bpel.runtime.persistence.Connection#closePreparedStatement(java.sql.PreparedStatement)
     */
    public void closePreparedStatement(PreparedStatement preparedStatement) {
        //Does nothing, since this class caches the connection
    }
    /**
     * @param query
     * @return
     */
    public final Statement createStatement() throws SQLException {
        return connection.createStatement();
    }

    /**
     * Refer to Connection.getXAResource.
     */
    public XAResource getXAResource() throws SQLException {
        XAResource xaResource = xaConnection.getXAResource();
        return xaResource;
    }
    
    /**
     * Refer to Connection.close.
     */
    public void close() {
        connMgr.freeConnectionByXID(xid);
    }
    
    /* (non-Javadoc)
     * @see com.stc.bpms.bpel.runtime.persistence.Connection#invalidate()
     */
    public void invalidate() throws SQLException {
        
    	Iterator iter =  pstmtCache.values().iterator();
    	while(iter.hasNext()) {
    	    PreparedStatement ps = (PreparedStatement) iter.next();
            try {
        	    ps.close();
            } catch (SQLException se) {
                LOGGER.log(Level.WARNING, "Exception while closing a PreparedStatement.", se);
            }
    	}
    	pstmtCache.clear();
        xaConnection.close();
    }
}
