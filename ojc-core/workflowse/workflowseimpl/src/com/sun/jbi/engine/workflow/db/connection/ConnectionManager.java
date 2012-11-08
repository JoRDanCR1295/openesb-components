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
 * @(#)ConnectionManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.db.connection;

import java.sql.SQLException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

import com.sun.jbi.engine.workflow.WorkflowException;
import com.sun.jbi.engine.workflow.db.connection.impl.ConnectionPool;
import com.sun.jbi.engine.workflow.db.connection.impl.ConnectionPoolXA;
import com.sun.jbi.engine.workflow.util.I18n;

/**
 * The ConnectionMgr for obtaining and returning a Connection
 * 
 * @author Sun Microsystems
 *
 */
public class ConnectionManager {

    private static Logger LOGGER = Logger.getLogger(ConnectionManager.class.getName());        
    private static ConnectionManager connMgr;
    private static boolean debug = false;
    private Properties dbConnProperties; 
    
    Map XAConnectionMap = Collections.synchronizedMap(new HashMap());
    ConnectionPool connPool = null;
    ConnectionPoolXA connPoolXA = null;

    /**
     * Used to initialize an instance of the Connection manager. The connection manager should be initialized
     * only once. Any attempt to initialize again would result in a ConnectionException being thrown.(It may be 
     * prudent to synchronoze this method to prevent multiple threads trying to initialize concurrently, but
     * the logic of initialization make this senario highly unlikely and hence can aviod the overhead.
     *
     * @param properties a <code>Properties</code> value
     * @exception WorkflowException if an error occurs
     */
    public static void init(Properties properties) throws ConnectionException {
        if (connMgr != null) {
            LOGGER.warning(I18n.loc("WLM-6034: Connection Manager can be initialized only once"));
            return;
        }
        
        connMgr = new ConnectionManager(properties);
    }

    public static boolean isSQLStateConnectionError(SQLException e) {
        String sqlState = e.getSQLState();
        SQLException nextException = null;
        
        if (sqlState != null && sqlState.startsWith("08") || e.getErrorCode() == 17002) {
            return true;
        } else if ((nextException = e.getNextException()) != null){
            return isSQLStateConnectionError(nextException);
        }
        return false;
    }
    
    // no one should be able to use this constructor
    private ConnectionManager(Properties properties) {
        debug = LOGGER.isLoggable(Level.FINE);
        if (debug) {
            LOGGER.fine(I18n.loc("WLM-3013: The ConnectionMgr is Instantiated"));
        }
        dbConnProperties = properties;
    }
    
    public Properties getDBConnProperties() {
        return dbConnProperties;
    }

    public static ConnectionManager getInstance() throws ConnectionException {
        if (connMgr == null) {
            throw new ConnectionException (I18n.loc("ConnectionMgr.Uninstantiated"));
        }
        return connMgr;
    }
    

 
    public Connection getConnection(Xid XID) throws SQLException, XAException  {
        if(XID != null) {
            if (connPoolXA == null) {
                synchronized(this) {
                    if (connPoolXA == null) {
                        connPoolXA = new ConnectionPoolXA(dbConnProperties);
                    }
                }
            }
            return getXAConnection(XID);
        } else {
            if (connPool == null) {
                synchronized(this) {
                    if (connPool == null) {
                        connPool = new ConnectionPool(dbConnProperties);
                    }
                }
            }
            return connPool.getConnection(this);
        }
    }
    
    public Connection getConnection() throws SQLException {
        if (connPool == null) {
            synchronized(this) {
                if (connPool == null) {
                    connPool = new ConnectionPool(dbConnProperties);
                }
            }
        }
        return connPool.getConnection(this);
    }

    public synchronized void freeConnection(Connection con) {
        connPool.freeConnection(con);
    }
    
    public void freeConnectionByXID(Xid XID) {
        Connection con = (Connection)XAConnectionMap.remove(XID.toString());
        if (con != null) {
            connPoolXA.freeConnection(con);
        }
    }

    /**
     * returns a specific DB XAResource, from the local cache, if not present then will create a new 
     * XAConnecton. Called from the AbstractXAResource to delegate the TX Manager calls.
     *
     * @param xid a <code>Xid</code> value
     * @return a <code>XAResource</code> value
     */
    public XAResource getXAResourceByXID(Xid xid) {
        XAResource res = null;
        try {
            Connection connExt = (Connection)XAConnectionMap.get(xid.toString());
            if (connExt == null) {
                LOGGER.log(Level.SEVERE, I18n.loc("WLM-7002: ConnectionMgr.getXAResourceByXid returning a null XA resource"));
                // This could be in the case of a fault send back from an invoked partner since its
                // immaterial to persist a fault before the rollback has happened, and in such a case
                // the ConnectionMgr would not even have a XAConnExt registered and hence this will be an
                // indication to the einsight XAResource to not delegate the calls. Only in this fault case
                // should the connExt be null and under no other condition.
                return null;
            }
            res =  connExt.getXaConnection().getXAResource();
        }  catch (Exception xaExe) {
            LOGGER.log(Level.SEVERE, I18n.loc("WLM-7003: ConnectionMgr.getXAResourceByXID: cannot get XAResource"),  xaExe);
        }
        return res;
    }

    /**
     * @param conn
     */
    public void invalidateConnection(Connection conn) {
        connPool.invalidateConnection(conn);
    }
    
    public void shutdown() {
      LOGGER.info("Shutting down the connection Mgr");
 
        if(connPool != null) {
            connPool.shutdown();
        } 
        if(connPoolXA != null) {
            connPoolXA.shutdown();
        }
    }

    private Connection getXAConnection(Xid xid) throws SQLException, XAException {
        Connection conn = null;
        conn = (Connection)XAConnectionMap.get(xid.toString());
        
        if (conn != null) {
            return conn;
        } else {
            try{
                conn = connPoolXA.getConnection(this, xid);
                XAResource xaRes = conn.getXaConnection().getXAResource();
                xaRes.start(xid, XAResource.TMNOFLAGS);
                XAConnectionMap.put(xid.toString(), conn);
            } catch (XAException xae) {
                if(xae.errorCode == -7) {
                    connPoolXA.invalidateConnection(conn);
                    return getXAConnection(xid);
                } else {
                    throw xae;
                }
            } catch (SQLException sqle) {
                if(isSQLStateConnectionError(sqle)) {
                    connPoolXA.invalidateConnection(conn);
                    return getXAConnection(xid);
                } else {
                    throw sqle;
                }
            }
            return conn;
        }
    }
    
    public boolean isDBConnectionBroken(SQLException e, Connection dbConn) {
        String sqlState = e.getSQLState();
        if (isSQLStateConnectionError(e)) {
            LOGGER.info("Database connection error. Trying again.. exception message .." + e.getMessage()
                    + " \nError Code = " + e.getErrorCode() + ", SQLState = " + sqlState);
            invalidateConnection(dbConn);
            dbConn = null;
            return true;
        }
        return false;
    }
    
    public Properties getProperty () {
        return dbConnProperties;
    }
} 
