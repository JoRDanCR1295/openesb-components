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
 * @(#)ConnectionPoolXA.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/***********************************************************************************************************************
 * Copyright (c) 2002, SeeBeyond Technology Corporation, All Rights Reserved This program, and all the routines
 * referenced herein, are the proprietary properties and trade secrets of SEEBEYOND TECHNOLOGY CORPORATION. Except as
 * provided for by license agreement, this program shall not be duplicated, used, or disclosed without written consent
 * signed by an officer of SEEBEYOND TECHNOLOGY CORPORATION.
 **********************************************************************************************************************/
package com.sun.jbi.engine.workflow.db.connection.impl;

import java.sql.SQLException;
import java.util.LinkedList;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.sql.XAConnection;
import javax.sql.XADataSource;
import javax.transaction.xa.Xid;

import com.sun.jbi.engine.workflow.db.connection.Connection;
import com.sun.jbi.engine.workflow.db.connection.ConnectionManager;
import com.sun.jbi.engine.workflow.db.connection.ConnectionProperties;
import com.sun.jbi.engine.workflow.db.dao.DAOFactory;
import com.sun.jbi.engine.workflow.db.dao.DBType;
import com.sun.jbi.engine.workflow.util.I18n;

/**
 * Class used for connection pooling. Maintains a list of free connections that will be used by the engine during
 * persistence. This class is used only for persistence for XA case. Refer to ConnectionPool for persistence for non XA
 * related activities
 * 
 * @author mbhasin
 * @version
 * @since eInsight 5.1
 */

public class ConnectionPoolXA {
    

    private static Logger LOGGER = Logger.getLogger(ConnectionPoolXA.class.getName());           
    /**
     * checkedout connection count.
     */
    private int checkedOutConnections = 0;

    /**
     * List of free connections.
     */
    private LinkedList freeConns;

    /**
     * The maximum number of connection that should be maintained. This is read from the engine configuration property
     * maxDBConnections.
     */
    private int maxConns;

    /**
     * Flags whether we should create new connections.
     */
    private boolean createNewConnections = false;

    /**
     * Flags whether we should output debug messages.
     */
    private boolean debug = false;

    /**
     * XADatasource used for creating physical connections.
     */
    private XADataSource xads = null;

    /**
     * <code>waitPeriod</code> The duration to wait before reattempting to get database connection. The wait period
     * will be used in case there is exception thrown for the sqlType defined.
     */
    //private long waitPeriod = Long.parseLong(System.getProperty("waitPeriod", "5000"));
    private long connectionRetryInterval;

    /**
     * <code>numTries</code> The number of times to try to connect to database before giving up.
     */
    //private int numTries = Integer.parseInt(System.getProperty("numTries", "5"));
    private int connectionRetries = Integer.parseInt(System.getProperty("numTries", "5"));

    private boolean isConnectionError = false;

    private boolean isDBConnectionVerified = false;
    
    /**
     * contructor takes connection properties and constructs datasource accordingly.
     * 
     * @param connProp Properties
     * @throws SQLException Exception
     */
    public ConnectionPoolXA(Properties connProp) throws SQLException {
        freeConns = new LinkedList();
        this.createNewConnections = true;
        this.maxConns               = Integer.parseInt(connProp.getProperty(ConnectionProperties.DB_MAX_CONNECTIONS, "25"));
        this.connectionRetries      = Integer.parseInt(connProp.getProperty(ConnectionProperties.DB_CONNECTION_RETRIES, "-1"));
        this.connectionRetryInterval = Integer.parseInt(connProp.getProperty(ConnectionProperties.DB_CONNECTION_RETRYINTERVAL, "10"));

        
        String dbType_Str = connProp.getProperty(ConnectionProperties.DB_TYPE); 
        DBType dbType = DAOFactory.getDBType(dbType_Str);
        
        if (dbType == null) {
            throw new RuntimeException (I18n.loc("WLM-6035: The DB Type {0} is unsupported", dbType_Str));           
        }
        xads = DAOFactory.getXADataSource(connProp, dbType);
    }

    /**
     * To obtain a xa connection from the pool. returns from the free connection list, if exists, if not, obtains a new
     * connection and returns it.
     * 
     * @param connMgr ConnectionMgr
     * @return Connection
     * @throws SQLException Exception
     */
    public synchronized Connection getConnection(ConnectionManager connMgr, Xid xid) throws SQLException {
        if (debug) {
            LOGGER.fine("getting a connection ");
        }
        if (freeConns.size() > 0) {
            checkedOutConnections++;
            return (Connection) freeConns.removeLast();
        }

        if (isConnectionError) {
            try {
                wait();
                if (isConnectionError) {
                    String errorMsg = "Could not obtain New Database Connection. Reason : Database Connection Failure";
                    throw new SQLException(errorMsg);
                }
            } catch (InterruptedException e) {
                throw new SQLException(e.getMessage());
            }
        }

        if (checkedOutConnections < maxConns) {
            Connection conn = new ConnectionImplXA(connMgr, getNewConnection(connMgr), xid);
            checkedOutConnections++;
            return conn;
        }

        // wait for someone to put one back in the pool
        while (freeConns.size() == 0) {
            try {
                wait();
            } catch (InterruptedException e) {
                throw new SQLException(e.getMessage());
            }
        }
        if (!createNewConnections) {
            return null;
        }

        if (freeConns.size() > 0) {
            checkedOutConnections++;
            return (Connection) freeConns.removeLast();
        }

        throw new SQLException("Error: Could not obtain Database Connection");
    }

    /**
     * Puts the connection back to the pool for later use.
     * 
     * @param con Connection
     */
    public synchronized void freeConnection(Connection con) {
        freeConns.add(con);
        checkedOutConnections--;
        notify();
    }

    /**
     * Obtains a new XAConnection from the datasource.
     * 
     * @param connMgr ConnectionMgr
     * @return
     * @throws SQLException Exception
     */
    private XAConnection getNewConnection(ConnectionManager connMgr) throws SQLException {
        XAConnection xacon = null;
        int counter = 0;
        String errorMsg = null;
        String sqlState = null;
        int errorCode = 0;

        while ((connectionRetries == -1) || (counter++ < connectionRetries)) {
            try {
                xacon = xads.getXAConnection();
                isDBConnectionVerified = true;
                break;
            } catch (SQLException e) {
            	if(!isDBConnectionVerified) {
            		throw new SQLException("Database Connection Not available. Please verify database is up and running. " 
            				+ "\nError Message" + errorMsg + " SQL State" + sqlState + " Error Code"
                            + errorCode);
            	}
                isConnectionError = true;
                sqlState = e.getSQLState();
                errorCode = e.getErrorCode();
                errorMsg = e.getMessage();

                if(connectionRetries == -1) {
                    LOGGER.info("Database Connection Failure: Trying to get new connection (Note: Connection Retry Parameter is configured as Infinite)");
                } else {
                    LOGGER.info("Database Connection Failure: Trying to get new connection- numTries = " + counter);
                }
                if (LOGGER.isLoggable(Level.FINE)) {
                    String message = "Database Connection Failure- Error Message :" + e.getMessage() + " SQL State :"
                            + e.getSQLState() + " Error Code :" + e.getErrorCode();
                    LOGGER.fine(message);
                }
                try {
                    wait(connectionRetryInterval);
                } catch (InterruptedException e1) {
                    e1.printStackTrace();
                }
            }
        }
        if (xacon != null) {
            isConnectionError = false;
            notifyAll();
            return xacon;
        } else {
            String message = "Database Connection Failed after trying for ConnectionRetries " + connectionRetries
                    + " (configuration) with Connection Retry Interval " + connectionRetries + " (configuration)";

            shutdown();
            LOGGER.severe("BPEL engine persistence database unavailable. Shutting down the engine." +
            "\nRequires redeployment of the application (ear)");
              throw new SQLException(message + "\nError Message" + errorMsg + " SQL State" + sqlState + " Error Code"
                    + errorCode);
        }
    }

    /**
     * @param conn Connection
     */
    public synchronized void invalidateConnection(Connection conn) {
        freeConns.remove(conn);
        checkedOutConnections--;
    }

    /**
     * Release the connections held during app server shutdown.
     */
    public synchronized void shutdown() {
        if (createNewConnections) {
            for (int i = 0; i < freeConns.size(); i++) {
                try {
                    Object obj = freeConns.get(i);
                    ((Connection) obj).invalidate();
                } catch (SQLException se) {
                    LOGGER.log(Level.WARNING, "sql exception while closing the connection" + se.getMessage(), se);
                }
            }
            freeConns.clear();
            createNewConnections = false;
        }
    }
}
