/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc.ftp.connection;

import com.sun.jbi.ftpbc.connection.Connection;
import com.sun.jbi.ftpbc.connection.ConnectionPool;
import com.sun.jbi.internationalization.Messages;

import java.util.LinkedList;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * a connection pool based on linked list;
 * @author jfu
 */
public class FTPBCConnectionPool implements ConnectionPool {

    private static final Messages mMessages =
            Messages.getMessages(FTPBCConnectionPool.class);
    private static Logger mLogger = Messages.getLogger(FTPBCConnectionPool.class);
    private final String mConnectionKey;
    private int mMinSize = 2;
    private int mMaxSize = 32;
    private final LinkedList<Connection> mConnections;

    private FTPBCConnectionPool(String key) {
        assert key != null || key.trim().length() == 0 : "Connection Key must not be empty (blank) when creating connection pool";
        mConnectionKey = key;
        mConnections = new LinkedList();
    }

    private FTPBCConnectionPool(String key, int minSize, int maxSize) {
        this(key);
        assert minSize >= 0 && maxSize > minSize : "When creating connection pool: max pool size > min pool size >= 0";
        mMinSize = minSize;
        mMaxSize = maxSize;
    }

    public static final ConnectionPool createConnectionPool(String key) {
        return new FTPBCConnectionPool(key);
    }

    public static final ConnectionPool createConnectionPool(String key, int minSize, int maxSize) {
        return new FTPBCConnectionPool(key, minSize, maxSize);
    }

    public void setMaxPoolSize(int size) {
        assert size > 0;
        mMaxSize = size;
    }

    public int getMaxPoolSize() {
        return mMaxSize;
    }

    public void setMinPoolSize(int size) {
        assert size >= 0;
        mMinSize = size;
    }

    public int getMinPoolSize() {
        return mMinSize;
    }

    public boolean addConnection(Connection conn) {
        assert mConnections != null : "Connection pool not available when addConnection(Connection conn) called, key=" + mConnectionKey;
        return mConnections.add(conn);
    }

    public boolean removeConnection(Connection conn) {
        assert mConnections != null : "Connection pool not available when removeConnection(Connection conn) called, key=" + mConnectionKey;
        return mConnections.remove(conn);
    }

    public Connection getConnection() {
        assert mConnections != null : "Connection pool not available when getConnection() called, key=" + mConnectionKey;
        return mConnections.poll();
    }

    /**
     * put back a connection and check max 
     * @param conn
     */
    public void returnConnection(Connection conn) {
        assert mConnections != null : "Connection pool not available when returnConnection(Connection conn) called, key=" + mConnectionKey;

        try {
            conn.reset();
        } catch (Exception ex) {
            // not a healthy connection, throw away
            conn.discard();
            return;
        }

        if (mConnections.size() < mMaxSize) {
            // set the cur timestamp before put it back
            conn.setLastUsed(System.currentTimeMillis());
            try {
                conn.startTimer();
            } catch (Exception ex) {
                // throw it away
                conn.discard();
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Exception when trying to stop timer in ConnectionPool.returnConnection()", ex);
                }
                return;
            }
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Return the connection key =" + conn.getKey() + " to the linked list");
            }
            mConnections.add(conn);
        } else {
            // throw away 
            conn.discard();
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Max Size reached, throw away the connection =" + conn.getKey() + " connection=" + conn);
            }
        }
    }

    /**
     * called by ConnectionManager to clean up the pool
     */
    public void cleanup() {
        while (!mConnections.isEmpty()) {
            Connection conn = mConnections.remove();
            if (conn != null) {
                conn.discard();
            }
        }
    }
}
