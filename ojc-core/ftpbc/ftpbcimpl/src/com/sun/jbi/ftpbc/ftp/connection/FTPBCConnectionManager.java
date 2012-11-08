/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc.ftp.connection;

import com.sun.jbi.ftpbc.connection.Connection;
import com.sun.jbi.ftpbc.connection.ConnectionPool;
import com.sun.jbi.ftpbc.ftp.FtpFileConfiguration;

import com.sun.jbi.internationalization.Messages;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A singleton with a Map of keyed list of connections
 * synchronized by a pair of read and write locks;
 * 
 * @author jfu
 */
public final class FTPBCConnectionManager {

    private static final Messages mMessages =
            Messages.getMessages(FTPBCConnectionManager.class);
    private static Logger mLogger = Messages.getLogger(FTPBCConnectionManager.class);
    // a keyed map of connection pools
    // the key is form in such a way that the connections
    // with identical keys can be used inter-changably, i.e.
    // re-used.
    static final Map<String, ConnectionPool> mConnectionPools;
    static final ReentrantReadWriteLock mRWL;
    static final java.util.concurrent.locks.Lock mRL;
    static final java.util.concurrent.locks.Lock mWL;

    static {
        mConnectionPools = new HashMap<String, ConnectionPool>();
        mRWL = new ReentrantReadWriteLock();
        mRL = mRWL.readLock();
        mWL = mRWL.writeLock();
    }

    public static final Connection getConnection(Properties cfg) throws Exception {
        String key = FtpFileConfiguration.getKey(cfg);
        assert key != null : "Failed to calculate the key for given configuration properties";
        Connection conn = null;
        ConnectionPool pool = null;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Get connection key=[" + key + "] ");
        }

        mRL.lock();

        try {
            pool = mConnectionPools.get(key);
            if (pool != null) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Pool obtained [" + pool + "] for key=[" + key + "] ");
                }
                conn = pool.getConnection();
                if (conn != null) {
                    conn.configure(cfg);
                }
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Found matching connection [" + conn + "] for key=[" + key + "] ");
                }
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Pool not found for key=[" + key + "] ");
                }
            }
        } finally {
            mRL.unlock();
        }

        if (conn == null) {
            // need to create pool, raise the lock to write lock
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Can not find connection for key=[" + key + "] in the existing pool list ..., need to create pool and connection ");
            }

            mWL.lock();

            try {
                // since between mRL.unlock() and mWL.lock()
                // the pool can be created by another thread, so check again...
                pool = mConnectionPools.get(key);
                if (pool != null) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Pool obtained [" + pool + "] for key=[" + key + "] ");
                    }
                    conn = pool.getConnection();
                    if (conn != null) {
                        conn.configure(cfg);
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, "Found matching connection [" + conn + "] for key=[" + key + "] ");
                        }
                    } else {
                        // the pool is out of connections
                        // so just create one
                        conn = FTPBCConnection.createConnection(cfg);
                        if (conn != null) {
                            pool.addConnection(conn);
                        } else {
                            throw new Exception("Error: can not create connection object for configuration :" + cfg.toString());
                        }
                    }
                } else {
                    // lazy init the pool
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Pool not found for key=[" + key + "], create it...");
                    }
                    int minSz = getInteger(ConnectionPool.POOL_MIN_SIZE, cfg);
                    int maxSz = getInteger(ConnectionPool.POOL_MAX_SIZE, cfg);
                    pool = FTPBCConnectionPool.createConnectionPool(key);
                    if (minSz >= 0 && maxSz > 0 && maxSz > minSz) {
                        // good configure
                        pool.setMinPoolSize(minSz);
                        pool.setMaxPoolSize(maxSz);
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, "Pool created for key=[" + key + "], pool=" + pool);
                        }
                    } else {
                        throw new Exception("Invalid connection pool configuration from runtime configuration...");
                    }
                    // put the min # of connections in the pool
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Before initializing pool with min [" + minSz + "] of connections.");
                    }
                    for (int i = 0; i < minSz; i++) {
                        conn = FTPBCConnection.createConnection(cfg);
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, "Connection created for key=[" + key + "], connection = " + conn);
                        }
                        if (conn != null) {
                            pool.addConnection(conn);
                        } else {
                            throw new Exception("Error: can not create connection object for configuration :" + cfg.toString());
                        }
                    }
                    mConnectionPools.put(key, pool);
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Registered connection with key=[" + key + "] in pool list... ");
                    }
                }
                conn = pool.getConnection();
            } finally {
                mWL.unlock();
            }
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Connection with key=[" + key + "] obtained ... connection =" + conn + " configuration=" + cfg.toString());
        }
        if (conn != null) {
            conn.stopTimer(); // max idle timer stop ticking
        } else {
            throw new Exception("Error: can not allocate connection object for configuration :" + cfg.toString());
        }
        return conn;
    }

    /**
     * put the connection back into the pool, checking high water mark also
     * @param conn
     */
    public static final void returnConnection(Connection conn) throws Exception {
        String key = conn.getKey();

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Returning connection with key=[" + key + "] connection =" + conn);
        }

        mWL.lock();
        try {
            ConnectionPool pool = mConnectionPools.get(key);
            if (pool != null) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Found pool [" + pool + "] for connection with key=[" + key + "] connection =" + conn + " about to return it===>>>");
                }
                pool.returnConnection(conn);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Found pool [" + pool + "] for connection with key=[" + key + "] connection =" + conn + " succesfully returned it===>>>");
                }
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Can not find pool for connection with key=[" + key + "] connection =" + conn + " no need to return it, thrown away...");
                }
                // when corresponding pool not found, throw away the connection 
                conn.stopTimer();
            }
        } finally {
            mWL.unlock();
        }
    }

    /**
     * checking max idle timeout and remove it if expired
     * @param conn
     * @throws java.lang.Exception
     */
    public static final void checkConnection(Connection conn) throws Exception {
        mWL.lock();
        try {
            ConnectionPool pool = mConnectionPools.get(conn.getKey());
            if (pool == null) {
                // the connection does not belong to any pool
                // throw it way
                conn.discard();
            } else {
                // normally the connection should be in the pool
                long elapsed = System.currentTimeMillis() - conn.getLastUsed();
                if (elapsed > conn.getMaxIdleTimeout()) {
                    // expired, evict
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Evicting connection, elapsed time since last use=" + elapsed + " max idle timeout=" + conn.getMaxIdleTimeout());
                    }
                    boolean removed = pool.removeConnection(conn);
                    if (!removed) {
                        throw new Exception("Error, Connection not removed, connection=" + conn);
                    }
                    conn.discard();
                }
            }
        } finally {
            mWL.unlock();
        }
    }

    /**
     * free up the connection pools
     * called by ComponentLifeCycle.stop()
     */
    public void cleanup() {
        try {
            if (mWL.tryLock(1000, TimeUnit.MILLISECONDS)) {
                try {
                    releaseAndCleanup();
                } finally {
                    mWL.unlock();
                }
                return;
            }
        } catch (InterruptedException e) {
            // ignore
        }
        // can not obtain lock, clean up any way
        releaseAndCleanup();
    }

    private static void releaseAndCleanup() {
        if (mConnectionPools.size() > 0) {
            Set keys = mConnectionPools.keySet();
            Iterator it = keys.iterator();
            while (it.hasNext()) {
                String key = (String) it.next();
                ConnectionPool pool = mConnectionPools.remove(key);
                if (pool != null) {
                }
            }
            mConnectionPools.clear();
        }
    }

    private static int getInteger(String name, Properties cfg) {
        Integer obj = (Integer) cfg.get(name);
        if (obj != null) {
            return obj.intValue();
        }
        return -1;
    }
}
