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
 * @(#)HL7BCConnectionManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.connection;


import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.hl7bc.I18n;
import com.sun.jbi.hl7bc.extservice.client.HL7Connector;
import org.apache.mina.common.IoHandler;


/**
 * @author Raghunadh Teegavarapu
 */
public final class HL7BCConnectionManager {
    private static Logger mLogger = Logger.getLogger(HL7BCConnectionManager.class.getName());

    static final Map<ConnectionInfo, ConnectionPool> mConnectionPools;

    static final ReentrantReadWriteLock mRWL;
    static final java.util.concurrent.locks.Lock mRL;
    static final java.util.concurrent.locks.Lock mWL;

    static {
        mConnectionPools = new HashMap<ConnectionInfo, ConnectionPool>();
        mRWL = new ReentrantReadWriteLock();
        mRL = mRWL.readLock();
        mWL = mRWL.writeLock();
    }

    public static final Connection getConnection(ConnectionInfo key) throws Exception {
        assert key != null : "Failed to calculate the key for given configuration properties";
        Connection conn = null;
        ConnectionPool pool = null;
        if ( mLogger.isLoggable(Level.FINE)) 
            mLogger.log(Level.FINE, "Get connection key=[" + key + "] ");
        
        mRL.lock(); 
        
        try { 
            pool = mConnectionPools.get(key); 
            if ( pool != null ) {
                if ( mLogger.isLoggable(Level.FINE)) 
                    mLogger.log(Level.FINE, "Pool obtained [" + pool + "] for key=[" + key + "] ");
                conn = pool.getConnection();
                if ( mLogger.isLoggable(Level.FINE)) 
                    mLogger.log(Level.FINE, "Found matching connection [" + conn + "] for key=[" + key + "] ");
            }
            else {
                if ( mLogger.isLoggable(Level.FINE)) 
                    mLogger.log(Level.FINE, "Pool not found for key=[" + key + "] ");
            }
        } 
        finally {
            mRL.unlock();
        }
        
        if ( conn == null ) {
            // need to create pool, raise the lock to write lock
            if ( mLogger.isLoggable(Level.FINE)) 
                mLogger.log(Level.FINE, "Can not find connection for key=[" + key + "] in the existing pool list ..., need to create pool and connection ");

            mWL.lock(); 
            
            try { 
                if ( pool == null ) {
                    int minSz = key.getMinPoolSize();
                    int maxSz = key.getMaxPoolSize();
                    pool = HL7BCConnectionPool.createConnectionPool(key);
                    if ( mLogger.isLoggable(Level.FINE)) 
                        mLogger.log(Level.FINE, "Pool created for key=[" + key + "], pool = " + pool);
                    if ( minSz >= 0 && maxSz > 0 && maxSz >= minSz ) {
                        // good configure
                        pool.setMinPoolSize(minSz);
                        pool.setMaxPoolSize(maxSz);
                    }
                    else {
                        throw new Exception("Invalid connection pool configuration from runtime configuration...");
                    }
                    if ( mLogger.isLoggable(Level.FINE)) 
                        mLogger.log(Level.FINE, "Connection created for key=[" + key + "], connection = " + conn);
                    mConnectionPools.put(key, pool);
                }
                conn = new HL7BCConnection();
                conn.createConnection(key);
                conn.getClientObject().join();
                // if ( mLogger.isLoggable(Level.FINE))
                   // mLogger.log(Level.FINE, "Connection created for key=[" + key + "], connection
                    // = " + conn);
                // mConnectionPools.put(key, pool);
                if ( mLogger.isLoggable(Level.FINE)) 
                    mLogger.log(Level.FINE, "Registered connection with key=[" + key + "] in pool list... ");
                
            } finally {
                mWL.unlock(); 
            }
        }
        
        mLogger.log(Level.INFO, I18n.msg("I0186: Opened new connection to {0} and added to connection pool. Max idle time is {1}",
                conn.getKey(), conn.getMaxIdleTimeout()));
        
        if ( mLogger.isLoggable(Level.FINE)) 
            mLogger.log(Level.FINE, "Connection with key=[" + key + "] obtained ... connection =" + conn);
        conn.stopTimer(); // max idle timer stop ticking
        return conn;
    }
    
    public static final Connection getPoolledConnection(ConnectionInfo key) throws Exception {
        Connection conn = null;
        ConnectionPool pool = null;
        mRL.lock(); 
        try { 
            pool = mConnectionPools.get(key); 
            if ( pool != null ) {
                conn = pool.getConnection();
                if ( mLogger.isLoggable(Level.FINE)) 
                    mLogger.log(Level.FINE, "Found matching connection [" + conn + "] for key=[" + key + "] ");
            }
            else {
                if ( mLogger.isLoggable(Level.FINE)) 
                    mLogger.log(Level.FINE, "Pool not found for key=[" + key + "] ");
            }
        } 
        finally {
            mRL.unlock();
        }
        
        if ( conn == null ) {
            if ( mLogger.isLoggable(Level.FINE)) 
                mLogger.log(Level.FINE, "Connection with key=[" + key + "] not obtained ... connection =" + conn);
        }else{
            if ( mLogger.isLoggable(Level.FINE)) 
                mLogger.log(Level.FINE, "Connection with key=[" + key + "] obtained ... connection =" + conn);
            conn.stopTimer(); // max idle timer stop ticking
            
        }       
        
        return conn;
    }
    
    /**
     * put the connection back into the pool, checking high water mark also
     * 
     * @param conn
     */
    public static final void returnConnection(Connection conn) throws Exception {
        ConnectionInfo key = conn.getKey();

        if ( mLogger.isLoggable(Level.FINE)) 
            mLogger.log(Level.FINE, "Returning connection with key=[" + key + "] connection =" + conn);
        
        mWL.lock(); 
        try { 
            ConnectionPool pool = mConnectionPools.get(key); 
            if ( pool != null ) {
                if ( mLogger.isLoggable(Level.FINE)) 
                    mLogger.log(Level.FINE, "Found pool [" + pool + "] for connection with key=[" + key + "] connection =" + conn + " about to return it===>>>");
                pool.returnConnection(conn);
                if ( mLogger.isLoggable(Level.FINE)) 
                    mLogger.log(Level.FINE, "Found pool [" + pool + "] for connection with key=[" + key + "] connection =" + conn + " succesfully returned it===>>>");
            }
            else {
                if ( mLogger.isLoggable(Level.FINE)) 
                    mLogger.log(Level.FINE, "Can not find pool for connection with key=[" + key + "] connection =" + conn + " no need to return it, thrown away...");
                // when corresponding pool not found, throw away the connection
                conn.stopTimer();
            }
        } finally {
            mWL.unlock(); 
        }
    }

    /**
     * checking max idle timeout and remove it if expired
     * 
     * @param conn
     * @throws java.lang.Exception
     */
    public static final void checkConnection(Connection conn) throws Exception {
        mWL.lock(); 
        try { 
            ConnectionPool pool = mConnectionPools.get(conn.getKey()); 
            if ( pool == null ) {
                // the connection does not belong to any pool
                // throw it way
                conn.discard();
            }
            else {
                // normally the connection should be in the pool
                long elapsed = System.currentTimeMillis() - conn.getLastUsed();
                if ( elapsed > conn.getMaxIdleTimeout() ) {
                    // expired, evict
                    mLogger.log(Level.INFO, I18n.msg("I0185: Closing connection to {0},  elapsed time since last use = {1}, max idle timeout = {2}",
                            conn.getKey(), elapsed, conn.getMaxIdleTimeout()));
                    boolean removed = pool.removeConnection(conn);
                    if ( !removed )
                        throw new Exception("Error, Connection not removed, connection=" + conn);
                    conn.discard();
                }
            }
        } finally {
            mWL.unlock(); 
        }
    }
    
    /**
     * free up the connection pools called by ComponentLifeCycle.stop()
     */
    public void cleanup() {
        try {
            if ( mWL.tryLock(1000, TimeUnit.MILLISECONDS) ) {
                try { 
                    releaseAndCleanup();
                } finally {
                    mWL.unlock(); 
                }
                return;
            }
        }
        catch (InterruptedException e) {
            // ignore
        }
        // can not obtain lock, clean up any way
        releaseAndCleanup();
    }
    
    /**
     * free up the connection pool called by ServiceUnitLifeCycle.stop()
     */
    public static void cleanupPoolAssoaciatedWithEndpoint(ConnectionInfo key) {
        try {
            if ( mWL.tryLock(1000, TimeUnit.MILLISECONDS) ) {
                try { 
                    releaseAndCleanupPool(key);
                } finally {
                    mWL.unlock(); 
                }
                return;
            }
        }
        catch (InterruptedException e) {
            // ignore
        }
        // can not obtain lock, clean up any way
        releaseAndCleanupPool(key);
    }

    private static void releaseAndCleanup() {
        if ( mConnectionPools.size() > 0 ) {
            Set keys = mConnectionPools.keySet();
            Iterator<ConnectionInfo> it = keys.iterator();
            while ( it.hasNext() ) {
                ConnectionInfo key = it.next();
                ConnectionPool pool = mConnectionPools.remove(key);
                if ( pool != null ) {
                	
                }
            }
            mConnectionPools.clear();
        }
    }

    private static void releaseAndCleanupPool(ConnectionInfo connInfo) {
        if(connInfo == null)
            return;
        if ( mConnectionPools.size() > 0 ) {
            Set keys = mConnectionPools.keySet();
            Iterator<ConnectionInfo> it = keys.iterator();
            while ( it.hasNext() ) {
                ConnectionInfo key = it.next();
                if((connInfo.getHost().equals(key.getHost()) &&
                        connInfo.getPort() == key.getPort() && connInfo.getEndpointName().equals(key.getEndpointName()))){                
                    ConnectionPool pool = mConnectionPools.remove(key);
                    if ( pool != null ) {
                        pool.cleanup();
                    }
                }
            }
        }
    }

    private static int getInteger(String name, Properties cfg) {
        Integer obj = (Integer)cfg.get(name);
        if ( obj != null ) {
            return obj.intValue();
        }
        return -1;
    }
    
    public static void setMinPoolSize(int size){
        if ( mConnectionPools.size() > 0 ) {
            Set keys = mConnectionPools.keySet();
            Iterator<ConnectionInfo> it = keys.iterator();
            while ( it.hasNext() ) {
                ConnectionInfo key = it.next();
                ConnectionPool pool = mConnectionPools.get(key);
                if ( pool != null ) {
                	pool.setMinPoolSize(size);
                }
            }
        }
    }
    
    public static void setMaxPoolSize(int size){
        if ( mConnectionPools.size() > 0 ) {
            Set keys = mConnectionPools.keySet();
            Iterator<ConnectionInfo> it = keys.iterator();
            while ( it.hasNext() ) {
                ConnectionInfo key = it.next();
                ConnectionPool pool = mConnectionPools.get(key);
                if ( pool != null ) {
                	pool.setMaxPoolSize(size);
                }
            }
        }
    	
    }
    
    public static void setMaxIdleTimeout(long timeout){
        if ( mConnectionPools.size() > 0 ) {
            Set keys = mConnectionPools.keySet();
            Iterator<ConnectionInfo> it = keys.iterator();
            while ( it.hasNext() ) {
                ConnectionInfo key = it.next();
                ConnectionPool pool = mConnectionPools.get(key);
                if ( pool != null ) {
                	pool.setMaxIdleTimeout(timeout);
                }
            }
        }
    }
}
