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
 * @(#)HL7BCConnectionPool.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.connection;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * a connection pool based on linked list;
 * 
 * @author Raghunadh Teegavarapu
 */
public class HL7BCConnectionPool implements ConnectionPool {
    private static Logger mLogger = Logger.getLogger(HL7BCConnectionPool.class.getName());

    private final ConnectionInfo mConnectionKey;
    private int mMinSize = 2;
    private int mMaxSize = 32;
    private final LinkedList<Connection> mConnections;
    
    private HL7BCConnectionPool(ConnectionInfo key) {
        mConnectionKey = key;
        mConnections = new LinkedList();
    }

    private HL7BCConnectionPool(ConnectionInfo key, int minSize, int maxSize) {
        this(key);
        assert minSize >= 0 && maxSize > minSize : "When creating connection pool: max pool size > min pool size >= 0";
        mMinSize = minSize;
        mMaxSize = maxSize;
    }

    public static final ConnectionPool createConnectionPool(ConnectionInfo key) {
        return new HL7BCConnectionPool(key);
    }
    
    public static final ConnectionPool createConnectionPool(ConnectionInfo key, int minSize, int maxSize) {
        return new HL7BCConnectionPool(key, minSize, maxSize);
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
     * 
     * @param conn
     */
    public void returnConnection(Connection conn) {
        assert mConnections != null : "Connection pool not available when returnConnection(Connection conn) called, key=" + mConnectionKey;
    
        try {
            if(!conn.isConnected()){
            	conn.discard();
            }
        }
        catch (Exception ex) {
            // not a healthy connection, throw away
            conn.discard();
            return;
        }
        
        if ( mConnections.size() < mMaxSize ) {
            // set the cur timestamp before put it back
            conn.setLastUsed(System.currentTimeMillis());
            try {
                conn.startTimer();
            } catch (Exception ex) {
                // throw it away
                conn.discard();
                if ( mLogger.isLoggable(Level.FINE) )
                    mLogger.log(Level.FINE, "Exception when trying to stop timer in ConnectionPool.returnConnection()", ex);
                return;
            }
            if ( mLogger.isLoggable(Level.FINE))
                mLogger.log(Level.FINE, "Return the connection key =" + conn.getKey() + " to the linked list");
            mConnections.add(conn);
        }
        else {
            // throw away
            conn.discard();
            if ( mLogger.isLoggable(Level.FINE))
                mLogger.log(Level.FINE, "Max Size reached, throw away the connection =" + conn.getKey() + " connection=" + conn);
        }
    }

    /**
     * called by ConnectionManager to clean up the pool
     */
    public void cleanup() {
        while ( !mConnections.isEmpty() ) {
            Connection conn = mConnections.remove();
            if ( conn != null ) {
                conn.discard();
            }
        }
    }

	public void setMaxIdleTimeout(long timeout) {
		Iterator<Connection> it = mConnections.iterator();
		while(it.hasNext()){
			Connection conn = it.next();
			conn.setMaxIdleTimeout(timeout);
		}
		
	}
}
