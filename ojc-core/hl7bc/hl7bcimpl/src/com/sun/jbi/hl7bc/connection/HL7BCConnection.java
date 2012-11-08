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
 * @(#)HL7BCConnection.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.connection;

import com.sun.jbi.hl7bc.connection.Connection;
import com.sun.jbi.hl7bc.extservice.client.ConnectionHelper;

import java.io.ByteArrayOutputStream;
import java.util.Properties;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.mina.common.ConnectFuture;

import org.apache.mina.common.IoSession;
import org.apache.mina.common.IoHandler;

/**
 * @author Raghunadh
 */
public class HL7BCConnection implements Connection {
    private static Logger mLogger = Logger.getLogger(HL7BCConnection.class.getName());

    private ConnectFuture mConnectFuture;

    private long mMaxIdleTimeout = 60000; // wake up interval for a timer task - examing millis

    // elapsed since last use

    private long mLastUsed;

    private Timer mTimer;

    private TimerTask mTask;

    private ConnectionInfo key;

    private IoSession mSession;

    private IoHandler ioHandler;

    public HL7BCConnection() {
    };

    public void createConnection(ConnectionInfo cfg) throws Exception {

        long maxIdle = cfg.getMaxIdleTimeout();
        ConnectFuture connectFuture = null;
        try {
            ConnectionHelper connHelper = new ConnectionHelper();
            connectFuture = connHelper.getConnection(cfg);
            IoSession ioSession = connectFuture.getSession();
            ioSession.setAttribute("currentInput", new ByteArrayOutputStream());
            ioSession.setAttribute("readBytes", new Long(0));
            this.mSession = ioSession;

        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "Exception while creating connection, e=" + ex.getMessage(), ex);
            throw ex;
        }

        this.mConnectFuture = connectFuture;
        if (maxIdle > 0)
            setMaxIdleTimeout(maxIdle);
        this.key = cfg;
    }

    public void startTimer() throws Exception {
         assert mMaxIdleTimeout <= 100 : "Invalid value [" + mMaxIdleTimeout + "] for MaxIdleTimeout, should be integer >= 100 in milis";
            
         if ( mTask != null )
             mTask.cancel();
            
         if ( mTimer != null )
             mTimer.cancel();
            
         mTask = new MaxIdleTask(this);
         mTimer = new Timer();
         mTimer.schedule(mTask, 0, mMaxIdleTimeout);
   }
    
   public void stopTimer() {
        if (mTimer != null) {
            mTimer.cancel();
            mTimer = null;
        }
        if (mTask != null) {
            mTask.cancel();
            mTask = null;
        }
    }

   public ConnectionInfo getKey() {
        if (this.key == null)
            throw new IllegalArgumentException(
                    "Invalid operation, attempting to get connection object key before the client is properly instantiated.");
        return key;
    }

    public ConnectFuture getClientObject() {
        return mConnectFuture;
    }

    public void setClientObject(ConnectFuture connectFuture) {
        this.mConnectFuture = connectFuture;
    }

    public void setMaxIdleTimeout(long timeout) {
        mMaxIdleTimeout = timeout;
    }

    public IoSession getIOSessionObject() {
        return this.mSession;
    }

    public long getMaxIdleTimeout() {
        return mMaxIdleTimeout;
    }

    public void setLastUsed(long curTime) {
        mLastUsed = curTime;
    }

    public long getLastUsed() {
        return mLastUsed;
    }

    public void discard() {
        if (mConnectFuture != null) {

            // if ( mConnectFuture.getSession() != null ) {
            // mLogger.log(Level.FINE, "Discarding the connection");
            // mConnectFuture.getSession().close();
            // }
            if (getIOSessionObject() != null) {
                mLogger.log(Level.FINE, "Discarding the connection");
                getIOSessionObject().close();
            }
        }
        if (mTimer != null) {
            mTimer.cancel();
        }
        if (mTask != null) {
            mTask.cancel();
        }
    }

    private static long getMaxIdleTimeout(Properties cfg) {
        Integer l = (Integer) cfg.get(Connection.CONN_MAX_IDEL_TIMEOUT);
        if (l != null) {
            return l.longValue();
        }
        return -1;
    }

    public boolean isConnected() {
        return mConnectFuture.isConnected();
    }

    public IoHandler getIoHandler() {
        return ioHandler;
    }

    public void setIoHandler(IoHandler ioHandler) {
        this.ioHandler = ioHandler;
    }

}
