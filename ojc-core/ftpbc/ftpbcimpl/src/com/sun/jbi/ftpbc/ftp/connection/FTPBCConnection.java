/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc.ftp.connection;

import com.sun.jbi.ftpbc.connection.Connection;
import com.sun.jbi.ftpbc.ftp.FtpInterface;
import com.sun.jbi.ftpbc.ftp.exception.ConfigurationException;
import com.sun.jbi.ftpbc.ftp.exception.FtpFileException;
import com.sun.jbi.ftpbc.ftp.exception.FtpInterfaceException;

import com.sun.jbi.internationalization.Messages;

import java.util.Properties;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author jfu
 */
public class FTPBCConnection implements Connection {

    private static final Messages mMessages =
            Messages.getMessages(FTPBCConnection.class);
    private static Logger mLogger = Messages.getLogger(FTPBCConnection.class);
    private FtpInterface mInterface; // represents the client to ecternal system
    private long mMaxIdleTimeout = 60000; // wake up interval for a timer task - examing millis elapsed since last use
    private long mLastUsed;
    private Timer mTimer;
    private TimerTask mTask;

    // private constructor for factory method createConnection()
    private FTPBCConnection() {
    }

    ;

    // factory method
    public static final Connection createConnection(Properties cfg) {
        FTPBCConnection conn = null;
        FtpInterface intf = null;

        long maxIdle = getMaxIdleTimeout(cfg);

        try {
            conn = new FTPBCConnection();
            intf = new FtpInterface();
            intf.initialize(cfg);
        } catch (FtpInterfaceException ex) {
            mLogger.log(Level.SEVERE, "Exception when initializing FTP Client Object, e=" + ex.getMessage(), ex);
        } catch (ConfigurationException ex) {
            mLogger.log(Level.SEVERE, "Exception when initializing FTP Client Object, e=" + ex.getMessage(), ex);
        }

        assert conn != null && intf != null : "Error occurred when creating Connection object...";

        conn.setClientObject(intf);
        if (maxIdle > 0) {
            conn.setMaxIdleTimeout(maxIdle);
        }
        return conn;
    }

    public void startTimer() throws Exception {
        assert mInterface == null : "Client object not available when startTimer() called...";
        assert mMaxIdleTimeout <= 100 : "Invalid value [" + mMaxIdleTimeout + "] for MaxIdleTimeout, should be integer >= 100 in milis";

        if (mTask != null) {
            mTask.cancel();
        }

        if (mTimer != null) {
            mTimer.cancel();
        }

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

    public String getKey() {
        if (mInterface == null) {
            throw new IllegalArgumentException("Invalid operation, attempting to get connection object key before the client is properly instantiated.");
        }
        String key = null;
        try {
            key = mInterface.getConfiguration().getKey();
        } catch (Exception ex) {
            Logger.getLogger(FTPBCConnection.class.getName()).log(Level.SEVERE, null, ex);
        }
        return key;
    }

    public Object getClientObject() {
        return mInterface;
    }

    public void setClientObject(Object client) {
        mInterface = (FtpInterface) client;
    }

    public void setMaxIdleTimeout(long timeout) {
        mMaxIdleTimeout = timeout;
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

    public void reset() throws Exception {
        try {
            mInterface.reset();
        } catch (FtpFileException ex) {
            throw new Exception("Exception when reset ClientObject [" + mInterface.getClass().getName() + "], ex=" + ex.getMessage(), ex);
        }
    }

    public void configure(Properties cfg) throws Exception {
        try {
            mInterface.getConfiguration().initialConfigValues(cfg);
        } catch (FtpFileException ex) {
            throw new Exception("Exception when setting ClientObject [" + mInterface.getClass().getName() + "] of a pooled connection with configuration parameters, ex=" + ex.getMessage(), ex);
        }
    }

    public void discard() {
        if (mInterface != null) {
            try {
                mInterface.reset();
            } catch (FtpInterfaceException ex) {
                // ignore
            }
            if (mInterface.getClient() != null) {
                mInterface.getClient().close();
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
}
