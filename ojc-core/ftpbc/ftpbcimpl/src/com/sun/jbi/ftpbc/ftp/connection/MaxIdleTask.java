/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc.ftp.connection;

import com.sun.jbi.ftpbc.connection.Connection;
import com.sun.jbi.internationalization.Messages;

import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Wake up at the interval of MaxIdleTimeout
 * and check if the connection is used within previous MaxIdleTimeout millis
 * if yes, 
 * if no, evict the connection from the pool
 * @author jfu
 */
public class MaxIdleTask extends TimerTask {

    private static final Messages mMessages =
            Messages.getMessages(MaxIdleTask.class);
    private static Logger mLogger = Messages.getLogger(MaxIdleTask.class);
    private Connection mConn;

    public MaxIdleTask(Connection conn) {
        mConn = conn;
    }

    public void run() {
        try {
            FTPBCConnectionManager.checkConnection(mConn);
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "Exception removing connection from the connection pool, e=" + ex.getMessage(), ex);
        }
    }
}
