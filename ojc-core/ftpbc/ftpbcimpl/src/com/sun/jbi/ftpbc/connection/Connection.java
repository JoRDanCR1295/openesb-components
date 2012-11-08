/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc.connection;

import java.util.Properties;

/**
 *
 * @author jfu
 */
public interface Connection {
    // connection configuration params name

    public static final String CONN_MAX_IDEL_TIMEOUT = "CONN_MAX_IDEL_TIMEOUT";

    public void startTimer() throws Exception;

    public void stopTimer();

    public String getKey();

    public Object getClientObject();

    public void setMaxIdleTimeout(long timeout);

    public long getMaxIdleTimeout();

    public void setLastUsed(long curTime);

    public long getLastUsed();

    public void configure(Properties cfg) throws Exception;

    public void reset() throws Exception;

    public void discard();
}
