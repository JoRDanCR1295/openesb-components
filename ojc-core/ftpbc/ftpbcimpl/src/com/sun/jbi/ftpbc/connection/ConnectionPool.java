/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc.connection;

/**
 *
 * @author jfu
 */
public interface ConnectionPool {
    // pool configuration params name

    public static final String POOL_MIN_SIZE = "POOL_MIN_SIZE";
    public static final String POOL_MAX_SIZE = "POOL_MAX_SIZE";

    public void setMaxPoolSize(int size);

    public int getMaxPoolSize();

    public void setMinPoolSize(int size);

    public int getMinPoolSize();

    public boolean addConnection(Connection conn);

    public boolean removeConnection(Connection conn);

    public Connection getConnection();

    public void returnConnection(Connection conn);

    public void cleanup();
}
