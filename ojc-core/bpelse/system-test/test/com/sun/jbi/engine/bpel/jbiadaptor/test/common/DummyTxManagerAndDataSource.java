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
 * @(#)DummyTxManagerAndDataSource.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.io.PrintWriter;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Savepoint;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import javax.sql.DataSource;
import javax.transaction.HeuristicMixedException;
import javax.transaction.HeuristicRollbackException;
import javax.transaction.InvalidTransactionException;
import javax.transaction.NotSupportedException;
import javax.transaction.RollbackException;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;

import com.sun.jbi.engine.bpel.core.bpel.connection.AbstractDBConnection;
import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnection;
import com.sun.jbi.engine.bpel.core.bpel.connection.DBNonXAConnection;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.util.Utility;
import com.sun.jbi.engine.bpel.jbiadaptor.test.persistence.EngineDriver;

/**
 * @author Sun Microsystems
 */
public class DummyTxManagerAndDataSource implements DataSource, TransactionManager{

    protected Map mTxMap = Collections.synchronizedMap(new HashMap());
    private Map mXAconnectionMap = Collections.synchronizedMap(new HashMap());
    
    private List suspendedTxList = Collections.synchronizedList(new ArrayList());    

    Properties mDBProperties;
    /** is changed by subclass  */
    protected boolean mNonXAFlag = false;
    protected boolean mIsClusteredTests = false;
    
    public DummyTxManagerAndDataSource (Properties properties){
        
        //Instantiate a DataSource based on the properties supplied
        try {
            initializeDataSource(properties);
        } catch (Exception e) {
            throw new RuntimeException (e);
        }
        if (!Utility.isEmpty(System.getProperty(Engine.IS_CLUSTERED))) {
            mIsClusteredTests = Boolean.getBoolean(Engine.IS_CLUSTERED);
        }
    }
    
    /**
     * @see javax.transaction.TransactionManager#begin()
     */
    public void begin() throws NotSupportedException, SystemException {
        DummyTransaction tx = new DummyTransaction(this);

        Connection connection = (Connection) mXAconnectionMap.remove(Thread.currentThread().getName());
        if(connection == null) {
            try {
                connection = getConnection();
            } catch (SQLException ex) {
                ex.printStackTrace();
                throw new RuntimeException(ex);
            }
        }
        tx.setConnection(connection);

        mTxMap.put(Thread.currentThread().getName(), tx);
    }
    
    /**
     * @see javax.transaction.TransactionManager#getTransaction()
     */
    public Transaction getTransaction() throws SystemException {
        return (Transaction) this.mTxMap.get(Thread.currentThread().getName());
    }
    
    /**
     * @see javax.transaction.TransactionManager#suspend()
     */
    public Transaction suspend() throws SystemException {
        Transaction tx = (Transaction) mTxMap.remove(Thread.currentThread().getName());
        if (tx == null) {
            throw new SystemException("No transaction associated with the Thread");
        }
        
        suspendedTxList.add(tx);
        return tx;
    }
    

    /**
     * @see javax.transaction.TransactionManager#resume(javax.transaction.Transaction)
     */
    public void resume(Transaction arg0) throws InvalidTransactionException, IllegalStateException, SystemException {
        boolean wasRemoved =  suspendedTxList.remove(arg0);
        
        if(wasRemoved) {
            mTxMap.put(Thread.currentThread().getName(), arg0);
        } else {
            throw new IllegalStateException ("Exception during Resume. The transaction does not exist."); 
        }
    }
    
    private static Map connectionPool = Collections.synchronizedMap(new HashMap());
    
    /**
     * @see javax.sql.DataSource#getConnection(java.lang.String, java.lang.String)
     */
    public Connection getConnection(String username, String password) throws SQLException {
        
        Connection connection;
        if (mNonXAFlag) {
            Connection con = DriverManager.getConnection(mDBProperties.getProperty(
                    ConnectionProperties.DB_URL), username, password);
            connection = new DummyNonXAConnection(con);
        } else {
            connection = (Connection) connectionPool.get(Thread.currentThread().getName());
            if(connection == null || connection.isClosed()) {
                Connection con = DriverManager.getConnection(mDBProperties.getProperty(
                        ConnectionProperties.DB_URL),
                        username,
                        password);

                if (mNonXAFlag) {
                } else {
                    connection = new DummyXAConnection(con);
                }
                if (!Utility.isEmpty(System.getProperty(EngineDriver.RECOVERY_TESTS_FLAG))) {
                    // if in recovery test mode
                    connection = new TestDBConnImpl((AbstractDBConnection) connection);
                }
                connection.setAutoCommit(false);        
                if(!mIsClusteredTests) {
                    connectionPool.put(Thread.currentThread().getName(), connection);
                }
            }
            DummyTransaction tx = (DummyTransaction) mTxMap.get(Thread.currentThread().getName());
            
            if(tx != null) {
                tx.setConnection(connection);
            } else {
                //Store the mConn temporarily
                mXAconnectionMap.put(Thread.currentThread().getName(), connection);
            } 
        }
        
        return connection;
    }
    
    public Connection getNonTxConnection() throws SQLException {
        
        return DriverManager.getConnection(mDBProperties.getProperty(
                ConnectionProperties.DB_URL),
                mDBProperties.getProperty(ConnectionProperties.DB_USERNAME),
                mDBProperties.getProperty(ConnectionProperties.DB_PASSWORD));
    }
    
    public void destroyConnectionPool(){
        Set keySet = connectionPool.keySet();
        Iterator iter = keySet.iterator();
        for(;iter.hasNext();){
            String threadName = (String)iter.next();
            Connection connection = (Connection)connectionPool.get(threadName);
            System.out.println("--Before calling close mConn");            
            try {
                if (!connection.isClosed()) {
                    connection.close();
                }
            } catch (SQLException e) {
                throw new RuntimeException(e);
            }
        }
        connectionPool.clear();
        System.out.println("--Cleared all the connections");                    
    }
    
    private void initializeDataSource(Properties properties) throws Exception {
        mDBProperties = properties;
        DriverManager.registerDriver(new org.apache.derby.jdbc.ClientDriver());
        DriverManager.registerDriver(new com.mysql.jdbc.Driver());
        DriverManager.registerDriver(new oracle.jdbc.OracleDriver());
    }    
    
    /**
     * @see javax.transaction.TransactionManager#commit()
     */
    public void commit() throws RollbackException, HeuristicMixedException, HeuristicRollbackException, SecurityException, IllegalStateException, SystemException {
    }

    /**
     * @see javax.transaction.TransactionManager#getStatus()
     */
    public int getStatus() throws SystemException {
        return 0;
    }

    /**
     * @see javax.transaction.TransactionManager#rollback()
     */
    public void rollback() throws IllegalStateException, SecurityException, SystemException {
    }

    /**
     * @see javax.transaction.TransactionManager#setRollbackOnly()
     */
    public void setRollbackOnly() throws IllegalStateException, SystemException {
    }

    /**
     * @see javax.transaction.TransactionManager#setTransactionTimeout(int)
     */
    public void setTransactionTimeout(int arg0) throws SystemException {
    }

    /**
     * @see javax.sql.DataSource#getConnection()
     */
    public Connection getConnection() throws SQLException {
        Connection connection;
        if (mNonXAFlag) {
            Connection con = DriverManager.getConnection(mDBProperties.getProperty(
                    ConnectionProperties.DB_URL),
                    mDBProperties.getProperty(ConnectionProperties.DB_USERNAME),
                    mDBProperties.getProperty(ConnectionProperties.DB_PASSWORD));
            connection = new DummyNonXAConnection(con);
            if (!Utility.isEmpty(System.getProperty(EngineDriver.RECOVERY_TESTS_FLAG))) {
                // if in recovery test mode
                connection = new TestDBConnImpl((AbstractDBConnection) connection);
            }
        } else {
            connection = (Connection)connectionPool.get(Thread.currentThread().getName());

            if(connection == null || connection.isClosed()) {

                Connection con = DriverManager.getConnection(mDBProperties.getProperty(
                        ConnectionProperties.DB_URL),
                        mDBProperties.getProperty(ConnectionProperties.DB_USERNAME),
                        mDBProperties.getProperty(ConnectionProperties.DB_PASSWORD));

                if (mNonXAFlag) {
                    connection = new DummyNonXAConnection(con);
                } else {
                    connection = new DummyXAConnection(con);
                }
                if (!Utility.isEmpty(System.getProperty(EngineDriver.RECOVERY_TESTS_FLAG))) {
                    // if in recovery test mode
                    connection = new TestDBConnImpl((AbstractDBConnection) connection);
                }
                connection.setAutoCommit(false);        
                if(!mIsClusteredTests) {
                    connectionPool.put(Thread.currentThread().getName(), connection);
                }
            }
            DummyTransaction tx = (DummyTransaction) mTxMap.get(Thread.currentThread().getName());

            if(tx != null) {
                tx.setConnection(connection);
            } else {
                //Store the mConn temporarily
                mXAconnectionMap.put(Thread.currentThread().getName(), connection);
            } 


        }
        return connection;
    }

    /**
     * @see javax.sql.DataSource#getLoginTimeout()
     */
    public int getLoginTimeout() throws SQLException {
        return 0;
    }

    /**
     * @see javax.sql.DataSource#getLogWriter()
     */
    public PrintWriter getLogWriter() throws SQLException {
        return null;
    }

    /**
     * @see javax.sql.DataSource#setLoginTimeout(int)
     */
    public void setLoginTimeout(int seconds) throws SQLException {
    }

    /**
     * @see javax.sql.DataSource#setLogWriter(java.io.PrintWriter)
     */
    public void setLogWriter(PrintWriter out) throws SQLException {
    }
}

class DummyNonXAConnection extends DBNonXAConnection implements Connection {
   
    DummyNonXAConnection(Connection connection) throws SQLException {
        super(connection);
    }
    
    /* (non-Javadoc)
     * @see java.sql.Connection#clearWarnings()
     */
    public void clearWarnings() throws SQLException {
        
        mConn.clearWarnings();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#commit()
     */
    public void commit() throws SQLException {
        
        mConn.commit();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#createStatement()
     */
    public Statement createStatement() throws SQLException {
        
        return mConn.createStatement();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#createStatement(int, int, int)
     */
    public Statement createStatement(int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {
        
        return mConn.createStatement(resultSetType, resultSetConcurrency, resultSetHoldability);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#createStatement(int, int)
     */
    public Statement createStatement(int resultSetType, int resultSetConcurrency) throws SQLException {
        
        return mConn.createStatement(resultSetType, resultSetConcurrency);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getAutoCommit()
     */
    public boolean getAutoCommit() throws SQLException {
        
        return mConn.getAutoCommit();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getCatalog()
     */
    public String getCatalog() throws SQLException {
        
        return mConn.getCatalog();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getHoldability()
     */
    public int getHoldability() throws SQLException {
        
        return mConn.getHoldability();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getMetaData()
     */
    public DatabaseMetaData getMetaData() throws SQLException {
        
        return mConn.getMetaData();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getTransactionIsolation()
     */
    public int getTransactionIsolation() throws SQLException {
        
        return mConn.getTransactionIsolation();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getTypeMap()
     */
    public Map<String, Class<?>> getTypeMap() throws SQLException {
        
        return mConn.getTypeMap();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getWarnings()
     */
    public SQLWarning getWarnings() throws SQLException {
        
        return mConn.getWarnings();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#isClosed()
     */
    public boolean isClosed() throws SQLException {
        
        return mConn.isClosed();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#isReadOnly()
     */
    public boolean isReadOnly() throws SQLException {
        
        return mConn.isReadOnly();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#nativeSQL(java.lang.String)
     */
    public String nativeSQL(String sql) throws SQLException {
        
        return mConn.nativeSQL(sql);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareCall(java.lang.String, int, int, int)
     */
    public CallableStatement prepareCall(String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {
        
        return mConn.prepareCall(sql, resultSetType, resultSetConcurrency, resultSetHoldability);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareCall(java.lang.String, int, int)
     */
    public CallableStatement prepareCall(String sql, int resultSetType, int resultSetConcurrency) throws SQLException {
        
        return mConn.prepareCall(sql, resultSetType, resultSetConcurrency);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareCall(java.lang.String)
     */
    public CallableStatement prepareCall(String sql) throws SQLException {
        
        return mConn.prepareCall(sql);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, int, int, int)
     */
    public PreparedStatement prepareStatement(String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {
        
        return mConn.prepareStatement(sql, resultSetType, resultSetConcurrency, resultSetHoldability);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, int, int)
     */
    public PreparedStatement prepareStatement(String sql, int resultSetType, int resultSetConcurrency) throws SQLException {
        
        return mConn.prepareStatement(sql, resultSetType, resultSetConcurrency);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, int)
     */
    public PreparedStatement prepareStatement(String sql, int autoGeneratedKeys) throws SQLException {
        
        return mConn.prepareStatement(sql, autoGeneratedKeys);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, int[])
     */
    public PreparedStatement prepareStatement(String sql, int[] columnIndexes) throws SQLException {
        
        return mConn.prepareStatement(sql, columnIndexes);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, java.lang.String[])
     */
    public PreparedStatement prepareStatement(String sql, String[] columnNames) throws SQLException {
        
        return mConn.prepareStatement(sql, columnNames);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String)
     */
    public PreparedStatement prepareStatement(String sql) throws SQLException {
        
        return mConn.prepareStatement(sql);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#releaseSavepoint(java.sql.Savepoint)
     */
    public void releaseSavepoint(Savepoint savepoint) throws SQLException {
        
        mConn.releaseSavepoint(savepoint);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#rollback()
     */
    public void rollback() throws SQLException {
        
        mConn.rollback();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#rollback(java.sql.Savepoint)
     */
    public void rollback(Savepoint savepoint) throws SQLException {
        
        mConn.releaseSavepoint(savepoint);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setAutoCommit(boolean)
     */
    public void setAutoCommit(boolean autoCommit) throws SQLException {
        
        mConn.setAutoCommit(autoCommit);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setCatalog(java.lang.String)
     */
    public void setCatalog(String catalog) throws SQLException {
        
        mConn.setCatalog(catalog);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setHoldability(int)
     */
    public void setHoldability(int holdability) throws SQLException {
        
        mConn.setHoldability(holdability);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setReadOnly(boolean)
     */
    public void setReadOnly(boolean readOnly) throws SQLException {
        
        mConn.setReadOnly(readOnly);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setSavepoint()
     */
    public Savepoint setSavepoint() throws SQLException {
        
        return mConn.setSavepoint();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setSavepoint(java.lang.String)
     */
    public Savepoint setSavepoint(String name) throws SQLException {
        
        return mConn.setSavepoint(name);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setTransactionIsolation(int)
     */
    public void setTransactionIsolation(int level) throws SQLException {
        
        mConn.setTransactionIsolation(level);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setTypeMap(java.util.Map)
     */
    public void setTypeMap(Map<String, Class<?>> arg0) throws SQLException {
        
        mConn.setTypeMap(arg0);
    }
}

class DummyXAConnection extends DBConnection implements Connection {

    DummyXAConnection(Connection connection) throws SQLException {
        super(connection);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#clearWarnings()
     */
    public void clearWarnings() throws SQLException {

        mConn.clearWarnings();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#close()
     */
    public void close() throws SQLException {

        //Ignore
    }

    public void release() throws SQLException {

        mConn.close();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#commit()
     */
    public void commit() throws SQLException {

        mConn.commit();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#createStatement()
     */
    public Statement createStatement() throws SQLException {

        return mConn.createStatement();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#createStatement(int, int, int)
     */
    public Statement createStatement(int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {

        return mConn.createStatement(resultSetType, resultSetConcurrency, resultSetHoldability);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#createStatement(int, int)
     */
    public Statement createStatement(int resultSetType, int resultSetConcurrency) throws SQLException {

        return mConn.createStatement(resultSetType, resultSetConcurrency);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getAutoCommit()
     */
    public boolean getAutoCommit() throws SQLException {

        return mConn.getAutoCommit();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getCatalog()
     */
    public String getCatalog() throws SQLException {

        return mConn.getCatalog();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getHoldability()
     */
    public int getHoldability() throws SQLException {

        return mConn.getHoldability();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getMetaData()
     */
    public DatabaseMetaData getMetaData() throws SQLException {

        return mConn.getMetaData();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getTransactionIsolation()
     */
    public int getTransactionIsolation() throws SQLException {

        return mConn.getTransactionIsolation();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getTypeMap()
     */
    public Map<String, Class<?>> getTypeMap() throws SQLException {

        return mConn.getTypeMap();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getWarnings()
     */
    public SQLWarning getWarnings() throws SQLException {

        return mConn.getWarnings();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#isClosed()
     */
    public boolean isClosed() throws SQLException {

        return mConn.isClosed();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#isReadOnly()
     */
    public boolean isReadOnly() throws SQLException {

        return mConn.isReadOnly();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#nativeSQL(java.lang.String)
     */
    public String nativeSQL(String sql) throws SQLException {

        return mConn.nativeSQL(sql);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareCall(java.lang.String, int, int, int)
     */
    public CallableStatement prepareCall(String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {

        return mConn.prepareCall(sql, resultSetType, resultSetConcurrency, resultSetHoldability);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareCall(java.lang.String, int, int)
     */
    public CallableStatement prepareCall(String sql, int resultSetType, int resultSetConcurrency) throws SQLException {

        return mConn.prepareCall(sql, resultSetType, resultSetConcurrency);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareCall(java.lang.String)
     */
    public CallableStatement prepareCall(String sql) throws SQLException {

        return mConn.prepareCall(sql);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, int, int, int)
     */
    public PreparedStatement prepareStatement(String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {

        return mConn.prepareStatement(sql, resultSetType, resultSetConcurrency, resultSetHoldability);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, int, int)
     */
    public PreparedStatement prepareStatement(String sql, int resultSetType, int resultSetConcurrency) throws SQLException {

        return mConn.prepareStatement(sql, resultSetType, resultSetConcurrency);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, int)
     */
    public PreparedStatement prepareStatement(String sql, int autoGeneratedKeys) throws SQLException {

        return mConn.prepareStatement(sql, autoGeneratedKeys);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, int[])
     */
    public PreparedStatement prepareStatement(String sql, int[] columnIndexes) throws SQLException {

        return mConn.prepareStatement(sql, columnIndexes);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, java.lang.String[])
     */
    public PreparedStatement prepareStatement(String sql, String[] columnNames) throws SQLException {

        return mConn.prepareStatement(sql, columnNames);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String)
     */
    public PreparedStatement prepareStatement(String sql) throws SQLException {

        return mConn.prepareStatement(sql);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#releaseSavepoint(java.sql.Savepoint)
     */
    public void releaseSavepoint(Savepoint savepoint) throws SQLException {

        mConn.releaseSavepoint(savepoint);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#rollback()
     */
    public void rollback() throws SQLException {

        mConn.rollback();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#rollback(java.sql.Savepoint)
     */
    public void rollback(Savepoint savepoint) throws SQLException {

        mConn.releaseSavepoint(savepoint);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setAutoCommit(boolean)
     */
    public void setAutoCommit(boolean autoCommit) throws SQLException {

        mConn.setAutoCommit(autoCommit);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setCatalog(java.lang.String)
     */
    public void setCatalog(String catalog) throws SQLException {

        mConn.setCatalog(catalog);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setHoldability(int)
     */
    public void setHoldability(int holdability) throws SQLException {

        mConn.setHoldability(holdability);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setReadOnly(boolean)
     */
    public void setReadOnly(boolean readOnly) throws SQLException {

        mConn.setReadOnly(readOnly);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setSavepoint()
     */
    public Savepoint setSavepoint() throws SQLException {

        return mConn.setSavepoint();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setSavepoint(java.lang.String)
     */
    public Savepoint setSavepoint(String name) throws SQLException {

        return mConn.setSavepoint(name);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setTransactionIsolation(int)
     */
    public void setTransactionIsolation(int level) throws SQLException {

        mConn.setTransactionIsolation(level);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setTypeMap(java.util.Map)
     */
    public void setTypeMap(Map<String, Class<?>> arg0) throws SQLException {

        mConn.setTypeMap(arg0);
    }
}
