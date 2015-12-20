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

package com.sun.jbi.engine.bpel.monitor.tool.db;

import java.io.PrintWriter;
import java.sql.Array;
import java.sql.Blob;
import java.sql.CallableStatement;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.NClob;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLClientInfoException;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.Savepoint;
import java.sql.SQLXML;
import java.sql.Statement;
import java.sql.Struct;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;
import java.util.concurrent.Executor;

import javax.sql.DataSource;
import javax.transaction.HeuristicMixedException;
import javax.transaction.HeuristicRollbackException;
import javax.transaction.InvalidTransactionException;
import javax.transaction.NotSupportedException;
import javax.transaction.RollbackException;
import javax.transaction.Status;
import javax.transaction.Synchronization;
import javax.transaction.SystemException;
import javax.transaction.Transaction;
import javax.transaction.TransactionManager;
import javax.transaction.xa.XAResource;

/**
 * @author Sun Microsystems
 */
public class DummyTxManagerAndDataSource implements DataSource, TransactionManager{

    private HashMap txMap = new HashMap();
    private HashMap testDBConnImplMap = new HashMap();
    private HashMap connectionMap = new HashMap();
    
    private ArrayList suspendedTxList = new ArrayList();    
    
    private String dbUrl;
    Properties mDBProperties;
    
    public DummyTxManagerAndDataSource (Properties properties){
        
        //Instantiate a DataSource based on the properties supplied
        try {
            initializeDataSource(properties);
        } catch (Exception e) {
            throw new RuntimeException (e);
        }
    }
    
    /**
     * @see javax.transaction.TransactionManager#begin()
     */
    public void begin() throws NotSupportedException, SystemException {
        DummyTransaction tx = new DummyTransaction(this);
        
        Connection connection = (Connection) connectionMap.remove(Thread.currentThread().getName());
        if(connection != null) {
            tx.setConnection(connection);
        }
        
        Connection testDBConnImpl = (Connection) testDBConnImplMap.remove(Thread.currentThread().getName());
        if(testDBConnImpl != null) {
            tx.setConnection(testDBConnImpl);
        }        
        
        this.txMap.put(Thread.currentThread().getName(), tx);
    }
    
    /**
     * @see javax.transaction.TransactionManager#getTransaction()
     */
    public Transaction getTransaction() throws SystemException {
        return (Transaction) this.txMap.get(Thread.currentThread().getName());
    }
    
    /**
     * @see javax.transaction.TransactionManager#suspend()
     */
    public Transaction suspend() throws SystemException {
        Transaction tx = (Transaction) txMap.remove(Thread.currentThread().getName());
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
            Connection connection = (Connection) connectionMap.remove(Thread.currentThread().getName());
            if(connection != null) {
                ((DummyTransaction)arg0).setConnection(connection);
            }
            
            Connection testDBConnImpl = (Connection) testDBConnImplMap.remove(Thread.currentThread().getName());
            if(testDBConnImpl != null) {
                ((DummyTransaction)arg0).setConnection(testDBConnImpl);
            }
            txMap.put(Thread.currentThread().getName(), arg0);
        } else {
            throw new IllegalStateException ("The transaction does not exist."); 
        }
    }
    
    private static Map connectionPool = Collections.synchronizedMap(new HashMap());
    
    /**
     * @see javax.sql.DataSource#getConnection(java.lang.String, java.lang.String)
     */
    public Connection getConnection(String username, String password) throws SQLException {
        
        Connection connection = (Connection)connectionPool.get(Thread.currentThread().getName());
        if(connection == null || connection.isClosed()) {
            Connection con = DriverManager.getConnection(mDBProperties.getProperty(
                    ConnectionProperties.DB_URL),
                    username,
                    password);
            connection = new DummyConnection (con);
            connectionPool.put(Thread.currentThread().getName(), connection);
        }

        DummyTransaction tx = (DummyTransaction) txMap.get(Thread.currentThread().getName());
        connection.setAutoCommit(false);        
        
        if(tx != null) {
            tx.setConnection(connection);
        } else {
            //Store the connection temporarily
            this.connectionMap.put(Thread.currentThread().getName(), connection);
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
            DummyConnection connection = (DummyConnection)connectionPool.get(threadName);
            System.out.println("--Before calling close connection");            
            try {
                connection.release();
            } catch (SQLException e) {
                throw new RuntimeException(e);
            }
        }
        connectionPool.clear();
        System.out.println("--Cleared all the connections");                    
    }
    
    public void registerTestDBConnImpl(Connection testDBConnImpl){
        DummyTransaction tx = (DummyTransaction) txMap.get(Thread.currentThread().getName());
        
        if(tx != null) {
            tx.setConnection(testDBConnImpl);
        } else {
            //Store the connection temporarily
            this.testDBConnImplMap.put(Thread.currentThread().getName(), testDBConnImpl);
        } 
    }
    
    private void initializeDataSource(Properties properties) throws Exception {
        mDBProperties = properties;
//        DriverManager.registerDriver(new org.apache.derby.jdbc.ClientDriver());
        DriverManager.registerDriver((Driver) Class.forName(mDBProperties.getProperty(ConnectionProperties.DB_DRIVER_CLASS_NAME)).newInstance());//        
//        new oracle.jdbc.OracleDriver());        
        dbUrl = properties.getProperty(ConnectionProperties.DB_URL);
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
        Connection connection = (Connection)connectionPool.get(Thread.currentThread().getName());
        if(connection == null || connection.isClosed()) {
            
            Connection con = DriverManager.getConnection(mDBProperties.getProperty(
                    ConnectionProperties.DB_URL),
                    mDBProperties.getProperty(ConnectionProperties.DB_USERNAME),
                    mDBProperties.getProperty(ConnectionProperties.DB_PASSWORD));
            connection = new DummyConnection (con);
            connectionPool.put(Thread.currentThread().getName(), connection);
        }

        DummyTransaction tx = (DummyTransaction) txMap.get(Thread.currentThread().getName());
        connection.setAutoCommit(false);        
        
        if(tx != null) {
            tx.setConnection(connection);
        } else {
            //Store the connection temporarily
            this.connectionMap.put(Thread.currentThread().getName(), connection);
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

    //@Override
    public Logger getParentLogger() {
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
    
    public void cleanup(){
        this.txMap.remove(Thread.currentThread().getName());
    }

    public boolean isWrapperFor( Class<?> iface ) throws SQLException {
        throw new UnsupportedOperationException();
    }
    
    /* (non-Javadoc)
     * @see java.sql.Wrapper#unwrap(java.lang.Class)
     */
    public <T> T unwrap( Class<T> iface ) throws SQLException {
        throw new UnsupportedOperationException();
    }
}

class DummyTransaction implements Transaction {
    
    private DummyTxManagerAndDataSource dummyTMAndDS;
    
    DummyTransaction (DummyTxManagerAndDataSource dummyTMAndDS) {
        this.dummyTMAndDS = dummyTMAndDS;
    }
    
    private Connection connection = null;
    private Synchronization synchronization;
    
    /**
     * @param connection the connection to set
     */
    public void setConnection(Connection connection) {
        this.connection = connection;
    }

    /**
     * @see javax.transaction.Transaction#commit()
     */
    public void commit() throws RollbackException, HeuristicMixedException, HeuristicRollbackException, SecurityException, IllegalStateException, SystemException {
        
        try {

            if (connection != null) {
                connection.commit();
            }
            
            if (synchronization != null){
                synchronization.afterCompletion(Status.STATUS_COMMITTED);
            }
            
            this.dummyTMAndDS.cleanup();

        } catch (SQLException e) {
            throw new RuntimeException (e);
        }
    }

    /**
     * @see javax.transaction.Transaction#rollback()
     */
    public void rollback() throws IllegalStateException, SystemException {
        try {
            
            if (connection != null) {
                this.dummyTMAndDS.cleanup();                
                connection.rollback();
            }
            
            if (synchronization != null){
                synchronization.afterCompletion(Status.STATUS_ROLLEDBACK);            
            }
            
        } catch (SQLException e) {
            throw new RuntimeException (e);
        }        
    }

    /**
     * @see javax.transaction.Transaction#registerSynchronization(javax.transaction.Synchronization)
     */
    public void registerSynchronization(Synchronization arg0) throws RollbackException, IllegalStateException, SystemException {
        synchronization = arg0;
    }
    
    /**
     * @see javax.transaction.Transaction#delistResource(javax.transaction.xa.XAResource, int)
     */
    public boolean delistResource(XAResource arg0, int arg1) throws IllegalStateException, SystemException {
        return false;
    }

    /**
     * @see javax.transaction.Transaction#enlistResource(javax.transaction.xa.XAResource)
     */
    public boolean enlistResource(XAResource arg0) throws RollbackException, IllegalStateException, SystemException {
        return false;
    }

    /**
     * @see javax.transaction.Transaction#getStatus()
     */
    public int getStatus() throws SystemException {
        return 0;
    }

    /**
     * @see javax.transaction.Transaction#setRollbackOnly()
     */
    public void setRollbackOnly() throws IllegalStateException, SystemException {
    }
}

class DummyConnection implements Connection {

    private Connection connection;
    
    DummyConnection(Connection connection) {
        this.connection = connection;
    }
    
    /* (non-Javadoc)
     * @see java.sql.Connection#clearWarnings()
     */
    public void clearWarnings() throws SQLException {
        
        connection.clearWarnings();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#abort()
     */
    public void abort(Executor executor) throws SQLException {
        
        //Ignore
    }
    
    /* (non-Javadoc)
     * @see java.sql.Connection#close()
     */
    public void close() throws SQLException {
        
        //Ignore
    }
    
    public void release() throws SQLException {
        
        connection.close();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#commit()
     */
    public void commit() throws SQLException {
        
        connection.commit();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#createStatement()
     */
    public Statement createStatement() throws SQLException {
        
        return connection.createStatement();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#createStatement(int, int, int)
     */
    public Statement createStatement(int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {
        
        return connection.createStatement(resultSetType, resultSetConcurrency, resultSetHoldability);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#createStatement(int, int)
     */
    public Statement createStatement(int resultSetType, int resultSetConcurrency) throws SQLException {
        
        return connection.createStatement(resultSetType, resultSetConcurrency);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getAutoCommit()
     */
    public boolean getAutoCommit() throws SQLException {
        
        return connection.getAutoCommit();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getCatalog()
     */
    public String getCatalog() throws SQLException {
        
        return connection.getCatalog();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getHoldability()
     */
    public int getHoldability() throws SQLException {
        
        return connection.getHoldability();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getMetaData()
     */
    public DatabaseMetaData getMetaData() throws SQLException {
        
        return connection.getMetaData();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getNetworkTimeout()
     */
    public int getNetworkTimeout() throws SQLException {
        
        return connection.getNetworkTimeout();
    }
    
    public String getSchema() throws SQLException {
        
        return connection.getSchema();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getTransactionIsolation()
     */
    public int getTransactionIsolation() throws SQLException {
        
        return connection.getTransactionIsolation();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getTypeMap()
     */
    public Map<String, Class<?>> getTypeMap() throws SQLException {
        
        return connection.getTypeMap();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#getWarnings()
     */
    public SQLWarning getWarnings() throws SQLException {
        
        return connection.getWarnings();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#isClosed()
     */
    public boolean isClosed() throws SQLException {
        
        return connection.isClosed();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#isReadOnly()
     */
    public boolean isReadOnly() throws SQLException {
        
        return connection.isReadOnly();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#nativeSQL(java.lang.String)
     */
    public String nativeSQL(String sql) throws SQLException {
        
        return connection.nativeSQL(sql);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareCall(java.lang.String, int, int, int)
     */
    public CallableStatement prepareCall(String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {
        
        return connection.prepareCall(sql, resultSetType, resultSetConcurrency, resultSetHoldability);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareCall(java.lang.String, int, int)
     */
    public CallableStatement prepareCall(String sql, int resultSetType, int resultSetConcurrency) throws SQLException {
        
        return connection.prepareCall(sql, resultSetType, resultSetConcurrency);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareCall(java.lang.String)
     */
    public CallableStatement prepareCall(String sql) throws SQLException {
        
        return connection.prepareCall(sql);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, int, int, int)
     */
    public PreparedStatement prepareStatement(String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {
        
        return connection.prepareStatement(sql, resultSetType, resultSetConcurrency, resultSetHoldability);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, int, int)
     */
    public PreparedStatement prepareStatement(String sql, int resultSetType, int resultSetConcurrency) throws SQLException {
        
        return connection.prepareStatement(sql, resultSetType, resultSetConcurrency);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, int)
     */
    public PreparedStatement prepareStatement(String sql, int autoGeneratedKeys) throws SQLException {
        
        return connection.prepareStatement(sql, autoGeneratedKeys);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, int[])
     */
    public PreparedStatement prepareStatement(String sql, int[] columnIndexes) throws SQLException {
        
        return connection.prepareStatement(sql, columnIndexes);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String, java.lang.String[])
     */
    public PreparedStatement prepareStatement(String sql, String[] columnNames) throws SQLException {
        
        return connection.prepareStatement(sql, columnNames);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#prepareStatement(java.lang.String)
     */
    public PreparedStatement prepareStatement(String sql) throws SQLException {
        
        return connection.prepareStatement(sql);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#releaseSavepoint(java.sql.Savepoint)
     */
    public void releaseSavepoint(Savepoint savepoint) throws SQLException {
        
        connection.releaseSavepoint(savepoint);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#rollback()
     */
    public void rollback() throws SQLException {
        
        connection.rollback();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#rollback(java.sql.Savepoint)
     */
    public void rollback(Savepoint savepoint) throws SQLException {
        
        connection.releaseSavepoint(savepoint);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setAutoCommit(boolean)
     */
    public void setAutoCommit(boolean autoCommit) throws SQLException {
        
        connection.setAutoCommit(autoCommit);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setCatalog(java.lang.String)
     */
    public void setCatalog(String catalog) throws SQLException {
        
        connection.setCatalog(catalog);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setHoldability(int)
     */
    public void setHoldability(int holdability) throws SQLException {
        
        connection.setHoldability(holdability);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setReadOnly(boolean)
     */
    public void setReadOnly(boolean readOnly) throws SQLException {
        
        connection.setReadOnly(readOnly);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setSavepoint()
     */
    public Savepoint setSavepoint() throws SQLException {
        
        return connection.setSavepoint();
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setSavepoint(java.lang.String)
     */
    public Savepoint setSavepoint(String name) throws SQLException {
        
        return connection.setSavepoint(name);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setNetworkTimeout()
     */
    public void setNetworkTimeout(Executor executor, int milliseconds) throws SQLException {
        
        connection.setNetworkTimeout(executor, milliseconds);
    }

    public void setSchema(String schema) throws SQLException {
        
        connection.setSchema(schema);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setTransactionIsolation(int)
     */
    public void setTransactionIsolation(int level) throws SQLException {
        
        connection.setTransactionIsolation(level);
    }

    /* (non-Javadoc)
     * @see java.sql.Connection#setTypeMap(java.util.Map)
     */
    public void setTypeMap(Map<String, Class<?>> arg0) throws SQLException {
        
        connection.setTypeMap(arg0);
    }

        /* (non-Javadoc)
         * @see java.sql.Wrapper#isWrapperFor(java.lang.Class)
         */
        @Override
        public boolean isWrapperFor( Class<?> iface ) throws SQLException {
            throw new SQLException("Unsupported.");
        }

        /* (non-Javadoc)
         * @see java.sql.Wrapper#unwrap(java.lang.Class)
         */
        @Override
        public <T> T unwrap( Class<T> iface ) throws SQLException {
            throw new SQLException("Unsupported.");
        }

        /* (non-Javadoc)
         * @see java.sql.Connection#createArrayOf(java.lang.String, java.lang.Object[])
         */
        @Override
        public Array createArrayOf( String typeName, Object[] elements )
                throws SQLException
        {
            throw new SQLException("Unsupported.");
        }

        /* (non-Javadoc)
         * @see java.sql.Connection#createBlob()
         */
        @Override
        public Blob createBlob() throws SQLException {
            throw new SQLException("Unsupported.");
        }

        /* (non-Javadoc)
         * @see java.sql.Connection#createClob()
         */
        @Override
        public Clob createClob() throws SQLException {
            throw new SQLException("Unsupported.");
        }

        /* (non-Javadoc)
         * @see java.sql.Connection#createNClob()
         */
        @Override
        public NClob createNClob() throws SQLException {
            throw new SQLException("Unsupported.");
        }

        /* (non-Javadoc)
         * @see java.sql.Connection#createSQLXML()
         */
        @Override
        public SQLXML createSQLXML() throws SQLException {
            throw new SQLException("Unsupported.");
        }

        /* (non-Javadoc)
         * @see java.sql.Connection#createStruct(java.lang.String, java.lang.Object[])
         */
        @Override
        public Struct createStruct( String typeName, Object[] attributes )
                throws SQLException
        {
            throw new SQLException("Unsupported.");
        }

        /* (non-Javadoc)
         * @see java.sql.Connection#getClientInfo()
         */
        @Override
        public Properties getClientInfo() throws SQLException {
            throw new SQLException("Unsupported.");
        }

        /* (non-Javadoc)
         * @see java.sql.Connection#getClientInfo(java.lang.String)
         */
        @Override
        public String getClientInfo( String name ) throws SQLException {
            throw new SQLException("Unsupported.");
        }

        /* (non-Javadoc)
         * @see java.sql.Connection#isValid(int)
         */
        @Override
        public boolean isValid( int timeout ) throws SQLException {
            throw new SQLException("Unsupported.");
        }

        /* (non-Javadoc)
         * @see java.sql.Connection#setClientInfo(java.util.Properties)
         */
        @Override
        public void setClientInfo( Properties properties )
                throws SQLClientInfoException
        {
            throw new UnsupportedOperationException("Operation not supported");
        }

        /* (non-Javadoc)
         * @see java.sql.Connection#setClientInfo(java.lang.String, java.lang.String)
         */
        @Override
        public void setClientInfo( String name, String value )
                throws SQLClientInfoException
        {
            throw new UnsupportedOperationException("Operation not supported");
        }


}
