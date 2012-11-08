/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.sun.jbi.bindings.synchronization;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * A database tables backed token mechanism used
 * to synchronize concurrent threads distributed across
 * clustered server instnaces or just multiple server domains.
 * 
 * these threads are related because they are operating on 
 * critical external resources, such as file system directories, files,
 * ftp directories, files etc. concurrently in such a way that 
 * some critical regions has to be protected by the global synchronization
 * so that the undesirable racing condition will not happen.
 * 
 * @author jfu
 */
public class DataBaseToken implements Sync {
    private static Logger mLogger = Logger.getLogger(DataBaseToken.class.getName());
    public static final String TOKEN_PROPERTY_CONNECTION = "org.glassfish.openesb.sync.database.conn"; 
    public static final String TOKEN_REG_TABLE_INFO = "org.glassfish.openesb.sync.database.registry.table.info"; 
    public static final String TOKEN_TKN_TABLE_INFO = "org.glassfish.openesb.sync.database.token.table.info"; 
    public static final String TOKEN_TKN_TABLE_NAME = "org.glassfish.openesb.sync.database.token.table.name"; 
    public static final String TOKEN_ID_VALUE = "org.glassfish.openesb.sync.database.token.id"; 
    public static final String TOKEN_PROPERTY_DB_URL = "org.glassfish.openesb.sync.database.url"; 
    public static final String TOKEN_PROPERTY_DB_DRIVER = "org.glassfish.openesb.sync.database.diver"; 
    public static final String TOKEN_PROPERTY_DATASRC_JNDI_NAME = "org.glassfish.openesb.sync.database.jndi.name"; 

    private static final String SQL_SELECT = "SELECT ";
    private static final String SQL_SELECT_COL_DELIM = ", ";
    private static final String SQL_FROM = " FROM ";
    private static final String SQL_WHERE = " WHERE ";
    private static final String SQL_FOR_UPDATE = " FOR UPDATE";
    private static final String SQL_DROP_TABLE = "DROP TABLE ";
    private static final String SQL_CREATE_TABLE = "CREATE TABLE ";
    private ResultSet mRegistryResultSet;
    private ResultSet mTokenResultSet;
    private Statement mTokenStatement;
    private Statement mRegistryStatement;
    private Connection mConn;
    private Connection mConnRegistration;
    private List mTokenRegInfo;
    private String mTokenID;
    private String mTokenTable;
    private List mTokenTableInfo;
    private String mQ4Token;
    private String mQ4Registry;
    private String mCreateTokenTable;
    private String mDropTokenTable;
    private AtomicBoolean bRegistered;

    /**
     * Schema for token database:
     * 
     * TOKEN_REGISTRY (TOKEN_ID VARCHAR(76), TOKEN_TABLE VARCHAR(33), T_STAMP TIMESTAMP, REF_COUNT INTEGER);
     * TOKEN_<unique-name-derived-from-TOKEN_ID> (LAST_MODIFIED TIMESTAMP);
     * 
     * Look up token ID in the TOKEN_REGISTRY table,
     * and obtain the associated token table info if the token table has been
     * created already, otherwise, create the token table, and
     * have the token table info ready
     * 
     * Note that the exclusive access to the token registry table
     * is achieved by using a UPDATABLE query cursor, all the web service operations
     * running on a cluster (or multiple clusters) has its own token entry 
     * in this token registry.
     * 
     * Tokens are registered in the registry at the service unit deployment time,
     * where the token table is created if not yet (due to clustering, it is possible 
     * that multiple instances of the same SU have been going through the JBI deploy life-cycle where 
     * the web service operations are registered and activated)
     * 
     * Tokens are de-registered in the registry at the service unit un-deployment time,
     * where the token table is dropped if the reference count become 0, due to clustering, it is 
     * possible that multiple instances of the same SU have been going through the 
     * JBI deploy life-cycle where the web services operations are de-activated and de-registered.
     * 
     * Database that hosts TOKEN_REGISTRY table and corresponding token tables are
     * created administratively, and the URI for accessing the database is configurable as specific JBI 
     * component's runtime configuration parameter, the table TOKEN_REGISTRY is also created
     * administratively.
     * 
     * Token tables are created at the runtime by the SU deployment life-cycle logic.
     * 
     * Performance considerations:
     * 
     * (1) creating and dropping of the token tables are synchronized on table TOKEN_REGISTRY -
     * this is considered to be an exclusive table level locking which will block all the readers/writers
     * doing token creation / dropping and doing token acquisition, assuming that the SU deployment / un-deployment
     * is not a frequent action on a production environment, the performance impact of the
     * exclusive locking is acceptable.
     * 
     * (2) acquisition of a token is considered a frequent action, by using a unique, dedicated token
     * table for each token (each WSDL operation has its token in the database) will make sure different 
     * operations proceed in parallel without being blocked by any other operations, maximize the
     * concurrency, however, the same operation being processed by multiple instances will be synchronized
     * on the token preventing racing condition from occurring.
     * 
     * DBMS considerations:
     * 
     * (1) DBMS specific DDL scripts:
     * Since the runtime token creation involves DDL such as table creation etc., where the syntax can be
     * database system depedent, a configurable approach is desirable, e.g. make the DDL statements for creating and dropping 
     * token table a JBI component runtime configuration parameter(s).
     * 
     * (2) DBMS specific limitations:
     * Since a per WSDL operation token is required to synchronize the concurrent processing of that operation, and
     * a token is backed by a unique table in the database to persist information and provide per operation locking,
     * DBMS has limit on maximum number of tables that can be created in a database, the actual number could also be 
     * limited by the underlying OS platform, it is reasonable to assume that the limit should be in the order
     * of thousands.
     * 
     * Since the WSDL operation name derived ID (TOKEN_ID) could contain characters so that it is not a valid
     * table name for a database system, TOKEN_REGISTRY table provide a mechanism to map token ID into a legitimate
     * table name so that the token table can be created by that name.
     * 
     * token life cycle:
     * 
     * (1) created token = Token.create(tokenInfo)
     * (2) register - token.register()
     * repeat (3)(4) 0 to N times
     * (3) token.acquire()
     * between (3)(4), there is the protected critical region
     * (4) token.release()
     * (5) token.deregister()
     * 
     * @param conn - connection to the database that hosts the tokens
     * @param queryInfo
     * @return
     * @throws java.lang.Exception
     */
    public static Sync createSync(Map config) {
        return new DataBaseToken(config);
    }

    /**
     * the private constructor for the factory method
     * @param conn - connection for the database containing token registry / tokens
     * @param tokenInfo - parameters required for : query the registry table and create / query token table
     */
    private DataBaseToken(Map config) {
        // token reg Info contains parameters for creating / acquiring tokens:
        // TOKEN_REGISTRY (TOKEN_ID, TOKEN_TABLE, T_STAMP, REF_COUNT)
        // tokenRegInfo[0]: registry table name - e.g. TOKEN_REGISTRY
        // tokenRegInfo[1]: 1st col, e.g. TOKEN_ID
        // tokenRegInfo[2]: 2nd col, e.g. TOKEN_TABLE
        // tokenRegInfo[3]: 3rd col, e.g. T_STAMP
        // tokenRegInfo[4]: 4th col, e.g. REF_COUNT
        
        // token table info
        // tokenTabInfo[5]: token ID value
        // token table's columns
        // tokenTabInfo[0]: 1st col, TOKEN_ID
        // tokenTabInfo[1]: 2nd col, T_STAMP - to be changed every time the token is acquired
        List<Connection> conns = (List)config.get(TOKEN_PROPERTY_CONNECTION);
        if ( conns == null || conns.size() != 2 ) {
            throw new IllegalArgumentException("Insufficient information for creating a database backed token table for WSDL operations: missing connection object to the token database.");
        }

        mConn = conns.get(0);
        if ( mConn == null ) {
            throw new IllegalArgumentException("Insufficient information for creating a database backed token table for WSDL operations: missing connection for access token tables.");
        }
        
        mConnRegistration = conns.get(1);
        if ( mConnRegistration == null ) {
            throw new IllegalArgumentException("Insufficient information for creating a database backed token table for WSDL operations: missing connection for access token registry table.");
        }

        mTokenID = (String)config.get(TOKEN_ID_VALUE);
        if (mTokenID == null || mTokenID.trim().length() == 0) {
            throw new IllegalArgumentException("Insufficient information for creating a database backed token table for WSDL operations: missing token ID.");
        }
        mTokenTable = (String)config.get(TOKEN_TKN_TABLE_NAME);
        if (mTokenTable == null || mTokenTable.trim().length() == 0 ) {
            throw new IllegalArgumentException("Insufficient information for creating a database backed token table for WSDL operations: missing token table name.");
        }
        mTokenRegInfo = (List)config.get(TOKEN_REG_TABLE_INFO);
        if (mTokenRegInfo == null || mTokenRegInfo.size() < 5) {
            throw new IllegalArgumentException("Insufficient information for creating a database backed token table for WSDL operations: missing token registry table information.");
        }
        mTokenTableInfo = (List)config.get(TOKEN_TKN_TABLE_INFO);
        if (mTokenTableInfo == null || mTokenTableInfo.size() < 4) {
            throw new IllegalArgumentException("Insufficient information for creating a database backed token table for WSDL operations: missing token table information, token ID:" + mTokenID);
        }
        bRegistered = new AtomicBoolean(false);
    }

    /**
     * look up the token in the token registry, if found, increment reference counter,
     * otherwise, create the token table and insert a row representing the token 
     * into the registry table
     *  
     */
    public synchronized void register() throws SyncException {
        if ( mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, "enter:register():::TOKEN_ID=" + mTokenID);
        // create a row in the db table represents a token for the operation
        // identified by the token ID (the unique operation ID derived by prefixing a operation name derived UUID with the operation
        // name to the first 36 characters)
        if (bRegistered.get()) {
            throw new IllegalStateException("Token :[" + mTokenID + "] already registered.");
        }
        try {
            mConnRegistration.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
            mRegistryStatement = mConnRegistration.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_UPDATABLE, ResultSet.CLOSE_CURSORS_AT_COMMIT);
            if ( mQ4Registry == null )
                mQ4Registry = getQuery4Registry();
            mRegistryResultSet = mRegistryStatement.executeQuery(mQ4Registry);
            boolean hasToken = mRegistryResultSet.next();
            if (!hasToken) {
                // first to register
                try {
                    mRegistryResultSet.moveToInsertRow();
                    mRegistryResultSet.updateString(1, mTokenID); // token ID
                    mRegistryResultSet.updateString(2, mTokenTable);
                    mRegistryResultSet.updateTimestamp(3, new Timestamp(System.currentTimeMillis()));
                    mRegistryResultSet.updateInt(4, 1);
                    mRegistryResultSet.insertRow();
                } catch (SQLException ex) {
                    throw new SyncException("SQLException when register token in token registry, ID=" + mTokenID, ex);
                }
                // create the corresponding table for the token
                Statement s = mConnRegistration.createStatement();
                if ( mCreateTokenTable == null )
                    mCreateTokenTable = getCreateTokenStatement();
                s.executeUpdate(mCreateTokenTable);
                // next insert a record
                // with TOKEN_ID = the current value of mTokenID
                mTokenStatement = mConnRegistration.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_UPDATABLE, ResultSet.CLOSE_CURSORS_AT_COMMIT);
                if ( mQ4Token == null )
                    mQ4Token = getQuery4Token();
                mTokenResultSet = mTokenStatement.executeQuery(mQ4Token);
                hasToken = mTokenResultSet.next(); // suppose to land on the only record
                if (!hasToken) {
                    // first time, create it
                    mTokenResultSet.moveToInsertRow();
                    mTokenResultSet.updateString(1, mTokenID);
                    mTokenResultSet.updateTimestamp(2, new Timestamp(System.currentTimeMillis()));
                    mTokenResultSet.insertRow();
                }
                else {
                    // this should not happen, give a warning for debugging purpose
                    if ( mLogger.isLoggable(Level.WARNING))
                        mLogger.log(Level.WARNING, "register():::there is already a token in token table..." + mTokenID);
                }
            } else {
                // the token already registered by other instances
                // just increment REF_COUNT
                int ref_cnt = mRegistryResultSet.getInt(4);
                mRegistryResultSet.updateInt(4, ref_cnt + 1);
                mRegistryResultSet.updateRow();
            }
            mConnRegistration.commit();
        } catch (SQLException se) {
            try {
                mConnRegistration.rollback();
            } catch (SQLException ex) {
                Logger.getLogger(DataBaseToken.class.getName()).log(Level.SEVERE, "register:::SQLException caught when rolling back register()...token ID=" + mTokenID , ex);
            }
            throw new SyncException("SQLException when register token : " + mTokenID, se);
        }
        finally {
            if ( mRegistryResultSet != null ) {
                try {
                    mRegistryResultSet.close();
                } catch (SQLException ex) {
                    // log and ignore
                    if ( mLogger.isLoggable(Level.SEVERE))
                        mLogger.log(Level.SEVERE, "register():::Exception when closing token registry result set, token ID=" + mTokenID, ex);
                }
                mRegistryResultSet = null;
            }
            if ( mRegistryStatement != null ) {
                try {
                    mRegistryStatement.close();
                } catch (SQLException ex) {
                    // log and ignore
                    if ( mLogger.isLoggable(Level.SEVERE))
                        mLogger.log(Level.SEVERE, "register():::Exception when closing the updatable query statement for token registry, token ID=" + mTokenID, ex);
                }
                mRegistryStatement = null;
            }
            if ( mTokenResultSet != null ) {
                try {
                    mTokenResultSet.close();
                } catch (SQLException ex) {
                    if ( mLogger.isLoggable(Level.SEVERE))
                        mLogger.log(Level.SEVERE, "register():::Exception when closing token table result set, token ID=" + mTokenID, ex);
                }
                mTokenResultSet = null;
            }
            if ( mTokenStatement != null ) {
                try {
                    mTokenStatement.close();
                } catch (SQLException ex) {
                    if ( mLogger.isLoggable(Level.SEVERE))
                        mLogger.log(Level.SEVERE, "register():::Exception when closing token table query statement, token ID=" + mTokenID, ex);
                }
                mTokenStatement = null;
            }
        }
        if ( mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, "exit:register():::TOKEN_ID=" + mTokenID);
        bRegistered.set(true);
    }

    /**
     * look up the token in the token registry, if found, decrement reference counter,
     * if the counter becomes 0, drop the table, and remove the row from the registry,
     * if not found, this is an abnormality
     */
    public synchronized void deregister() throws SyncException {
        if ( mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, "enter:deregister():::TOKEN_ID=" + mTokenID);
        if (!bRegistered.get()) {
            throw new IllegalStateException("Token [" + mTokenID + "] already deregistered.");
        }
        try {
            mConnRegistration.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
            mRegistryStatement = mConnRegistration.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_UPDATABLE, ResultSet.CLOSE_CURSORS_AT_COMMIT);
            if ( mQ4Registry == null )
                mQ4Registry = getQuery4Registry();
            mRegistryResultSet = mRegistryStatement.executeQuery(mQ4Registry);
            boolean hasToken = mRegistryResultSet.next();
            if (hasToken) {
                int ref_cnt = mRegistryResultSet.getInt(4);
                if (ref_cnt <= 1) {
                    mRegistryResultSet.deleteRow();
                    dropTokenTable();
                }
                else {
                    mRegistryResultSet.updateInt(4, ref_cnt - 1);
                    mRegistryResultSet.updateRow();
                }
            } else {
                // try drop table anyway
                try {
                    dropTokenTable();
                } catch (Exception e) {
                    // ignore
                }
            }
            mConnRegistration.commit();
        } catch (SQLException se) {
            try {
                mConnRegistration.rollback();
            } catch (SQLException ex) {
                Logger.getLogger(DataBaseToken.class.getName()).log(Level.SEVERE, "deregister:::SQLException caught when rolling back deregister()...token ID=" + mTokenID , ex);
            }
            throw new SyncException("SQLException when deregister token: [" + mTokenID + "]", se);
        }
        finally {
            if ( mRegistryResultSet != null ) {
                try {
                    mRegistryResultSet.close();
                } catch (SQLException ex) {
                    // log and ignore
                    if ( mLogger.isLoggable(Level.SEVERE))
                        mLogger.log(Level.SEVERE, "deregister():::Exception when closing token registry result set...", ex);
                }
                mRegistryResultSet = null;
            }
            if ( mRegistryStatement != null ) {
                try {
                    mRegistryStatement.close();
                } catch (SQLException ex) {
                    // log and ignore
                    if ( mLogger.isLoggable(Level.SEVERE))
                        mLogger.log(Level.SEVERE, "deregister():::Exception when closing the updatable query statement for token registry...", ex);
                }
                mRegistryStatement = null;
            }
        }
        bRegistered.set(false);
        if ( mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, "exit:deregister():::TOKEN_ID=" + mTokenID);
    }

    /**
     * acquire the token - put a lock on the token table
     * @throws java.sql.SQLException
     */
    public Sync acquire() throws SyncException {
        if ( mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, "enter:acquire():::TOKEN_ID=" + mTokenID);
        if (!bRegistered.get()) {
            throw new IllegalStateException("Token : [" + mTokenID + "] not registered yet, can not acquire it.");
        }
        try {
            mConn.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED);
            mTokenStatement = mConn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_UPDATABLE, ResultSet.CLOSE_CURSORS_AT_COMMIT);
            if ( mQ4Token == null )
                mQ4Token = getQuery4Token();
            mTokenResultSet = mTokenStatement.executeQuery(mQ4Token);
            boolean hasToken = mTokenResultSet.next(); // suppose to land on the only record
            if (!hasToken) {
                try {
                    mTokenResultSet.close();
                }
                catch (Exception e) {
                    // ignore
                }
                try {
                    mTokenStatement.close();
                }
                catch (Exception e) {
                    // ignore
                }
                throw new SyncException("Invalid token table state, no token entry found in token table with ID=" + mTokenID);
            } else {
                // col index starst at 1
                // TOKEN_ID, T_STAMP
                mTokenResultSet.updateTimestamp(2, new Timestamp(System.currentTimeMillis()));
                mTokenResultSet.updateRow();
            }
        } catch (SQLException se) {
            throw new SyncException("SQL Exception when acquiring token: [" + mTokenID + "]", se);
        }
        if ( mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, "return:acquire():::TOKEN_ID=" + mTokenID);
        return this;
    }

    /**
     * release the token - unlock the exclusive clock on the token table
     */
    public void release() {
        if ( mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, "enter:release():::TOKEN_ID=" + mTokenID);
        if (!bRegistered.get()) {
            throw new IllegalStateException("Token : [" + mTokenID + "] not registered yet, can not release it.");
        }
        try {
            if (mTokenStatement != null) {
                mTokenStatement.close();
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            mTokenStatement = null;
        }

        try {
            if (mTokenResultSet != null) {
                mTokenResultSet.close();
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            mTokenResultSet = null;
        }
        if ( mLogger.isLoggable(Level.FINE))
            mLogger.log(Level.FINE, "exit:release():::TOKEN_ID=" + mTokenID);
    }

    /**
     * helper - drop the token table
     * @throws java.sql.SQLException
     */
    private void dropTokenTable() throws SQLException {
        Statement s = mConn.createStatement();
        if ( mDropTokenTable == null )
            mDropTokenTable = getDropTokenStatement();
        s.executeUpdate(mDropTokenTable);
        s.close();
    }

    /**
     * helper - form a SQL string querying the registry for the given token ID
     * @return - the SQL statement
     */
    private String getQuery4Registry() {
        // columns:
        // TOKEN_ID, TOKEN_TABLE, T_STAMP, REF_COUNT
        // tokenRegInfo[0]: registry table name
        // tokenRegInfo[1]: 1st col, e.g. TOKEN_ID
        // tokenRegInfo[2]: 2nd col, e.g. TOKEN_TABLE
        // tokenRegInfo[3]: 3rd col, e.g. T_STAMP
        // tokenRegInfo[4]: 4th col, e.g. REF_COUNT
        //                                  so that a exclusive lock will be placed
        StringBuffer sb = new StringBuffer();
        sb.append(SQL_SELECT);
        sb.append((String) mTokenRegInfo.get(1));
        sb.append(SQL_SELECT_COL_DELIM);
        sb.append((String) mTokenRegInfo.get(2));
        sb.append(SQL_SELECT_COL_DELIM);
        sb.append((String) mTokenRegInfo.get(3));
        sb.append(SQL_SELECT_COL_DELIM);
        sb.append((String) mTokenRegInfo.get(4));
        sb.append(SQL_FROM);
        sb.append((String) mTokenRegInfo.get(0));
        sb.append(SQL_WHERE);
        sb.append((String) mTokenRegInfo.get(1)).append("='").append(mTokenID).append("'");
        sb.append(SQL_FOR_UPDATE);
        return sb.toString();
    }

    /**
     * helper - form the SQL query string from the token table
     * @return - the SQL statement
     */
    private String getQuery4Token() {
        StringBuffer sb = new StringBuffer();
        sb.append(SQL_SELECT);
        sb.append((String) mTokenTableInfo.get(0)); // e.g. TOKEN_ID
        sb.append(SQL_SELECT_COL_DELIM);
        sb.append((String) mTokenTableInfo.get(2)); // e.g. T_STAMP
        sb.append(SQL_FROM);
        sb.append(mTokenTable);
        sb.append(SQL_WHERE);
        sb.append((String) mTokenTableInfo.get(0)).append("='").append(mTokenID).append("'");
        sb.append(SQL_FOR_UPDATE);
        return sb.toString();
    }

    /**
     * helper - return a drop table statement
     * @return
     */
    private String getDropTokenStatement() {
        return SQL_DROP_TABLE.concat(mTokenTable);
    }
    
    /**
     * helper - return a create table statement
     * @return
     */
    private String getCreateTokenStatement() {
        StringBuffer sb = new StringBuffer();
        sb.append("(");
        sb.append(mTokenTableInfo.get(0));
        sb.append(" ");
        sb.append(mTokenTableInfo.get(1));
        sb.append(SQL_SELECT_COL_DELIM);
        sb.append(mTokenTableInfo.get(2));
        sb.append(" ");
        sb.append(mTokenTableInfo.get(3));
        sb.append(")");
        return SQL_CREATE_TABLE.concat(mTokenTable).concat(sb.toString());
    }
    
    /**
     * full information about the token
     * @return - the full info as a string
     */
    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append(this.getClass().getName()).append(":::");
        sb.append("Token ID:").append(mTokenID).append("|||");
        if ( mQ4Registry == null )
            mQ4Registry = getQuery4Registry();
        sb.append("SQL query for registry:").append(mQ4Registry).append("|||");
        if ( mQ4Token == null )
            mQ4Token = getQuery4Token();
        sb.append("SQL query for token:").append(mQ4Token).append("|||");
        if ( mCreateTokenTable == null )
            mCreateTokenTable = getCreateTokenStatement();
        sb.append("SQL create token:").append(mCreateTokenTable).append("|||");
        if ( mDropTokenTable == null )
            mDropTokenTable = getDropTokenStatement();
        sb.append("SQL drop token:").append(mDropTokenTable).append("|||");
        return sb.toString();
    }
}
