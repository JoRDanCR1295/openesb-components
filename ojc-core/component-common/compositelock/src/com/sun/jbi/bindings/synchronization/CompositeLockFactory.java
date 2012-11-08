/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.bindings.synchronization;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * a factory that creates tokens of different 
 * type according to the configuration
 * 
 * @author jfu
 */
public class CompositeLockFactory {
    private Map mConfig;
    private Sync.TOKEN_TYPES mType;

    public static CompositeLockFactory newInstance(Map config) throws SyncException {
        return new CompositeLockFactory(config);
    }
    
    private CompositeLockFactory(Map config) throws SyncException {
        if ( config != null && config.size() > 0 ) {
            mConfig = config;
            String type = (String)mConfig.get(Sync.SYNC_TOKEN_TYPE);
            if ( type == null || type.trim().length() == 0 ) {
                throw new IllegalArgumentException("Missing token type property when instantiating token factory" + mConfig);
            }
            if ( type.equals(Sync.SYNC_DB_TOKEN)) {
                mType = Sync.TOKEN_TYPES.DATABASE_TOKEN;
                List<Connection> conns = null;
                String clazz = (String)mConfig.get(DataBaseToken.TOKEN_PROPERTY_DB_DRIVER);
                if (clazz != null && clazz.trim().length() > 0 ) {
                    String url = (String)mConfig.get(DataBaseToken.TOKEN_PROPERTY_DB_URL);
                    try {
                        // must be jdbc db url + driver clazz
                        Class.forName(clazz);
                    } catch (ClassNotFoundException ex) {
                        Logger.getLogger(CompositeLockFactory.class.getName()).log(Level.SEVERE, "JDBC driver class not found : " +  clazz, ex);
                        throw new SyncException(ex);
                    }
                    try {
                        conns = new Vector();
                        conns.add(0, DriverManager.getConnection(url));
                        conns.add(1, DriverManager.getConnection(url));
                        conns.get(1).setAutoCommit(false);
                    } catch (SQLException ex) {
                        Logger.getLogger(CompositeLockFactory.class.getName()).log(Level.SEVERE, "SQLException caught when instantiating JDBC connection: URL=" + url , ex);
                        throw new SyncException(ex);
                    }
                } else {
                    // must be jndi name
                    throw new UnsupportedOperationException("Token persistence by jndi name not implemented yet...");
                }
                // this impl uses two connections - one in auto commit
                // the other not.
                // the auto commit is for all the acquire/release
                // the manual commit is for register / de-register
                mConfig.put(DataBaseToken.TOKEN_PROPERTY_CONNECTION, conns);
            }
            else if ( type.equals(Sync.SYNC_FILE_TOKEN)) {
                mType = Sync.TOKEN_TYPES.FILE_TOKEN;
            }
            else {
                throw new IllegalArgumentException("Invalid (not supported) token type property [" + type + "] when instantiating token factory" + mConfig);
            }
        }
        else {
            mType = Sync.TOKEN_TYPES.NULL_TOKEN;
        }
    }
    
    /**
     * produce a per operation token from the factory
     * @return - the token for thread synchronization
     */
    public CompositeLock createCompositeLock(String aKey, Map perOpParams, PersistStore aStore) {
        HashMap params = new HashMap();
        if ( mConfig != null && mConfig.size() > 0 )
            params.putAll(mConfig);
        params.putAll(perOpParams);
        return new CompositeLock(aKey, aStore, (mType == Sync.TOKEN_TYPES.DATABASE_TOKEN ? DataBaseToken.createSync(params) : mType == Sync.TOKEN_TYPES.FILE_TOKEN ? FileToken.createSync(params) : null));
    }
}
