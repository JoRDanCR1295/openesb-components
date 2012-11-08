/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.bindings.synchronization;

/**
 * generic interface for a persistence based sync object
 * which can be used by concurrent threads synchronize on a 
 * critical region;
 * @author jfu
 */
public interface Sync {
    public enum TOKEN_TYPES {DATABASE_TOKEN, FILE_TOKEN, NULL_TOKEN};
    public static final String SYNC_TOKEN_TYPE = "org.glassfish.openesb.sync.token.type";
    public static final String SYNC_FILE_TOKEN = "file";
    public static final String SYNC_DB_TOKEN = "database";
    public void register() throws SyncException;
    public void deregister() throws SyncException;
    public Sync acquire() throws SyncException;
    public void release();
}
