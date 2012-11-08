/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.bindings.synchronization;

/**
 * a wrapper for underlying exceptions
 * specific to the underlying persistence
 * operations;
 * 
 * @author jfu
 */
public class SyncException extends Exception {
    
    public SyncException() {
        super();
    }
    
    public SyncException(String m) {
        super(m);
    }

    public SyncException(Exception e) {
        super(e);
    }

    public SyncException(String m, Exception e) {
        super(m, e);
    }
            
}
