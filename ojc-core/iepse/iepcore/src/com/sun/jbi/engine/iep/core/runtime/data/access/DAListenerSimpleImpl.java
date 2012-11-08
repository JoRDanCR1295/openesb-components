/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.iep.core.runtime.data.access;

/**
 *
 * @author rdwivedi
 */
public class DAListenerSimpleImpl implements DAListener{
    
    CustomExtDBTable mCustom = null;
    public DAListenerSimpleImpl(CustomExtDBTable table) {
        mCustom = table;
    }
    public void process(OperatorDA da) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}
