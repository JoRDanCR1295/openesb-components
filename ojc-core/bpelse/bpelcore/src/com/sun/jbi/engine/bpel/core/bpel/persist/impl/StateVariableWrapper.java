/* *************************************************************************
 *
 *          Copyright (c) 2002, Sun Microsystems, Inc.
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems, Inc.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems, Inc.
 *
 ***************************************************************************/
package com.sun.jbi.engine.bpel.core.bpel.persist.impl;

import com.sun.jbi.engine.bpel.core.bpel.model.runtime.RuntimeVariable;

/**
 * @author Sun Inc
 * Sep 7, 2007
 */
public class StateVariableWrapper {

    private String mStateId;
    private RuntimeVariable mVar;
    
    StateVariableWrapper(String stateId, RuntimeVariable var) {
        mStateId = stateId;
        mVar = var;
    }
    
    String getStateId() {
        return mStateId;
    }
    
    RuntimeVariable getVar() {
        return mVar;
    }
}
