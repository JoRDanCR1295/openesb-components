/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: AsteriskRuntime.java,v 1.1 2008/01/20 16:38:50 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk;

import com.sun.jbi.sample.component.common.ComponentRuntime;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;

public class AsteriskRuntime extends ComponentRuntime {
    
    /** Creates a new instance of MyBindingRuntime */
    public AsteriskRuntime() {
        super();
    }
    /**
     * overriding the parent's createComponentLifeCycle to create
     * MyBinding specific component lifecycle implementation.
     */
    @Override
    protected ComponentLifeCycle createComponentLifeCycle() {
        return new AsteriskComponentLifeCycle(this);
    }
    
    /**
     * if this component supports service unit deployment, then return the
     * service unit manager, else return null.
     * Extended classes can override this method and do their own ServiceUnitManager
     * specific creation.
     */
    @Override
    protected ServiceUnitManager createServiceUnitManager() {
        return new AsteriskSUManager(this);
    }
    
    
}
