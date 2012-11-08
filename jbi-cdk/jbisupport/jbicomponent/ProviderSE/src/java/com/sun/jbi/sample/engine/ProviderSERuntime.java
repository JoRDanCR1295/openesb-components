/*
 * ProviderSERuntime.java
 */

package com.sun.jbi.sample.engine;

import com.sun.jbi.sample.component.common.ComponentRuntime;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;

/**
 * This class extends the ComponentRuntime that implements javax.jbi.component.Component
 * interface required for the component contract at runtime.
 *
 * This class provides the component specific ComponentLifeCycle implementation
 * as well as the component specific ServiceUnitManager implementation. 
 *
 * Add any additional component runtime specific functionality here.
 *
 * @see javax.jbi.component.Component
 * @see com.sun.jbi.sample.component.common.ComponentRuntime
 * @see com.sun.jbi.sample.component.common.BasicComponentLifecycle
 * @see com.sun.jbi.sample.component.common.deployment.AbstractServiceUnitManager
 * @author chikkala
 */
public class ProviderSERuntime extends ComponentRuntime {
    
    /** Creates a new instance of MyEngineRuntime */
    public ProviderSERuntime() {
        super();
    }
    
    /**
     * overriding the parent's createComponentLifeCycle to create
     * component specific component lifecycle implementation.
     */
    @Override
    protected ComponentLifeCycle createComponentLifeCycle() {
        return new ProviderSEComponentLifeCycle(this);
    }
    
    /**
     * overrides the parent's createServiceUnitManager to create component 
     * specific service unit manager
     */
    @Override
    protected ServiceUnitManager createServiceUnitManager() {
        return new ProviderSESUManager(this);
    }
    
}
