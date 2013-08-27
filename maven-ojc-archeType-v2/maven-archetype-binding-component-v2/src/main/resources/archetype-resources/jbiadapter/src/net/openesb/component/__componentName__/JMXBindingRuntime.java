#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * JMXBindingRuntime.java
 */
package net.openesb.component.${componentName};

import net.openesb.component.${componentName}.common.ComponentRuntime;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;

/**
 * This class extends the ComponentRuntime that implements
 * javax.jbi.component.Component interface required for the component contract
 * at runtime.
 *
 * This class provides the component specific ComponentLifeCycle implementation
 * as well as the component specific ServiceUnitManager implementation.
 *
 * Add any additional component runtime specific functionality here.
 *
 * @see javax.jbi.component.Component
 * @see com.sun.jbi.sample.component.common.ComponentRuntime
 * @see com.sun.jbi.sample.component.common.BasicComponentLifecycle
 * @see
 * com.sun.jbi.sample.component.common.deployment.AbstractServiceUnitManager
 * @author chikkala
 */
public class JMXBindingRuntime extends ComponentRuntime {

    /**
     * Creates a new instance of MyBindingRuntime
     */
    public JMXBindingRuntime() {
        super();
    }

    /**
     * overriding the parent's createComponentLifeCycle to create MyBinding
     * specific component lifecycle implementation.
     */
    @Override
    protected ComponentLifeCycle createComponentLifeCycle() {
        return new JMXBindingComponentLifeCycle(this);
    }

    /**
     * if this component supports service unit deployment, then return the
     * service unit manager, else return null. Extended classes can override
     * this method and do their own ServiceUnitManager specific creation.
     */
    @Override
    protected ServiceUnitManager createServiceUnitManager() {
        return new JMXBindingSUManager(this);
    }
}
