/*
 * JMXBindingComponentLifeCycle.java
 */
package com.sun.jbi.sample.binding;

import com.sun.jbi.sample.component.common.BasicComponentLifeCycle;
import com.sun.jbi.sample.component.common.DefaultMessageExchangeReceiver;
import com.sun.jbi.sample.component.common.MessageExchangeReceiver;
import com.sun.jbi.sample.component.common.RuntimeContext;
import javax.jbi.JBIException;
import javax.jbi.component.Component;

/**
 * This class extends the basic ComponentLifeCycle implementation to provide component
 * specific implementation of the ComponentLifeCycle.
 *
 * @see javax.jbi.ComponentLifeCycle
 * @see com.sun.jbi.sample.component.common.BasicComponentLifeCycle
 * @author chikkala
 */
public class JMXBindingComponentLifeCycle extends BasicComponentLifeCycle {
    
    /** constructor for the ComponentLifecycle implementation. */
    public JMXBindingComponentLifeCycle(Component compRuntime) {
        super(compRuntime);
    }
    /**
     * creates DefaultMessageExchangeReceiver to handles receiving and processing
     * the message exchanges from the delivery channel.
     */
    @Override
    protected MessageExchangeReceiver createMessageExchangeReceiver() {
        return new DefaultMessageExchangeReceiver();
    }
    /**
     * chance to extended classes to do the component specific init
     * @throws javax.jbi.JBIException
     */
    @Override
    protected void doInit() throws JBIException {
        // NOOP
        RuntimeContext.getInstance().setLogger(this.getClass().getName(), null);
    }
    
}
