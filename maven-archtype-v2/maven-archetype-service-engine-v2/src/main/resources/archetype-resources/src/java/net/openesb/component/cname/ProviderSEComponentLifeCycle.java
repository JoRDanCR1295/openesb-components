#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * ProviderSEComponentLifeCycle.java
 */
package net.openesb.component.${artifactId};

import net.openesb.component.${artifactId}.common.BasicComponentLifeCycle;
import net.openesb.component.${artifactId}.common.DefaultMessageExchangeReceiver;
import net.openesb.component.${artifactId}.common.MessageExchangeReceiver;
import net.openesb.component.${artifactId}.common.RuntimeContext;
import javax.jbi.JBIException;
import javax.jbi.component.Component;

/**
 * This class extends the basic ComponentLifeCycle implementation to provide
 * component specific implementation of the ComponentLifeCycle.
 *
 * @see javax.jbi.ComponentLifeCycle
 * @see com.sun.jbi.sample.component.common.BasicComponentLifeCycle
 * @author chikkala
 */
public class ProviderSEComponentLifeCycle extends BasicComponentLifeCycle {

    /**
     * constructor
     */
    public ProviderSEComponentLifeCycle(Component compRuntime) {
        super(compRuntime);
    }

    /**
     * creates DefaultMessageExchangeReceiver to handles receiving and
     * processing the message exchanges from the delivery channel.
     */
    @Override
    protected MessageExchangeReceiver createMessageExchangeReceiver() {
        return new DefaultMessageExchangeReceiver();
    }

    /**
     * chance to extended classes to do the component specific init
     *
     * @throws javax.jbi.JBIException
     */
    @Override
    protected void doInit() throws JBIException {
        // NOOP
        RuntimeContext.getInstance().setLogger(this.getClass().getName(), null);
    }
}
