/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: AsteriskComponentLifeCycle.java,v 1.1 2008/01/20 16:38:50 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk;

import com.sun.jbi.sample.component.common.BasicComponentLifeCycle;
import com.sun.jbi.sample.component.common.DefaultMessageExchangeReceiver;
import com.sun.jbi.sample.component.common.MessageExchangeReceiver;
import com.sun.jbi.sample.component.common.RuntimeContext;
import javax.jbi.JBIException;
import javax.jbi.component.Component;

public class AsteriskComponentLifeCycle extends BasicComponentLifeCycle {
    
    /** constructor for the ComponentLifecycle implementation. */
    public AsteriskComponentLifeCycle(Component compRuntime) {
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
