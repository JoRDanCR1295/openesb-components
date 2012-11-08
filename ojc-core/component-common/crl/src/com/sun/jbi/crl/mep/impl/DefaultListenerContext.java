/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)DefaultListenerContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.component.lifecycle.ComponentManager;
import com.sun.jbi.crl.mep.CallbackRegistry;
import com.sun.jbi.crl.mep.CorrelationMap;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.ExchangeRouter;
import com.sun.jbi.crl.mep.ListenerContext;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchangeFactory;

/**
 * Default implementation of {@link ListenerContext}.
 * 
 * @author Kevan Simpson
 */
public class DefaultListenerContext extends AbstractCustomContext 
									implements ListenerContext {
    // we don't need a pool, as our default impl is stateless and thread-safe
    private ExchangeContext mExchangeCtx = null;
    private ComponentManager mComponentMgr = null;
    
    public DefaultListenerContext(ComponentManager cmgr) {
    	super(cmgr.getComponentContext());
        setComponentManager(cmgr);
        setExchangeContext(new DefaultExchangeContext(null, this));
    }

    /**
     * Sets the {@link ExchangeContext} value, which is expected
     * to be stateless and thread-safe.
     * @param ctx
     */
    public void setExchangeContext(ExchangeContext ctx) {
        // this method is not part of the ListenerContext interface
        mExchangeCtx = ctx;
    }

    /** @see com.sun.jbi.crl.mep.ListenerContext#acquireExchangeContext() */
    public ExchangeContext acquireExchangeContext() {
        return mExchangeCtx;
    }

    /** @see com.sun.jbi.crl.mep.ListenerContext#getCallbackRegistry() */
    public CallbackRegistry getCallbackRegistry() {
        return getComponentManager().getCallbackRegistry();
    }

    /** @see com.sun.jbi.crl.mep.ListenerContext#getCorrelationMap() */
    public CorrelationMap getCorrelationMap() {
        return getComponentManager().getCorrelationMap();
    }

    /** @see com.sun.jbi.crl.mep.ListenerContext#getCRLMessageExchangeFactory() */
    public CRLMessageExchangeFactory getCRLMessageExchangeFactory() {
        return getComponentManager().getExchangeFactory();
    }

    /** @see com.sun.jbi.crl.mep.ListenerContext#getEndpointManager() */
    public EndpointManager getEndpointManager() {
        return getComponentManager().getEndpointManager();
    }

    /** @see com.sun.jbi.crl.mep.ListenerContext#getExchangeRouter() */
    public ExchangeRouter getExchangeRouter() {
        return getComponentManager().getAcceptManager().getExchangeRouter();
    }

    /** @see com.sun.jbi.crl.mep.ListenerContext#getMessagingChannel() */
    public MessagingChannel getMessagingChannel() {
        return getComponentManager().getManagerContext().getMessagingChannel();
    }

    protected ComponentManager getComponentManager() {
        return mComponentMgr;
    }
    protected void setComponentManager(ComponentManager cmgr) {
        mComponentMgr = cmgr;
    }
}
