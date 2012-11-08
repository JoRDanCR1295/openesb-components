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
 * @(#)ListenerContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep;

import javax.jbi.component.ComponentContext;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchangeFactory;

/**
 * Defines context for {@link MessageListener}.
 * 
 * @author Kevan Simpson
 */
public interface ListenerContext extends ComponentContext {
	/**
	 * Fetches the {@link CorrelationMap} for this component.
	 * @return this component's <code>CorrelationMap</code>.
	 */
    public CorrelationMap getCorrelationMap();

	/**
	 * Fetches the {@link CallbackRegistry} for this component.
	 * @return this component's <code>CallbackRegistry</code>.
	 */
    public CallbackRegistry getCallbackRegistry();

	/**
	 * Fetches the {@link CRLMessageExchangeFactory} for this component.
	 * @return this component's <code>CRLMessageExchangeFactory</code>.
	 */
    public CRLMessageExchangeFactory getCRLMessageExchangeFactory();
    
	/**
	 * Fetches the {@link ExchangeRouter} for this component.
	 * @return this component's <code>ExchangeRouter</code>.
	 */
    public ExchangeRouter getExchangeRouter();
    
	/**
	 * Fetches the {@link EndpointManager} for this component.
	 * @return this component's <code>EndpointManager</code>.
	 */
    public EndpointManager getEndpointManager();
    
	/**
	 * Acquires an {@link ExchangeContext} for this component.
	 * @return an <code>ExchangeContext</code> for this component.
	 */
    public ExchangeContext acquireExchangeContext();
    // release defined on ExchangeContext
    
    /**
     * Fetches the {@link MessagingChannel} for this component.
     * @return the <code>MessagingChannel</code> for this component.
     */
    public MessagingChannel getMessagingChannel();
}
