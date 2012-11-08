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
 * @(#)DefaultExchangeContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep.impl;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;

import org.w3c.dom.Document;

import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.crl.mep.CallbackRegistry;
import com.sun.jbi.crl.mep.ContextPool;
import com.sun.jbi.crl.mep.CorrelationMap;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.ListenerContext;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchangeFactory;
import com.sun.jbi.crl.xml.XmlUtil;

/**
 * Default implementation of {@link ExchangeContext}.
 * 
 * @author Kevan Simpson
 */
public class DefaultExchangeContext implements ExchangeContext {
    private ContextPool mPool = null;
    private ListenerContext mListenerCtx = null;
    private DocumentBuilder mBuilder = null;
    
    public DefaultExchangeContext(ContextPool pool, ListenerContext lctx) {
        mPool = pool;
        mListenerCtx = lctx;
        mBuilder = XmlUtil.newDocBuilder();
    }

    /** @see com.sun.jbi.crl.mep.ExchangeContext#getCallbackRegistry() */
    public CallbackRegistry getCallbackRegistry() {
        return mListenerCtx.getCallbackRegistry();
    }

    /** @see com.sun.jbi.crl.mep.ExchangeContext#getCorrelationMap() */
    public CorrelationMap getCorrelationMap() {
        return mListenerCtx.getCorrelationMap();
    }

    /** @see com.sun.jbi.crl.mep.ExchangeContext#getEndpointManager() */
    public EndpointManager getEndpointManager() {
        return mListenerCtx.getEndpointManager();
    }

    /** Fetches the message exchange factory associated with the channel. */
    public CRLMessageExchangeFactory getExchangeFactory() {
        return mListenerCtx.getCRLMessageExchangeFactory();
    }
    
    /** @see com.sun.jbi.crl.mep.ExchangeContext#lookupServiceEndpoint(javax.xml.namespace.QName, java.lang.String) */
    public ServiceEndpoint lookupServiceEndpoint(QName serviceName, String endpointName) {
        return mListenerCtx.getEndpoint(serviceName, endpointName);
    }

    /** Returns newly-created DOM document. */
    public Document newDocument() { 
        return (mBuilder == null) ? null : mBuilder.newDocument(); 
    }
    
    /** Releases this context resource. */
    public void release() { 
        if (mPool != null) mPool.releaseContext(this); 
    }
}
