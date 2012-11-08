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

package com.sun.jbi.component.toolkit.lifecycle.impl;


import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

import com.sun.jbi.common.classloader.CustomClassLoaderUtil;
import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;

/**
 * Default implementation of {@link ManagerContext}.
 * 
 * @author Kevan Simpson
 */
public class DefaultManagerContext implements MutableContext {
    // we don't need a pool, as our default impl is stateless and thread-safe
//    private ComponentManager mComponentMgr;
    private final ComponentContext mCtx;
    private final MessagingChannel mChannel;
    private final Map<String, Object> mCorrelationMap;
    private EndpointManager mEndptMgr;
    private ExchangeHandler mHandler;
    private ComponentConfig mConfig;
    private final CustomClassLoaderUtil mCustomCLUtil;

    public DefaultManagerContext(ComponentContext ctx, 
                                 CustomClassLoaderUtil cclu) throws JBIException {
        this(ctx, null, null, null, cclu);
    }

    public DefaultManagerContext(ComponentContext ctx, 
                                 EndpointManager emgr, 
                                 ExchangeHandler handler,
                                 ComponentConfig config,
                                 CustomClassLoaderUtil cclu) throws JBIException {
        mCtx = ctx;
        mChannel = new BaseMessagingChannel(ctx);
        mConfig = config;
        mCorrelationMap = Collections.synchronizedMap(new HashMap<String, Object>());
        mCustomCLUtil = cclu;
        setConfiguration(config);
        // anoint endpoint manager and exchange handler with this context
        setEndpointManager(emgr);
        setExchangeHandler(handler);
    }
    
    /** @see com.sun.jbi.component.toolkit.lifecycle.ManagerContext#getComponentContext() */
    public ComponentContext getComponentContext() {
        return mCtx;
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ManagerContext#getConfiguration() */
    public ComponentConfig getConfiguration() {
        return mConfig;
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ManagerContext#getCorrelationMap() */
    public Map<String, Object> getCorrelationMap() {
        return mCorrelationMap;
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ManagerContext#getCustomClassLoaderUtil() */
    public CustomClassLoaderUtil getCustomClassLoaderUtil() {
        return mCustomCLUtil;
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ManagerContext#getEndpointManager() */
    public EndpointManager getEndpointManager() {
        return mEndptMgr;
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ManagerContext#getExchangeHandler() */
    public ExchangeHandler getExchangeHandler() {
        return mHandler;
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.ManagerContext#getMessagingChannel() */
    public MessagingChannel getMessagingChannel() {
        return mChannel;
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.MutableContext#setConfiguration(com.sun.jbi.common.qos.config.ComponentConfig) */
    public void setConfiguration(ComponentConfig config) {
        mConfig = config;
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.MutableContext#setEndpointManager(com.sun.jbi.component.toolkit.endpoint.EndpointManager) */
    public void setEndpointManager(EndpointManager endptMgr) {
        mEndptMgr = endptMgr;
        if (mEndptMgr != null) {
            mEndptMgr.setContext(this);
        }
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.MutableContext#setExchangeHandler(com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler) */
    public void setExchangeHandler(ExchangeHandler handler) {
        mHandler = handler;
        if (mHandler != null) {
            mHandler.setContext(this);
        }
    }
}
