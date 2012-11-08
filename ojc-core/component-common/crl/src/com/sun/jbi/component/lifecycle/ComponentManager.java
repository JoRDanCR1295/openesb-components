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
 * @(#)ComponentManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.lifecycle;

import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;

import com.sun.jbi.common.classloader.CustomClassLoaderUtil;
import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.crl.mep.AcceptManager;
import com.sun.jbi.crl.mep.CallbackRegistry;
import com.sun.jbi.crl.mep.CorrelationMap;
import com.sun.jbi.crl.mep.ManagerContext;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchangeFactory;

/**
 * Represents a CRL-based JBI component.
 * 
 * @author Kevan Simpson
 */
public interface ComponentManager extends Component, ComponentLifeCycle {
    public AcceptManager getAcceptManager();
//    public void setAcceptManager(AcceptManager amgr);
    
    public ComponentContext getComponentContext();
//    public void setComponentContext(ComponentContext ctx);
    
    public CorrelationMap getCorrelationMap();
//    public void setCorrelationMap(CorrelationMap cmap);
    
    public CallbackRegistry getCallbackRegistry();
//    public void setCallbackRegistry(CallbackRegistry registry);
    
    public CRLMessageExchangeFactory getExchangeFactory();
    
    public EndpointManager getEndpointManager();
//    public void setEndpointManager(EndpointManager emgr);
    
    public ManagerContext getManagerContext();
    
    /**
     * return the thread context class-loader utility for this
     * component. 
     * @return <code>CustomClassLoaderUtil</code>
     */
    public CustomClassLoaderUtil getCustomClassLoaderUtil();
}
