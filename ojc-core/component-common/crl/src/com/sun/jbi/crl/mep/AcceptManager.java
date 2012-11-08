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
 * @(#)AcceptManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.mep;

import javax.jbi.JBIException;

import com.sun.jbi.component.lifecycle.ComponentManager;

/**
 * Coordinator between accepted message exchanges and message listeners.
 * 
 * @author Kevan Simpson
 */
public interface AcceptManager {
    public void startAccepting() throws JBIException;
    public void stopAccepting() throws JBIException;
    public void shutDown() throws JBIException;
    
//    public EndpointFactory getEndpointFactory();
//    public void setEndpointFactory(EndpointFactory fac);

    public ComponentManager getComponentManager();
    public void setComponentManager(ComponentManager cmgr);
    
    public ExchangeRouter getExchangeRouter();
    public void setExchangeRouter(ExchangeRouter router);

    public ListenerContext getListenerContext();
    public void setListenerContext(ListenerContext ctx);
    
    public MessageListenerFactory getListenerFactory();
    public void setListenerFactory(MessageListenerFactory factory);

    public ThreadManager getThreadManager();
    public void setThreadManager(ThreadManager mgr);

    public int getPollerCount();
    public void setPollerCount(int count);
}
