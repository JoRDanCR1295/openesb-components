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

package com.sun.jbi.component.toolkit.lifecycle;

import java.util.Map;
import javax.jbi.component.ComponentContext;
import com.sun.jbi.common.classloader.CustomClassLoaderUtil;
import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.component.toolkit.endpoint.EndpointManager;

/**
 * Defines context for {@link ComponentManager}.
 * 
 * @author Kevan Simpson
 */
public interface ManagerContext {
    /**
     * Returns the JBI context for this component.
     * @return the JBI context for this component.
     */
    public ComponentContext getComponentContext();
    
    /**
     * Returns the configuration for this component.
     * @return the configuration for this component.
     */
    public ComponentConfig getConfiguration();
    
    /**
     * Correlates important component data with a message exchange by its id.
     * @return the correlation map for this component.
     */
    public Map<String, Object> getCorrelationMap();
    
    /**
     * Returns the endpoint manager for this component.
     * @return the endpoint manager for this component.
     */
    public EndpointManager getEndpointManager();

    /**
     * Returns the exchange handler for this component.
     * @return the exchange handler for this component.
     */
    public ExchangeHandler getExchangeHandler();

    /**
     * Returns the QoS-enhanced <code>DeliveryChannel</code> for this component.
     * @return the QoS-enhanced <code>DeliveryChannel</code> for this component.
     */
    public MessagingChannel getMessagingChannel();
    
    /**
     * Returns the custom classloading utility for this component.
     * @return the custom classloading utility for this component.
     */
    public CustomClassLoaderUtil getCustomClassLoaderUtil();
}
