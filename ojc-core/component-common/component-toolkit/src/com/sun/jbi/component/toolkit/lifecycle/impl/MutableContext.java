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
 * @(#)MutableContext.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.lifecycle.impl;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.ManagerContext;

/**
 * Mutable extension of {@link ManagerContext} interface.
 * @author Kevan Simpson
 */
public interface MutableContext extends ManagerContext {
    /**
     * Sets the {@link EndpointManager} for this component.
     * @param endptMgr An <code>EndpointManager</code>.
     */
    public void setEndpointManager(EndpointManager endptMgr);

    /**
     * Sets the {@link ExchangeHandler} for this component.
     * @param endptMgr An <code>ExchangeHandler</code>.
     */
    public void setExchangeHandler(ExchangeHandler handler);

    /**
     * Sets the {@link ComponentConfig} for this component.
     * @param config The component's configuration.
     */
    public void setConfiguration(ComponentConfig config);
}
