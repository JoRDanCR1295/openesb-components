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
 * @(#)AleSEComponentManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.ale;

import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

import com.sun.jbi.common.tale.core.domain.TaleDomain;
import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager;
/**
 * ALE service engine implementation.
 * 
 * @author Kevan Simpson
 */
public class AleSEComponentManager extends AbstractComponentManager {
	/* Domain manager, lifespan equal to component lifespan. */
    private TaleDomain mDomain;
    
    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#init(javax.jbi.component.ComponentContext) */
    public void init(ComponentContext ctx) throws JBIException {
        super.init(ctx);
    
        if (log().isLoggable(Level.FINE)) {
            log().fine("ALESE-3001: Initializing ALE configuration database...");
        }        
        mDomain = TaleDomain.newInstance(ctx, getConfig());
        
        /*
         * To avoid reinitializing the domain, we will store it 
         * in the correlation map using the classname as the key.
         */
        // TODO evaluate this
        getManagerContext().getCorrelationMap().put(
                TaleDomain.class.getName(), mDomain);
    }
    
    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initExchangeHandler() */
    protected ExchangeHandler initExchangeHandler() {
        return new AleSEExchangeHandler(getComponentContext()); 
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initEndpointManager() */
    protected EndpointManager initEndpointManager() {
        return new AleSEEndpointManager(getComponentContext());
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initRuntimeMBean() */
    protected Object initRuntimeMBean() throws JBIException {
        return new AleSEConfiguration(getComponentContext(), getConfig());
    }
}
