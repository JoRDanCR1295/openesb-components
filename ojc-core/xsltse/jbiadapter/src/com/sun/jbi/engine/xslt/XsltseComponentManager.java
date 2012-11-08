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
 * @(#)XsltseComponentManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.xslt;

import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager;
import com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType;
import com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.OperationalState;
import com.sun.transform.engine.EngineConfig;
import com.sun.transform.engine.TransformEndpointManager;
import com.sun.transform.engine.EngineConfig.TransformEngine;
import com.sun.transform.engine.runtime.TransformSendFailureListener;
import com.sun.transform.engine.runtime.impl.TransformServiceUnitManager;

/**
 * Xslt service engine implementation.
 * 
 * @author Kevan Simpson
 */
public class XsltseComponentManager extends AbstractComponentManager {
    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#init(javax.jbi.component.ComponentContext) */
    public void init(ComponentContext context) throws JBIException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("XSLTSE-3001: Initializing XSLT service engine...");
        }
        // initializes logger and then some
        super.init(context);
        
        // TODO determine better way to integrate SendFailureListener
        getManagerContext().getMessagingChannel().addSendFailureListener(
                new TransformSendFailureListener(getManagerContext()));
        
        String info = I18n.loc("XSLTSE-5001: Initialized XSLT service engine successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(), 
		  		  		  null, null, ComponentType.ServiceEngine, 
		  		  		  OperationalState.STARTING, "XSLTSE-5001");
    }
    
    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#start() */
    public void start() throws JBIException {
    	if (log().isLoggable(Level.FINE)) {
    		log().fine("XSLTSE-3002: Starting XSLT service engine...");
    	}
    	
        super.start();
        
        String info = I18n.loc("XSLTSE-5002: Started XSLT service engine successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(), 
				  		  null, null, ComponentType.ServiceEngine, 
				  		  OperationalState.STARTED, "XSLTSE-5002");
    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#stop() */
    public void stop() throws JBIException {
    	if (log().isLoggable(Level.FINE)) {
    		log().fine("XSLTSE-3003: Stopping XSLT service engine...");
    	}
    	
        super.stop();
        
        String info = I18n.loc("XSLTSE-5003: Stopped XSLT service engine successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(), 
		  		  		  null, null, ComponentType.ServiceEngine, 
		  		  		  OperationalState.STOPPED, "XSLTSE-5003");
    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#shutDown() */
    public void shutDown() throws JBIException {
    	if (log().isLoggable(Level.FINE)) {
    		log().fine("XSLTSE-3004: Shutting down XSLT service engine...");
    	}
    	
    	super.shutDown();
    	
        String info = I18n.loc("XSLTSE-5004: Shut down XSLT service engine successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(), 
		  		  		  null, null, ComponentType.ServiceEngine, 
		  		  		  OperationalState.SHUTDOWN, "XSLTSE-5004");
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initEndpointManager() */
    protected EndpointManager initEndpointManager() {
        String engineLib = getConfig().getProperty(
                EngineConfig.TRANSFORM_ENGINE_PROPERTY).getValue();
        EngineConfig engine = EngineConfig.newInstance(
                TransformEngine.valueOf(engineLib));
        return new TransformEndpointManager(getComponentContext(), engine);
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initExchangeHandler() */
    protected ExchangeHandler initExchangeHandler() {
        // pass alerter in case loading of configured XsltEngine fails
        return new XsltSEExchangeHandler(getComponentContext());
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initRuntimeMBean() */
    protected Object initRuntimeMBean() throws JBIException {
        return new XsltSEConfig(getComponentContext(), getConfig());
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initServiceUnitManager() */
    protected ServiceUnitManager initServiceUnitManager() {
        return new TransformServiceUnitManager(getManagerContext());
    }
}
