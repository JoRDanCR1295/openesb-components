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
 * @(#)ScriptseComponentManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.scriptse.jbiadapter;

import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;

import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager;
import com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType;
import com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.OperationalState;

public class ScriptseComponentManager extends AbstractComponentManager {
     /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#init(javax.jbi.component.ComponentContext) */
    public void init(ComponentContext ctx) throws JBIException {
          if (log().isLoggable(Level.FINE)) {
            log().fine("SCPTSE-3001: Initializing  Scripting Engine ...");
        }   
          
        super.init(ctx);
    
       //need to init scripting engine here
        String info = I18n.loc("SCPTSE-5001: Initialized scripting engine successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(), 
		  		  		  null, null, ComponentType.ServiceEngine, 
		  		  		  OperationalState.STARTING, "SCPTSE-5001");
        
    }
    
     /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#start() */
    public void start() throws JBIException {
    	if (log().isLoggable(Level.FINE)) {
    		log().fine("SCPTSE-3002: Starting SCPTSE service engine...");
    	}
    	
        super.start();
        
        String info = I18n.loc("SCPTSE-5002: Started SCPTSE service engine successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(), 
				  		  null, null, ComponentType.ServiceEngine, 
				  		  OperationalState.STARTED, "SCPTSE-5002");
    }
    
    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#stop() */
    public void stop() throws JBIException {
    	if (log().isLoggable(Level.FINE)) {
    		log().fine("SCPTSE-3003: Stopping SCPTSE service engine...");
    	}
    	
        super.stop();
        
        String info = I18n.loc("SCPTSE-5003: Stopped SCPTSE service engine successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(), 
		  		  		  null, null, ComponentType.ServiceEngine, 
		  		  		  OperationalState.STOPPED, "SCPTSE-5003");
    }
    
    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#shutDown() */
    public void shutDown() throws JBIException {
    	if (log().isLoggable(Level.FINE)) {
    		log().fine("SCPTSE-3004: Shutting down SCPTSE service engine...");
    	}
    	
    	super.shutDown();
    	
        String info = I18n.loc("SCPTSE-5004: Shut down SCPTSE service engine successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(), 
		  		  		  null, null, ComponentType.ServiceEngine, 
		  		  		  OperationalState.SHUTDOWN, "SCPTSE-5004");
    }
    
    
     /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initExchangeHandler() */
    protected ExchangeHandler initExchangeHandler() {
        return new ScriptseExchangeHandler(getComponentContext());  
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initEndpointManager() */
    protected EndpointManager initEndpointManager() {
        return  new ScriptseEndpointManager(getComponentContext());
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initRuntimeMBean() */
    protected Object initRuntimeMBean() throws JBIException {
        return new ScriptseConfiguration(getComponentContext(), getConfig());
    }
    
     /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initServiceUnitManager() */
    protected ServiceUnitManager initServiceUnitManager() {
        return new ScriptseServiceUnitManager(getManagerContext());
    }
}
