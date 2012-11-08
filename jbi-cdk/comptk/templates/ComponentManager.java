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
 * @(#)%CLASS_PREFIX%ComponentManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package %PKG%;

import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;

import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager;
import com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType;
import com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.OperationalState;

/**
 * %PROJ_SHORT_NAME% %COMP_TYPE_DESC% implementation.
 * 
 * @author %AUTHOR%
 */
public class %CLASS_PREFIX%ComponentManager extends AbstractComponentManager {
    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#init(javax.jbi.component.ComponentContext) */
    public void init(ComponentContext context) throws JBIException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("%LOG_PREFIX%-3001: Initializing %PROJ_SHORT_NAME% %COMP_TYPE_DESC%...");
        }
        // initializes logger and then some
        super.init(context);
        
        // TODO (optional) Implement the class below as part of Redelivery support
        /*
        getManagerContext().getMessagingChannel().addSendFailureListener(
                new %CLASS_PREFIX%SendFailureListener(getManagerContext()));
        */
        
        String info = I18n.loc("%LOG_PREFIX%-5001: Initialized %PROJ_SHORT_NAME% %COMP_TYPE_DESC% successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(), 
		  		  		  null, null, ComponentType.%COMP_TYPE_ENUM%, 
		  		  		  OperationalState.STARTING, "%LOG_PREFIX%-5001");
    }
    
    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#start() */
    public void start() throws JBIException {
    	if (log().isLoggable(Level.FINE)) {
    		log().fine("%LOG_PREFIX%-3002: Starting %PROJ_SHORT_NAME% %COMP_TYPE_DESC%...");
    	}
    	
    	// starts NMR polling threads
        super.start();
        
        String info = I18n.loc("%LOG_PREFIX%-5002: Started %PROJ_SHORT_NAME% %COMP_TYPE_DESC% successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(), 
				  		  null, null, ComponentType.%COMP_TYPE_ENUM%, 
				  		  OperationalState.STARTED, "%LOG_PREFIX%-5002");
    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#stop() */
    public void stop() throws JBIException {
    	if (log().isLoggable(Level.FINE)) {
    		log().fine("%LOG_PREFIX%-3003: Stopping %PROJ_SHORT_NAME% %COMP_TYPE_DESC%...");
    	}
    	
    	// stops NMR polling threads
        super.stop();
        
        String info = I18n.loc("%LOG_PREFIX%-5003: Stopped %PROJ_SHORT_NAME% %COMP_TYPE_DESC% successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(), 
		  		  		  null, null, ComponentType.%COMP_TYPE_ENUM%, 
		  		  		  OperationalState.STOPPED, "%LOG_PREFIX%-5003");
    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#shutDown() */
    public void shutDown() throws JBIException {
    	if (log().isLoggable(Level.FINE)) {
    		log().fine("%LOG_PREFIX%-3004: Shutting down %PROJ_SHORT_NAME% %COMP_TYPE_DESC%...");
    	}
    	
    	
    	super.shutDown();
    	
        String info = I18n.loc("%LOG_PREFIX%-5004: Shut down %PROJ_SHORT_NAME% %COMP_TYPE_DESC% successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(), 
		  		  		  null, null, ComponentType.%COMP_TYPE_ENUM%, 
		  		  		  OperationalState.SHUTDOWN, "%LOG_PREFIX%-5004");
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initEndpointManager() */
    protected EndpointManager initEndpointManager() {
        return new %CLASS_PREFIX%EndpointManager(getComponentContext());
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initExchangeHandler() */
    protected ExchangeHandler initExchangeHandler() {
        return new %CLASS_PREFIX%ExchangeHandler(getComponentContext()); 
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initRuntimeMBean() */
    protected Object initRuntimeMBean() throws JBIException {
        return new %CLASS_PREFIX%Config(getComponentContext(), getConfig());
    }

    /*
     *    TODO (optional)
     *    Most components do not need a custom ServiceUnitManager, 
     *    but if you do, uncomment the following lines and
     *    implement %CLASS_PREFIX%ServiceUnitManager 
     */
//    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initServiceUnitManager() */
//    protected ServiceUnitManager initServiceUnitManager() {
//        return new %CLASS_PREFIX%ServiceUnitManager(getManagerContext());
//    }
    
    
    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#postShutDown() */
    @Override
    protected void postShutDown() throws JBIException {
        // TODO (optional) If your component needs to clean up resources after MessagingChannel closes
        super.postShutDown();
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#preShutDown() */
    @Override
    protected void preShutDown() throws JBIException {
        // TODO (optional) If your component needs to clean up resources prior to MessagingChannel close
        super.preShutDown();
    }

}
