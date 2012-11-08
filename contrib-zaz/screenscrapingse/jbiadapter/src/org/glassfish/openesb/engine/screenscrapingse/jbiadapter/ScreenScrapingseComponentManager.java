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
 * @(#)ScreenScraptingseComponentManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.engine.screenscrapingse.jbiadapter;

import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.MBeanNames;

import javax.management.MBeanServer;
import javax.management.ObjectName;

import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProvider;

import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.lifecycle.ExchangeHandler;
import com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager;
import com.sun.jbi.component.toolkit.util.alerter.Event.ComponentType;
import com.sun.jbi.component.toolkit.util.alerter.NotificationEvent.OperationalState;

public class ScreenScrapingseComponentManager extends AbstractComponentManager {

    private StatusProviderHelper mStatusProviderHelper;

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#init(javax.jbi.component.ComponentContext) */
    public void init(ComponentContext ctx) throws JBIException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("SSSE-3001: Initializing  ScreenScraping Engine ...");
        }
        try {
            MBeanServer mbServer = ctx.getMBeanServer();
            MBeanNames mbNames = ctx.getMBeanNames();
            ObjectName statsMbeanName = mbNames.createCustomComponentMBeanName("Statistics");
            mStatusProviderHelper = new StatusProviderHelper(ctx.getComponentName(),
                    statsMbeanName,
                    mbServer);
            mStatusProviderHelper.registerMBean();
        } catch (Exception ex) {
            throw new JBIException(
                    I18n.loc("SSSE-5006: Status MBean registration failed"), ex);
        }

        super.init(ctx);

        //need to init scripting engine here
        String info = I18n.loc("SSSE-5001: Initialized scripting engine successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(),
                null, null, ComponentType.ServiceEngine,
                OperationalState.STARTING, "SSSE-5001");

    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#start() */
    public void start() throws JBIException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("SSSE-3002: Starting SSSE service engine...");
        }

        super.start();

        String info = I18n.loc("SSSE-5002: Started SSSE service engine successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(),
                null, null, ComponentType.ServiceEngine,
                OperationalState.STARTED, "SSSE-5002");
    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#stop() */
    public void stop() throws JBIException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("SSSE-3003: Stopping SSSE service engine...");
        }

        super.stop();

        String info = I18n.loc("SSSE-5003: Stopped SSSE service engine successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(),
                null, null, ComponentType.ServiceEngine,
                OperationalState.STOPPED, "SSSE-5003");
    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#shutDown() */
    public void shutDown() throws JBIException {
        if (log().isLoggable(Level.FINE)) {
            log().fine("SSSE-3004: Shutting down SSSE service engine...");
        }

        super.shutDown();

        String info = I18n.loc("SSSE-5004: Shut down SSSE service engine successfully!");
        log().info(info);
        getAlerter().info(info, getComponentContext().getComponentName(),
                null, null, ComponentType.ServiceEngine,
                OperationalState.SHUTDOWN, "SSSE-5004");
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initExchangeHandler() */
    protected ExchangeHandler initExchangeHandler() {
        return new ScreenScrapingseExchangeHandler(getComponentContext());
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initEndpointManager() */
    protected EndpointManager initEndpointManager() {
        return new ScreenScrapingseEndpointManager(getComponentContext(), mStatusProviderHelper);
    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initRuntimeMBean() */
    protected Object initRuntimeMBean() throws JBIException {
        return new ScreenScrapingseConfiguration(getComponentContext(), getConfig());

    }

    /** @see com.sun.jbi.component.toolkit.lifecycle.impl.AbstractComponentManager#initServiceUnitManager() */
    protected ServiceUnitManager initServiceUnitManager() {
        return new ScreenScrapingseServiceUnitManager(getManagerContext());
    }
}
