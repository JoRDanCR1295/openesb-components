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
 * Copyright 2007-2008 ZAZ Consulting, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.zaz.jbi.engine.screenscrapingse;

import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.component.endpoint.impl.DefaultEndpointManager;
import com.sun.jbi.component.lifecycle.impl.AbstractComponentManager;
import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.crl.mep.AcceptManager;
import com.sun.jbi.crl.mep.ExchangeRouter;
import com.sun.jbi.crl.mep.impl.DefaultAcceptManager;
import com.sun.jbi.crl.mep.impl.PatternRoleKey;
import com.sun.jbi.crl.mep.impl.SimpleProcessorFactory;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;

import com.zaz.jbi.engine.screenscrapingse.ScreenScrapingseEndpointFactory;
import com.zaz.jbi.engine.screenscrapingse.process.ScreenScrapingSEInOnlyConsumer;
import com.zaz.jbi.engine.screenscrapingse.process.ScreenScrapingSEInOnlyProvider;
import com.zaz.jbi.engine.screenscrapingse.process.ScreenScrapingSEInOutConsumer;
import com.zaz.jbi.engine.screenscrapingse.process.ScreenScrapingSEInOutProvider;

import java.io.File;
import java.io.FileInputStream;

import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.DeliveryChannel;

/**
 * Screen Scraping service engine implementation.
 *
 * @author ZAZ Consulting
 */
public class ScreenScrapingseComponentManager extends AbstractComponentManager {
    // TODO acquire Logger from ComponentContext?

    private static final Logger mLogger = Logger.getLogger(ScreenScrapingseComponentManager.class.getName());
    private StatusProviderHelper mStatusProviderHelper;

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#init(javax.jbi.component.ComponentContext)
     */
    public void init(ComponentContext context) throws JBIException {
        super.init(context);

        mLogger.info("Initializing Screen Scraping service engine");

        try {
            mStatusProviderHelper = new StatusProviderHelper("Screen Scraping Service Engine Status",
                    StatusProviderMBean.COMPONENT_TYPE_ENGINE,
                    context.getComponentName(), context.getMBeanServer());
            mStatusProviderHelper.registerMBean();

            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.info("Registered Status Provider MBean for " +
                        context.getComponentName());
            }
        } catch (Exception e) {
            mLogger.log(Level.WARNING,
                    "Failed to register status provider MBean", e);
            throw new JBIException("Failed to register status provider MBean", e);
        }

        try {
            // See "Prepare Installation Context" section in JBI 1.0 pr spec 6.4.2.1.1
            Properties configProp = new Properties();
            String workspaceRoot = context.getWorkspaceRoot();
            String configFile = workspaceRoot + File.separator +
                    ConfigPersistence.PERSISTENT_CONFIG_FILE_NAME;
            configProp.load(new FileInputStream(configFile));
            mLogger.info("Screen Scraping Service Engine config properties: \n" +
                    String.valueOf(configProp));
        } catch (Exception ex) {
            String error = "Failed to initialize Screen Scraping SE: " +
                    ex.getMessage();
            mLogger.log(Level.SEVERE, error, ex);
            throw new JBIException(error, ex);
        }

        mLogger.info("Initialized Screen Scraping service engine successfully");
    }

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#shutDown()
     */
    public void shutDown() throws JBIException {
        mLogger.info("Shutting down Screen Scraping service engine");

        try {
            // Must close DeliveryChannel before stopping accept threads
            // because they may be pending on accept().
            // Closing channel will wake it up from accept()
            DeliveryChannel channel = getComponentContext().getDeliveryChannel();

            if (channel != null) {
                channel.close();
            }

            super.shutDown();
        } catch (Exception e) {
            String msg = "Screen Scraping service engine shut down error: " +
                    e.getMessage();
            mLogger.log(Level.WARNING, msg, e);
            throw new JBIException(msg, e);
        }

        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (Exception e) {
            String msg = "Failed to unregister status provider MBean for " +
                    getComponentContext().getComponentName();
            mLogger.log(Level.WARNING, msg, e);
            throw new JBIException(msg, e);
        }

        mLogger.info("Shut down Screen Scraping service engine successfully");
    }

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#start()
     */
    public void start() throws JBIException {
        mLogger.info("Starting Screen Scraping service engine");
        super.start();
        mLogger.info("Started Screen Scraping service engine successfully");
    }

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#stop()
     */
    public void stop() throws JBIException {
        mLogger.info("Stopping Screen Scraping service engine");
        super.stop();
        mLogger.info("Stopped Screen Scraping service engine successfully");
    }

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initAcceptManager()
     */
    protected AcceptManager initAcceptManager() {
        // initialize AcceptManager
        AcceptManager amgr = new DefaultAcceptManager(this);

        ExchangeRouter router = amgr.getExchangeRouter();
        router.register(PatternRoleKey.IN_OUT_PROVIDER, //new ScreenScrapingSEInOutProvider());
                new SimpleProcessorFactory(new ScreenScrapingSEInOutProvider()));
        router.register(PatternRoleKey.IN_OUT_CONSUMER,
                new SimpleProcessorFactory(new ScreenScrapingSEInOutConsumer()));
        router.register(PatternRoleKey.IN_ONLY_PROVIDER,
                new SimpleProcessorFactory(new ScreenScrapingSEInOnlyProvider()));
        router.register(PatternRoleKey.IN_ONLY_CONSUMER,
                new SimpleProcessorFactory(new ScreenScrapingSEInOnlyConsumer()));

        return amgr;
    }

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initEndpointManager()
     */
    protected EndpointManager initEndpointManager() {
        return new DefaultEndpointManager(new ScreenScrapingseEndpointFactory());
    }

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initServiceUnitManager(javax.jbi.component.ComponentContext)
     */
    protected ServiceUnitManager initServiceUnitManager() {
        return new ScreenScrapingseServiceUnitManager(getManagerContext(),
                getEndpointManager());
    }
}
