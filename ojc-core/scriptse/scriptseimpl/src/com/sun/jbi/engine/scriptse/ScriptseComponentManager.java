/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */
package com.sun.jbi.engine.scriptse;

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
import com.sun.jbi.engine.scriptse.ScriptseEndpointFactory;
import com.sun.jbi.engine.scriptse.process.ScriptseInOnlyConsumer;
import com.sun.jbi.engine.scriptse.process.ScriptseInOnlyProvider;
import com.sun.jbi.engine.scriptse.process.ScriptseInOutConsumer;
import com.sun.jbi.engine.scriptse.process.ScriptseInOutProvider;

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
 * Script service engine implementation.
 *
 * @author Prashanth B.R
 */
public class ScriptseComponentManager extends AbstractComponentManager {
    // TODO acquire Logger from ComponentContext?
    private static final Logger mLogger = Logger.getLogger(
            ScriptseComponentManager.class.getName()
        );
    private StatusProviderHelper mStatusProviderHelper;

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#init(javax.jbi.component.ComponentContext)
     */
    public void init(ComponentContext context) throws JBIException {
        super.init(context);

        mLogger.info("Initializing Script service engine");

        try {
            mStatusProviderHelper = new StatusProviderHelper(
                    "ScriptSE Status", StatusProviderMBean.COMPONENT_TYPE_ENGINE,
                    context.getComponentName(), context.getMBeanServer()
                );
            mStatusProviderHelper.registerMBean();

            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.info("Registered Status Provider MBean for " + context.getComponentName());
            }
        } catch (Exception e) {
            mLogger.log(Level.WARNING, "Failed to register status provider MBean", e);
            throw new JBIException("Failed to register status provider MBean", e);
        }

        try {
            // See "Prepare Installation Context" section in JBI 1.0 pr spec 6.4.2.1.1
            Properties configProp = new Properties();
            String workspaceRoot = context.getWorkspaceRoot();
            String configFile = workspaceRoot + File.separator +
                ConfigPersistence.PERSISTENT_CONFIG_FILE_NAME;
            configProp.load(new FileInputStream(configFile));
            mLogger.info("Scriptse config properties: \n" + String.valueOf(configProp));

            //            ScriptSEServiceUnitManager serviceUnitManager =
            //                    (ScriptSEServiceUnitManager) getServiceUnitManager();
            //            serviceUnitManager.initialize(mStatusProviderHelper);
        } catch (Exception ex) {
            String error = "Failed to initialize Script SE: " + ex.getMessage();
            mLogger.log(Level.SEVERE, error, ex);
            throw new JBIException(error, ex);
        }

        mLogger.info("Initialized Script service engine successfully");
    }

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#shutDown()
     */
    public void shutDown() throws JBIException {
        mLogger.info("Shutting down Script service engine");

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
            String msg = "Script service engine shut down error: " + e.getMessage();
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

        mLogger.info("Shut down Script service engine successfully");
    }

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#start()
     */
    public void start() throws JBIException {
        mLogger.info("Starting Script service engine");
        super.start();
        mLogger.info("Started Script service engine successfully");
    }

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#stop()
     */
    public void stop() throws JBIException {
        mLogger.info("Stopping Script service engine");
        super.stop();
        mLogger.info("Stopped Script service engine successfully");
    }

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initAcceptManager()
     */
    protected AcceptManager initAcceptManager() {
        // initialize AcceptManager
        AcceptManager amgr = new DefaultAcceptManager(this);

        // override default ExchangeContext
        //        DefaultListenerContext ctx = (DefaultListenerContext) amgr.getListenerContext();
        //        ctx.setExchangeContext(new ScriptseExchangeContext(ctx, null));//mScriptMapEntryTable));
        // register processors
        ExchangeRouter router = amgr.getExchangeRouter();
        router.register(
            PatternRoleKey.IN_OUT_PROVIDER, //new ScriptseInOutProvider());
            new SimpleProcessorFactory(new ScriptseInOutProvider())
        );
        router.register(
            PatternRoleKey.IN_OUT_CONSUMER, new SimpleProcessorFactory(new ScriptseInOutConsumer())
        );
        router.register(
            PatternRoleKey.IN_ONLY_PROVIDER,
            new SimpleProcessorFactory(new ScriptseInOnlyProvider())
        );
        router.register(
            PatternRoleKey.IN_ONLY_CONSUMER,
            new SimpleProcessorFactory(new ScriptseInOnlyConsumer())
        );

        return amgr;
    }

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initEndpointManager()
     */
    protected EndpointManager initEndpointManager() {
        return new DefaultEndpointManager(new ScriptseEndpointFactory());
    }

    /**
     *
     * @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initServiceUnitManager(javax.jbi.component.ComponentContext)
     */
    protected ServiceUnitManager initServiceUnitManager() {
        return new ScriptseServiceUnitManager(getManagerContext(), getEndpointManager());
    }
}
