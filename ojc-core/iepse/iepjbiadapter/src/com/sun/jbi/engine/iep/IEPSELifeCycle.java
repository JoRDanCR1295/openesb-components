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
 * @(#)IEPSELifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep;

import com.sun.jbi.alerter.NotificationEvent;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.management.ObjectName;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;

import com.sun.jbi.engine.iep.core.runtime.IEPEngine;
import com.sun.jbi.engine.iep.core.runtime.util.NDC;
import com.sun.jbi.engine.iep.core.runtime.util.Util;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;


import com.sun.jbi.engine.iep.core.runtime.util.alert.IEPSEAlertSender;

import java.util.Properties;

/**
 * IEPSELifeCycle.java
 *
 * Created on September 8, 2005, 12:47 AM
 *
 * @author Bing Lu
 */
public class IEPSELifeCycle implements ComponentLifeCycle, IEPConfig {

    private static final Messages mMessages = Messages.getMessages(IEPSELifeCycle.class);
    private static Logger mLogger = Messages.getLogger(IEPSELifeCycle.class);
    private IEPSEComponent mComponent;
    private ExtendedComponentContext mExtendedContext;

    private String createSuccessMessage(String taskName, String componentName) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createSuccessMessage(taskName);
        return retMsg;
    }

    private String createExceptionMessage(String componentName, String taskName, String status, String locToken, String locParam, String locMessage, Throwable exObj) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createExceptionMessage(taskName, locToken, locMessage, locParam, exObj);

        return retMsg;
    }

    public IEPSELifeCycle(IEPSEComponent component) {
        mComponent = component;
    }

    /**
     * Initialize the component. This performs initialization required by the
     * component but does not make it ready to process messages. This method
     * is called once for each life cycle of the component
     *
     * @param context - the component's context.
     * @throws javax.jbi.JBIException if the component is unable to initialize.
     */
    public void init(ComponentContext context) throws JBIException {
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSELifeCycle.init");
            Messages.registerContext(context);
            // reinitialize to get JBI Logger 
            mLogger = Messages.getLogger(IEPSELifeCycle.class);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSELifeCycle.IEP_service_engine_initialization_starts"));
            }

            mExtendedContext = mComponent.getExtendedContext();
            mExtendedContext.init(context);

            IEPSEServiceUnitManager serviceUnitManager =
                    (IEPSEServiceUnitManager) mComponent.getServiceUnitManager();
            serviceUnitManager.initialize(mExtendedContext);

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSELifeCycle.IEP_service_engine_initialization_succeeded"));
            }
        } catch (Exception e) {
            mLogger.log(Level.SEVERE, mMessages.getString("IEPSELifeCycle.IEP_service_engine_initialization_failed"), e);
            String extMsg = createExceptionMessage(context.getComponentName(),
                    "init",
                    "FAILED",
                    "IepSeLifeCycle",
                    "_Init_1",
                    "IEP_service_engine_initialization_error",
                    e);
            throw new javax.jbi.JBIException(extMsg, e);
        } finally {
            NDC.exit("Application", "IEPSE", "Task", "IEPSELifeCycle.init");
        }
    }


    /**
     * Get the JMX ObjectName for any additional MBean for this component.
     * If there is none, return null.
     *
     * @return ObjectName the JMX object name of the additional MBean or null
     * if there is no additional MBean.
     */
    public ObjectName getExtensionMBeanName() {
        return null;
    }

    /**
     * Start the component. This makes the component ready to process messages.
     * This method is called after init() completes when the JBI implementation
     * is starting up, and when the component is being restarted after a
     * previous call to shutDown(). If stop() was called previously but
     * shutDown() was not, then start() can be called again without another call
     * to init().
     *
     * @throws javax.jbi.JBIException if the component is unable to start.
     */
    public void start() throws javax.jbi.JBIException {
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSELifeCycle.start");
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSELifeCycle.Starting_IEP_service_engine"));
            }
            IEPEngine engine = mExtendedContext.getEngine();
            mExtendedContext.loadConfigProperties();
            boolean dbPropChanged = mExtendedContext.diffConfigFiles().size() > 0; // any change requires engine destory?
            mExtendedContext.mergeConfigPropertiesAndFiles(); // merge all changes regardless whether engine destroy required.
            mExtendedContext.loadBootstrapProperties();
            boolean isNewInstall = mExtendedContext.isNewInstall();
            Properties configProp = mExtendedContext.getConfigProperties();
            if (dbPropChanged || isNewInstall) {
                boolean isClustered = Boolean.parseBoolean(configProp.getProperty(PROP_IS_CLUSTERED));
                // 1. install -> start
                // Open ESB installer installs sun-iep-engine, and pakcages sun-iep-engine directory 
                // but not the javadb directory. The installer must package bootstrap.properties with 
                // newInstall set to true so that javadb initialization will take place.
                // 2. db property change: stop -> start
                // 3. db property change: shutdown -> start
                // 
                // Note that when installing IEPSE in the clustered mode, it is
                // the user's responsibility to make sure that the targetting database doesn't
                // have an IEPSE pre-installed. More specifically, 
                // a) the targetting database doesn't have any table whose name is one of EMS_ENGINE, EMS_OUTPUT, EMS_PLAN, and EMS_TOKEN.
                // b) the targetting database doesn't have any table whose name whose name is in the pattern of q_{integer}_o{integer}.
                // and 
                // c) the targetting database doesn't have any stored procedure named as 'IepStoredProcedures'
                engine.init(configProp);
                if (!isClustered) {
                    engine.destroy();
                    engine.init(configProp);
                }
                mExtendedContext.startHeartBeatThread();
            } else if (!engine.isInitialized()) {
                // 4. no db property change: shutdown -> start
                // While the iep-service-engine is in shutdown mode, the app. server
                // can be in the shutdown mode, it is a user error to switch to a different database
                // during that time. 
                engine.setConfigProperties(configProp);

                // In case of server crash, we will have lost DbSpecial in Util.
                if (Util.getDbSpecial() == null) {
                    Util.setDbSpecial(configProp);
                }
                mExtendedContext.startHeartBeatThread();
            } else {
                // 5. no db property change: stop -> start
                engine.setConfigProperties(configProp);
            }
            mExtendedContext.startIOThreads();
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSELifeCycle.IEP_service_engine_started_successfully"));
            }
            //Alert
            IEPSEAlertSender.getInstance().alertEngineStatusChange(engine.getId(), NotificationEvent.OPERATIONAL_STATE_STARTED);
            //
        } catch (Exception ex) {
            throw new javax.jbi.JBIException(mMessages.getString("IEPSELifeCycle.IEP_service_engine_start_failed"), ex);
        } finally {
            NDC.exit("Application", "IEPSE", "Task", "IEPSELifeCycle.start");
        }
        
    }

    /**
     * Stop the component. This makes the component stop accepting messages for
     * processing. After a call to this method, start() can be called again
     * without first calling init().
     *
     * @throws javax.jbi.JBIException if the component is unable to stop.
     */
    public void stop() throws javax.jbi.JBIException {
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSELifeCycle.stop");
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSELifeCycle.Stopping_IEP_service_engine"));
            }
            mExtendedContext.stopIOThreads();
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSELifeCycle.IEP_service_engine_stopped_successfully"));
            }
            //Alert
            IEPEngine engine = mExtendedContext.getEngine();
            IEPSEAlertSender.getInstance().alertEngineStatusChange(engine.getId(), NotificationEvent.OPERATIONAL_STATE_STOPPED);
            //
        } catch (Exception ex) {
            throw new javax.jbi.JBIException(mMessages.getString("IEPSELifeCycle.IEP_service_engine_stop_failed"), ex);
        } finally {
            NDC.exit("Application", "IEPSE", "Task", "IEPSELifeCycle.stop");
        }
    }

    /**
     * Shut down the component. This performs cleanup before the component is
     * terminated. Once this method has been called, init() must be called
     * before the component can be started again with a call to start().
     *
     * @throws javax.jbi.JBIException if the component is unable to shut down.
     */
    public void shutDown() throws javax.jbi.JBIException {
        try {
            NDC.enter("Application", "IEPSE", "Task", "IEPSELifeCycle.shutDown");
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSELifeCycle.IEP_service_engine_shut_down_starts"));
            }
            mExtendedContext.stopHeartBeatThread();
            mExtendedContext.destory();
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, mMessages.getString("IEPSELifeCycle.IEP_service_engine_shut_down_succeeded"));
            }
            //Alert
            //A note of getting the engine id:
            //The DefaultIEPEngine is initialized when the start is called. If the engine is started and then stopped
            //and shutdown then the engine id can be got from the DefaultIEPEngine. Consider the case when the engine 
            //is stopped and then the appserver is restarted. In this case only the init will be called. Consequently
            //the DefaultIEPEngine is not initialized. If the user calls shutdown, then the engine id cannot be obtained
            //from the DefaultIEPEngine because it was not initialized. So we need to get it from the ExtendedContext which
            //does get initialized when the init is called.
            String engineId = (String) mExtendedContext.getConfigProperties().getProperty(PROP_ENGINE_ID);
            IEPSEAlertSender.getInstance().alertEngineStatusChange(engineId, NotificationEvent.OPERATIONAL_STATE_SHUTDOWN);
        } catch (Exception ex) {
            throw new javax.jbi.JBIException(mMessages.getString("IEPSELifeCycle.IEP_service_engine_shut_down_failed"), ex);
        } finally {
            NDC.exit("Application", "IEPSE", "Task", "IEPSELifeCycle.shutDown");
        }
    }
}
