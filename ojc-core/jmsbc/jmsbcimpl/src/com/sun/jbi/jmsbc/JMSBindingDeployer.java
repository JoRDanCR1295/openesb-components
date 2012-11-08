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
 * @(#)JMSBindingDeployer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.management.DeploymentException;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.component.ComponentContext;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;

import com.sun.jbi.jmsbc.jms.ChannelManager;
import com.sun.jbi.jmsbc.mbeans.JMSBCRuntimeConfigurationMBean;
import com.sun.jbi.jmsbc.mbeans.RuntimeConfigurationMBean;
import com.sun.jbi.jmsbc.recovery.ConnectionInfoPersister;
import com.sun.jbi.jmsbc.util.AlertsUtil;
import com.sun.jbi.jmsbc.validator.EndpointValidator;

public class JMSBindingDeployer implements ServiceUnitManager {

    private static final Messages mMessages =
        Messages.getMessages(JMSBindingDeployer.class);
    private static final Logger mLogger =
        Messages.getLogger(JMSBindingDeployer.class);
    
    private Map<String, ServiceUnit> mServiceUnits;
    private ComponentContext mContext;
    private StatusProviderHelper mStatusProviderHelper;
    private ChannelManager mJMSChannelMgr;
    private ConnectionInfoPersister mConnPersister;
    private JMSBCRuntimeConfigurationMBean mRuntimeConfig;
    private JBITaskMessageBuilder mMsgBuilder;
    private Map mInboundMessageExchanges;
    
    /**
     * JMSBindingDeployer constructor
     */
    public JMSBindingDeployer(ComponentContext context,
                              StatusProviderHelper statusProviderHelper,
                              ChannelManager jmsChannelMgr,
                              ConnectionInfoPersister connPersister,
                              JMSBCRuntimeConfigurationMBean runtimeConfig,
                              Map inboundMessageExchanges) {
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mServiceUnits = new Hashtable<String, ServiceUnit>();
        mJMSChannelMgr = jmsChannelMgr;
        mConnPersister = connPersister;
        mRuntimeConfig = runtimeConfig;
        mMsgBuilder = new DefaultJBITaskMessageBuilder();
        mMsgBuilder.setComponentName(mContext.getComponentName());
        mInboundMessageExchanges = inboundMessageExchanges;
    }

    /**
     * JMSBindingDeployer constructor
     */
    protected JMSBindingDeployer(ComponentContext context,
                              StatusProviderHelper statusProviderHelper,
                              ChannelManager jmsChannelMgr,
                              Map<String, ServiceUnit> serviceUnits,
                              ConnectionInfoPersister connPersister,
                              JMSBCRuntimeConfigurationMBean runtimeConfig) {
        this(context, statusProviderHelper, jmsChannelMgr, connPersister, runtimeConfig, null);
        mServiceUnits = serviceUnits;
    }
    
    ////////
    //
    //  ServiceUnitManager Interface Methods
    //
    ////////

    /**
     * Initiate a BC Deployment.
     *
     * @param suId - service unit ID
     * @param suPath - Path of the service assembly file
     */
    public String deploy(String suId,
                         String suPath)
        throws DeploymentException {

        String taskName = "deploy";  // no i18n
        
        ServiceUnit su = null;
        try {
            su = mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId,
                                         suPath,
                                         mContext,
                                         mStatusProviderHelper,
                                         mJMSChannelMgr,
                                         mConnPersister,
                                         mRuntimeConfig,
                                         mInboundMessageExchanges);
            }
            su.deploy();
        } catch (Exception ex) {
            if (su != null) {
                try {
                    su.stop();
                    su.shutdown();
                } catch (Throwable th) {
                    // Ignore on purpose.
                }
            }

            String errMsg = mMessages.getString("JMSBC-E0726.ServiceUnitDeployFailed",
                                new Object []{suId, suPath, ex.getLocalizedMessage()});
            
            mLogger.log(Level.SEVERE, errMsg, ex);
            AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0726.ServiceUnitDeployFailed",
                                new Object []{suId, suPath, ex.getLocalizedMessage()}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    suId, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0726");
            
            mMsgBuilder.throwException(taskName,
                                       "JMSBC_DEPLOY_1",
                                       errMsg,
                                       null,
                                       ex);            
        }
        
        //Put the service unit map, only after the deployment succeeds
        mServiceUnits.put(suId, su);
                        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO,
                        "JMSBC-I0709.ServiceUnitDeployed",
                        new Object[]{suId, suPath});
        }
        
        return mMsgBuilder.createSuccessMessage(taskName);
    }
    
    public void init(String suId, String suPath) throws DeploymentException {
        String taskName = "init";  // no i18n
                
        ServiceUnit su = null;
        try {
            su = mServiceUnits.get(suId);
            
            if (su == null) {
                su = new ServiceUnitImpl(suId,
                                         suPath,
                                         mContext,
                                         mStatusProviderHelper,
                                         mJMSChannelMgr,
                                         mConnPersister,
                                         mRuntimeConfig,
                                         mInboundMessageExchanges);
            }
            
            validateServiceUnitForUniqueness(su);                
            su.init(suPath);
            mServiceUnits.put(suId, su);
        } catch (Exception ex) {
            // Clean up our state
            if (su != null) {
                try {
                    su.stop();
                } catch (Throwable th) {
                    // Ignore on purpose.
                }
            }
            
            String errMsg = mMessages.getString("JMSBC-E0725.ServiceUnitInitializeFailed",
                                new Object []{suId, suPath, ex.getLocalizedMessage()});
            
            mLogger.log(Level.SEVERE, errMsg, ex);
            AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0725.ServiceUnitInitializeFailed",
                                new Object []{suId, suPath, ex.getLocalizedMessage()}), 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    suId, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0725");
            mMsgBuilder.throwException(taskName,
                                       "JMSBC_INIT_1",
                                       errMsg,
                                       null,
                                       ex);
        }
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO,
                        "JMSBC-I0710.ServiceUnitInitialized",
                        new Object[]{suId, suPath});
        }                
    }
    
    
    public void shutDown(String suId) throws DeploymentException {
        String taskName = "shutDown";  // no i18n
        
        ServiceUnit su = mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.shutdown();
            } catch (Exception ex) {
                String errMsg = mMessages.getString("JMSBC-E0727.ServiceUnitShutdownFailed",
                                    new Object []{suId, ex.getLocalizedMessage()});
                AlertsUtil.getAlerter().critical(errMsg, 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    suId, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0727");
                mLogger.log(Level.SEVERE, errMsg, ex);

                mMsgBuilder.throwException(taskName,
                                           "JMSBC_SHUTDOWN_1",
                                           errMsg,
                                           null,
                                           ex);
            }
        }
        
        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG, 
                        "JMSBC-I0714.ServiceUnitShutdown",
                        new Object[]{suId});
        }
    }
    
    public void start(String suId) throws DeploymentException {
        String taskName = "start";  // no i18n        
        ServiceUnit su = mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.start();
            } catch (Exception ex) {
                String errMsg = mMessages.getString("JMSBC-E0728.ServiceUnitStartFailed",
                                    new Object []{suId, ex.getLocalizedMessage()});

                mLogger.log(Level.SEVERE, errMsg, ex);
                AlertsUtil.getAlerter().critical(errMsg, 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    suId, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0728");
                mMsgBuilder.throwException(taskName,
                                           "JMSBC_START_1",
                                           errMsg,
                                           null,
                                           ex);
            }
        }
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, 
                        "JMSBC-I0711.ServiceUnitStarted",
                        new Object[]{suId});
        }
    }
    
    public void stop(String suId) throws DeploymentException {
        String taskName = "stop";  // no i18n        
        ServiceUnit su = mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.stop();
            } catch (Exception ex) {
                String errMsg = mMessages.getString("JMSBC-E0729.ServiceUnitStopFailed",
                                    new Object []{suId, ex.getLocalizedMessage()});

                mLogger.log(Level.SEVERE, errMsg, ex);
                AlertsUtil.getAlerter().critical(errMsg, 
                    JMSBindingComponent.SHORT_DISPLAY_NAME, 
                    suId, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "JMSBC-E0729");
                mMsgBuilder.throwException(taskName,
                                           "JMSBC_STOP_1",
                                           errMsg,
                                           null,
                                           ex);
            }
        }
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, 
                        "JMSBC-I0712.ServiceUnitStopped",
                        new Object[]{suId});
        }
    }
    
    
    /**
     * Cancel a Service Deployment.  If the deployment is in use
     * (has dependencies), then this operation may fail.
     *
     * @param name - name of the service unit
     * @param root - root of the service unit
     */
    public String undeploy(String name, String root)
        throws DeploymentException {

        String retMsg = null;
        String taskName = "undeploy";
        
        // make sure the service unit is properly shut down
        shutDown(name);
        
        if (mServiceUnits.containsKey(name)) {
            mServiceUnits.remove(name);
        }
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO,
                        "JMSBC-I0713.ServiceUnitUndeployed",
                        new Object[]{name, root});
        }
        
        retMsg = mMsgBuilder.createSuccessMessage(taskName);
        return retMsg;
    }
    
    
    ////////
    //
    //  JMSBindingDeployer Public Methods
    //
    ////////

    public Collection<ServiceUnit> getServiceUnits() {
        return Collections.unmodifiableCollection(mServiceUnits.values());
    }

    ////////
    //
    //  JMSBindingDeployer Private Methods
    //
    ////////
            
    private boolean validateServiceUnitForUniqueness(ServiceUnit su) throws Exception {
    	Collection<Endpoint> theEndpoints = su.getEndpoints();
    	for (Endpoint aEndpoint : theEndpoints) {
    	    Set<Map.Entry<String, ServiceUnit>> suEntries = mServiceUnits.entrySet();
    	    for (Map.Entry<String, ServiceUnit> suEntry : suEntries) {
                ServiceUnit aServiceUnit = suEntry.getValue();
                if (aServiceUnit.getServiceUnitId().equals(su.getServiceUnitId())) {
                    // validation within the service unit is done at SU deployment time
                    continue;
                }
                Collection<Endpoint> activatedEndpoints = aServiceUnit.getEndpoints();
                
                EndpointValidator.validateEndpointForUniqueness(activatedEndpoints, aEndpoint, true);
            }
        }
        
        return true;
    }
}
