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
 * @(#)HttpSoapBindingDeployer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import java.nio.ByteBuffer;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.jbi.component.ServiceUnitManager;

import com.sun.jbi.httpsoapbc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.httpsoapbc.util.AlertsUtil;
import com.sun.jbi.httpsoapbc.security.impl.CredentialValidatorManager;

/**
 * HttpSoapBindingDeployer is responsible for lifecycle management of
 * ServiceUnits.  This class handles interfacing between ServiceUnits
 * and the JBI Framework.  The main responsibilities of this class is
 * error handling of deployment as defined by the JBI specification,
 * and management of ServiceUnit objects.
 * <p>
 * Management of ServiceUnits is pretty simple.  ServiceUnits are kept
 * in a Map.  Depending on the lifecycle method that is called,
 * ServiceUnits are either initialized, started, stopped, or shutdown.
 * <p>
 * This class also handles the proper return messages as specified by
 * the JBI specification.  Lifecycle methods require status messages
 * in a particular format.  Exception messages must also conform to
 * that format.
 * <p>
 * Finally, the HttpSoapBindingDeployer also acts as a liason between
 * the HttpSoapBindingLifeCycle class.  Using the Observable pattern,
 * represented by the EndpointChangeSubject and EndpointChangeListener
 * interfaces, the HttpSoapbindingDeployer can let interested observers
 * (namely the HttpSoapBindingLifeCycle) know when an EndpointBean
 * has been initialized, activated, deactivated, and shutdown.
 */
public class HttpSoapBindingDeployer
    implements ServiceUnitManager, EndpointChangeSubject {

    private static final Messages mMessages =
        Messages.getMessages(HttpSoapBindingDeployer.class);
    

    private ComponentContext mContext;
    private RuntimeConfigurationMBean mRuntimeConfig;
    private Map mServiceUnits;
    private Collection mEndpointChangeListeners;
    private JBITaskMessageBuilder mMsgBuilder;
    private Logger mLogger;
    private CredentialValidatorManager mCredValidatorMgr;
    
    public HttpSoapBindingDeployer(ComponentContext context, RuntimeConfigurationMBean runtimeConfig) {

        mContext = context;
        mRuntimeConfig = runtimeConfig;
        mMsgBuilder = new DefaultJBITaskMessageBuilder();
        mMsgBuilder.setComponentName(mContext.getComponentName());

        mLogger = Messages.getLogger(getClass());
        mServiceUnits = new HashMap();
        mEndpointChangeListeners = new HashSet();
        mCredValidatorMgr = new CredentialValidatorManager(runtimeConfig);
    }

    HttpSoapBindingDeployer(ComponentContext context,
                            RuntimeConfigurationMBean runtimeConfig,
                            Map serviceUnits, 
                            Collection endpointChangeListeners) {
        this (context, runtimeConfig);
        mServiceUnits = serviceUnits;
        mEndpointChangeListeners = endpointChangeListeners;        
    }
    
    /**
     * Deploy a Service Unit to the component. This is called by the JBI
     * implementation in order to deploy the given artifact to the implementing
     * component.
     * @param suId the name of the Service Unit being deployed.
     * @param asaFilePath the full path to the Service Unit artifact
     * root directory.
     * @return a deployment status message.
     * @throws javax.jbi.management.DeploymentException if the deployment
     * operation is unsuccessful.
     */
    public String deploy(String suId, String asaFilePath)
        throws DeploymentException {

        String taskName = "deploy"; // NO18N
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,
                        "HTTPBC-R00201.Deploying_service_unit",
                        new Object[] {suId, asaFilePath});
        }
        
        ServiceUnit su = null;
        try {
            su = (ServiceUnit)mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId, mContext,
                                         mRuntimeConfig,
                                         mEndpointChangeListeners,
                                         mCredValidatorMgr);
            }
            
            su.deploy(asaFilePath);

            
        } catch (Throwable ex) {
            // Clean up our state
            if (su != null) {
                try {
                    su.stop();
                    su.shutdown();
                } catch (Throwable th) {
                    // Ignore on purpose.
                }
            }

            String msg =
                mMessages.getString("HTTPBC-E00201.Deploy_failed",
                                    ex.getMessage());
            if (mLogger.isLoggable(Level.SEVERE)) {
            	mLogger.log(Level.SEVERE, msg, ex);
            }
            
            AlertsUtil.getAlerter().critical(msg, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             suId, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-E00201");
            mMsgBuilder.throwException(taskName,
                                       "SOAPBC_DEPLOY_2",
                                       msg,
                                       null,
                                       ex);
        }

        //Put the service unit map, only if the deployment succeeds
        mServiceUnits.put(suId, su);

        return mMsgBuilder.createSuccessMessage(taskName);
    }
    
    /**
     * Initialize the deployment. This is the first phase of a two-phase
     * start, where the component must prepare to receive service requests
     * related to the deployment (if any).
     * @param suId the name of the Service Unit being initialized.
     * @param path the full path to the Service Unit artifact
     * root directory.
     * @throws javax.jbi.management.DeploymentException if the Service Unit is
     * not deployed, or is in an incorrect state.
     */
    public void init(String suId, String path) throws DeploymentException {
        String taskName = "init"; // NOI18N
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,
                        "HTTPBC-R00202.Initializing_service_unit",
                        new Object[] {suId, path});
        }

        ServiceUnit su = null;
        try {
            // Prepare for start if the deployment hasn't been processed yet.
            // This happens for example upon re-start, where deploy() will not
            // be called first.
            su = (ServiceUnit)mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId, mContext, 
                                         mRuntimeConfig,
                                         mEndpointChangeListeners,
                                         mCredValidatorMgr);
                mServiceUnits.put(suId, su);
            }    
            
            su.init(path);

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "HTTPBC-R00203.Service_unit_initialized", suId);
            }
        } catch (Throwable ex) {
            // Clean up our state
            if (su != null) {
                try {
                    su.stop();
                } catch (Throwable th) {
                    // Ignore on purpose.
                }
            }
            
            // Now throw an error
            String msg =
                mMessages.getString("HTTPBC-E00203.Initialize_failed",
                                    ex.getMessage());
            if (mLogger.isLoggable(Level.SEVERE)) {
            	mLogger.log(Level.SEVERE, msg, ex);
            }
            
            AlertsUtil.getAlerter().critical(msg, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             suId, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-E00203");
            mMsgBuilder.throwException(taskName,
                                       "SOAPBC_INIT_1",
                                       msg,
                                       null,
                                       ex);
        }
    }
   
    /**
     * Start the deployment. This is the second phase of a two-phase start,
     * where the component can now initiate service requests related to the
     * deployment.
     * @param suId the name of the Service Unit being started.
     * @throws javax.jbi.management.DeploymentException if the Service Unit
     * is not deployed, or is in an incorrect state.
     */
    public void start(String suId) throws DeploymentException {
        String taskName = "start"; // NOI18N
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "HTTPBC-R00204.Starting_service_unit", suId);
        }

        ServiceUnit su = (ServiceUnit)mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.start();
            } catch (Throwable ex) {
                String msg =
                    mMessages.getString("HTTPBC-E00205.Start_failed",
                                        ex.getMessage());
                if (mLogger.isLoggable(Level.SEVERE)) {
            	    mLogger.log(Level.SEVERE, msg, ex);
                }
                
                AlertsUtil.getAlerter().critical(msg, 
                                                 HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                 suId, 
                                                 AlertsUtil.getServerType(),
                                                 AlertsUtil.COMPONENT_TYPE_BINDING,
                                                 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                 NotificationEvent.EVENT_TYPE_ALERT,
                                                 "HTTPBC-E00205");
                mMsgBuilder.throwException(taskName,
                                           "SOAPBC_START_1",
                                           msg,
                                           null,
                                           ex);
            }
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "HTTPBC-R00205.Service_unit_started", suId);
        }        
    }
    
    /**
     * Stop the deployment. This causes the component to cease generating
     * service requests related to the deployment. This returns the deployment
     * to a state equivalent to after <code>init()</code> was called.
     * @param suId the name of the Service Unit being stopped.
     * @throws javax.jbi.management.DeploymentException if the Service Unit
     * is not deployed, or is in an incorrect state.
     */
    public void stop(String suId) throws DeploymentException {
        String taskName = "stop"; // NOI18N
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "HTTPBC-R00206.Stopping_service_unit", suId);
        }

        ServiceUnit su = (ServiceUnit)mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.stop();
            } catch (Throwable ex) {
                String msg = mMessages.getString("HTTPBC-E00206.Stop_failed");
                AlertsUtil.getAlerter().critical(msg, 
                                                 HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                 suId, 
                                                 AlertsUtil.getServerType(),
                                                 AlertsUtil.COMPONENT_TYPE_BINDING,
                                                 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                 NotificationEvent.EVENT_TYPE_ALERT,
                                                 "HTTPBC-E00206");
                mMsgBuilder.throwException(taskName,
                                           "SOAPBC_STOP_1",
                                           msg,
                                           null,
                                           ex);
            }
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "HTTPBC-R00207.Service_unit_stopped", suId);
        }        
    }
    
    /**
     * Shut down the deployment. This causes the deployment to return to the
     * state it was in after <code>deploy()</code> and before
     * <code>init()</code>.
     * @param suId the name of the Service Unit being shut down.
     * @throws javax.jbi.management.DeploymentException if the Service Unit
     * is not deployed, or is in an incorrect state.
     */
    public void shutDown(String suId) throws DeploymentException {
        String retMsg = null;
        String taskName = "shutDown"; // NOI18N
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "HTTPBC-R00208.Shutting_down_service_unit", suId);
        }

        ServiceUnit su = (ServiceUnit)mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.shutdown();
            } catch (Throwable ex) {
                String msg = mMessages.getString("HTTPBC-E00210.Shutdown_failed");
                AlertsUtil.getAlerter().critical(msg, 
                                                 HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                                 suId, 
                                                 AlertsUtil.getServerType(),
                                                 AlertsUtil.COMPONENT_TYPE_BINDING,
                                                 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                                 NotificationEvent.EVENT_TYPE_ALERT,
                                                 "HTTPBC-E00210");
                mMsgBuilder.throwException(
                        taskName,
                        "SOAPBC_SHUTDOWN_1",
                        msg,
                        null,
                        ex);
            }
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "HTTPBC-R00209.Service_unit_shutdown", suId);
        }
    }
   
    /**
     * Cancel a Service Deployment.  If the deployment is in use
     * (has dependencies), then this operation may fail.
     *
     * @param suId the name of the Service Unit being initialized.
     * @param path the full path to the Service Unit artifact
     * root directory.
     * @return a deployment message
     * @throws javax.jbi.management.DeploymentException if the Service Unit is
     * not deployed, or is in an incorrect state.
     */
    public String undeploy(String id, String path) throws DeploymentException {
        String retMsg = null;
        String taskName = "undeploy"; // NOI18N
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,
                        "Undeploying service unit " + id + " from " + path);
        }
        
        ServiceUnit su = (ServiceUnit)mServiceUnits.get(id);
        if (su != null) {
            try {
                su.undeploy(path);
            } catch (Throwable th) {
                // Ignore on purpose
            }
            mServiceUnits.remove(id);
        }

        retMsg = mMsgBuilder.createSuccessMessage(taskName);
        return retMsg;        
    }

    
    ////////
    //
    //  EndpointChangeSubject Interface Methods
    //
    ////////

    public void addEndpointChangeListener(EndpointChangeListener listener) {
        mEndpointChangeListeners.add(listener);
    }

    public void addEndpointChangeListener(Collection listeners) {
        mEndpointChangeListeners.addAll(listeners);
    }

    public void removeEndpointChangeListener(EndpointChangeListener listener) {
        mEndpointChangeListeners.remove(listener);
    }

    ////////
    //
    // Resource Query methods
    //
    ////////

    public ByteBuffer queryResource(String context, Endpoint endpoint) throws Exception {
        // We don't care about the port number.  We're using the ResourceLocator
        // to just get the service unit id and the context
        ResourceLocator locator = new ResourceLocator(-1, context);
        String serviceUnitId = locator.getServiceUnitId();
        ServiceUnit su = (ServiceUnit)mServiceUnits.get(serviceUnitId);
        if (su == null)
            return null;

        return su.getResource(locator.getResourceLocation(), endpoint);
    }
    
}
