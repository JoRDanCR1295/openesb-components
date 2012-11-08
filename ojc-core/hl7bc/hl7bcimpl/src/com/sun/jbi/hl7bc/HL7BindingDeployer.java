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
 * @(#)HL7BindingDeployer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.HashMap;
import java.util.HashSet;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.component.ComponentContext;
import com.sun.jbi.alerter.NotificationEvent;

import com.sun.jbi.hl7bc.validator.EndpointValidator;
import com.sun.jbi.hl7bc.util.AlertsUtil;
import com.sun.jbi.hl7bc.I18n;

import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;

/**
 * This class implements the ServiceUnitManager interface. The ServiceUnitManager interface defines
 * component-supplied methods for managing service unit deployments.
 * 
 * @author S. Nageswara Rao, Raghunadh
 */
public class HL7BindingDeployer implements ServiceUnitManager {

    private static Logger mLogger = Logger.getLogger(HL7BindingDeployer.class.getName());

    private HashMap mServiceUnits;

    private ComponentContext mContext;

    private HL7BindingComponent mLifeCycle;

    /**
     * @param context
     */
    public HL7BindingDeployer(ComponentContext context, HL7BindingComponent lifeCycle) {
        mContext = context;
        mLifeCycle = lifeCycle;
        mServiceUnits = new HashMap();
    }

    /**
     * Initiate a BC Deployment.
     * 
     * @param suId - service unit ID
     * @param asaFilePath - Path of the service assembly file
     */

    public String deploy(String suId, String suPath) throws DeploymentException {

        String taskName = "deploy";

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("Deploying service unit : {0} from {1}.", suId, suPath ));
        }

        ServiceUnit su = null;
        try {
            su = (ServiceUnit) mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId, suPath, mContext, mLifeCycle.getRuntimeConfigurationMBean(),
                        mLifeCycle.getStatusProviderHelper(), mLifeCycle.getInboundReceiver(), mLifeCycle.getOutboundReceiver());
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
            String errMsg = I18n.msg("E0158: Service unit {0} failed to deploy, an exception was raised. {1}",
                      suId, ex.getMessage() );
            mLogger.log(Level.SEVERE, errMsg);
            AlertsUtil.getAlerter().critical(errMsg, 
                    HL7BindingComponent.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "HL7BC-E0158"); 
            String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "HL7BC_DEPLOY_1",
                    null, errMsg, ex);
            throw new DeploymentException(exMsg, ex);
        }

        // Put the service unit map, only after the deployment succeeds
        mServiceUnits.put(suId, su);
        return createSuccessMessage(taskName, mContext.getComponentName());
    }

    public void init(String suId, String suPath) throws DeploymentException {
        String taskName = "init";

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("Initializing service unit : {0} from {1}.", suId, suPath));
        }

        ServiceUnit su = null;
        try {
            // Prepare for start if the deployment hasn't been processed yet.
            su = (ServiceUnit) mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId, suPath, mContext, mLifeCycle.getRuntimeConfigurationMBean(),
                        mLifeCycle.getStatusProviderHelper(), mLifeCycle.getInboundReceiver(), mLifeCycle.getOutboundReceiver());
            }
            su.init();
            validateServiceUnitForUniqueness(su);
            mServiceUnits.put(suId, su);

            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, I18n.msg("Successfully initialized service unit {0}.", suId));
            }
        } catch (Exception ex) {
            // Clean up our state
            if (su != null) {
                try {
                    su.stop();
                } catch (Throwable th) {
                    // Ignore on purpose.
                }
            }
            String errMsg = I18n.msg("E0159: Failed to initialize service unit {0} due to :",
                    suId, ex.getMessage());
            mLogger.log(Level.SEVERE, errMsg, ex);
            AlertsUtil.getAlerter().critical(errMsg, 
                    HL7BindingComponent.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "HL7BC-E0159");
            String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "HL7BC_INIT_1",
                    null, errMsg, ex);
            throw new DeploymentException(exMsg, ex);

        }
    }

    public void shutDown(String suId) throws DeploymentException {
        String taskName = "shutDown";

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("Shutting down service unit : {0}.", suId));
        }

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.shutdown();
            } catch (Exception ex) {
                String errMsg = I18n.msg("E0160: Service unit {0} failed to shut down, an exception was raised. {1}", suId,
                        ex.getMessage());
                mLogger.log(Level.SEVERE, errMsg, ex);
                String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED",
                        "HL7BC_SHUTDOWN_1", null, errMsg, ex);
                AlertsUtil.getAlerter().critical(exMsg, 
                        HL7BindingComponent.SHORT_DISPLAY_NAME, 
                        null, 
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "HL7BC-E0160");
                throw new DeploymentException(exMsg, ex);
            }
        }
    }

    public void start(String suId) throws DeploymentException {
        String taskName = "start";
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("Starting service unit : {0}.", suId));
        }

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.start();
            } catch (Exception ex) {
                String errMsg = I18n.msg("E0161: Service unit {0} failed to start, an exception was raised. {1}",
                        suId, ex.getMessage()  );
                mLogger.log(Level.SEVERE, errMsg, ex);
                String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "HL7BC_START_1",
                        null, errMsg, ex);
                AlertsUtil.getAlerter().critical(exMsg, 
                        HL7BindingComponent.SHORT_DISPLAY_NAME, 
                        null, 
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "HL7BC-E0161"); 
                throw new DeploymentException(exMsg, ex);

            }
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("Successfully started HL7 BC."));
        }
    }

    public void stop(String suId) throws DeploymentException {
        String taskName = "stop";

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("Stopping service unit : {0}.", suId));
        }

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.stop();
            } catch (Exception ex) {
                String errMsg = I18n.msg("E0162: Service unit {0} failed to stop, an exception was raised. {1}", suId,
                        ex.getMessage() );
                mLogger.log(Level.SEVERE, errMsg);
                String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "HL7BC_STOP_1",
                        null, errMsg, ex);
                AlertsUtil.getAlerter().critical(exMsg, 
                        HL7BindingComponent.SHORT_DISPLAY_NAME, 
                        null, 
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "HL7BC-E0161");
                throw new DeploymentException(exMsg, ex);
            }
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("Successfully stopped service unit : {0}.", suId));
        }
    }

    /**
     * Cancel a Service Deployment. If the deployment is in use (has dependencies), then this
     * operation may fail.
     * 
     * @param name - name of the service unit
     * @param root - root of the service unit
     */
    public String undeploy(String name, String root) throws DeploymentException {

        String retMsg = null;
        String taskName = "undeploy";

        // make sure the service unit is properly shut down
        shutDown(name);
        if (mServiceUnits.containsKey(name)) {
            mServiceUnits.remove(name);
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("Undeploying service unit : {0} from {1}.", name, root ));
        }
        retMsg = createSuccessMessage(taskName, mContext.getComponentName());
        return retMsg;
    }

    // //////
    //
    // HL7BindingDeployer Public Methods
    //
    // //////
    public Collection<ServiceUnit> getServiceUnits() {
        return Collections.unmodifiableCollection(mServiceUnits.values());
    }

    // //////
    //
    // HL7BindingDeployer Private Methods
    //
    // //////

    private String createSuccessMessage(String taskName, String componentName) {

        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createSuccessMessage(taskName);
        return retMsg;
    }

    private String createExceptionMessage(String componentName,
                                          String taskName,
                                          String status,
                                          String locToken,
                                          String locParam,
                                          String locMessage,
                                          Throwable exObj) {

        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String retMsg = msgBuilder.createExceptionMessage(taskName, locToken, locMessage, locParam, exObj);
        return retMsg;
    }

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

    /**
     * package protected method. Used solely for JUnit test purposes
     */

    public HashMap getServiceUnitsMap() {
        return mServiceUnits;
    }

}
