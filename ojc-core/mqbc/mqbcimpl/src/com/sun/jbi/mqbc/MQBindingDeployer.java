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
 * @(#)MQBindingDeployer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import com.sun.jbi.mqbc.monitoring.EventLogger;
import com.sun.jbi.mqbc.monitoring.EventManagementFrameworkAlerter;
import com.sun.jbi.mqbc.recovery.ConnectionInfoPersister;
import com.sun.jbi.mqbc.validator.EndpointValidator;


public class MQBindingDeployer implements ServiceUnitManager {
    
    private final EventLogger mLogger;
    
    private Map<String, ServiceUnit> mServiceUnits;
    private MQComponentContext mContext;
    private StatusProviderHelper mStatusProviderHelper;
    
    private MQBindingComponent mLifeCycle;
    private ConnectionInfoPersister mConnPersister;
    
    public MQBindingDeployer(MQComponentContext context,
            StatusProviderHelper statusProviderHelper, MQBindingComponent lifeCycle,
            ConnectionInfoPersister connPersister) {
        mLogger = new EventLogger(Util.getLogger(context,
                getClass().getName()),
                EventManagementFrameworkAlerter.alerter);
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mServiceUnits = new HashMap<String, ServiceUnit>();
        mLifeCycle = lifeCycle;
        mConnPersister = connPersister;
        
    }
    
    ////////
    //
    //  ServiceUnitManager Interface Methods
    //
    ////////
    
    public String deploy(String suId,
            String asaFilePath)
            throws DeploymentException {
        
        String taskName = "deploy";
        
        ServiceUnit su = null;
        try {
            su = mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId,
                        asaFilePath,
                        mContext,
                        mLifeCycle.getRuntimeConfigurationMBean(),
                        mStatusProviderHelper,
                        mLifeCycle.getInboundReceiver(),mConnPersister);
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

            String errMsg =
                    I18n.msg("1040: Service Unit {0} deployment failed!", suId);
            String exMsg = createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "FAILED",
                    "MQBC_DEPLOY_1",
                    null,
                    errMsg,
                    ex);
            throw new DeploymentException(exMsg, ex);
        }

        mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                NotificationEvent.OPERATIONAL_STATE_STARTED,
                I18n.msg("1041: Service Unit {0} deployed from {1}.",
                        suId,
                        asaFilePath));

        //Put the service unit map, only after the deployment succeeds
        mServiceUnits.put(suId, su);
        
        return createSuccessMessage(taskName, mContext.getComponentName());
    }
    
    public void init(String suId, String suPath) throws DeploymentException {
        String taskName = "init";
        
        ServiceUnit su  = null;
        try {
            // Prepare for start if the deployment hasn't been processed yet.
            su = mServiceUnits.get(suId);
            
            if (su == null) {
                //when this scenario could happen maybe in very rear case of jbi deployment it is init()
                //never dploy()
                su = new ServiceUnitImpl(suId,suPath, mContext,
                        mLifeCycle.getRuntimeConfigurationMBean(),
                        mStatusProviderHelper,
                        mLifeCycle.getInboundReceiver(),mConnPersister);
                mServiceUnits.put(suId, su);
            }
            su.init();
              
        } catch (Exception ex) {
            // Clean up our state
            if (su != null) {
                try {
                    su.stop();
                } catch (Throwable th) {
                    // Ignore on purpose.
                }
            }
            String errMsg = I18n.msg(
                    "1042: Service Unit {0} initialization failed!", suId);
            String exMsg = createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "FAILED",
                    "MQBC_PROCESS_2",
                    null,
                    errMsg,
                    ex);
            throw new DeploymentException(exMsg, ex);
            
        }

        mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                NotificationEvent.OPERATIONAL_STATE_STARTED,
                I18n.msg("1043: Service Unit {0} initialized from {1}.",
                        suId,
                        suPath));

    }
    
    
    public void shutDown(String suId) throws DeploymentException {
        String taskName = "shutDown";
        
        ServiceUnit su = mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.shutdown();
            } catch (Exception  ex) {
                String errMsg = I18n.msg(
                        "1044: Service Unit {0} shutdown failed!", suId);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "MQBC_STOP_1",
                        suId,
                        errMsg, ex);
                throw new DeploymentException(exMsg, ex);
            }
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("1045: Service Unit {0} shut down.", suId));
        }
    }
    
    public void start(String suId) throws DeploymentException {
        String taskName = "start";
        
        ServiceUnit su = mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.start();
            } catch (Exception  ex) {
                String errMsg = I18n.msg(
                        "1046: Service Unit {0} startup failed!", suId);
                mLogger.severe(NotificationEvent.SEVERITY_TYPE_CRITICAL,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        errMsg);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "MQBC_INIT_1",
                        null,
                        errMsg , ex);
                throw new DeploymentException(exMsg, ex);
            }
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("1047: Service Unit {0} started.", suId));
        }
    }
    
    public void stop(String suId) throws DeploymentException {
        String taskName = "stop";
        
        ServiceUnit su = mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.stop();
            } catch (Exception  ex) {
                String errMsg = I18n.msg(
                        "1048: Service Unit {0} stop failed!", suId);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "MQBC_START_1",
                        suId,
                        errMsg , ex);
                throw new DeploymentException(exMsg, ex);
            }
            mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                    NotificationEvent.OPERATIONAL_STATE_STARTED,
                    I18n.msg("1049: Service Unit {0} stopped.", suId));
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
        
        String taskName = "undeploy";
        
        // make sure the service unit is properly shut down
        shutDown(name);
        
        
        if (mServiceUnits.containsKey(name)) {
            mServiceUnits.remove(name);
        }

        mLogger.info(NotificationEvent.SEVERITY_TYPE_INFO,
                NotificationEvent.OPERATIONAL_STATE_STARTED,
                I18n.msg("1050: Service Unit {0} undeployed from {1}.",
                        name,
                        root));
        return createSuccessMessage(taskName, mContext.getComponentName());
    }
    
    
    
    
    
    
    
    ////////
    //
    //  MQBindingDeployer Public Methods
    //
    ////////
    
    public Collection<ServiceUnit> getServiceUnits() {
        return Collections.unmodifiableCollection(mServiceUnits.values());
    }
    
    ////////
    //
    //  MQBindingDeployer Private Methods
    //
    ////////
    
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
    
}
