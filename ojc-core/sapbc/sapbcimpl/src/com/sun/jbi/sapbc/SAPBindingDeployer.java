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
 * @(#)SAPBindingDeployer.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc;

import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import java.util.Collection;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import java.util.logging.Level;


/**
 * Deployment Manager for SAP Binding Component Service Units.
 */
public class SAPBindingDeployer implements ServiceUnitManager {
    public SAPBindingDeployer(ComponentContext context, SAPBindingLifeCycle lifeCycle) {
        if (context == null) {
            throw new NullPointerException("context");
        }
        if (lifeCycle == null) {
            throw new NullPointerException("lifeCycle");
        }
        mContext = context;
        mLifeCycle = lifeCycle;
        mServiceUnits = new HashMap<String, ServiceUnit>();
        mLogger = Messages.getLogger(SAPBindingDeployer.class);
    }
    
    public String deploy(String suId, String suPath) throws DeploymentException {
        String taskName = "deploy";
        ServiceUnit su = null;
        
        Utils.checkLog(mLogger, Level.INFO, "SAPBindingDeployer.Deploy_su", new Object[]{suId, suPath});
        
        if (suId == null || "".equals(suId.trim())) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_deploy_null_suid");
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_DEPLOY_1",
                    null,
                    errMsg,
                    null);
            throw new DeploymentException(exMsg);
        }
        if (suPath == null || "".equals(suPath.trim())) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_deploy_null_supath");
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_DEPLOY_1",
                    null,
                    errMsg,
                    null);
            throw new DeploymentException(exMsg);
        }
        
        
        synchronized (mServiceUnits) {
            try {
                su = (ServiceUnit)mServiceUnits.get(suId);
                if (su == null) {
                    su = new ServiceUnitImpl(suId,
                            suPath,
                            mContext,
                            mLifeCycle.getRuntimeConfigurationMBean(),
                            mLifeCycle.getStatusProviderHelper(),
                            mLifeCycle.getInboundReceiver()
                            );
                } else {
                    String errMsg = mMessages.getString("SAPBindingDeployer.Failed_deploy_already_deployed", suId);
                    mLogger.log(Level.SEVERE, errMsg);
                    String exMsg =
                            createExceptionMessage(mContext.getComponentName(),
                            taskName,
                            "SAPBC_DEPLOY_1",
                            null,
                            errMsg,
                            null);
                    throw new DeploymentException(exMsg);
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
                String errMsg = mMessages.getString("SAPBindingDeployer.Failed_deploy_su", suId);
                mLogger.log(Level.SEVERE, errMsg);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "SAPBC_DEPLOY_1",
                        null,
                        errMsg,
                        null);
                throw new DeploymentException(exMsg);
            }
            mServiceUnits.put(suId, su);
        }
        
        
        return createSuccessMessage(taskName, mContext.getComponentName());
    }
    
    public void init(String suId, String suPath) throws DeploymentException {
        String taskName = "init";
        
        Utils.checkLog(mLogger, Level.INFO, "SAPBindingDeployer.Initializing_su", new Object[]{suId, suPath});
        if (suId == null || "".equals(suId.trim())) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_init_null_suid");
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_INIT_1",
                    null,
                    errMsg,
                    null);
            throw new DeploymentException(exMsg);
        }
        
        if (suPath == null || "".equals(suPath.trim())) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_init_null_supath");
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_INIT_1",
                    null,
                    errMsg,
                    null);
            
            throw new DeploymentException(exMsg);
        }
        
        synchronized (mServiceUnits) {
            ServiceUnit su = null;
            try {
                su = (ServiceUnit)mServiceUnits.get(suId);
                // Prepare for start if the deployment hasn't been processed yet.
                if (su == null) {
                    su = new ServiceUnitImpl(suId,
                            suPath,
                            mContext,
                            mLifeCycle.getRuntimeConfigurationMBean(),
                            mLifeCycle.getStatusProviderHelper(),
                            mLifeCycle.getInboundReceiver()
                            );
                    
                }
                su.init();
                mServiceUnits.put(suId, su);
                
                Utils.checkLog(mLogger, Level.INFO, "SAPBindingDeployer.Initialized_su", new Object[]{suId, suPath});
            } catch (Exception ex) {
                ex.printStackTrace();
                String errMsg = mMessages.getString("SAPBindingDeployer.Failed_init_su", new Object[]{suId, ex.getMessage()});
                mLogger.log(Level.SEVERE, errMsg);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "SAPBC_INIT_1",
                        null,
                        errMsg,
                        ex);
                throw new DeploymentException(exMsg,ex);
            }
        }
    }
    
    public void start(String suId) throws DeploymentException {
        String taskName = "start";
        Utils.checkLog(mLogger, Level.INFO, "SAPBindingDeployer.Starting_su", suId);
        
        if (suId == null || "".equals(suId.trim())) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_start_null_suid");
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_START_1",
                    null,
                    errMsg,
                    null);
            throw new DeploymentException(exMsg);
        }
        
        ServiceUnit su;
        synchronized (mServiceUnits) {
            su = mServiceUnits.get(suId);
        }
        
        if (su == null) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_start_not_deployed", suId);
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_START_1",
                    null,
                    errMsg,
                    null);
            throw new DeploymentException(exMsg);
        }
        
        if (!su.isInitialized()) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_start_not_init",suId);
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_START_1",
                    null,
                    errMsg,
                    null);
            throw new DeploymentException(exMsg);
        }
        
        try {
            su.start();
            Utils.checkLog(mLogger, Level.INFO, "SAPBindingDeployer.Started_su", suId);
        } catch (JBIException ex) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_start_su", new Object[]{suId, ex.getMessage()});
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_START_1",
                    null,
                    errMsg,
                    ex);
            throw new DeploymentException(exMsg,ex);
        }
    }
    
    public void stop(String suId) throws DeploymentException {
        String taskName = "stop";
        ServiceUnit su;
        
        Utils.checkLog(mLogger, Level.INFO, "SAPBindingDeployer.Stopping_su", suId);
        
        if (suId == null || "".equals(suId.trim())) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_stop_null_suid");
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_STOP_1",
                    null,
                    errMsg,
                    null);
            throw new DeploymentException(exMsg);
        }
        
        synchronized (mServiceUnits) {
            su = mServiceUnits.get(suId);
        }
        
        if (su == null) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_stop_not_deployed", suId);
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_STOP_1",
                    null,
                    errMsg,
                    null);
            throw new DeploymentException(exMsg);
        }
        
        // Not checking initialized and started states, because a stop transition
        // from either state is benign (quietly ignored) -- or is it?
        
        try {
            su.stop();
            Utils.checkLog(mLogger, Level.INFO, "SAPBindingDeployer.Stopped_su", suId);
        } catch (Exception  ex) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_stop_su", ex.getMessage());
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_STOP_1",
                    null,
                    errMsg,
                    ex);
            throw new DeploymentException(exMsg,ex);
        }
    }
    
    public void shutDown(String suId) throws DeploymentException {
        String taskName = "shutDown";
        ServiceUnit su;
        
        Utils.checkLog(mLogger, Level.INFO, "SAPBindingDeployer.Shutting_down_su", suId);
        
        if (suId == null || "".equals(suId.trim())) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_shutdown_null_suid");
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_SHUTDOWN_1",
                    null,
                    errMsg,
                    null);
            throw new DeploymentException(exMsg);
        }
        
        synchronized (mServiceUnits) {
            su = mServiceUnits.get(suId);
        }
        
        if (su == null) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_shutdown_not_deployed", suId);
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_SHUTDOWN_1",
                    null,
                    errMsg,
                    null);
            throw new DeploymentException(exMsg);
        }
        
        // Not checking initialized and started states, because a stop transition
        // from either state is benign (quietly ignored) -- or is it?
        
        try {
            if (su != null) {
                su.shutdown();
                Utils.checkLog(mLogger, Level.INFO, "SAPBindingDeployer.Shutdown_su", suId);
            }
        } catch (Exception  ex) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_shutdown_su", ex.getMessage());
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_SHUTDOWN_1",
                    null,
                    errMsg,
                    ex);
            throw new DeploymentException(exMsg,ex);
        }
    }
    
    /**
     * Cancel a Service Deployment.  If the deployment is in use
     * (has dependencies), then this operation may fail.
     *
     * @param suId the name of the Service Unit being initialized.
     * @param suPath the full path to the Service Unit artifact
     * root directory.
     * @return a deployment message
     * @throws javax.jbi.management.DeploymentException if the Service Unit is
     * not deployed, or is in an incorrect state.
     */
    public String undeploy(String suId, String suPath) throws DeploymentException {
        String taskName = "undeploy"; // NOI18N
        
        Utils.checkLog(mLogger, Level.INFO, "SAPBindingDeployer.Undeploy_su", suId);
        
        if (suId == null || "".equals(suId.trim())) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_undeploy_null_suid");
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_UNDEPLOY_1",
                    null,
                    errMsg,
                    null);
            throw new DeploymentException(exMsg);
        }
        
        if (suPath == null || "".equals(suPath.trim())) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_undeploy_null_supath");
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_UNDEPLOY_1",
                    null,
                    errMsg,
                    null);
            throw new DeploymentException(exMsg);
        }
        
        // Make sure we call shutdown which will clear out the mDeployedIds map
        // and make sure everything is ready to be undeployed.  Calls to deploy()
        // do not necessarily result in calls to shutdown() and then undeploy().
        shutDown(suId);
        
        ServiceUnit su = (ServiceUnit)mServiceUnits.get(suId);
        
        if (su != null && !su.isShutdown()) {
            String errMsg = mMessages.getString("SAPBindingDeployer.Failed_undeploy_not_shutdown", suId);
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "SAPBC_UNDEPLOY_1",
                    null,
                    errMsg,
                    null);
            throw new DeploymentException(exMsg);
        } else {
            mServiceUnits.remove(suId);
            Utils.checkLog(mLogger, Level.INFO, "SAPBindingDeployer.Undeployed_su", suId);
        }
        
        return createSuccessMessage(taskName, mContext.getComponentName());
    }
     
    public HashMap<String, ServiceUnit> getServiceUnits() {
        return mServiceUnits;
    }
    
    private String createSuccessMessage(String taskName, String componentName) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        return msgBuilder.createSuccessMessage(taskName);
    }
    
    private String createExceptionMessage(String componentName,
            String taskName,
            String locToken,
            String locParam,
            String locMessage,
            Throwable exObj) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        String returnStr = null;
        if (exObj == null)
            returnStr = msgBuilder.createExceptionMessage(taskName, locToken, locMessage, locParam);
        else
            returnStr = msgBuilder.createExceptionMessage(taskName, locToken, locMessage, locParam, exObj);
        return returnStr;
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
                
                while(activatedEndpoints.iterator().hasNext()) {
                    Endpoint activeEndpoint = activatedEndpoints.iterator().next();
                }
                
                // Don't think this is needed since we can control the startup of service
                // endpoints (SAP clients and servers) can be controlled within the binding
                // component
                // If this needs to be implemented look under the filebc
                //EndpointValidator.validateEndpointForUniqueness (activatedEndpoints, aEndpoint, true);
            }
        }
        
        return true;
    }
    
    private static final Messages mMessages =
            Messages.getMessages(SAPBindingDeployer.class);
    
    private static Logger mLogger;
    
    private HashMap<String, ServiceUnit> mServiceUnits;
    private ComponentContext mContext;
    private SAPBindingLifeCycle mLifeCycle;
}
