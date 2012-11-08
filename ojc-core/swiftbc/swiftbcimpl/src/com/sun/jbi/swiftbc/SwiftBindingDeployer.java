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
 * @(#)SwiftBindingDeployer.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc;

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

import com.sun.jbi.swiftbc.validator.EndpointValidator;

import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;

/**
 * This class implements the ServiceUnitManager interface. The ServiceUnitManager interface defines
 * component-supplied methods for managing service unit deployments.
 *
 * @author S. Nageswara Rao
 */
public class SwiftBindingDeployer implements ServiceUnitManager {
    
    private static final Messages mMessages = Messages.getMessages(SwiftBindingDeployer.class);
    
    private static Logger mLogger = Messages.getLogger(SwiftBindingDeployer.class);
    
    private HashMap mServiceUnits;
    
    private ComponentContext mContext;
    
    private SwiftBindingComponent mLifeCycle;
    
    /**
     * @param context
     */
    public SwiftBindingDeployer(ComponentContext context, SwiftBindingComponent lifeCycle) {
        mContext = context;
        mLifeCycle = lifeCycle;
        mServiceUnits = new HashMap();
    }
    
    // //////
    //
    // ServiceUnitManager Interface Methods
    //
    // //////
    
    public String deploy(String suId, String suPath) throws DeploymentException {
        
        String taskName = "deploy";
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "SwiftBD_Deploy_SU", new Object[] { suId, suPath });
        }
        
        ServiceUnit su = null;
        try {
            su = (ServiceUnit)mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId, suPath, mContext, mLifeCycle.getRuntimeConfigurationMBean(), mLifeCycle.getStatusProviderHelper(),
                        mLifeCycle.getInboundReceiver());
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
            String errMsg = mMessages.getString("SwiftBD_Failed_deploy_SU", ex.getMessage());
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "FAILED",
                    "swiftbc_DEPLOY_1",
                    null,
                    errMsg,
                    ex);
            throw new DeploymentException(exMsg, ex);
        }
        
        //Put the service unit map, only after the deployment succeeds
        mServiceUnits.put(suId, su);
        return createSuccessMessage(taskName, mContext.getComponentName());
    }
    
    public void init(String suId, String suPath) throws DeploymentException {
        String taskName = "init";
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "SwiftBD_Init_SU", new Object[] { suId, suPath });
        }
        
        ServiceUnit su = null;
        try {
            // Prepare for start if the deployment hasn't been processed yet.
            su = (ServiceUnit)mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId, suPath, mContext, mLifeCycle.getRuntimeConfigurationMBean(), mLifeCycle.getStatusProviderHelper(),
                        mLifeCycle.getInboundReceiver());
            }
            su.init();
            validateServiceUnitForUniqueness(su);
            mServiceUnits.put(suId, su);
            
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "swiftbc_Complete_init_SU", suId);
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
            String errMsg = mMessages.getString("SwiftBD_Failed_init_SU", ex.getMessage());
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "FAILED",
                    "swiftbc_INIT_1",
                    null,
                    errMsg,
                    ex);
            throw new DeploymentException(exMsg, ex);
            
        }
    }
    
    public void shutDown(String suId) throws DeploymentException {
        String taskName = "shutDown";
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "SwiftBD_Shutdown_SU", suId);
        }
        
        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.shutdown();
            } catch (Exception ex) {
                String errMsg = mMessages.getString("SwiftBD_Error_shutdown_SU", ex.getMessage());
                mLogger.log(Level.SEVERE, errMsg);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "FBC_SHUTDOWN_1",
                        null,
                        errMsg,
                        ex);
                throw new DeploymentException(exMsg, ex);
            }
        }
    }
    
    public void start(String suId) throws DeploymentException {
        String taskName = "start";
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "SwiftBD_Starting_SU", suId);
        }
        
        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.start();
            } catch (Exception ex) {
                
                String errMsg = mMessages.getString("SwiftBD_Error_start_SU", ex.getMessage());
                mLogger.log(Level.SEVERE, errMsg);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "swiftbc_START_1",
                        null,
                        errMsg,
                        ex);
                throw new DeploymentException(exMsg, ex);
                
            }
        }
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "SwiftBD_Complete_start_BC");
        }
    }
    
    public void stop(String suId) throws DeploymentException {
        String taskName = "stop";
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "SwiftBD_Stop_SU", suId);
        }
        
        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.stop();
            } catch (Exception ex) {
                String errMsg = mMessages.getString("SwiftBD_Error_stop_SU", ex.getMessage());
                mLogger.log(Level.SEVERE, errMsg);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "swiftbc_STOP_1",
                        null,
                        errMsg,
                        ex);
                throw new DeploymentException(exMsg, ex);
            }
        }
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "SwiftBD_Complete_stop_SU", suId);
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
        
        if (mServiceUnits.containsKey(name)) {
            mServiceUnits.remove(name);
        }
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "SwiftBD_Undeploy_SU", new Object[] { name, root });
        }
        retMsg = createSuccessMessage(taskName, mContext.getComponentName());
        return retMsg;
    }
    
    // //////
    //
    // SwiftBindingDeployer Public Methods
    //
    // //////
    public Collection getServiceUnits() {
        return Collections.unmodifiableCollection(mServiceUnits.values());
    }
    
    // //////
    //
    // SwiftBindingDeployer Private Methods
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
