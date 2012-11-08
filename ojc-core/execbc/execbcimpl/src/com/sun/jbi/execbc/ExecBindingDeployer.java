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
 * @(#)ExecBindingDeployer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc;

import com.sun.jbi.execbc.util.EPUtil;
import com.sun.jbi.execbc.validator.EndpointValidator;
import com.sun.jbi.execbc.Endpoint.EndpointType;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Logger;
import java.util.logging.Level;


/** This class implements the ServiceUnitManager interface.
 *  The ServiceUnitManager interface defines component-supplied
 *  methods for managing service unit deployments.
 *
 * @author Sherry Weng
 */
public class ExecBindingDeployer implements ServiceUnitManager {
    private static final Messages mMessages =
            Messages.getMessages(ExecBindingDeployer.class);
    private static Logger mLogger = Messages.getLogger(ExecBindingDeployer.class);
    
    private HashMap mServiceUnits;
    private ComponentContext mContext;
    private ExecBindingLifeCycle mLifeCycle;
    
    public ExecBindingDeployer(ComponentContext context, ExecBindingLifeCycle lifeCycle) {
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
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FBD_Deploy_SU", new Object[]{suId, suPath});
        }
        
        ServiceUnit su = null;
        try {
            su = (ServiceUnit)mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId,
                        suPath,
                        mContext,
                        mLifeCycle.getRuntimeConfigurationMBean(),
                        mLifeCycle.getStatusProviderHelper(),
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
            String errMsg = mMessages.getString("FBD_Failed_deploy_SU", ex.getMessage());
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "FAILED",
                    "FBC_DEPLOY_1",
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
            mLogger.log(Level.INFO, "FBD_Init_SU", new Object[] {suId, suPath});
        }
        
        ServiceUnit su = null;
        try {
            // Prepare for start if the deployment hasn't been processed yet.
            su = (ServiceUnit)mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId,
                        suPath,
                        mContext,
                        mLifeCycle.getRuntimeConfigurationMBean(),
                        mLifeCycle.getStatusProviderHelper(),
                        mLifeCycle.getInboundReceiver());
                
            }
            su.init();
            validateServiceUnitForUniqueness(su);
            mServiceUnits.put(suId, su);
            
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "FBD_Complete_init_SU", suId);
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
            String errMsg = mMessages.getString("FBD_Failed_init_SU", ex.getMessage());
            mLogger.log(Level.SEVERE, errMsg);
            String exMsg =
                    createExceptionMessage(mContext.getComponentName(),
                    taskName,
                    "FAILED",
                    "FBC_INIT_1",
                    null,
                    errMsg,
                    ex);
            throw new DeploymentException(exMsg, ex);
        }
    }
    
    
    
    public void start(String suId) throws DeploymentException {
        String taskName = "start";
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FBD_Starting_SU", suId);
        }
        
        ServiceUnit su = (ServiceUnit)mServiceUnits.get(suId);
        
        if (su != null) {
            try {
                su.start();
            } catch (Exception ex) {
                String errMsg = mMessages.getString("FBD_Error_start_SU", ex.getMessage());
                mLogger.log(Level.SEVERE, errMsg);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "FBC_START_1",
                        null,
                        errMsg,
                        ex);
                throw new DeploymentException(exMsg, ex);
            }
        }
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FBD_Complete_start_BC");
        }
    }
    
    public void stop(String suId) throws DeploymentException {
        String taskName = "stop";
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FBD_Stop_SU", suId);
        }
        
        ServiceUnit su = (ServiceUnit)mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.stop();
            } catch (Exception  ex) {
                String errMsg = mMessages.getString("FBD_Error_stop_SU", ex.getMessage());
                mLogger.log(Level.SEVERE, errMsg);
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "FBC_STOP_1",
                        null,
                        errMsg,
                        ex);
                throw new DeploymentException(exMsg, ex);
            }
        }
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FBD_Complete_stop_SU", suId);
        }
    }
    
    public void shutDown(String suId) throws DeploymentException {
        String taskName = "shutDown";
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FBD_Shutdown_SU", suId);
        }
        ServiceUnit su = (ServiceUnit)mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.shutdown();
                //                if (mServiceUnits.containsKey(suId)) {
                //                    mServiceUnits.remove(suId);
                //                }
            } catch (Exception  ex) {
                String errMsg = mMessages.getString("FBD_Error_shutdown_SU", ex.getMessage());
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
    
    /**
     * Cancel a Service Deployment.  If the deployment is in use
     * (has dependencies), then this operation may fail.
     *
     * @param name - name of the service unit
     * @param root - root of the service unit
     */
    public String undeploy(String name, String root) throws DeploymentException {
        String taskName = "undeploy";
        
        // make sure the service unit is properly shut down
        shutDown(name);
        
        if (mServiceUnits.containsKey(name)) {
            ServiceUnit su = (ServiceUnit)mServiceUnits.remove(name);
        }
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "FBD_Undeploy_SU", new Object[]{name, root});
        }
        return createSuccessMessage(taskName, mContext.getComponentName());
    }
    
    public HashMap getServiceUnits() {
        return mServiceUnits;
    }
    
    private String createSuccessMessage(String taskName, String componentName) {
        JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);
        return msgBuilder.createSuccessMessage(taskName);
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
        return msgBuilder.createExceptionMessage(taskName, locToken, locMessage, locParam, exObj);
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
