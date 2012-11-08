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
 * @(#)SNMPServiceUnitManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc;

import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;

import java.util.Collection;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * SNMP BC ServiceUnitManager implementation
 *
 * @author echou
 */
public class SNMPServiceUnitManager implements ServiceUnitManager {

    private static final Messages mMessages =
        Messages.getMessages(SNMPServiceUnitManager.class);
    private static final Logger mLogger =
        Logger.getLogger(SNMPServiceUnitManager.class.getName());
    
    private ComponentContext mContext;
    private StatusProviderHelper mStatusProviderHelper;
    private Map<String, InboundMessageProcessorListenerEndpoint> inboundMsgExchanges;
    
    // <suId, serviceUnit>
    private ConcurrentMap<String, ServiceUnit> serviceUnits;
    
    // global registry of all deployed MOFs
    private ConcurrentMap<String, MOF> mofs;
    
    // global registry of all deployed Adaptations
    private ConcurrentMap<String, SNMPAdaptation> adaptations;
    
    // global registry of all deployed PMs
    private ConcurrentMap<String, PM> pms;
    
    /**
     * SNMPServiceUnitManager constructor
     * @param context 
     * @param statusProviderHelper 
     * @param inboundMsgExchanges 
     */
    public SNMPServiceUnitManager(ComponentContext context,
            StatusProviderHelper statusProviderHelper,
            Map<String, InboundMessageProcessorListenerEndpoint> inboundMsgExchanges) {
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        this.inboundMsgExchanges = inboundMsgExchanges;
        
        serviceUnits = new ConcurrentHashMap<String, ServiceUnit> ();
        mofs = new ConcurrentHashMap<String, MOF> ();
        adaptations = new ConcurrentHashMap<String, SNMPAdaptation> ();
        pms = new ConcurrentHashMap<String, PM> ();
        
    }
    
    ////////
    //
    //  ServiceUnitManager Interface Methods
    //
    ////////

    public String deploy(String suId,
                         String asaFilePath)
        throws DeploymentException {

        String retMsg = null;
        String taskName = "deploy";
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,
                        "deploy service unit {0} to {1}",
                        new Object[]{suId, asaFilePath});
        }
        
        retMsg = createSuccessMessage(taskName, mContext.getComponentName());
        
        return retMsg;
    }
    
    public void init(String suId, String suPath) throws DeploymentException {
        String taskName = "init";
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,
                        "init service unit {0} at {1}",
                        new Object[]{suId, suPath});
        }
        
        try {
            ServiceUnit su = new ServiceUnit(
                    suId,
                    suPath,
                    this,
                    mContext,
                    mStatusProviderHelper);
            ServiceUnit prevValue = 
                    serviceUnits.putIfAbsent(su.getServiceUnitId(), su);
            if (prevValue == null) {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, 
                                "init service unit {0} successful",
                                new Object[]{suId});
                }
            } else {
//                 mLogger.log(Level.SEVERE, "Duplicate ServiceUnit " + su.getServiceUnitId() +
//                         ", cannot proceed with deployment.");
                throw new Exception("Duplicate ServiceUnit " + su.getServiceUnitId() +
                        ", cannot proceed with deployment.");
            }
        } catch (Exception ex) {
            String exMsg = createExceptionMessage(mContext.getComponentName(),
                                                  taskName,
                                                  "FAILED",
                                                  "SNMPBC_PROCESS_2",
                                                  suId,
                                                  "Processing deployment error: "
                                                      + ex.getLocalizedMessage(),
                                                  ex);
//             mLogger.log(Level.SEVERE,
//                         "init service unit failed",
//                         new Object[]{ex});
            
            throw new DeploymentException(exMsg, ex);

        }
    }
    
    
    public void shutDown(String suId) throws DeploymentException {
        String taskName = "shutDown";
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Shutdown : " + suId);
        }
        
        ServiceUnit su = serviceUnits.get(suId);
        if (su != null) {
            try {
                su.shutdown();
            } catch (Exception ex) {
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "SNMPBC_STOP_1",
                        null,
                        "SNMP BC stop error " + ex.getLocalizedMessage() , ex);
                throw new DeploymentException(exMsg, ex);
            }
        }
    }
    
    public void start(String suId) throws DeploymentException {
        String taskName = "start";
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "start service unit {0}",
                        new Object[]{suId});
        }
        
        ServiceUnit su = serviceUnits.get(suId);
        if (su != null) {
            try {
                su.start();
            } catch (Exception ex) {
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "SNMPBC_START_1",
                        null,
                        "SNMP BC start error " + ex.getLocalizedMessage() , ex);
                throw new DeploymentException(exMsg, ex);
            }
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "start service unit {0} successful",
                        new Object[]{suId});
        }
    }
    
    public void stop(String suId) throws DeploymentException {
        String taskName = "stop";
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "stop service unit {0}",
                        new Object[]{suId});
        }
        
        ServiceUnit su = serviceUnits.get(suId);
        if (su != null) {
            try {
                su.stop();
            } catch (Exception ex) {
                String exMsg =
                        createExceptionMessage(mContext.getComponentName(),
                        taskName,
                        "FAILED",
                        "SNMPBC_START_1",
                        null,
                        "SNMP BC stop error " + ex.getLocalizedMessage() , ex);
                throw new DeploymentException(exMsg, ex);
            }
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "stop service unit{0} successful",
                        new Object[]{suId});
        }
    }
    
    /**
     * Cancel a Service Deployment.  If the deployment is in use
     * (has dependencies), then this operation may fail.
     *
     * @param name - name of the service unit
     * @param root - root of the service unit
     * @return 
     * @throws javax.jbi.management.DeploymentException 
     */
    public String undeploy(String name, String root)
        throws DeploymentException {

        String retMsg = null;
        String taskName = "undeploy";
        
        ServiceUnit prevValue = serviceUnits.remove(name);
        if (prevValue == null) {
            mLogger.log(Level.WARNING, "undeploy of ServiceUnit " + name +
                    " failed, does not exist.");
        }
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,
                        "undeploy service unit {0} at {1} successful",
                        new Object[]{name, root});
        }
        retMsg = createSuccessMessage(taskName, mContext.getComponentName());
        return retMsg;
    }

    public Collection<ServiceUnit> getServiceUnits() {
        return serviceUnits.values();
    }
    
    public ComponentContext getComponentContext() {
        return mContext;
    }
    
    public Map<String, InboundMessageProcessorListenerEndpoint> getInboundMsgExchanges() {
        return inboundMsgExchanges;
    }
    
    public ConcurrentMap<String, MOF> getMofs() {
        return mofs;
    }
    
    public ConcurrentMap<String, SNMPAdaptation> getAdaptations() {
        return adaptations;
    }
    
    public ConcurrentMap<String, PM> getPMs() {
        return pms;
    }
    
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
    
    
}
