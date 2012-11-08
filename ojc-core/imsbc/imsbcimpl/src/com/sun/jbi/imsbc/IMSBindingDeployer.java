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

package com.sun.jbi.imsbc;

import java.util.Map;
import java.util.HashMap;
import java.util.Collection;
import java.util.Collections;
import java.util.ResourceBundle;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.component.ComponentContext;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.alerter.NotificationEvent;

import com.sun.jbi.imsbc.ims.Channel;
import com.sun.jbi.imsbc.mbeans.RuntimeConfigurationMBean;
import com.sun.jbi.imsbc.util.AlertsUtil;

/**
 *
 * @author Sun Microsystems
 */

public class IMSBindingDeployer implements ServiceUnitManager {

    private static final Messages mMessages = Messages.getMessages(IMSBindingDeployer.class);

    private static final Logger mLogger = Messages.getLogger(IMSBindingDeployer.class);

    private Map<String, ServiceUnit> mServiceUnits;

    private ComponentContext mContext;

    private StatusProviderHelper mStatusProviderHelper;

    private RuntimeConfigurationMBean mRuntimeConfig;    

    /**
     * IMSBindingDeployer constructor
     */
    public IMSBindingDeployer(ComponentContext context, StatusProviderHelper statusProviderHelper,
    						  RuntimeConfigurationMBean runtimeConfig) {
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mServiceUnits = new HashMap<String, ServiceUnit>();
        mRuntimeConfig = runtimeConfig;
    }

    /**
     * IMSBindingDeployer constructor
     */
    protected IMSBindingDeployer(ComponentContext context, StatusProviderHelper statusProviderHelper,
                                 Map<String, ServiceUnit> serviceUnits, RuntimeConfigurationMBean runtimeConfig) {
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mServiceUnits = serviceUnits;
        mRuntimeConfig = runtimeConfig;
    }

    ////////
    //
    //  ServiceUnitManager Interface Methods
    //
    ////////

    public String deploy(String suId, String asaFilePath) throws DeploymentException {

        String retMsg = null;
        String taskName = "deploy";

        ServiceUnit su = null;
        try {
            su = mServiceUnits.get(suId);
            if (su == null) {
                su = new ServiceUnitImpl(suId,
                                         asaFilePath,
                                         mContext,
										 mRuntimeConfig,
                                         mStatusProviderHelper);
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

            String errMsg = mMessages.getString("IMSBC-E00210.IBD_Deploy_SU_Failed",
                                new Object []{suId, asaFilePath, ex.getLocalizedMessage()});
            
            mLogger.log(Level.SEVERE, errMsg, ex);
            AlertsUtil.getAlerter().critical(errMsg, 
									IMSBindingComponent.SHORT_DISPLAY_NAME, 
									suId, 
									AlertsUtil.getServerType(),
									AlertsUtil.COMPONENT_TYPE_BINDING,
									NotificationEvent.OPERATIONAL_STATE_RUNNING, 
									NotificationEvent.EVENT_TYPE_ALERT,
									"IMSBC-E00210");
            
            String exMsg = createExceptionMessage(mContext.getComponentName(),
													taskName,
													"FAILED",
													"IMSBC_DEPLOY_1",
													null,
													errMsg,
													ex);
            throw new DeploymentException(exMsg, ex);         
        }
		
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00201.IBD_Deploy_SU",  new Object[] { suId, asaFilePath }));
        
		mServiceUnits.put(suId, su);
               
        retMsg = createSuccessMessage(taskName, mContext.getComponentName());
        
        return retMsg;


    }

    public void init(String suId, String suPath) throws DeploymentException {
        String taskName = "init";
        String retMsg = null;
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00202.IBD_Initialize_SU", new Object[] { suId, suPath }));

		ServiceUnit su = null;

        try {
    		su = mServiceUnits.get(suId);
			if (su == null) {
				su = new ServiceUnitImpl(suId, 
										 suPath, 
										 mContext, 
					                     mRuntimeConfig, 
					                     mStatusProviderHelper);
            }
			su.init();
            mServiceUnits.put(suId, su);
			
			if (mLogger.isLoggable(Level.INFO)) 
				mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00203.IBD_Initialize_SU_Succeeded", suId ));
        } catch (Exception ex) {

            // Clean up our state
            if (su != null) {
                try {
                    su.stop();
                } catch (Throwable th) {
                    // Ignore on purpose.
                }
            }

            String exMsg = mMessages.getString("IMSBC-E00204.IBD_Initialize_SU_Failed",
													new Object []{suId, ex.getLocalizedMessage()});
			mLogger.log(Level.SEVERE, exMsg);
           
			AlertsUtil.getAlerter().critical(exMsg,
											 IMSBindingComponent.SHORT_DISPLAY_NAME, 
											 suId, 
											 AlertsUtil.getServerType(),
											 AlertsUtil.COMPONENT_TYPE_BINDING,
											 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
											 NotificationEvent.EVENT_TYPE_ALERT,
											 "IMSBC-E00204");

            throw new DeploymentException(exMsg, ex);

        }
    }

    public void shutDown(String suId) throws DeploymentException {
        String retMsg = null;
        String taskName = "shutDown";
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.info("Shutdown : " + suId);

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.shutdown();
            } catch (Exception ex) {
                String exMsg = mMessages.getString("IMSBC-E00210.IBD_Shutdown_SU_Failed",
													new Object []{suId, ex.getLocalizedMessage()});
                mLogger.log(Level.SEVERE, exMsg, ex);
				AlertsUtil.getAlerter().critical(exMsg, 
												 IMSBindingComponent.SHORT_DISPLAY_NAME, 
												 suId, 
												 AlertsUtil.getServerType(),
												 AlertsUtil.COMPONENT_TYPE_BINDING,
												 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
												 NotificationEvent.EVENT_TYPE_ALERT,
												 "IMSBC-E00210");
                throw new DeploymentException(exMsg, ex);
            }
        }
    }

    public void start(String suId) throws DeploymentException {
        String taskName = "start";
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00205.IBD_Start_SU_Called", suId ));

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.start();
            } catch (Exception ex) {
                String exMsg =  mMessages.getString("IMSBC-E00211.IBD_Start_SU_Failed",
													new Object []{suId, ex.getLocalizedMessage()});
                mLogger.log(Level.SEVERE, exMsg, ex);
                AlertsUtil.getAlerter().critical(exMsg, 
												 IMSBindingComponent.SHORT_DISPLAY_NAME, 
												 suId, 
												 AlertsUtil.getServerType(),
												 AlertsUtil.COMPONENT_TYPE_BINDING,
												 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
												 NotificationEvent.EVENT_TYPE_ALERT,
												 "IMSBC-E00211");
                throw new DeploymentException(exMsg, ex);
            }
        }
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00206.IBD_Start_SU_Succeeded", suId ));
    }

    public void stop(String suId) throws DeploymentException {
        String taskName = "stop";
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00207.IBD_Stop_SU_Called", suId ));

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.stop();
            } catch (Exception ex) {
                String exMsg = mMessages.getString("IMSBC-E00213.IBD_Stop_SU_Failed",
													new Object []{suId, ex.getLocalizedMessage()});
                AlertsUtil.getAlerter().critical(exMsg, 
												 IMSBindingComponent.SHORT_DISPLAY_NAME, 
												 suId, 
												 AlertsUtil.getServerType(),
												 AlertsUtil.COMPONENT_TYPE_BINDING,
												 NotificationEvent.OPERATIONAL_STATE_RUNNING, 
												 NotificationEvent.EVENT_TYPE_ALERT,
												 "IMSBC-E00213");
                throw new DeploymentException(exMsg, ex);
            }
        }
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00208.IBD_Stop_SU_Succeeded", suId ));
    }

    /**
     * Cancel a Service Deployment.  If the deployment is in use
     * (has dependencies), then this operation may fail.
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
		if (mLogger.isLoggable(Level.FINE)) 
			mLogger.log(Level.FINE, mMessages.getString("IMSBC-R00209.IBD_Undeploy_SU_Succeeded", new Object[] { name, root }));
        retMsg = createSuccessMessage(taskName, mContext.getComponentName());
        return retMsg;
    }

    ////////
    //
    //  IMSBindingDeployer Public Methods
    //
    ////////

    public Collection getServiceUnits() {
        return Collections.unmodifiableCollection(mServiceUnits.values());
    }

    ////////
    //
    //  IMSBindingDeployer Private Methods
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

}
