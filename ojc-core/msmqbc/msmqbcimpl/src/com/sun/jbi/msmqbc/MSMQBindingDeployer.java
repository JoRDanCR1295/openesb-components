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
 * @(#)MSMQBindingDeployer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc;

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

import com.sun.jbi.msmqbc.msmq.ChannelManager;
import com.sun.jbi.msmqbc.msmq.Channel;
import com.sun.jbi.msmqbc.mbeans.RuntimeConfigurationMBean;

/**
 *
 * @author Sun Microsystems
 */

public class MSMQBindingDeployer implements ServiceUnitManager {

    private static final Messages mMessages = Messages.getMessages(MSMQBindingDeployer.class);

    private static final Logger mLogger = Messages.getLogger(MSMQBindingDeployer.class);

    private Map mServiceUnits;

    private ComponentContext mContext;

    private StatusProviderHelper mStatusProviderHelper;

    private ChannelManager mMSMQChannelMgr;

    private InboundReceiver mInboundProcessorMgr;

	private RuntimeConfigurationMBean mRuntimeConfig;

    /**
     * MSMQBindingDeployer constructor
     */
    public MSMQBindingDeployer(ComponentContext context, StatusProviderHelper statusProviderHelper,
            ChannelManager msmqChannelMgr, InboundReceiver inboundProcessorMgr, RuntimeConfigurationMBean runtimeConfig) {
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mServiceUnits = new HashMap();
        mMSMQChannelMgr = msmqChannelMgr;
        mInboundProcessorMgr = inboundProcessorMgr;
		mRuntimeConfig = runtimeConfig;
    }

    /**
     * MSMQBindingDeployer constructor
     */
    protected MSMQBindingDeployer(ComponentContext context, StatusProviderHelper statusProviderHelper,
            ChannelManager msmqChannelMgr, InboundReceiver inboundProcessorMgr, Map serviceUnits, RuntimeConfigurationMBean runtimeConfig) {
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mServiceUnits = serviceUnits;
        mMSMQChannelMgr = msmqChannelMgr;
        mInboundProcessorMgr = inboundProcessorMgr;
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

        mLogger.log(Level.INFO, "MSMQBindingDeployer_DEPLOY", new Object[] { suId, asaFilePath });

        retMsg = createSuccessMessage(taskName, mContext.getComponentName());

        return retMsg;
    }

    public void init(String suId, String suPath) throws DeploymentException {
        String taskName = "init";
        String retMsg = null;

        mLogger.log(Level.INFO, "MSMQBindingDeployer_INIT", new Object[] { suId, suPath });

        try {
            if (mServiceUnits.get(suId) == null) {
                ServiceUnit su = new ServiceUnitImpl(suId, suPath, mContext, mRuntimeConfig, mStatusProviderHelper, mMSMQChannelMgr,
                        mInboundProcessorMgr);
                su.init(suPath);
                mServiceUnits.put(suId, su);

                mLogger.log(Level.INFO, "MSMQBindingDeployer_INIT_SUCCEEDED", new Object[] { suId });
            }
        } catch (Exception ex) {
            String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "MSMQBC_PROCESS_2",
                    suId, "Processing deployment error: " + ex.getLocalizedMessage(), ex);
            mLogger.log(Level.SEVERE, "MSMQBindingDeployer_INIT_FAILED", new Object[] { ex });

            throw new DeploymentException(exMsg, ex);

        }
    }

    public void shutDown(String suId) throws DeploymentException {
        String retMsg = null;
        String taskName = "shutDown";

        mLogger.info("Shutdown : " + suId);

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.shutdown();
            } catch (Exception ex) {
                String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "MSMQBC_STOP_1",
                        suId, "MSMQ BC stop error " + ex.getLocalizedMessage(), ex);
                throw new DeploymentException(exMsg, ex);
            }
        }
    }

    public void start(String suId) throws DeploymentException {
        String taskName = "start";

        mLogger.log(Level.INFO, "MSMQBindingDeployer_START_CALLED", new Object[] { suId });

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.start();
            } catch (Exception ex) {
                String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED",
                        "MSMQBC_START_1", suId, "MSMQ BC start error " + ex.getLocalizedMessage(), ex);
                throw new DeploymentException(exMsg, ex);
            }
        }

        mLogger.log(Level.INFO, "MSMQBindingDeployer_START_SUCCEEDED", new Object[] { suId });
    }

    public void stop(String suId) throws DeploymentException {
        String taskName = "stop";

        mLogger.log(Level.INFO, "MSMQBindingDeployer_STOP_CALLED", new Object[] { suId });

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.stop();
            } catch (Exception ex) {
                String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED",
                        "MSMQBC_START_1", suId, "MSMQ BC stop error " + ex.getLocalizedMessage(), ex);
                throw new DeploymentException(exMsg, ex);
            }
        }

        mLogger.log(Level.INFO, "MSMQBindingDeployer_STOP_SUCCEEDED", new Object[] { suId });
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

        mLogger.log(Level.FINE, "MSMQBindingDeployer_UNDEPLOY_SUCCEEDED", new Object[] { name, root });
        retMsg = createSuccessMessage(taskName, mContext.getComponentName());
        return retMsg;
    }

    ////////
    //
    //  MSMQBindingDeployer Public Methods
    //
    ////////

    public Collection getServiceUnits() {
        return Collections.unmodifiableCollection(mServiceUnits.values());
    }

    ////////
    //
    //  MSMQBindingDeployer Private Methods
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
