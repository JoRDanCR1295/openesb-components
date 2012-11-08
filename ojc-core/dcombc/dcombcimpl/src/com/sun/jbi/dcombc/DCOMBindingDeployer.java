/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/
package com.sun.jbi.dcombc;

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

import com.sun.jbi.dcombc.dcom.ChannelManager;
import com.sun.jbi.dcombc.dcom.Channel;
import com.sun.jbi.dcombc.mbeans.RuntimeConfigurationMBean;

/**
 * DCOM BC implementation for ServiceUnitManager
 *
 * @author Chandrakanth Belde
 */

public class DCOMBindingDeployer implements ServiceUnitManager {
	/**
	 *
	 */
    private static final Messages mMessages = Messages.getMessages(DCOMBindingDeployer.class);

    private static final Logger mLogger = Messages.getLogger(DCOMBindingDeployer.class);

    private Map mServiceUnits;

    private ComponentContext mContext;

    private StatusProviderHelper mStatusProviderHelper;

    private ChannelManager mDCOMChannelMgr;

    private InboundReceiver mInboundProcessorMgr;

	private RuntimeConfigurationMBean mRuntimeConfig;

    /**
     * DCOMBindingDeployer constructor
     */
    public DCOMBindingDeployer(ComponentContext context, StatusProviderHelper statusProviderHelper,
            ChannelManager dcomChannelMgr, InboundReceiver inboundProcessorMgr, RuntimeConfigurationMBean runtimeConfig) {
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mServiceUnits = new HashMap();
        mDCOMChannelMgr = dcomChannelMgr;
        mInboundProcessorMgr = inboundProcessorMgr;
		mRuntimeConfig = runtimeConfig;
    }

    /**
     * DCOMBindingDeployer constructor
     */
    protected DCOMBindingDeployer(ComponentContext context, StatusProviderHelper statusProviderHelper,
            ChannelManager dcomChannelMgr, InboundReceiver inboundProcessorMgr, Map serviceUnits, RuntimeConfigurationMBean runtimeConfig) {
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mServiceUnits = serviceUnits;
        mDCOMChannelMgr = dcomChannelMgr;
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

        mLogger.log(Level.INFO, "DCOMBindingDeployer.DEPLOY", new Object[] { suId, asaFilePath });

        retMsg = createSuccessMessage(taskName, mContext.getComponentName());

        return retMsg;
    }

    public void init(String suId, String suPath) throws DeploymentException {
        String taskName = "init";
        String retMsg = null;

        mLogger.log(Level.INFO, "DCOMBindingDeployer.INIT", new Object[] { suId, suPath });

        try {
            if (mServiceUnits.get(suId) == null) {
                ServiceUnit su = new ServiceUnitImpl(suId, suPath, mContext, mRuntimeConfig, 
											mStatusProviderHelper, mDCOMChannelMgr, mInboundProcessorMgr);
                su.init(suPath);
                mServiceUnits.put(suId, su);

                mLogger.log(Level.INFO, "DCOMBindingDeployer.INIT_SUCCEEDED", new Object[] { suId });
            }
        } catch (Exception ex) {
            String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "DCOMBC_PROCESS_2",
                    suId, "Processing deployment error: " + ex.getLocalizedMessage(), ex);
            mLogger.log(Level.SEVERE, "DCOMBindingDeployer.INIT_FAILED", new Object[] { ex });

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
                String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED", "DCOMBC_STOP_1",
                        suId, "DCOM BC stop error " + ex.getLocalizedMessage(), ex);
                throw new DeploymentException(exMsg, ex);
            }
        }
    }

    public void start(String suId) throws DeploymentException {
        String taskName = "start";

        mLogger.log(Level.INFO, "DCOMBindingDeployer.START_CALLED", new Object[] { suId });

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.start();
            } catch (Exception ex) {
                String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED",
                        "DCOMBC_START_1", suId, "DCOM BC start error " + ex.getLocalizedMessage(), ex);
                throw new DeploymentException(exMsg, ex);
            }
        }

        mLogger.log(Level.INFO, "DCOMBindingDeployer.START_SUCCEEDED", new Object[] { suId });
    }

    public void stop(String suId) throws DeploymentException {
        String taskName = "stop";

        mLogger.log(Level.INFO, "DCOMBindingDeployer.STOP_CALLED", new Object[] { suId });

        ServiceUnit su = (ServiceUnit) mServiceUnits.get(suId);
        if (su != null) {
            try {
                su.stop();
            } catch (Exception ex) {
                String exMsg = createExceptionMessage(mContext.getComponentName(), taskName, "FAILED",
                        "DCOMBC_START_1", suId, "DCOM BC stop error " + ex.getLocalizedMessage(), ex);
                throw new DeploymentException(exMsg, ex);
            }
        }

        mLogger.log(Level.INFO, "DCOMBindingDeployer.STOP_SUCCEEDED", new Object[] { suId });
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

        mLogger.log(Level.FINE, "DCOMBindingDeployer.UNDEPLOY_SUCCEEDED", new Object[] { name, root });
        retMsg = createSuccessMessage(taskName, mContext.getComponentName());
        return retMsg;
    }

    ////////
    //
    //  DCOMBindingDeployer Public Methods
    //
    ////////

    public Collection getServiceUnits() {
        return Collections.unmodifiableCollection(mServiceUnits.values());
    }

    ////////
    //
    //  DCOMBindingDeployer Private Methods
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
