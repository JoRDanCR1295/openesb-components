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
 * @(#)DTELSELifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.dtel;

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Properties;
import java.util.Hashtable;
import java.io.File;
import java.io.FileInputStream;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.messaging.DeliveryChannel;
import javax.management.ObjectName;

import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.configuration.ConfigPersistence;

public class DTELSELifeCycle implements ComponentLifeCycle {
    private static final Logger mLogger = Logger.getLogger(DTELSELifeCycle.class.getName());
    private DTELSEComponent mComponent;
    private ComponentContext mContext;
    private DeliveryChannel mChannel;
    private DTELSEInOutThread mThread;
    private DtelMapEntryTable mDtelMapEntryTable;
    private StatusProviderHelper mStatusProviderHelper;
    private Hashtable mWsdlMap = new Hashtable();

    public DTELSELifeCycle(DTELSEComponent component) {
        mComponent = component;
    }
    
    
    /**
     * Initialize the component. This performs initialization required by the 
     * component but does not make it ready to process messages. This method 
     * is called once for each life cycle of the component
     *
     * @param context - the component's context.
     * @throws javax.jbi.JBIException if the component is unable to initialize.
     */
    public void init(ComponentContext context) throws javax.jbi.JBIException {
        mLogger.info("Initializing DTEL service engine");
        try {
            mStatusProviderHelper = new StatusProviderHelper(StatusProviderMBean.COMPONENT_TYPE_ENGINE, context.getComponentName(), context.getMBeanServer());
            mStatusProviderHelper.registerMBean();
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.info("Registered Status Provider MBean for " + context.getComponentName());
            }            
        } catch (Exception e) {
            mLogger.log(Level.WARNING, "Failed to register status provider MBean", e);
            throw new JBIException("Failed to register status provider MBean", e);
        }
        try {
            mContext = context;
            mChannel = context.getDeliveryChannel();

            // See "Prepare Installation Context" section in JBI 1.0 pr spec 6.4.2.1.1
            Properties configProp = new Properties();
            String workspaceRoot = context.getWorkspaceRoot();
            String configFile = workspaceRoot + File.separator + ConfigPersistence.PERSISTENT_CONFIG_FILE_NAME;
            configProp.load(new FileInputStream(configFile));
            mLogger.info("dtelse config properties: \n" + configProp.toString());

            mDtelMapEntryTable = new DtelMapEntryTable();
            
            DTELSEServiceUnitManager serviceUnitManager = (DTELSEServiceUnitManager)mComponent.getServiceUnitManager();
            serviceUnitManager.initialize(mDtelMapEntryTable, mContext, mChannel, mStatusProviderHelper, mWsdlMap);
            
            mThread = new DTELSEInOutThread(mChannel, mDtelMapEntryTable);
            mThread.start();
        } catch (Exception ex) {
            throw new javax.jbi.JBIException(ex);
        }
        mLogger.info("Initialized DTEL service engine successfully");
    }
    
    /**
     * Get the JMX ObjectName for any additional MBean for this component. 
     * If there is none, return null.
     *
     * @return ObjectName the JMX object name of the additional MBean or null
     * if there is no additional MBean.
     */
    public ObjectName getExtensionMBeanName() {
        return null;
    }

    /**
     * Start the component. This makes the component ready to process messages. 
     * This method is called after init() completes when the JBI implementation 
     * is starting up, and when the component is being restarted after a 
     * previous call to shutDown(). If stop() was called previously but 
     * shutDown() was not, then start() can be called again without another call
     * to init(). 
     *
     * @throws javax.jbi.JBIException if the component is unable to start.
     */
    public void start() throws javax.jbi.JBIException {
        mLogger.info("Starting DTEL service engine");
        mLogger.info("Started DTEL service engine successfully");
    }
    
    /**
     * Stop the component. This makes the component stop accepting messages for 
     * processing. After a call to this method, start() can be called again 
     * without first calling init(). 
     *
     * @throws javax.jbi.JBIException if the component is unable to stop.
     */
    public void stop() throws javax.jbi.JBIException {
        mLogger.info("Stopping DTEL service engine");
        mLogger.info("Stopped DTEL service engine successfully");
    }
    
    /**
     * Shut down the component. This performs cleanup before the component is 
     * terminated. Once this method has been called, init() must be called 
     * before the component can be started again with a call to start(). 
     *
     * @throws javax.jbi.JBIException if the component is unable to shut down.
     */
    public void shutDown() throws javax.jbi.JBIException {
        mLogger.info("Shutting down DTEL service engine");
        try {
            // Must close mChannel before stop mInOnlyThread
            // because mThread may be pending on accept().
            // Closing channel will wake it up from accept()
            // See DTELSEInOutThread's run()
            if (mThread != null) {
                mThread.cease();
            }
            if(mChannel != null) {
                mChannel.close();
            }
            if (mThread != null) {
                stopThread(mThread);
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new javax.jbi.JBIException("DTEL service engine shut down error", e);
        }
        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (Exception e) {
            mLogger.log(Level.WARNING, "Failed to un-register status provider MBean for " + mContext.getComponentName(), e);
            throw new JBIException("Failed to un-register status provider MBean for " + mContext.getComponentName(), e);
        }
        mLogger.info("Shut down DTEL service engine successfully");
    }

    private void stopThread(Thread thread) throws InterruptedException {
        boolean state = true;
        while(state) {
            if(thread.isAlive()) {
                state = true;
                Thread.currentThread();
                Thread.sleep(100L);
            } else {
                state = false;
                mLogger.info("DTEL thread see: " + thread.isAlive());
            }

        }
    }

}
