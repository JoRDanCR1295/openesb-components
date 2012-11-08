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
 * @(#)BPELSELifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;

import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationEmitter;
import javax.management.NotificationListener;
import javax.transaction.TransactionManager;
import javax.xml.namespace.QName;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.engine.bpel.DeploymentBindings.InComingKey;
import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionConfiguration;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELMemoryMonitor;
import com.sun.jbi.engine.bpel.core.bpel.engine.BPELSERegistry;
import com.sun.jbi.engine.bpel.core.bpel.engine.Engine;
import com.sun.jbi.engine.bpel.core.bpel.engine.InComingEventModel;
import com.sun.jbi.engine.bpel.core.bpel.engine.ThreadManager;
import com.sun.jbi.engine.bpel.core.bpel.engine.XmlResourceProviderPool;
import com.sun.jbi.engine.bpel.core.bpel.engine.impl.EngineImpl;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELConfigurationException;
import com.sun.jbi.engine.bpel.core.bpel.exception.BPELRuntimeException;
import com.sun.jbi.engine.bpel.core.bpel.trace.BPELTraceManager;
import com.sun.jbi.engine.bpel.impl.ActionableMBeanImpl;
import com.sun.jbi.engine.bpel.util.I18n;


/**
 * BPELSE implementation of JBI component LifeCycle
 *
 * @author Sun Microsystems
 */
public class BPELSELifeCycle implements ComponentLifeCycle, Component, NotificationListener, ThreadManager {

    private static Logger LOGGER = Logger.getLogger(BPELSELifeCycle.class.getName());
    private Engine mEngine;
    private ComponentContext mContext;
    private MessagingChannel mChannel;
    private EngineChannel mEngineChannel;
    private ServiceUnitManager mServiceUnitMgr;
    private DeploymentBindings mDepBindings;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    private BPELSERuntimeConfiguration mConfigMBean;
    private ManagementMbeanRegister mManagementMbeanRegister;
    private BPELSEManagementMBean mManagementMbean;
    private ManagementMbeanRegister mExtendedMgmtActionsMbeanRegister;
    private ActionableMBean mActionableMbean;
   
    private int mThreadCount;
    private BPELSEHelper threadObj;
    private List mThreadsList;
    
    /**
     * default constructor
     */
    public BPELSELifeCycle() {
    }

    /**
     * Initialize the SE. This performs initialization required by the SE but does not make it
     * ready to process messages. This method is called once for each life cycle of the SE.
     *
     * @param context the engine context.
     *
     * @throws javax.jbi.JBIException if the SE is unable to initialize.
     * @throws JBIException DOCUMENT ME!
     */
    public void init(ComponentContext context) throws javax.jbi.JBIException {        
        mContext = context;
        
        if (LOGGER.isLoggable(Level.FINE)) {
        	LOGGER.log(Level.FINE, I18n.loc("BPJBI-3024: Initializing BPEL service engine"));
        }

        // incomplete works, it needs more runtime configuration helper works
        try {
            
            mRuntimeConfigHelper = new RuntimeConfigurationHelper(
                    context.getMBeanNames().createCustomComponentMBeanName("Configuration"), context.getMBeanServer());
            mManagementMbeanRegister = new ManagementMbeanRegister( context.getMBeanNames().createCustomComponentMBeanName("Administration"), 
                    context.getMBeanServer());
            mExtendedMgmtActionsMbeanRegister = new ManagementMbeanRegister( context.getMBeanNames().createCustomComponentMBeanName("ManagementActions"), 
                    context.getMBeanServer());
            
            mConfigMBean = new BPELSERuntimeConfiguration(context);
            
            mRuntimeConfigHelper.registerMBean(mConfigMBean);
        } 
        catch (Exception ex) {
            throw new JBIException(I18n.loc("BPJBI-7018: Unable to register mbean"), ex);
        }        
        
        // Register handle configuration change notifications
        mConfigMBean.addNotificationListener(this, null, null);

        threadObj = new BPELSEHelper();

        mEngineChannel = new EngineChannel(threadObj);

        BPELSERegistry registry = BPELSERegistry.getInstance();
        
        //Register the transaction manager
        registry.register(TransactionManager.class.getName(), mContext.getTransactionManager());
        
        // create the ConnectionConfiguration and set it on the Engine.
        ConnectionConfiguration connConfig = new ConnectionConfigurationImpl(context, mConfigMBean.getProperties());

        try {
        	mEngine = new EngineImpl(mConfigMBean.getProperties(), mContext.getNamingContext());
        } catch (BPELConfigurationException e) {
            throw new JBIException(e);
        }

        mEngine.setApplicationVariables(mConfigMBean.retrieveApplicationVariablesMap());
        mEngine.setOutChannel(mEngineChannel);
        mEngine.setConnectionConfiguration(connConfig);
        mEngine.setThreadMgr(this);

        mDepBindings = new DeploymentBindings();

        mChannel = new BaseMessagingChannel(context);

        threadObj.setComponentContext(mContext);
        threadObj.setDeliveryChannel(mChannel);
        threadObj.setExchangeFactory(mChannel);
        threadObj.setDeploymentBindings(mDepBindings);
        threadObj.mEngineChannel = mEngineChannel;      
        threadObj.mEngine = mEngine;

        mThreadCount = mConfigMBean.getThreadCount();
        
        //Register a XmlResourceProviderPool with the Registry. We use the name of
        //the interface XmlResourceProviderPool to register the pool.
        XmlResourceProviderPoolImpl xmlResProviderPool;
        try {
            xmlResProviderPool = new XmlResourceProviderPoolImpl(mThreadCount);
        } catch (Exception e) {
            throw new JBIException(e);
        }
        
        registry.register(XmlResourceProviderPool.class.getName(), xmlResProviderPool);
        
        mEngine.setDeploymentLookup(mChannel.getLookup());
        mServiceUnitMgr = new BPELSEDeployer(mEngine, mContext, mDepBindings, this);
        try {
            
            mManagementMbean = new BPELSEManagement (mEngine, this);        	
			mManagementMbeanRegister.registerMBean(mManagementMbean);
            
            mActionableMbean = new ActionableMBeanImpl(mEngine);
            mExtendedMgmtActionsMbeanRegister.registerMBean(mActionableMbean);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			throw new JBIException(I18n.loc("BPJBI-7018: Unable to register mbean"), e);
		} 
        
        LOGGER.log(Level.INFO, I18n.loc("BPJBI-5001: BPEL service engine initialized"));
        
        
    }
    
    /**
     * Get the JMX ObjectName for the DeploymentMBean for this SE. If there is none, return null.
     *
     * @return ObjectName the JMX object name of the DeploymentMBean or null if there is no
     *         DeploymentMBean.
     */
    public javax.jbi.component.ServiceUnitManager getServiceUnitManager() {
        return mServiceUnitMgr;
    }

    /**
     * Get the JMX ObjectName for any additional MBean for this SE. If there is none, return null.
     *
     * @return ObjectName the JMX object name of the additional MBean or null if there is no
     *         additional MBean.
     */
    public javax.management.ObjectName getExtensionMBeanName() {
        return null;
    }

    /**
     * Start the SE. This makes the SE ready to process messages. This method is called after
     * init() completes when the JBI framework is starting up, and when the SE is being restarted
     * after a previous call to shutdown(). If stop() was called previously but shutdown() was
     * not, start() can be called again without another call to init().
     *
     * @throws javax.jbi.JBIException if the SE is unable to start.
     */
    public void start() throws javax.jbi.JBIException {
        LOGGER.log(Level.INFO, I18n.loc("BPJBI-5002: Starting BPEL service engine"));
        try {
			mEngine.preStart();
		} catch (BPELConfigurationException e) {
			throw new JBIException(e.getMessage());
		}
		
        mThreadsList = new ArrayList(); 
        for (int i = 0; i < mThreadCount; i++) {
              Thread thrObj = new BPELSEInOutThread(threadObj, i); 
              thrObj.start();
              mThreadsList.add(thrObj);
        }
        
        LOGGER.log(Level.INFO, I18n.loc(
                "BPJBI-5003: \nBPEL service engine started with following configurations:" 
                + "\nPersistence Enabled : {0}"
                + "\nMonitor Enabled : {1}"
                + "\nPersistence Database NonXA JNDI ResourceName : {2}"
                + "\nPersistence Database XA JNDI Resource Name : {3}"
                + "\nEngine Expiry Interval (failover setting) : {4} seconds"
                + "\nDebug Enabled : {5}"
                + "\nDebug Port : {6}"
                + "\nThread Count : {7}"
                + "\nEngine Installed on Application Server Cluster : {8}"
                + "\nEngine Configured to run in Cluster Profile : {9}"
                + "\nScalability Related : {10} "
                + "\nWaiting Request Life Span : {11}"
                + "\nValidation Enabled : {12}",
        		String.valueOf(mConfigMBean.getPersistenceEnabled()), 
        		String.valueOf(mConfigMBean.getMonitoringEnabled()), 
        		mConfigMBean.getDatabaseNonXAJNDIName(), 
        		mConfigMBean.getDatabaseXAJNDIName(), 
        		mConfigMBean.getEngineExpiryInterval(), 
        		String.valueOf(mConfigMBean.getDebugEnabled()), 
        		mConfigMBean.getDebugPort(), 
        		String.valueOf(mThreadCount), 
        		System.getProperty(Engine.IS_CLUSTERED),
        		mEngine.isClustered(),
        		getScalabilityRelatedConfigurations(),
        		String.valueOf(mConfigMBean.getWaitingRequestLifeSpan()),
                        String.valueOf(mConfigMBean.getValidationEnabled())));
        
        BPELTraceManager.getInstance().alertEngineStatusChange(mEngine.getId(), NotificationEvent.OPERATIONAL_STATE_STARTED);
    }

    
    private String getScalabilityRelatedConfigurations() {
    	StringBuffer scalabilityConfig = new StringBuffer();
    	if (mEngine.isPersistenceEnabled()) {

    		BPELMemoryMonitor monitor = mEngine.getBpelMemoryMonitor();
    	
    		if (!monitor.isScalabilityEnabled()) {
    			scalabilityConfig.append("Passivation Solution for BPELSE Scalability is Disabled.");
    		} else {

    			if (monitor.isPhase1Enabled()) {
    				scalabilityConfig.append("\n\tPhase 1 Solution (Variable Passivation) : Enabled.");
    			} else {
    				scalabilityConfig.append("\n\tPhase 1 Solution (Variable Passivation) : Disabled.");
    			}

    			if (monitor.isPhase2Enabled()) {
    				scalabilityConfig.append("\n\tPhase 2 Solution (Instance Passivation) : Enabled.");
    			} else {
    				scalabilityConfig.append("\n\tPhase 2 Solution (Instance Passivation) : Disabled.");
    			}
    			
    			if ((monitor.isPhase1Enabled()) || (monitor.isPhase2Enabled())) {

    				java.text.DecimalFormat df = new java.text.DecimalFormat("#%");
    				scalabilityConfig.append("\n\tStart passivation when heap utilization grows beyond: " + df.format(mEngine.getBpelMemoryMonitor().getUpperMemoryThreshold()));
    				scalabilityConfig.append("\n\tStop passivation when heap utilization drops below: " + df.format(mEngine.getBpelMemoryMonitor().getLowerMemoryThreshold())); 
    				scalabilityConfig.append("\n\tInstance Idleness Threshold : " + mEngine.getBpelMemoryMonitor().getIdleThresholdEnd() / 1000 + " Seconds"); 
    			}
    		}
    	} else {
    		scalabilityConfig.append("Engine Running in Non-Persistence Mode, Passivation Solution for BPELSE Scalability Not Available.");
    	}
    	
    	return scalabilityConfig.toString();
    }
    
    /**
     * Stop the SE. This makes the SE stop accepting messages for processing. After a call to this
     * method, start() can be called again without first calling init().
     *
     * @throws javax.jbi.JBIException if the SE is unable to stop.
     */
    public void stop() throws javax.jbi.JBIException {
        LOGGER.log(Level.INFO, I18n.loc("BPJBI-5004: Stopping BPEL service engine"));
        mEngine.preStop();
        for (int i = 0; i < mThreadCount; i++) {
            ((BPELSEInOutThread) mThreadsList.get(i)).stopMe();
        }
        boolean waitOnThreads = true;
        while(waitOnThreads){
            
            waitOnThreads = false;
            
            for (int i = 0; i < mThreadCount; i++) {
                BPELSEInOutThread thrObj = (BPELSEInOutThread) mThreadsList.get(i);
                if(thrObj.isAlive()){
                    waitOnThreads = true;
                    try {
                        thrObj.join(10);
                    } catch (InterruptedException e) {
                        // ignore it
                    }
                }
            }
        }
        mThreadsList = null;
        
        LOGGER.log(Level.INFO, I18n.loc("BPJBI-5005: BPEL service engine stopped"));
        BPELTraceManager.getInstance().alertEngineStatusChange(mEngine.getId(), NotificationEvent.OPERATIONAL_STATE_STOPPED);
    }
    
    /**
     * Releases all the threads that are waiting on the delivery channel. The threads unblock
     * from the delivery channel and become available for processing the ready to run queue.
     */
    public void notifyThreads(){
        for (int i = 0; i < mThreadCount; i++) {
            ((BPELSEInOutThread) mThreadsList.get(i)).releaseFromDeliveryChannel();
        }
    }
    
    /**
     * Shutdown the SE. This performs cleanup before the SE is terminated. Once this method has
     * been called, init() must be called before the SE can be started again with a call to
     * start().
     *
     * @throws javax.jbi.JBIException if the SE is unable to shut down.
     * @throws JBIException DOCUMENT ME!
     */
    public void shutDown() throws javax.jbi.JBIException {
        // CR:6420724, init might not run when bpelse
        // gets into a state: 3 (stopped), start is not possible
        // we have to shut it down without throwing exception
        LOGGER.log(Level.INFO, I18n.loc("BPJBI-5006: Shutting down BPEL service engine"));
        mEngine.preShutdown();
        String engineId = mEngine.getId();
        mEngine = null;
        
        try {
            mChannel.close();
            mServiceUnitMgr = null;
        } catch (BPELRuntimeException ex) {
            throw new javax.jbi.JBIException(ex);
        }

        try {
            mRuntimeConfigHelper.unregisterMBean();
            mManagementMbeanRegister.unregisterMBean();
            mExtendedMgmtActionsMbeanRegister.unregisterMBean();
        } catch (Exception ex) {
            LOGGER.log(Level.WARNING, I18n.loc("BPJBI-6014: Failed to un-register runtime configuration mbean for {0}",
                    mContext.getComponentName()), ex);
            throw new JBIException(I18n.loc("BPJBI-6014: Failed to un-register runtime configuration mbean for {0}",
                    mContext.getComponentName()), ex);
        }

        //Remove the the XmlResourceProviderPool from the registry
        BPELSERegistry.getInstance().remove(XmlResourceProviderPool.class.getName());
        
        LOGGER.log(Level.INFO, I18n.loc("BPJBI-5007: BPEL service engine shut down"));
        BPELTraceManager.getInstance().alertEngineStatusChange(engineId, NotificationEvent.OPERATIONAL_STATE_SHUTDOWN);
    }

    /**
     * getter for LifeCycle instance
     * 
     * @return componentLifeCycle this
     */
    public javax.jbi.component.ComponentLifeCycle getLifeCycle() {
        return this;
    }

    /**
     * getter for service description
     *
     * @param serviceEndpoint ServiceEndpoint reference
     *
     * @return Document dom document
     */
    public org.w3c.dom.Document getServiceDescription(
        ServiceEndpoint serviceEndpoint) {
        return null;
    }

    /**
     * verify if engine is able to send outbound message
     *
     * @param serviceEndpoint ServiceEndpoint reference
     * @param msgEx MessageExchange
     *
     * @return true
     */
    public boolean isExchangeWithConsumerOkay(ServiceEndpoint serviceEndpoint,
        javax.jbi.messaging.MessageExchange msgEx) {
        return true;
    }

    /**
     * verify if engine is able to process the incoming message exchange
     *
     * @param serviceEndpoint Service endpoint reference
     * @param msgEx JBI MessageExchange
     *
     * @return boolean  true: if engine can process the message false: if engine can not process
     *         the message
     */
    public boolean isExchangeWithProviderOkay(ServiceEndpoint serviceEndpoint,
        javax.jbi.messaging.MessageExchange msgEx) {
        URI pattern = msgEx.getPattern();
        if (LOGGER.isLoggable(Level.FINE)) {
        	LOGGER.log(Level.FINE, I18n.loc("BPJBI-3002: Pattern for exchange Id {0} is {1}", 
        			msgEx.getExchangeId(), pattern));
        }

        String pat = pattern.toString().trim();
        QName serviceName = null;
        String endPoint = "";
        String operation = "";

        if (pat.equals("http://www.w3.org/2004/08/wsdl/in-out".trim())) { // NO I18N
            InOut inOut = (InOut) msgEx;
            serviceName = inOut.getEndpoint().getServiceName();
            endPoint = inOut.getEndpoint().getEndpointName();
        } else if (pat.equals("http://www.w3.org/2004/08/wsdl/in-only".trim())) { // NO I18N
            InOnly inOnly = (InOnly) msgEx;
            serviceName = inOnly.getEndpoint().getServiceName();
            endPoint = inOnly.getEndpoint().getEndpointName();
        } else if (pat.equals("http://www.w3.org/2004/08/wsdl/robust-in-only".trim())) { // NO I18N
            LOGGER.log(Level.SEVERE, I18n.loc("BPJBI-7020: Robust in-only is not supported {0}", 
            		msgEx.getExchangeId()));
            return false;
        } else if (pat.equals("http://www.w3.org/2004/08/wsdl/out-in".trim())) { // NO I18N
            LOGGER.log(Level.SEVERE, 
            		I18n.loc("BPJBI-7021: Received out-in message {0}. Pattern Out-In is not supported any more.", 
            				msgEx.getExchangeId()));
            return false;
        } else if (pat.equals("http://www.w3.org/2004/08/wsdl/out-only".trim())) { // NO I18N
            LOGGER.log(Level.SEVERE, I18n.loc("BPJBI-7022: Pattern Out-Only is not supported any more {0}", 
            		msgEx.getExchangeId()));
            return false;
        } else if (pat.equals("http://www.w3.org/2004/08/wsdl/robust-out-only".trim())) { // NO I18N
            LOGGER.log(Level.SEVERE, I18n.loc("BPJBI-7023: Robust Out-Only is not supported {0}",
                    msgEx.getExchangeId()));
            return false;
        } else {
            LOGGER.log(Level.SEVERE, I18n.loc("BPJBI-7024: Received invalid pattern info {0}", msgEx.getExchangeId()));
            return false;
        }

        InComingKey key = mDepBindings.createInComingBindingsKey(serviceName, endPoint, operation);
        InComingEventModel model = mDepBindings.getInComingEventModel(key);

        if (model == null) {
            return false;
        }

        return true;
    }

    /**
     * resolving EndpointReference from dom document
     *
     * @param documentFragment dom document
     *
     * @return ServiceEndpoint service endpoint reference
     */
    public ServiceEndpoint resolveEndpointReference(
        org.w3c.dom.DocumentFragment documentFragment) {
        //todovb check just bpel ServiceEndpoints can be resolved otherwise null
        return mEngineChannel.getEPRComposer().resolveEPR(documentFragment, false);
    }
    
    /**
     * Fetches this component's {@link NotificationEmitter} for adding and 
     * removing {@link NotificationListener}s.
     * @return this component's <code>NotificationEmitter</code>.
     */
    public NotificationEmitter getNotificationEmitter() {
        return mConfigMBean;
    }
    
    /** @see javax.management.NotificationListener#handleNotification(javax.management.Notification, java.lang.Object)
     */
    public void handleNotification(Notification notification, Object obj) {

        if (notification instanceof AttributeChangeNotification) {
            AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
            String attrName = attrNotif.getAttributeName();
            if (Engine.THREAD_COUNT.equals(attrName)) {
            	int oldThreadCount = mThreadCount;

            	mThreadCount = Integer.valueOf((String) attrNotif.getNewValue());

            	for (int i = oldThreadCount; i < mThreadCount; i++) {
            		Thread thrObj = new BPELSEInOutThread(threadObj, i);
            		if (LOGGER.isLoggable(Level.FINE)) {
            			LOGGER.log(Level.FINE, 
            					I18n.loc("BPJBI-3008: BPEL service engine thread {0} starting.", new Integer(i)));
            		}
            		thrObj.start();
            		if (LOGGER.isLoggable(Level.FINE)) {
            			LOGGER.log(Level.FINE, 
            					I18n.loc("BPJBI-3009: BPEL service engine thread {0} started.", new Integer(i)));
            		}
            		mThreadsList.add(thrObj);
            	}

            	for (int i = oldThreadCount - 1; i >= mThreadCount; i--) {
            		((BPELSEInOutThread) mThreadsList.remove(i)).stopMe();
            	}

            	//Resize the XmlResourceProviderPool with the the new thread count
            	XmlResourceProviderPoolImpl xmlResProviderPool 
            			= (XmlResourceProviderPoolImpl)BPELSERegistry.getInstance()
            				.lookup(XmlResourceProviderPool.class.getName());
            	try {
            		xmlResProviderPool.resizePool(mThreadCount);
            	} catch (Exception e) {
            		throw new RuntimeException(e);
            	}

            } else {
            	Properties props = new Properties();
            	props.setProperty(attrName, (String) attrNotif.getNewValue());
            	mEngine.resetProperties(props);
            }
        }
    }
}
