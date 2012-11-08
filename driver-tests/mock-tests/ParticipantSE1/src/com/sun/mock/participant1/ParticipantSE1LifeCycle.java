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
 * @(#)WorkflowSELifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.mock.participant1;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.net.URL;
import java.util.Hashtable;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.AttributeChangeNotification;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanRegistrationException;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.management.ObjectName;
import javax.wsdl.Definition;

import org.w3c.dom.Element;

import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.component.endpoint.impl.DefaultEndpointManager;
import com.sun.jbi.component.lifecycle.impl.AbstractComponentManager;
import com.sun.jbi.crl.mep.AcceptManager;
import com.sun.jbi.crl.mep.ExchangeRouter;
import com.sun.jbi.crl.mep.impl.DefaultAcceptManager;
import com.sun.jbi.crl.mep.impl.DefaultListenerContext;
import com.sun.jbi.crl.mep.impl.PatternRoleKey;
import com.sun.jbi.crl.mep.impl.SimpleProcessorFactory;

public class ParticipantSE1LifeCycle extends AbstractComponentManager {
    private static final Logger mLogger = Logger.getLogger(ParticipantSE1LifeCycle.class.getName());
    private ComponentContext mContext;
    private DeliveryChannel mChannel;
    private Participant1Engine mEngine;
    
    private Definition mDefinition;
    
    public ParticipantSE1LifeCycle() {
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
        super.init(context);
        mLogger.info("Initializing Workflow service engine");
        this.mDefinition = Util.initDefinition();
        
        mEngine = new Participant1Engine (getAcceptManager());
        
        ParticipantSE1InOutProvider inOutProvider = new ParticipantSE1InOutProvider(mDefinition, mEngine, getAcceptManager());  
        
        ExchangeRouter router = getAcceptManager().getExchangeRouter();
        router.register(PatternRoleKey.IN_OUT_PROVIDER, //new XsltseInOutProvider());
                        new SimpleProcessorFactory(inOutProvider));        
        
        ParticipantSE1InOnlyConsumer inOnlyConsumer = new ParticipantSE1InOnlyConsumer(mEngine, getAcceptManager());
        router.register(PatternRoleKey.IN_ONLY_CONSUMER, 
                new SimpleProcessorFactory(inOnlyConsumer));        
        
        ParticipantSE1InOutConsumer inOutConsumer = new ParticipantSE1InOutConsumer(this.mDefinition, mEngine, getAcceptManager());
        router.register(PatternRoleKey.IN_OUT_CONSUMER, 
                new SimpleProcessorFactory(inOutConsumer));        
        
        mEngine.initialize(inOutConsumer, inOutProvider);
        try {
            mContext = context;
            mChannel = context.getDeliveryChannel();

            
            initializeContext(context);
       
        } catch (Exception ex) {
        	throw new javax.jbi.JBIException("WLMSELifeCycle.unable_to_register_mbean", ex);
        }
              
        
        mLogger.info("Initialized Workflow service engine successfully");
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
        mLogger.info("Starting WLM service engine");
        super.start();
        try {
        	ServiceEndpoint serviceEndpoint = mContext.activateEndpoint(ServiceConstants.PARTICIPANT1_IN_OUT_SERVICE_NAME, ServiceConstants.PARTICIPANT1_IN_OUT_ENDPOINT_NAME);
        } catch(Exception ex) {
        	mLogger.log(Level.SEVERE, "Failed to activate", ex);
        }
        
        //mEngine.start();
        mLogger.info("Started WLM service engine successfully");
    }
    
    /**
     * Stop the component. This makes the component stop accepting messages for 
     * processing. After a call to this method, start() can be called again 
     * without first calling init(). 
     *
     * @throws javax.jbi.JBIException if the component is unable to stop.
     */
    public void stop() throws javax.jbi.JBIException {
        mLogger.info("Stopping WLM service engine");
        super.stop();
        //mEngine.stop();
        mLogger.info("Stopped WLM service engine successfully");
    }
    
    /**
     * Shut down the component. This performs cleanup before the component is 
     * terminated. Once this method has been called, init() must be called 
     * before the component can be started again with a call to start(). 
     *
     * @throws javax.jbi.JBIException if the component is unable to shut down.
     */
    public void shutDown() throws javax.jbi.JBIException {
        mLogger.info("Shutting down WLM service engine");
        super.shutDown();
        //mEngine.shutdown();
        mLogger.info("Shut down WLM service engine successfully");
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
                mLogger.info("WLM thread see: " + thread.isAlive());
            }

        }
    }
    
    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initAcceptManager() */
    protected AcceptManager initAcceptManager() {
        // initialize AcceptManager
        AcceptManager amgr = new DefaultAcceptManager(this);        
        DefaultListenerContext ctx = (DefaultListenerContext) amgr.getListenerContext();
        //ctx.setExchangeContext(new WorkflowSEExchangeContext(ctx, mWorkflowMapEntryTable));        
        return amgr;
    }    
    
    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initEndpointManager() */
    protected EndpointManager initEndpointManager() {
        return new DefaultEndpointManager(null);
    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initServiceUnitManager(javax.jbi.component.ComponentContext) */
    protected ServiceUnitManager initServiceUnitManager() {
        return new ParticipantSE1ServiceUnitManager(this);
    }
    
    
    private void initializeContext(ComponentContext context) throws JBIException {
    	/*EngineContext engineContext = new EngineContext ();
        engineContext.setInitialContext(context.getNamingContext());
        engineContext.setWorkflowMapEntryTable(mWorkflowMapEntryTable);
        engineContext.setConfig(mConfigMBean.getConfiguration());
        mEngine.setContext(engineContext);*/
    }


    
}
