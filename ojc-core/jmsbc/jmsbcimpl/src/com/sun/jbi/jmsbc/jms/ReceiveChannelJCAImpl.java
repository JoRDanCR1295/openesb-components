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
 * @(#)ReceiveChannelJCAImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.jms;

import java.net.URLClassLoader;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;
import java.util.HashMap;

import javax.resource.spi.ActivationSpec;
import javax.resource.spi.ResourceAdapter;
import javax.resource.spi.endpoint.MessageEndpointFactory;

import com.stc.jmsjca.core.TxMgr;
import com.stc.jmsjca.unifiedjms.RAUnifiedActivationSpec;
import com.sun.jbi.jmsbc.JMSBindingComponent;
import com.sun.jbi.jmsbc.LogSupport;

import com.sun.jbi.jmsbc.jca.BootstrapContextImpl;
import com.sun.jbi.jmsbc.jca.WorkManagerImpl;
import com.sun.jbi.jmsbc.jca.ASManager;
import com.sun.jbi.jmsbc.jca.RAManager;
import com.sun.jbi.jmsbc.jca.Constants;
import com.sun.jbi.jmsbc.util.AlertsUtil;

import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.jmsbc.extensions.JMSAddress;
import com.sun.jbi.jmsbc.extensions.JMSOperation;
import com.sun.jbi.jmsbc.extensions.JMSOptions;
import com.sun.jbi.jmsbc.extensions.JMSOption;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.internationalization.Messages;

/**
 *
 * An implementaion of the "in" flow channel which utilizes the JMS JCA 
 * for external interaction
 */
public class ReceiveChannelJCAImpl implements ReceiveChannelJCA {
    private JMSAddress jmsAddress = null;
    private JMSOperation jmsOperation = null;
    private boolean isOpened = false;
    private boolean isStarted = false;
    private MessageEndpointFactory factory = null;
    private ActivationSpec as = null;
    private static final Messages mMessages =
        Messages.getMessages(ReceiveChannelJCAImpl.class);
    private static final Logger mLogger =
        Messages.getLogger(ReceiveChannelJCAImpl.class);
    
    private static final long DEFAULT_POOLTIMEOUT = 0;  // wait indefinitely until connection is available
    
    private ResourceAdapter ra = null;
    private WorkManagerImpl workManager = null;
    private BootstrapContextImpl bootstrapContext = null;
    private int mMaximumConcurrentConsumers = 0;
    private long mAppendRedeliveryHandlingWaitTime = 0;
    
    private String mNDCcontext;
    
    private ClassLoader classloader = null;
    /** 
     * Creates a new instance of ReceiveChannelJCAImpl 
     *
     */
    
    public ReceiveChannelJCAImpl(String ndcContext) {
        mNDCcontext = ndcContext;
    }

    public void initialize(JMSAddress jmsAddress, 
                           JMSOperation jmsOperation) {
        this.jmsAddress = jmsAddress;
        this.jmsOperation = jmsOperation;
        workManager = new WorkManagerImpl();
        bootstrapContext = new BootstrapContextImpl(workManager);
        classloader = this.jmsAddress.getClassLoader();
    }
    
    public void setMessageEndpointFactory(MessageEndpointFactory factory) {
        this.factory = factory;
    }

    public void open() throws ChannelException {
    	ClassLoader cls = Util.setContextClassLoader(classloader);
    	try{
    		internalOpen();
    	}finally{
    		Util.setContextClassLoader(cls);
    	}
    }
    synchronized private void internalOpen() throws ChannelException {
        if (!isOpened) {
            try {
                
                
            	if((jmsOperation.getBatchSize()!=null) && jmsOperation.getBatchSize() > 1 && jmsOperation.getTransaction().equals(JMSConstants.TRANSACTION_XA)){
            		//xa transaction in batch mode is not supported
            		jmsOperation.setBatchSize(1);
            		
            		
            		if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            			mLogger.log(LogSupport.LEVEL_DEBUG,
                                "ReceiveChannelJCAImpl_RESET_BATCH");
                    } 
             	}
            	
            	JMSOptions options = jmsOperation.getOptions();

                if (options == null) {
                    options = new JMSOptions();
                }

                if (options.getOptions() == null) {
                    Map optionsMap = new HashMap();
                    options.setOptions(optionsMap);                
                }

                Map optionsMap = options.getOptions();                
                
                handleMappingToOptions(optionsMap, jmsAddress, jmsOperation);
                
                //
                // First get the appropriate ResourceAdapter
                //
                ra = RAManager.getInstance()
                              .acquireRA(jmsAddress, options, jmsOperation);
                if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                    mLogger.log(LogSupport.LEVEL_DEBUG,
                                "ReceiveChannelJCAImpl_OPEN_SUCCESS",
                                new Object[]{jmsAddress.getConnectionURL()});
                }
            } catch (Exception ex) {
                String errMsg = mMessages.getString("JMSBC-E0203.ChannelOpenFailed",
                            new Object[]{jmsAddress.getConnectionURL()});
                throw new ChannelException(errMsg, ex);
            }            
            isOpened = true;
        }
    }

    synchronized public void close() throws ChannelException {
        if (isOpened) {
            
            stop();

            JMSOptions options = jmsOperation.getOptions();

            if (options == null) {
                options = new JMSOptions();
            }

            if (options.getOptions() == null) {
                Map optionsMap = new HashMap();
                options.setOptions(optionsMap);                
            }

            Map optionsMap = options.getOptions();
            
            handleMappingToOptions(optionsMap, jmsAddress, jmsOperation);
            
            RAManager.getInstance()
                     .releaseRA(jmsAddress, options, jmsOperation);
            
            closeWorkManager();
            isOpened = false;
        }        
    }

    public void start() throws ChannelException {
    	ClassLoader cls = Util.setContextClassLoader(classloader);
    	try{
    		internalStart();
    	}finally{
    		Util.setContextClassLoader(cls);
    	}
    }
    synchronized private void internalStart() throws ChannelException {
        if (isOpened) {
            if (!isStarted) {
                if (factory == null) {
                        String errMsg = mMessages.getString("JMSBC-E0204.NoMessageEndpointFactory");
                        throw new ChannelException(errMsg);
                }
                else {
                    try {
                        ra.start(bootstrapContext);

                        as = ASManager.getInstance().
                                       getActivationSpec(mNDCcontext,
                                                         jmsAddress,
                                                         jmsOperation);
                        if(mMaximumConcurrentConsumers > 0){
                        	((RAUnifiedActivationSpec)as).setEndpointPoolMaxSize(mMaximumConcurrentConsumers);
                        }
                        if(mAppendRedeliveryHandlingWaitTime > 0){
                        	//That means re-delivery is enabled using QOS so override whatever is specified
                        	//JMS WSDL binding 
                        	RAUnifiedActivationSpec rau = (RAUnifiedActivationSpec)as;
                        	String overide = "2:10";
                            String msg = mMessages.getString("JMSBC-I0201.RedeliveryOverriden",
                                    new Object[] {rau.getRedeliveryHandling()});
                        	rau.setRedeliveryHandling(overide);
                        }
                        	
                        ra.endpointActivation(factory, as);
                        
                        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                            mLogger.log(LogSupport.LEVEL_DEBUG,
                                        "ReceiveChannelJCAImpl_START_SUCCESS",
                                        new Object[]{jmsAddress.getConnectionURL(),
                                                     jmsOperation.getDestination()});
                        }                        
                    } catch (Exception ex) {
                        String errMsg = mMessages.getString("JMSBC-E0205.ChannelStartFailed",
                                                            new Object[] {jmsAddress.getConnectionURL()});
                        throw new ChannelException(errMsg, ex);
                    }
                    isStarted = true;
                }
            }
        } else {
            String errMsg = mMessages.getString("JMSBC-E0206.ConnectionClosed",
                        new Object[]{jmsAddress.getConnectionURL(),
                                     "start()V"});
            throw new ChannelException(errMsg);
        }
        
    }

    synchronized public void stop() throws ChannelException {
        if (isStarted) {
            if (as != null) {
                // Deactivate the endpoint
                ra.endpointDeactivation (factory, as);
                as = null;

                //closeWorkManager();
            }
            isStarted = false;
        }
    }

	private void closeWorkManager() throws ChannelException {
		boolean success = true;
		
		// Wait for any work threads to complete if cc mode is used
		if (jmsOperation.getConcurrencyMode().equals(Constants.CONCURRENCTY_CONNECTION_CONSUMER)) {
		    long waitTime = 30L;
		    try {
		        success = workManager.shutdownAndWaitForWorkersToComplete(waitTime);
		    } catch (Throwable ex) {
		        String errMsg = mMessages.getString("JMSBC-E0207.WaitFailedForThreadsToComplete",
		                    new Object[]{jmsAddress.getConnectionURL(),
		                                 jmsOperation.getDestination()});
		        throw new ChannelException(errMsg);
		    }
		    
		    if (!success) {
		        mLogger.log(Level.WARNING,
		                    "JMSBC-W0201.WaitTimedOutForThreadsToComplete",
		                    new Object[]{new Long(waitTime),
		                                 jmsAddress.getConnectionURL(),
		                                 jmsOperation.getDestination()}); 
		        AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0201.WaitTimedOutForThreadsToComplete",
		                    new Object[]{new Long(waitTime),
		                                 jmsAddress.getConnectionURL(),
		                                 jmsOperation.getDestination()}), 
		            JMSBindingComponent.SHORT_DISPLAY_NAME, 
		            null, 
		            AlertsUtil.getServerType(),
		            AlertsUtil.COMPONENT_TYPE_BINDING,
		            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
		            NotificationEvent.EVENT_TYPE_ALERT,
		            "JMSBC-W0201");                        
		    }
		}
		
		if (success && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
		    mLogger.log(LogSupport.LEVEL_DEBUG,
		                "ReceiveChannelJCAImpl_STOP_SUCCESS",
		                new Object[]{jmsAddress.getConnectionURL(),
		                             jmsOperation.getDestination()});
		}
	}    

    synchronized public boolean isOpen() {
        return isOpened;
    }

    synchronized public boolean isStarted() {
        return isStarted;
    }
    
    private void handleMappingToOptions (Map optionsMap, 
                                         JMSAddress jmsAddr,
                                         JMSOperation jmsOper) {
        // Defaults for connection pool behavior: 
        // wait if no more connections are available in the connection pool
        if (!optionsMap.containsKey(Constants.JMSJCA_RA_OPTION_pooltimeout)) {
            JMSOption jmsOpt = new JMSOption();
            jmsOpt.setName(Constants.JMSJCA_RA_OPTION_pooltimeout);
            jmsOpt.setValue(Long.toString(DEFAULT_POOLTIMEOUT));
            optionsMap.put(Constants.JMSJCA_RA_OPTION_pooltimeout, jmsOpt);
        }

        // if nonXA, set ACC to true in the options, otherwise will fail
        // for some provider (e.g., wepshere mq).
        if (!jmsOper.getTransaction().equals(JMSConstants.TRANSACTION_XA)) {
            if (!optionsMap.containsKey(Constants.JMSJCA_RA_OPTION_ACC)) {
                JMSOption jmsOpt = new JMSOption();
                jmsOpt.setName(Constants.JMSJCA_RA_OPTION_ACC);
                jmsOpt.setValue("true");
                optionsMap.put(Constants.JMSJCA_RA_OPTION_ACC, jmsOpt);
            }
            if (!optionsMap.containsKey(Constants.JMSJCA_RA_OPTION_NoXA)) {
                JMSOption jmsOpt = new JMSOption();
                jmsOpt.setName(Constants.JMSJCA_RA_OPTION_NoXA);
                jmsOpt.setValue("true");
                optionsMap.put(Constants.JMSJCA_RA_OPTION_NoXA, jmsOpt);
            }
        }

        // handle jndi related info
        if (jmsAddr.getConnectionURL().startsWith(
				Constants.SCHEME_JMS_GENERIC_JNDI)
				&& (jmsAddr.getConnectionFactoryName() != null && jmsAddr
						.getConnectionFactoryName().length() > 0)) {
            JMSOption jndiCFNameOpt = new JMSOption();
            String optName = Constants.JMSJCA_RA_OPTION_TopicCF;
            if (jmsOper.getDestinationType().equals(JMSConstants.QUEUE)) {
                optName = Constants.JMSJCA_RA_OPTION_QueueCF;
            } 
        
            jndiCFNameOpt.setName(optName);
            jndiCFNameOpt.setValue(jmsAddr.getConnectionFactoryName());
            optionsMap.put(optName, jndiCFNameOpt);  
        }
    }

	public void setMaximumConcurrentConsumers(int maximumConcurrentConsumers) {
		mMaximumConcurrentConsumers = maximumConcurrentConsumers;
	}

	public void setAppendRedeliveryHandlingWaitTime(
			long appendRedeliveryHandlingWaitTime) {
		mAppendRedeliveryHandlingWaitTime = appendRedeliveryHandlingWaitTime;
	}
}
