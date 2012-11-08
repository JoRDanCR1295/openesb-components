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
 * @(#)ServiceUnitImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc;

import java.util.Collection;
import java.util.Collections;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;

import com.sun.jbi.jmsbc.Endpoint.EndpointState;
import com.sun.jbi.jmsbc.Endpoint.EndpointType;

import com.sun.jbi.jmsbc.packaging.EndpointConfiguration;
import com.sun.jbi.jmsbc.packaging.EndpointConfigurationFactory;
import com.sun.jbi.jmsbc.packaging.WSDLConfigurations;

import com.sun.jbi.jmsbc.extensions.JMSConstants;
import com.sun.jbi.jmsbc.extensions.JMSOperation;

import com.sun.jbi.jmsbc.jms.Channel;
import com.sun.jbi.jmsbc.jms.ChannelManager;
import com.sun.jbi.jmsbc.jms.ChannelException;
import com.sun.jbi.jmsbc.jms.ChannelManagerException;

import com.sun.jbi.jmsbc.recovery.JMSConnectionInfoRecord;
import com.sun.jbi.jmsbc.recovery.JMSJndiConnectionInfoRecord;
import com.sun.jbi.jmsbc.recovery.ConnectionInfoPersister;
import com.sun.jbi.jmsbc.recovery.ConnectionInfoPersistException;
import com.sun.jbi.jmsbc.util.AlertsUtil;
import com.sun.jbi.jmsbc.util.JMSBCContext;
import com.sun.jbi.jmsbc.util.ShutdownSynchronization;
import com.sun.jbi.jmsbc.util.SynchronizedStateChange;

import com.sun.jbi.jmsbc.mbeans.JMSBCRuntimeConfigurationMBean;
import com.sun.jbi.jmsbc.mbeans.RuntimeConfigurationMBean;

import com.sun.encoder.Encoder;

/**
 * ServiceUnitImpl represents a ServiceUnit deployed in a ServiceUnitManager.
 * <p>
 * The initialization of a ServiceUnit and its respective Endpoints depends
 * on two main configuration items: the endpoints.xml
 * file and the WSDL(s) representing the Service Unit.  Initialization of the
 * ServiceUnit is done through the Observer pattern when parsing each of the
 * WSDL files.  This class only cares about WSDLs containing the extensibility
 * elements pertinent to this Binding Component.
 * <p>
 * This is important since the actual representation of all the initialization
 * artifacts may change in the future.  In other words, endpoints.xml
 * file may disappear, and the extensibility elements for each BC is
 * different.  However, by encapsulating the initialization logic away from
 * model of ServiceUnitManagers, ServiceUnits, and Endpoints, we can more
 * easily change how models are initialized without breaking too much. 
 * <p>
 * If the artifacts change, only this class and the classes in the
 * com.sun.jbi.jmsbc.packaging package should need to change.
 *
 */
public class ServiceUnitImpl implements ServiceUnit {
    
    private static final int WAIT_TIME = 1000 * 30;
    private static final Messages mMessages =
        Messages.getMessages(ServiceUnitImpl.class);
    private static final Logger mLogger =
        Messages.getLogger(ServiceUnitImpl.class);
    
    private String mId;
    private String mSuPath;    
    private ComponentContext mContext;
    private StatusProviderHelper mStatusProviderHelper;    
    private Map<String, Endpoint> mEndpoints;
    private ChannelManager mJMSChannelMgr;
    private boolean channelsPrepared = false;
    private WSDLConfigurations mWSDLConfig;
    private ConnectionInfoPersister mConnPersister;
    private JMSBCRuntimeConfigurationMBean mRuntimeConfig;
    private EndpointConfiguration mEndpointConfig;
    private Map<EndpointInfo, List<ServiceQuality>> mEndpointQos;
    private Map mInboundMessageExchanges;
    private MessagingChannel channel;
    
    public ServiceUnitImpl(String id,
                           String suPath,
                           ComponentContext context,
                           StatusProviderHelper statusProviderHelper,
                           ChannelManager jmsChannelMgr,
                           ConnectionInfoPersister connPersister,
                           JMSBCRuntimeConfigurationMBean runtimeConfig,
                           Map inboundMessageExchanges) {
        mId = id;
        mSuPath = suPath;
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mEndpoints = Collections.synchronizedMap(new HashMap<String, Endpoint>());
        mJMSChannelMgr = jmsChannelMgr;
        mWSDLConfig = new WSDLConfigurations(id, suPath, runtimeConfig); 
        mConnPersister = connPersister;
        mRuntimeConfig = runtimeConfig;
        mInboundMessageExchanges = inboundMessageExchanges;
        channel = (MessagingChannel)JMSBCContext.getRef().getChannel();
	
     
        
    }

    ////////
    //
    //  ServiceUnit Interface Methods
    //
    ////////

    /**
     * Retrieves the Id of this ServiceUnit.
     *
     * @return       the name of the Service as a QName
     */
    public String getServiceUnitId() {
        return mId;
    }

    /**
     * Deploy the ServiceUnit. 
     *
     * @param        serviceUnitRootPath path to the ServiceUnit
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    synchronized public void deploy() throws JBIException {
        try {
            mEndpointConfig = initializeEndpointConfig();
            Map<String, String[]> envVariableMap = mWSDLConfig.parseForEnvironmentVariables(mEndpointConfig.endpoints(), mRuntimeConfig.retrieveApplicationVariablesMap());
            if (envVariableMap.size() > mRuntimeConfig.retrieveApplicationVariablesMap().size()) {
                // number of environment variable tokens used in JMS WSDLs 
                // is greater than the ones defined 
                mRuntimeConfig.updateApplicationVariablesMap(envVariableMap);
            }
        } catch (Throwable ex) {            
            throw new JBIException(ex);
        }
     }
    
    /**
     * Initializes the ServiceUnit.  Parses the serviceUnitRootPath
     * to create the Endpoint objects.  A ServiceUnit may have
     * mutiple WSDLs, each with multiple endpoints that we care about.
     * <p>
     * This method will initialize the WSDLs, transforming it into a
     * set of Endpoints
     *
     * @param        serviceUnitRootPath path to the ServiceUnit
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
     synchronized public void init(String serviceUnitRootPath) throws JBIException {

    	//Get the QOS properties and apply them to the delivery channel
        
        channel.installServiceQualities(mId, serviceUnitRootPath);
    	
        // Initialize the endpoints for this ServiceUnit.  Basically sets
        // up the static configuration information for each Endpoint.
        Endpoint endpoint = null;
        ArrayList provisionedEPs = new ArrayList();
        try {
            // Prepare for endpoint configuration map if deploy() hasn't been called yet
            if (mEndpointConfig == null) {
                mEndpointConfig = initializeEndpointConfig();
            }
            
            Collection endpoints =
                mWSDLConfig.parse(mEndpointConfig.endpoints(), mRuntimeConfig.retrieveApplicationVariablesMap(), mRuntimeConfig.retrieveApplicationConfigurationsMap());

            if (endpoints.size() == 0) {
                mLogger.log(Level.WARNING, "JMSBC-W0715.ServiceUnitHasNoJMSWSDLs", mId);
                AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0715.ServiceUnitHasNoJMSWSDLs"), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            mId, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0715");  
            }
            
            Iterator it = endpoints.iterator();
            while (it.hasNext()) {
                endpoint = (Endpoint)it.next();

                // Initializes encoder map for the current endpoint.
                // This also triggers the encoder codegen if the
                // specified encoder is not cached.
                Map encoderMapping = mWSDLConfig.getPartEncoderMapping(endpoint);
                endpoint.setMessagePartEncoderMapping(encoderMapping);
                
                // validate configuration
                //validateEndpointConfiguration(endpoint);
                
                // Store the state of the Endpoint
                endpoint.setState(EndpointState.STOPPED);

                // Store the status...
                StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
                QName serviceName = endpoint.getServiceName();
                String portName = endpoint.getEndpointName();
                String uniqueName = null;
                if (endpoint.getEndpointType() == EndpointType.INBOUND) {
                    uniqueName = mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
                    reporting.addConsumingEndpoint(uniqueName);
                } else {
                    uniqueName = mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
                    reporting.addProvisioningEndpoint(uniqueName);
                }
                endpoint.setEndpointStatus(reporting.getEndpointStatus(uniqueName));
                
                //Get and set QOS
                if(endpoint.getEndpointType() == Endpoint.EndpointType.INBOUND){
                	//We need to apply QOS on inbound endpoint only
                    EndpointInfo endpointInfo = new EndpointInfo (false, 
                            endpoint.getEndpointName(), 
                            null,
                            endpoint.getServiceName(),
                            null);
                    List<ServiceQuality> qoss = channel.getServiceQualitiesForEndpoint(endpointInfo);
                    if (qoss != null) {
                    	endpoint.setServiceQualities(qoss.toArray(new ServiceQuality[0]));
                    }
                }
                
                endpoint.setServiceUnit(this);
                mEndpoints.put(getEndpointKey(endpoint), endpoint);
            }
            
            // Initialize all channels (consuming and provisioning)
            if (!channelsPrepared) {
                prepareChannels();
                channelsPrepared = true;
            }

            // Provision outbound endpoints
            Iterator iter = mEndpoints.keySet().iterator();
            while (iter.hasNext()) {
                Endpoint ep = (Endpoint)mEndpoints.get(iter.next());
                if (ep.getEndpointType()==EndpointType.OUTBOUND) {
                    activateOutboundEndpoint(ep);
                    provisionedEPs.add(ep);
                }
            }
            
        } catch (Throwable ex) {
            // Clean up - Remove any provision endpoints
            Iterator iter = provisionedEPs.iterator();
            while (iter.hasNext()) {
                Endpoint ep = (Endpoint)mEndpoints.get(iter.next());
                try {
                    deactivateOutboundEndpoint(ep);
                } catch (Throwable t) {
                    mLogger.log(Level.WARNING, 
                                mMessages.getString("JMSBC-W0716.EndpointDeactivationFailed",
                                    new Object[]{ep.getEndpointName(),
                                                 ep.getServiceName().toString(),
                                                 mId,                                
                                                 t.getLocalizedMessage()}),
                                t);
                    AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0716.EndpointDeactivationFailed",
                                    new Object[]{ep.getEndpointName(),
                                                 ep.getServiceName().toString(),
                                                 mId,                                
                                                 t.getLocalizedMessage()}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            mId, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0716");                                 
                    continue;
                }
            }
            
            // remove all previously created channels
            if (channelsPrepared) {
                cleanupChannels();
                channelsPrepared = false;
            }                
            
            // remove all endpoints
            mEndpoints.clear();
            
            throw new JBIException(ex);
        }        
    }



    /**
     * Starts this ServiceUnit.  This involves activating
     * all Endpoints that are part of this ServiceUnit.  All Endpoints
     * are moved into the EndpointState.RUNNING state.
     * <p>
     * TODO: What should happen if not all the Endpoints
     * can be activated?  Should I deactivate them or just leave
     * them?  For now, I'm going to assume that this method is
     * transactional.  Either all the Endpoints activate or none.
     * If any one fails to activate, the other activated Endpoints
     * will be deactivated.
     *
     * @exception    JBIException if a any Endpoint fails
     * to activate
     */
	synchronized public void start() throws JBIException {        
        if (!channelsPrepared) {
            prepareChannels();
            channelsPrepared = true;
        }            
        
        HashSet activatedEndpoints = new HashSet();
        
        if (mEndpoints != null) {
            Endpoint currEndpoint = null;
            Iterator it = mEndpoints.values().iterator();
            while (it.hasNext()) {
                currEndpoint = (Endpoint)it.next();
                try {
                    startConsuming(currEndpoint);
                    activatedEndpoints.add(currEndpoint);
                } catch (Throwable t) {
                    // Cleanup - stop any consuming endpoints
                    Iterator it2 = activatedEndpoints.iterator();
                    while (it2.hasNext()) {
                        Endpoint endpoint = (Endpoint)it2.next();
                        try {
                            stopConsuming(endpoint);
                        } catch (Throwable t2) {
                            mLogger.log(Level.SEVERE,
                                        mMessages.getString("JMSBC-E0755.EndpointStopFailed",
                                            new Object[]{currEndpoint.getEndpointName(),
                                                         currEndpoint.getServiceName().toString(),
                                                         mId,
                                                         t2.getLocalizedMessage()}),
                                        t2);
                            AlertsUtil.getAlerter().critical(mMessages.getString("JMSBC-E0755.EndpointStopFailed",
                                            new Object[]{currEndpoint.getEndpointName(),
                                                         currEndpoint.getServiceName().toString(),
                                                         mId,
                                                         t2.getLocalizedMessage()}), 
                                      JMSBindingComponent.SHORT_DISPLAY_NAME, 
                                      mId, 
                                      AlertsUtil.getServerType(),
                                      AlertsUtil.COMPONENT_TYPE_BINDING,
                                      NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                      NotificationEvent.EVENT_TYPE_ALERT,
                                      "JMSBC-E0755");
                            continue;
                        }
                    }
                    
                    // remove all previously created channels
                    if (channelsPrepared) {
                        cleanupChannels();
                        channelsPrepared = false;
                    }                

                    if (currEndpoint != null) {
                        String errMsg = mMessages.getString("JMSBC-E0754.EndpointStartFailed",
                                        new Object[]{currEndpoint.getEndpointName(),
                                                     currEndpoint.getServiceName().toString()});

                        throw new JBIException(errMsg, t);
                    } else {
                        throw new JBIException(t);                        
                    }
                }
            }            
        }        
    }

    /**
     * Stops this ServiceUnit.  This involves deactivating
     * all Endpoints that are part of this ServiceUnit.  All
     * Endpoints are moved into the EndpointState.STOPPED state.
     * <p>
     * TODO: What should happen if not all Endpoints deactivate?
     * Unlike the activate() method, I'm NOT going to assume
     * this is transactional.  It seems silly to deactivate a number of
     * Endpoint, and if one fails, re-activate them.  I'll just throw
     * an error, and have the user decide how to deal with it.
     *
     * @exception    JBIException if any Endpoint fails
     * to deactivate
     */
    synchronized public void stop() throws JBIException {
        Iterator it = mEndpoints.values().iterator();
        while (it.hasNext()) {
            Endpoint endpoint = (Endpoint)it.next();
            try {
                stopConsuming(endpoint);
            } catch (Throwable ex) {
                String errMsg = mMessages.getString("JMSBC-E0755.EndpointStopFailed",
                                    new Object[]{endpoint.getEndpointName(),
                                                 endpoint.getServiceName().toString(),
                                                 mId,
                                                 ex.getLocalizedMessage()});

                throw new JBIException(errMsg);
            }
        }
    }

    /**
     * Shuts down this ServiceUnit.  Simply moves all Endpoints to the
     * shutdown state.
     *
     * @exception    JBIException if the ServiceUnit fails to shutdown
     */
    synchronized public void shutdown() throws JBIException {
//        if (channelsPrepared) {
//            cleanupChannels();
//            channelsPrepared = false;
//        }
        
        Endpoint currEndpoint = null;
        try {                        
            Iterator it = mEndpoints.values().iterator();

            while (it.hasNext()) {
                currEndpoint = (Endpoint)it.next();
                int rs = 0;
                try{
                    rs = ShutdownSynchronization.acquireChangeState(currEndpoint.getKey(), WAIT_TIME);
                    if(rs == SynchronizedStateChange.PASS){
                        // Set the state of the endpoint
                        currEndpoint.setState(EndpointState.SHUTDOWN);
                    }else if(rs == SynchronizedStateChange.CHANGE_IN_PROGRESS){
                        mLogger.log(Level.WARNING, mMessages.getString(
    							"JMSBC-W0720.ShutdownWarning",
    							new Object[] { currEndpoint.getKey() }));
    						AlertsUtil.getAlerter().warning(mMessages.getString(
    							          "JMSBC-W0720.ShutdownWarning",
    							          new Object[] { currEndpoint.getKey() }), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            mId, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0720");  
                    	continue;
                    }else if(rs == SynchronizedStateChange.TIMEOUT){
                    	//Still mark the endpoint state to shutdown and then release the lock
                        currEndpoint.setState(EndpointState.SHUTDOWN);
                        mLogger.log(Level.WARNING, mMessages.getString(
    							"JMSBC-W0721.ShutdownWarning",
    							new Object[] { currEndpoint.getKey() }));
    							  		AlertsUtil.getAlerter().warning(mMessages.getString(
    							          "JMSBC-W0721.ShutdownWarning",
    							          new Object[] { currEndpoint.getKey() }), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            mId, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0721");  
                    }
                }finally{
                	if(rs == SynchronizedStateChange.PASS ||  rs == SynchronizedStateChange.TIMEOUT){
                        //release life cycle lock
                        ShutdownSynchronization.releaseChangeState(currEndpoint.getKey());
                	}
                }
                
                acknowledgeAllPendingMessages(mEndpoints.keySet());
                
                //Now stop the channels
                if (channelsPrepared) {
                    try {
                        removeChannels(currEndpoint);
                    } catch (Throwable t) {
                        mLogger.log(Level.WARNING,
                                    mMessages.getString("JMSBC-W0717.EndpointRemoveChannelsFailed",
                                        new Object[]{currEndpoint.getEndpointName(),
                                    		currEndpoint.getServiceName().toString(),
                                                     mId,
                                                     t.getLocalizedMessage()}),
                                    t);
    							  		AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0717.EndpointRemoveChannelsFailed",
                                        new Object[]{currEndpoint.getEndpointName(),
                                    		currEndpoint.getServiceName().toString(),
                                                     mId,
                                                     t.getLocalizedMessage()}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            mId, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0717");                                  
                        continue;
                    }
                }

                // Set the status of the endpoint
                StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
                QName serviceName = currEndpoint.getServiceName();
                String portName = currEndpoint.getEndpointName();
                String uniqueName = null;
                if (currEndpoint.getEndpointType()==EndpointType.INBOUND) {
                    uniqueName = mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
                    reporting.removeConsumingEndpoints(new String[] {uniqueName});
                } else {
                    mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
                    reporting.removeProvisioningEndpoints(new String[] {uniqueName});
                }
                // Dispose the encoders
                Map encoders = currEndpoint.getMessagePartEncoderMapping();
                for (Iterator it2 = encoders.values().iterator(); it2.hasNext();) {
                    Encoder aEncoder = (Encoder)it2.next();
                    // Dispose the encoder. This also
                    // triggers the removal of its codegen artifacts
                    aEncoder.dispose();
                }                
            }
            
            // Clear the encoder cache
            mWSDLConfig.clearEncoderCache();
        } catch (Throwable t) {
            if (currEndpoint != null) {
                String errMsg = mMessages.getString("JMSBC-E0756.EndpointShutdownFailed",
                            new Object[]{currEndpoint.getEndpointName(),
                                         currEndpoint.getServiceName().toString()});
                throw new JBIException (errMsg, t);
            } else { // some other exception with no endpoint details
                throw new JBIException (t);            
            }
        }finally{
            channelsPrepared = false;
        }
        
        
        //Remove QOS from channel
        channel.uninstallServiceQualities(mId);

    }

	private void acknowledgeAllPendingMessages(Set endpoints) {
		//Take the copy of the keys in the original Map
		if(mInboundMessageExchanges != null){//only in case of tests this map would be null
		    ArrayList listeners = new ArrayList(mInboundMessageExchanges.size());
		    listeners.addAll(mInboundMessageExchanges.values());
		    for(Iterator iter = listeners.iterator(); iter.hasNext();){
			Object obj = iter.next();
			if(obj instanceof InboundReplyContext){
		    		InboundReplyContext listener = (InboundReplyContext)obj;
		    		if(endpoints.contains(getEndpointKey(listener.getEndpoint()))){
		    		    listener.getReplyListener().shutdown();
		    		}
			}
		    }
		}
	}

    /**
     * Retrieves an Endpoint from this ServiceUnit 
     *
     * @param        serviceName the name of the service
     * @param        endpointName the name of the Endpoint to retrieve
     * @param        endpointType the endpoint type, either 
     *               Endpoint.EndpointType.INBOUND or Endpoint.EndpointType.OUTBOUND
     * @returns      The Endpoint instance or null if no endpoint is found
     *               that matches the given endpointName and serviceName.
     */
    synchronized  public Endpoint getEndpoint(String serviceName, 
                                String endpointName,
                                int endpointType) {

        Endpoint endpoint = null;
        
        endpoint = (Endpoint)mEndpoints.get(getEndpointKey(serviceName,
                                                           endpointName,
                                                           endpointType));                
        return endpoint;
    }

    /**
     * Retrieves the set of Endpoints handled by this ServiceUnit.
     *
     * @return       the set of Endpoints
     */
    synchronized  public Collection<Endpoint> getEndpoints() {
        return Collections.unmodifiableCollection(mEndpoints.values());
    }
    
    ////////
    //
    //  ServiceUnitImpl private Methods
    //
    ////////

    private EndpointConfiguration initializeEndpointConfig() throws Exception {
    	EndpointConfiguration endpointConfig = null;
        if (mEndpointConfig == null) {
                endpointConfig =
                    EndpointConfigurationFactory.getEndpointConfiguration(mSuPath);
        }
        
        return endpointConfig;
    }

    protected void startConsuming(Endpoint endpoint) throws Exception {
    	if (endpoint.getEndpointType() == EndpointType.INBOUND) {
    	    start(endpoint);
    	}    	
    }
    
    protected void activateOutboundEndpoint(Endpoint endpoint) throws Exception {
    	if (endpoint.getEndpointType() == EndpointType.OUTBOUND) {
            
            // make sure the channel is started first
            start(endpoint);
            
            // now activate the jbi endpoint for provisioning
            ServiceEndpoint endpointReference = 
                mContext.activateEndpoint(endpoint.getServiceName(),
                                          endpoint.getEndpointName());
            endpoint.setServiceEndpoint(endpointReference);
            
            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG,                             
                            "ServiceUnitImpl_PROVISIONED_ENDPOINT",
                            new Object[]{endpoint.getServiceName().toString(),
                                         endpoint.getEndpointName(),
                                         mId});
            }
        }
    }
    
    protected void stopConsuming(Endpoint endpoint) throws Exception {
        if (endpoint.getEndpointType() == EndpointType.INBOUND) {
            stop(endpoint);
        }
    }
    
    protected void deactivateOutboundEndpoint(Endpoint endpoint) throws Exception {
        if (endpoint.getEndpointType() == EndpointType.OUTBOUND) {     
                        
            // deactivate jbi endpoint first to stop provisioing
            ServiceEndpoint endpointReference = endpoint.getServiceEndpoint();
            mContext.deactivateEndpoint(endpointReference);

            if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
                mLogger.log(LogSupport.LEVEL_DEBUG,                             
                            "ServiceUnitImpl_REMOVED_PROVISIONED_ENDPOINT",
                            new Object[]{endpoint.getServiceName().toString(),
                                         endpoint.getEndpointName(),
                                         mId});
            }
            
            // now stop outbound channel
            stop(endpoint);
        }        
    }    
        
    private void start (Endpoint endpoint) throws Exception {

        String direction = endpoint.getEndpointType()==Endpoint.EndpointType.OUTBOUND?"provisioning":"consuming";
                
        // start all channels for the endpoint
        Map operations = endpoint.getJMSOperations();
        Iterator iter = operations.keySet().iterator();
        while (iter.hasNext()) {
            QName operation = (QName)iter.next();
            ServiceEndpoint serviceEndpoint = endpoint.getServiceEndpoint();
            JMSOperation jmsOperation = (JMSOperation)operations.get(operation);
            
            Channel jmsChannel = mJMSChannelMgr.lookup(endpoint, 
                                                       operation);                
                        
            jmsChannel.start();
        }
        
        endpoint.setState(EndpointState.RUNNING);

        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG, 
                        "ServiceUnitImpl_ENDPOINT_STARTED",
                        new Object[]{direction,
                                     endpoint.getEndpointName(),
                                     endpoint.getServiceName().toString(),
                                     mId});
        }
    }

    private void stop(Endpoint endpoint){

        //First set the status to stopped
        endpoint.setState(EndpointState.STOPPED);
        acknowledgeAllPendingMessages(Collections.singleton(getEndpointKey(endpoint)));
        
        String direction = endpoint.getEndpointType()==Endpoint.EndpointType.OUTBOUND?"provisioning":"consuming";
        
        // stop all channels for the endpoint
        Map operations = endpoint.getJMSOperations();
        Iterator iter = operations.keySet().iterator();
        while (iter.hasNext()) {
            QName operation = (QName)iter.next();
			try {
				Channel jmsChannel = mJMSChannelMgr.lookup(endpoint, 
				                                           operation);
				jmsChannel.stop();
			} catch (ChannelManagerException e) {
				mLogger.log(Level.WARNING, e.getMessage(), e);
			} catch (ChannelException e) {
				mLogger.log(Level.WARNING, e.getMessage(), e);
			}
        }
        
        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG, 
                        "ServiceUnitImpl_ENDPOINT_STOPPED",
                        new Object[]{direction,
                                     endpoint.getEndpointName(),
                                     endpoint.getServiceName().toString(),
                                     mId});
        }
    }

    private void prepareChannels() throws JBIException {
    	
        ArrayList successEndpoints = new ArrayList();
        Endpoint endpoint = null;
        try {
            Iterator it = mEndpoints.values().iterator();
            while (it.hasNext()) {
                endpoint = (Endpoint)it.next();
                addChannels(endpoint);
            }
        } catch (Throwable t) {            
            // cleanup other channels for other endpoints (all for one, one for all)
            Iterator i = successEndpoints.iterator();
            while(i.hasNext()) {
                Endpoint ep = (Endpoint)i.next();
                try {
                    removeChannels(ep);
                } catch (Throwable t2) {
                    mLogger.log(Level.WARNING,
                                mMessages.getString("JMSBC-W0717.EndpointRemoveChannelsFailed",
                                    new Object[]{ep.getEndpointName(),
                                                 ep.getServiceName().toString(),
                                                 mId,
                                                 t2.getLocalizedMessage()}),
                                t2);
                    AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0717.EndpointRemoveChannelsFailed",
                                    new Object[]{ep.getEndpointName(),
                                                 ep.getServiceName().toString(),
                                                 mId,
                                                 t2.getLocalizedMessage()}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            mId, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0717");  
                    continue;
                }
            }
            
            if (endpoint != null) {
                String errMsg = mMessages.getString("JMSBC-E0757.EndpointPrepareChannelsFailed",
                            new Object[]{endpoint.getEndpointName(),
                                         endpoint.getServiceName().toString()});
                AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-E0757.EndpointPrepareChannelsFailed"), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            mId, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-E0757");  
                throw new JBIException (errMsg, t);
            } else {
                throw new JBIException (t);
            }
        }
    }
    
    
    private void addChannels (Endpoint endpoint) throws Exception {
        Map operations = endpoint.getJMSOperations();
        Iterator iter = operations.keySet().iterator();
        ArrayList operationsList = new ArrayList();
        boolean persistConnInfo = false;
        try {
            while (iter.hasNext()) {
                QName operation = (QName)iter.next();
                Channel jmsChannel = mJMSChannelMgr.addChannel(endpoint, operation);
                operationsList.add(operation);
                JMSOperation jmsop = (JMSOperation)operations.get(operation);
                if (jmsop.getTransaction().equals(JMSConstants.TRANSACTION_XA)){
                    persistConnInfo = true;
                }
                try {
                    jmsChannel.open();
                } catch (ChannelException ex) {
                    if (endpoint.getEndpointType() == Endpoint.EndpointType.OUTBOUND) {
                        mLogger.log(Level.WARNING,
                                    mMessages.getString ("JMSBC-W0718.OutboundChannelOpenFailed",
                                        new Object[]{mId,
                                                     endpoint.getServiceName().toString(),
                                                     endpoint.getEndpointName(),
                                                     operation.toString(),
                                                     ex.getLocalizedMessage()}),
                                    ex);
                    AlertsUtil.getAlerter().warning(mMessages.getString ("JMSBC-W0718.OutboundChannelOpenFailed",
                                        new Object[]{mId,
                                                     endpoint.getServiceName().toString(),
                                                     endpoint.getEndpointName(),
                                                     operation.toString(),
                                                     ex.getLocalizedMessage()}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            mId, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0718");                             
                    } else { // propagate error for inbound
                        throw ex;
                    }
                }
            }
        } catch (Exception ex) {
            // Remove channels added before failure (inclusive of failure).
            Iterator opsIter = operationsList.iterator();
            while (opsIter.hasNext()) {
                QName op = (QName)opsIter.next();
                Channel channel = mJMSChannelMgr.removeChannel(endpoint, op);
                try {
                    channel.close();
                } catch (Throwable t) {
                    continue;
                }
            }
            
            throw ex;  // propogate
        }

        try {
            if (persistConnInfo && mConnPersister != null) {
                JMSConnectionInfoRecord rec;
                
                if (endpoint.getJMSAddress().getConnectionURL().equals(JMSConstants.JNDI_GENERIC_JMS_SCHEME)) {
                    Properties jndiEnv = endpoint.getJMSAddress().getJndiEnv().toProperties();
                    rec = new JMSJndiConnectionInfoRecord(endpoint.getJMSAddress().getConnectionURL(),
                                                          endpoint.getJMSAddress().getUsername(),
                                                          endpoint.getJMSAddress().getPassword(),
                                                          endpoint.getJMSAddress().getJmsjcaOptions() == null?null:endpoint.getJMSAddress().getJmsjcaOptions().getOptions(),
                                                          endpoint.getJMSAddress().getConnectionFactoryName(),
                                                          endpoint.getJMSAddress().getProviderURL(),
                                                          endpoint.getJMSAddress().getInitialContextFactory(),
                                                          endpoint.getJMSAddress().getSecurityPrincial(),
                                                          endpoint.getJMSAddress().getSecurityCredentials(),
                                                          jndiEnv);
                    mConnPersister.persist(new JMSConnectionInfoRecord [] {rec});
                } else if(!endpoint.getJMSAddress().getConnectionURL().startsWith(JMSConstants.LOOKUP_JMS_SCHEME)){
                    rec = new JMSConnectionInfoRecord(endpoint.getJMSAddress().getConnectionURL(),
                                                        endpoint.getJMSAddress().getUsername(),
                                                        endpoint.getJMSAddress().getPassword(),
                                                        endpoint.getJMSAddress().getJmsjcaOptions() == null?null:endpoint.getJMSAddress().getJmsjcaOptions().getOptions());
                    mConnPersister.persist(new JMSConnectionInfoRecord [] {rec});
                }
            }        
        } catch (ConnectionInfoPersistException ex) {
            mLogger.log(Level.WARNING,
                        mMessages.getString("JMSBC-W0719.ConnectionInfoPersistError",
                            new Object[]{endpoint.getServiceName(),
                                         endpoint.getEndpointName(),
                                         endpoint.getJMSAddress().getConnectionURL(),
                                         ex.getLocalizedMessage()}),
                        ex);
            AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0719.ConnectionInfoPersistError",
                            new Object[]{endpoint.getServiceName(),
                                         endpoint.getEndpointName(),
                                         endpoint.getJMSAddress().getConnectionURL(),
                                         ex.getLocalizedMessage()}), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            mId, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0719");                         
        }
    }
    
    private void cleanupChannels() {
        if (mEndpoints != null) {
            Endpoint endpoint = null;
            Iterator it = mEndpoints.values().iterator();
            while (it.hasNext()) {
                endpoint = (Endpoint)it.next();
                try {
                    removeChannels(endpoint);
                } catch (Throwable t) {
                    mLogger.log(Level.WARNING,
                                mMessages.getString("JMSBC-W0717.EndpointRemoveChannelsFailed",
                                    new Object[]{endpoint.getEndpointName(),
                                                 endpoint.getServiceName().toString(),
                                                 mId,
                                                 t.getLocalizedMessage()}),
                                t);
                   AlertsUtil.getAlerter().warning(mMessages.getString("JMSBC-W0717.EndpointRemoveChannelsFailed"), 
                            JMSBindingComponent.SHORT_DISPLAY_NAME, 
                            mId, 
                            AlertsUtil.getServerType(),
                            AlertsUtil.COMPONENT_TYPE_BINDING,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                            NotificationEvent.EVENT_TYPE_ALERT,
                            "JMSBC-W0717");                                
                    continue;
                }
            }
        }
    }
    
    private void removeChannels (Endpoint endpoint) throws Exception {
        Map operations = endpoint.getJMSOperations();
        Iterator iter = operations.keySet().iterator();
        while (iter.hasNext()) {
            QName operation = (QName)iter.next();
            Channel channel = null;
            if (endpoint.getEndpointType() == Endpoint.EndpointType.OUTBOUND) {
                // Use ServiceEndpoint name from JBI as part of key
                channel = mJMSChannelMgr.removeChannel(endpoint, 
                                                       operation);
            } else {
                // Use Endpoint name from portmap as part of key
                channel = mJMSChannelMgr.removeChannel(endpoint, 
                                                       operation);                
            }
            channel.close();
        }        
    }
        
    private String getEndpointKey(Endpoint endpoint) {
        return getEndpointKey (endpoint.getServiceName().toString(),
                               endpoint.getEndpointName(),
                               endpoint.getEndpointType());
    }
    
    private String getEndpointKey(String serviceName, 
                                  String endpointName,
                                  int endpointType) {
        return mId + 
               serviceName + 
               endpointName +
               EndpointImpl.endpointTypeToString(endpointType);               
    }
 
    /** package protected method.
      * Used solely for JUnit test purposes
      */
    void setEndpoints(Map endpoints) {
        mEndpoints = endpoints;
    }

	synchronized public void resume(Endpoint ep) throws Exception {
		start(ep);
		sendAlert(ep, "JMSBC-W0733.ResumedEndpoint");
	}

	synchronized  public void suspend(Endpoint ep) throws Exception {
		stop(ep);
		sendAlert(ep, "JMSBC-W0732.SuspendedEndpoint");
	}

	private void sendAlert(Endpoint ep, String str) {
		String msg = mMessages.getString(str,
				new Object[] { ep.toString() });
		mLogger.log(Level.WARNING, msg);
		AlertsUtil.getAlerter().warning(msg, AlertsUtil.SUN_JMS_BINDING, mId,
				AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
				NotificationEvent.OPERATIONAL_STATE_RUNNING,
				NotificationEvent.EVENT_TYPE_ALERT, "JMSBC-W0732");
	}
    
}
