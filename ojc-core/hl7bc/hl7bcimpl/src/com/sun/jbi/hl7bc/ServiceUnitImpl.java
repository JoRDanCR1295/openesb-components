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

package com.sun.jbi.hl7bc;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.encoder.Encoder;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;
//import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.hl7bc.Endpoint.EndpointState;
import com.sun.jbi.hl7bc.Endpoint.EndpointType;
import com.sun.jbi.hl7bc.packaging.EndpointConfiguration;
import com.sun.jbi.hl7bc.packaging.EndpointConfigurationFactory;
import com.sun.jbi.hl7bc.packaging.WSDLConfigurations;
import com.sun.jbi.hl7bc.configuration.RuntimeConfigurationMBean;
import com.sun.jbi.hl7bc.connection.HL7BCConnectionManager;
import com.sun.jbi.hl7bc.connection.ConnectionInfo;
import com.sun.jbi.hl7bc.util.AlertsUtil;
import com.sun.jbi.hl7bc.I18n;

/**
 * ServiceUnitImpl represents a ServiceUnit deployed in a ServiceUnitManager.
 * <p>
 * The initialization of a ServiceUnit and its respective Endpoints depends on two main
 * configuration items: the endpoints.xml file and the WSDL(s) representing the Service Unit.
 * Initialization of the ServiceUnit is done through the Observer pattern when parsing each of the
 * WSDL files. This class only cares about WSDLs containing the extensibility elements pertinent to
 * this Binding Component.
 * <p>
 * This is important since the actual representation of all the initialization artifacts may change
 * in the future. In other words, endpoints.xml file may disappear, and the extensibility elements
 * for each BC is different. However, by encapsulating the initialization logic away from model of
 * ServiceUnitManagers, ServiceUnits, and Endpoints, we can more easily change how models are
 * initialized without breaking too much.
 * <p>
 * If the artifacts change, only this class and the classes in the com.sun.jbi.hl7.packaging package
 * should need to change.
 * 
 * @author S. Nageswara Rao, Raghunadh
 */
public class ServiceUnitImpl implements ServiceUnit {

    private static Logger mLogger = Logger.getLogger(ServiceUnitImpl.class.getName());

    private String mId;

    private String mSuPath;

    private ComponentContext mContext;

    private StatusProviderHelper mStatusProviderHelper;

    private RuntimeConfigurationMBean mRuntimeConfiguration;

    private InboundReceiver mInboundReceiver;

    private WSDLConfigurations mWSDLConfig;

    private EndpointConfiguration mEndpointConfig;

    private Collection<Endpoint> mEndpoints;

    private final Collection<Endpoint> mSuspendedEndpoints;

    private MessagingChannel mChannel;

	private OutboundReceiver mOutboundReceiver;

   // private Map<EndpointInfo, List<ServiceQuality>> mEndpointQos;

    public ServiceUnitImpl(String id, String suPath, ComponentContext context, RuntimeConfigurationMBean runtimeConfig,
            StatusProviderHelper statusProviderHelper, InboundReceiver inboundReceiver, OutboundReceiver outboundReceiver) {
        mId = id;
        mSuPath = suPath;
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mRuntimeConfiguration = runtimeConfig;
        mInboundReceiver = inboundReceiver;
        mOutboundReceiver = outboundReceiver;
        mEndpoints = new HashSet<Endpoint>();
        mSuspendedEndpoints = new HashSet<Endpoint>();
        mWSDLConfig = new WSDLConfigurations(suPath);
        mChannel = HL7ComponentContext.getInstance().getBindingChannel();
    }

    // //////
    //
    // ServiceUnit Interface Methods
    //
    // //////

    /**
     * Retrieves the Id of this ServiceUnit.
     * 
     * @return the name of the Service as a QName
     */
    public String getServiceUnitId() {
        return mId;
    }

    public String getServiceUnitPath() {
        return mSuPath;
    }

    /**
     * Deploy the ServiceUnit.
     * 
     * @exception JBIException if unable to initialize this ServiceUnit
     */
    public void deploy() throws JBIException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("ServiceUnit deploy method called."));
        }
        try {
            mEndpointConfig = initializeEndpointConfig();
            Map<String, String[]> envVariableMap = mWSDLConfig.parseForEnvironmentVariables(
                    mEndpointConfig.endpoints(), mRuntimeConfiguration.retrieveApplicationVariablesMap());
            if (envVariableMap.size() > mRuntimeConfiguration.countVariables()) {
                // number of environment variable tokens used in File WSDLs
                // is greater than the ones defined
                mRuntimeConfiguration.updateApplicationVariablesMap(envVariableMap);
            }
            // Required for V3 support
            // mWSDLConfig.parseForV3ACKXsds();
        } catch (Exception ex) {
            String errMsg = I18n.msg("E0179: Failed to deploy service unit {0} due to : {1}", mId,
                    ex.getLocalizedMessage() );
            mLogger.log(Level.SEVERE, errMsg, ex);
            AlertsUtil.getAlerter().critical(errMsg, HL7BindingComponent.SHORT_DISPLAY_NAME, mId,
                    AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0179");
            throw new JBIException(errMsg);
        }
    }

    /**
     * Initializes the ServiceUnit. Parses the serviceUnitRootPath to create the Endpoint objects. A
     * ServiceUnit may have mutiple WSDLs, each with multiple endpoints that we care about.
     * <p>
     * This method will initialize the WSDLs, transforming it into a set of Endpoints
     * 
     * @exception JBIException if unable to initialize this ServiceUnit
     */
    public void init() throws JBIException {

        // Initialize the endpoints for this ServiceUnit. Basically sets
        // up the static configuration information for each Endpoint.
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("ServiceUnit init method called."));
        }
        Endpoint currEndpoint = null;
        Collection<Endpoint> endpoints = null;
        Collection<Endpoint> activatedOutboundEndpoints = new HashSet<Endpoint>();

       /* DeploymentLookup lookUpUtil = new DeploymentLookup(mContext);
        try {
            mEndpointQos = lookUpUtil.lookupServiceQualities(mSuPath);
        } catch (DeploymentException ex) {
            mLogger.log(Level.WARNING, I18n.msg("W0115: Failed to retrieve QOS from service unit root path {0}.", mSuPath), ex);
            AlertsUtil.getAlerter().warning(I18n.msg("W0115: Failed to retrieve QOS from service unit root path {0}.", mSuPath),
                    HL7BindingComponent.SHORT_DISPLAY_NAME, mId, AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING, NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-W0115");
        }
        if (mEndpointQos != null && mEndpointQos.size() > 0) {
            for (Iterator itor = mEndpointQos.keySet().iterator(); itor.hasNext();) {
                EndpointInfo endpointInfo = (EndpointInfo) itor.next();
                ServiceQuality[] qos = (ServiceQuality[]) ((List) mEndpointQos.get(endpointInfo)).toArray(new ServiceQuality[0]);
                mChannel.addServiceQualityToEndpoint(endpointInfo, qos);
            }
        }*/
		// for unit test only 
		if(mChannel == null){
			return;
		}
		 mChannel.installServiceQualities(mId, mSuPath);
        try {
            // Prepare for endpoint configuration map if deploy() hasn't been called yet
            if (mEndpointConfig == null) {
                mEndpointConfig = initializeEndpointConfig();
            }
            endpoints = mWSDLConfig.createEndpoints(mEndpointConfig.endpoints(),
                    mRuntimeConfiguration.retrieveApplicationVariablesMap(),
                    mRuntimeConfiguration.retrieveApplicationConfigurationsMap());
            if (endpoints.size() == 0) {
                mLogger.log(Level.WARNING, I18n.msg("W0116: The service unit {0} does not contain any WSDL(s) for which the HL7 Binding Component is responsible", mId));
                AlertsUtil.getAlerter().warning(I18n.msg("W0116: The service unit {0} does not contain any WSDL(s) for which the HL7 Binding Component is responsible", mId),
                        HL7BindingComponent.SHORT_DISPLAY_NAME, mId, AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING, NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-W0116");
            }
            // Initialize the state of all these endpoints.
            for (Endpoint endpoint : endpoints) {
                currEndpoint = endpoint;
                // Initializes encoder map for the current endpoint.
                // This also triggers the encoder codegen if the
                // specified encoder is not cached.
                Map encoderMapping = mWSDLConfig.getPartEncoderMapping(currEndpoint.getDefinition(),
                        currEndpoint.getServiceName().toString(), currEndpoint.getEndpointName(),
                        currEndpoint.getEndpointType(), currEndpoint.getHL7Operations(),
                        currEndpoint.getOperationMsgExchangePattern());
                currEndpoint.setMessagePartEncoderMapping(encoderMapping);
                getQOSConfigurations(currEndpoint);

                // Store the status...
                StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
                QName serviceName = currEndpoint.getServiceName();
                String portName = currEndpoint.getEndpointName();
                String uniqueName = null;
                if (currEndpoint.getEndpointType() == EndpointType.INBOUND) {
                    uniqueName = mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
                    reporting.addConsumingEndpoint(uniqueName);
                } else {
                    uniqueName = mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
                    reporting.addProvisioningEndpoint(uniqueName);
                    activateOutboundEndpoint(currEndpoint);
                    activatedOutboundEndpoints.add(currEndpoint);
                }
                currEndpoint.setEndpointStatus(reporting.getEndpointStatus(uniqueName));
                // set the serviceunit on the endpoint
                currEndpoint.setServiceUnit(this);
            }
            mEndpoints.clear();
            mEndpoints.addAll(endpoints);
            endpoints.clear();
        } catch (Exception ex) {
            String errMsg = ex.getLocalizedMessage();
            if (currEndpoint != null) {
                errMsg = I18n.msg("E0180: Failed to Initialize the end point with service name {0} and endpoint name {1} : {2}", mId,
                        currEndpoint.getServiceName(), currEndpoint.getEndpointName(), ex.getLocalizedMessage() );
                mLogger.log(Level.SEVERE, errMsg, ex);
                AlertsUtil.getAlerter().critical(errMsg, HL7BindingComponent.SHORT_DISPLAY_NAME, mId,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0180");
            } else {
                errMsg = I18n.msg("E0181: Failed to Initialize the null end point with service name {0} and exception is = {1}", mId,
                        ex.getLocalizedMessage() );
                mLogger.log(Level.SEVERE, errMsg, ex);
                AlertsUtil.getAlerter().critical(errMsg, HL7BindingComponent.SHORT_DISPLAY_NAME, mId,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0181");
            }

            for (Endpoint activatedOutboundEndpoint : activatedOutboundEndpoints) {
                deactivateOutboundEndpoint(activatedOutboundEndpoint);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Endpoint Deactivated: {0}", activatedOutboundEndpoint.getEndpointName()));
                }
            }

            throw new JBIException(errMsg);
        }
    }

    /**
     * Starts this ServiceUnit. This involves activating all Endpoints that are part of this
     * ServiceUnit. All Endpoints are moved into the EndpointState.RUNNING state.
     * <p>
     * TODO: What should happen if not all the Endpoints can be activated? Should I deactivate them
     * or just leave them? For now, I'm going to assume that this method is transactional. Either
     * all the Endpoints activate or none. If any one fails to activate, the other activated
     * Endpoints will be deactivated.
     * 
     * @exception JBIException if a any Endpoint fails to activate
     */
    public void start() throws JBIException {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("ServiceUnit start method called."));
        }
        HashSet<Endpoint> activatedInboundEndpoints = new HashSet<Endpoint>();

        Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint = null;
        try {
            while (it.hasNext()) {
                currEndpoint = (Endpoint) it.next();
                startConsuming(currEndpoint);
                activatedInboundEndpoints.add(currEndpoint);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Consuming endpoint started: {0}", currEndpoint.getEndpointName()));
                }
            }
        } catch (Exception ex) {
            String errMsg = ex.getLocalizedMessage();
            if (currEndpoint != null) {
                errMsg = I18n.msg("E0182: Failed to start the end point with service name {0} and endpoint name {1} : {2}", mId,
                        currEndpoint.getServiceName(), currEndpoint.getEndpointName(), ex.getLocalizedMessage());
                mLogger.log(Level.SEVERE, errMsg, ex);
                AlertsUtil.getAlerter().critical(errMsg, HL7BindingComponent.SHORT_DISPLAY_NAME, mId,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0182");
            } else {
                errMsg = I18n.msg("E0183: Failed to start the null end point with service name {0} and exeption is {1}",  mId,
                        ex.getLocalizedMessage());
                mLogger.log(Level.SEVERE, errMsg, ex);
                AlertsUtil.getAlerter().critical(errMsg, HL7BindingComponent.SHORT_DISPLAY_NAME, mId,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0183");
            }
            for (Object activatedInboundEndpoint : activatedInboundEndpoints) {
                Endpoint endpoint = (Endpoint) activatedInboundEndpoint;
                stopConsuming(endpoint);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Endpoint : {0} stopped ", endpoint.getEndpointName()));
                }
            }

            throw new JBIException(errMsg);
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("Successfully started service unit : {0}.", getServiceUnitId()));
        }
    }

    /**
     * Stops this ServiceUnit. This involves deactivating all Endpoints that are part of this
     * ServiceUnit. All Endpoints are moved into the EndpointState.STOPPED state.
     * 
     * @exception JBIException if any Endpoint fails to deactivate
     */
    public void stop() throws JBIException {
        for (Endpoint endpoint : mEndpoints) {
            if (endpoint.getEndpointType() == EndpointType.INBOUND) {
                stopConsuming(endpoint);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Endpoint {0} stopped ", endpoint.getEndpointName()));
                }
            }

        }
    }

    /**
     * Shuts down this ServiceUnit. Simply moves all Endpoints to the shutdown state.
     * 
     * @exception JBIException if the ServiceUnit fails to shutdown
     */
    public void shutdown() throws JBIException {

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("ServiceUnit shutdown method called."));
        }
        // remove the QoS attributes for the undeployed endpoints
       /* if (mEndpointQos != null && mEndpointQos.size() > 0) {
            for (Iterator it = mEndpointQos.keySet().iterator(); it.hasNext();) {
                EndpointInfo endpointInfo = (EndpointInfo) it.next();
                ServiceQuality[] qos = (ServiceQuality[]) ((List) mEndpointQos.get(endpointInfo)).toArray(new ServiceQuality[0]);
                mChannel.removeServiceQualityFromEndpoint(endpointInfo, qos);
            }
        }*/
        
        // for unit test only
        if ( mChannel == null ) {
            return;
        }
        
        // remove the QoS attributes for the undeployed endpoints
        Map<EndpointInfo, List<ServiceQuality>> qosMap = mChannel.getLookup().getQoSAssembly(mId).getServiceQualities();

        if (qosMap != null && qosMap.size() > 0) {
            for (Iterator it = qosMap.keySet().iterator(); it.hasNext();) {
                EndpointInfo endpointInfo = (EndpointInfo) it.next();
                ServiceQuality[] qos = 
                    (ServiceQuality[])((List)qosMap.get(endpointInfo)).toArray(new ServiceQuality[0]);
                mChannel.removeServiceQualityFromEndpoint(endpointInfo, qos);
            }
        }
        //mChannel.uninstallServiceQualities(mId);

        Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint = null;

        while (it.hasNext()) {
            currEndpoint = (Endpoint) it.next();

            // Set the status of the endpoint
            StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
            QName serviceName = currEndpoint.getServiceName();
            String portName = currEndpoint.getEndpointName();
            String uniqueName = null;
            if (currEndpoint.getEndpointType() == EndpointType.INBOUND) {
                uniqueName = mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
                reporting.removeConsumingEndpoints(new String[] { uniqueName });
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Consuming endpoint {0} shutdown", uniqueName));
                }
            } else {
                uniqueName = mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
                reporting.removeProvisioningEndpoints(new String[] { uniqueName });
                deactivateOutboundEndpoint(currEndpoint);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Provisioning endpoint {0} shutdown", uniqueName));
                }
                mOutboundReceiver.removeOutboundMessageProcessor(currEndpoint);
            }

            // Dispose the encoders
            Map encoders = currEndpoint.getMessagePartEncoderMapping();
            if (encoders != null) {
                for (Iterator it2 = encoders.values().iterator(); it2.hasNext();) {
                    Encoder aEncoder = (Encoder) it2.next();
                    // Dispose the encoder. This also
                    // triggers the removal of its codegen artifacts
                    aEncoder.dispose();
                }
            }
            
            // clean the connection pool associated witht the current endpoint.
            try{
            	if(!mRuntimeConfiguration.getAlwaysCreatesNewConnection()){
					// create ConnectionInfo Key.
					ConnectionInfo connInfo = new ConnectionInfo();
					connInfo.setHost(currEndpoint.getHL7Address().getHL7ServerLocation());
					connInfo.setPort(currEndpoint.getHL7Address().getHL7ServerPort());
					String endpointName = currEndpoint.getEndpointName() + currEndpoint.getUniqueName();
					connInfo.setEndpointName(endpointName);
					HL7BCConnectionManager.cleanupPoolAssoaciatedWithEndpoint(connInfo);
				}
            }catch(Exception ex){
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, I18n.msg("Unable to clean the connection pool for endpoint : {0}.", currEndpoint.getEndpointName()));
                }
            }
        }
        // Clear the encoder cache
        mWSDLConfig.clearEncoderCache();
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("Successfully shutdown service unit : {0}.", getServiceUnitId()));
        }
    }

    /**
     * Retrieves the set of Endpoints handled by this ServiceUnit.
     * 
     * @return the set of Endpoints
     */
    public Collection<Endpoint> getEndpoints() {
        return Collections.unmodifiableCollection(mEndpoints);
    }

    // //////
    //
    // ServiceUnitImpl Protected Methods
    //
    // //////

    protected void activateOutboundEndpoint(Endpoint endpoint) throws JBIException {
        if (endpoint.getEndpointType() != EndpointType.OUTBOUND) {
            return;
        }
        ServiceEndpoint endpointReference = mContext.activateEndpoint(endpoint.getServiceName(),
                endpoint.getEndpointName());
        endpoint.setServiceEndpoint(endpointReference);        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("Activated outbound endpoint for service name {0} and endpoint name {1}.", endpoint.getServiceName(),
                    endpoint.getEndpointName() ));
        }

    }

    protected void deactivateOutboundEndpoint(Endpoint endpoint) throws JBIException {
        if (endpoint.getEndpointType() != EndpointType.OUTBOUND) {
            return;
        }

        ServiceEndpoint endpointReference = endpoint.getServiceEndpoint();
        mContext.deactivateEndpoint(endpointReference);

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, I18n.msg("Deactivated outbound endpoint with service name {0} and endpoint name {1}.", endpoint.getServiceName(),
                    endpoint.getEndpointName() ));
        }
    }

    protected void startConsuming(Endpoint endpoint) throws JBIException {
        
    	switch (endpoint.getEndpointType()) {
    	case EndpointType.INBOUND:
            try {
                mInboundReceiver.addInboundMessageProcessor(endpoint);
            } catch (Exception ex) {
                String errMsg = I18n.msg("E0184: Failed to start inbound message processor for endpoint : {0}.", endpoint.getEndpointName(),
    				ex.getLocalizedMessage());
                mLogger.log(Level.SEVERE, errMsg, ex);
                AlertsUtil.getAlerter().critical(errMsg, HL7BindingComponent.SHORT_DISPLAY_NAME, mId,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0184");
                throw new JBIException(errMsg, ex);
            }
            break;

    	case EndpointType.OUTBOUND:
            try {
            	mOutboundReceiver.addOutboundMessageProcessor(endpoint);
            } catch (Exception ex) {
                String errMsg = I18n.msg("E0343: Failed to start outbound message processor for endpoint : {0}.", endpoint.getEndpointName(),
    				ex.getLocalizedMessage());
                mLogger.log(Level.SEVERE, errMsg, ex);
                AlertsUtil.getAlerter().critical(errMsg, HL7BindingComponent.SHORT_DISPLAY_NAME, mId,
                        AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, NotificationEvent.EVENT_TYPE_ALERT, "HL7BC-E0184");
                throw new JBIException(errMsg, ex);
            }
            break;
         
        default:
        	// Shouldn't happen
        	throw new IllegalStateException(Integer.toString(endpoint.getEndpointType()));
    	}
        
        
    }

    protected void stopConsuming(Endpoint endpoint) throws JBIException {
        if (endpoint.getEndpointType() != EndpointType.INBOUND) {
            return;
        }
        try {
            mInboundReceiver.removeInboundMessageProcessor(endpoint);
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, I18n.msg("E0185: Failed to stop inbound message processor for endpoint : {0}.", endpoint.getEndpointName()));
            throw new JBIException(ex.getLocalizedMessage(), ex);
        }
    }

    private EndpointConfiguration initializeEndpointConfig() throws Exception {
        EndpointConfiguration endpointConfig = null;
        if (mEndpointConfig == null) {
            endpointConfig = EndpointConfigurationFactory.getEndpointConfiguration(mSuPath);
        }

        return endpointConfig;
    }

    /*private void getQOSConfigurations(Endpoint endpoint, Map<EndpointInfo, List<ServiceQuality>> qosMap) {
        if (qosMap != null && qosMap.size() > 0) {
            // Until there's a total transitioning to use the common-util,
            // there's a need to
            // create EndpointInfo using EndpointIndentifier
            EndpointInfo endpointInfo = new EndpointInfo(false, endpoint.getEndpointName(), null,
                    endpoint.getServiceName(), null);
            List<ServiceQuality> qoss = qosMap.get(endpointInfo);
            if (qoss == null) {
                // there is no QoS configuration on this endpoint
                return;
            }
            Iterator<ServiceQuality> qossIter = qoss.iterator();
            while (qossIter.hasNext()) {
                ServiceQuality qos = qossIter.next();
                // get the throttling configuration
                if (qos instanceof ThrottlingConfig) {
                    ThrottlingConfig throttleConfig = (ThrottlingConfig) qos;
                    endpoint.setMaxConcurrencyLimit(throttleConfig.getMaxConcurrencyLimit());
                } else if (qos instanceof RedeliveryConfig) { // get the redelivery configuration
                    RedeliveryConfig redeliveryConfig = (RedeliveryConfig) qos;
                    endpoint.setRedeliveryConfiguration(redeliveryConfig);
                }
                // no other QoS services at the moment...
            }
        }
    }*/

	private void getQOSConfigurations(Endpoint endpoint) throws DeploymentException {
        // Until there's a total transitioning to use the common-util,
        // there's a need to
        // create EndpointInfo using EndpointIndentifier
        Map<EndpointInfo, List<ServiceQuality>> qosMap = mChannel.getLookup().getQoSAssembly(mId).getServiceQualities();
        if (qosMap != null && qosMap.size() > 0) {
            EndpointInfo endpointInfo = new EndpointInfo(false, endpoint.getEndpointName(), null,
                    endpoint.getServiceName(), null);
            List<ServiceQuality> qoss = qosMap.get(endpointInfo);
            if (qoss == null) {
                // there is no QoS configuration on this endpoint
                return;
            }
            Iterator<ServiceQuality> qossIter = qoss.iterator();
            while (qossIter.hasNext()) {
                ServiceQuality qos = qossIter.next();
                // get the throttling configuration
                if (qos instanceof ThrottlingConfig) {
                    ThrottlingConfig throttleConfig = (ThrottlingConfig) qos;
                    endpoint.setMaxConcurrencyLimit(throttleConfig.getMaxConcurrencyLimit());
                } else if (qos instanceof RedeliveryConfig) { // get the redelivery configuration
                    RedeliveryConfig redeliveryConfig = (RedeliveryConfig) qos;
                    endpoint.setRedeliveryConfiguration(redeliveryConfig);
                }
                // no other QoS services at the moment...
            }
        }
    }

    /**
     * Determines if the specified endpoint is in a suspended state. A suspended endpoint is a
     * started endpoint, not stopped, not shut down, but not accepting calls for its service.
     * 
     * @param endpoint An endpoint managed by the service unit
     * @return true if the endpoint is suspended, false otherwise.
     * @throws javax.jbi.JBIException if the specified endpoint is null, or is not one managed by
     *             the service, or its state cannot be determined for whatever reason.
     */
    public boolean isSuspended(Endpoint endpoint) throws JBIException {
        synchronized (mSuspendedEndpoints) {
            return mSuspendedEndpoints.contains(endpoint);
        }
    }

    /**
     * Suspends the specified endpoint. The endpoint must be one that the service unit is managing.
     * An endpoint that is suspended is started, not stopped nor shut down, but not accepting calls
     * for its service.
     * 
     * @param endpoint An endpoint managed by the service unit
     * @throws javax.jbi.JBIException if the specified endpoint is null, is not one managed by the
     *             service unit, or its suspension cannot be effected for whatever reason.
     */
    public void suspend(Endpoint endpoint) throws JBIException {
        synchronized (mSuspendedEndpoints) {
            if (mSuspendedEndpoints.contains(endpoint)) {
                throw new JBIException(I18n.msg("E0186: Cannot suspend endpoint {0},{1} - already suspended", endpoint.getServiceName().toString(),
					endpoint.getEndpointName() ));
            }
            if (endpoint.getEndpointType() == EndpointType.INBOUND) {
                try {
                    mInboundReceiver.suspendInboundMessageProcessor(endpoint);
                } catch (Exception exc) {
                    throw new JBIException(exc);
                }
            } /* else {
                deactivateOutboundEndpoint(endpoint);
            } */

            mSuspendedEndpoints.add(endpoint);
        }
    }

    /**
     * Resumes (unsuspends) the specified endpoint. The endpoint must be one that the service unit
     * is managing. An endpoint that is resumed is started and accepting calls for its service.
     * 
     * @param endpoint An endpoint managed by the service unit
     * @throws javax.jbi.JBIException if the specified endpoint is null, is not one managed by the
     *             service unit, or its resumption cannot be effected for whatever reason.
     */
    public void resume(Endpoint endpoint) throws JBIException {
        synchronized (mSuspendedEndpoints) {
            if (!mSuspendedEndpoints.contains(endpoint)) {
                throw new JBIException(I18n.msg("E0187: Cannot resume endpoint {0},{1} - not suspended", endpoint.getServiceName().toString(),
					endpoint.getEndpointName()));
            }
            if (endpoint.getEndpointType() == EndpointType.INBOUND) {
                try {
                    mInboundReceiver.resumeInboundMessageProcessor(endpoint);
                } catch (Exception exc) {
                    throw new JBIException(exc);
                }

            } /* else {
                activateOutboundEndpoint(endpoint);
            }*/

            mSuspendedEndpoints.remove(endpoint);
        }
    }

    /**
     * package protected method. Used solely for JUnit test purposes
     */
    void setEndpoints(List endpoints) {
        mEndpoints = endpoints;
    }
}
