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

import java.text.MessageFormat;
import java.util.List;
import java.util.Map;
import java.util.HashSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.namespace.QName;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;

import com.sun.encoder.Encoder;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;

import com.sun.jbi.imsbc.ims.Channel;
import com.sun.jbi.imsbc.Endpoint.EndpointState;
import com.sun.jbi.imsbc.Endpoint.EndpointType;
import com.sun.jbi.imsbc.packaging.EndpointConfiguration;
import com.sun.jbi.imsbc.packaging.EndpointConfigurationFactory;
import com.sun.jbi.imsbc.packaging.InvalidConfigurationException;
import com.sun.jbi.imsbc.packaging.WSDLConfigurations;
import com.sun.jbi.imsbc.extensions.IMSOperation;
import com.sun.jbi.imsbc.IMSException;
import com.sun.jbi.imsbc.mbeans.RuntimeConfigurationMBean;
import com.sun.jbi.imsbc.util.AlertsUtil;

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
 * com.sun.jbi.imsbc.packaging package should need to change.
 *
 * @author Sun Microsystems
 */
public class ServiceUnitImpl implements ServiceUnit {

    static final Messages mMessages = Messages.getMessages(ServiceUnitImpl.class);

    static final Logger mLogger = Messages.getLogger(ServiceUnitImpl.class);

    private String mId;

    private ComponentContext mContext;

    private StatusProviderHelper mStatusProviderHelper;

    private Collection<Endpoint> mEndpoints;
    
    private String mSuPath;

    private InboundReceiver mInboundProcessorMgr;

    private WSDLConfigurations mWSDLConfig;
    
    private RuntimeConfigurationMBean mRuntimeConfig;

    private EndpointConfiguration mEndpointConfig;


    public ServiceUnitImpl(String suId, String suPath, ComponentContext context, RuntimeConfigurationMBean runtimeConfig, 
            StatusProviderHelper statusProviderHelper) {
        mId = suId;
        mSuPath = suPath;
        mContext = context;
        mRuntimeConfig = runtimeConfig;
        mStatusProviderHelper = statusProviderHelper;
        mWSDLConfig = new WSDLConfigurations(suPath);
        mEndpoints = new HashSet<Endpoint>();
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
     public void deploy() throws JBIException {
  		 if (mLogger.isLoggable(Level.INFO))
			 mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00317.SU_Deploy_Called"));
         try {
             mEndpointConfig = initializeEndpointConfig();
             Map<String, String[]> envVariableMap = mWSDLConfig.parseForEnvironmentVariables(
                     mEndpointConfig.endpoints(),
                     mRuntimeConfig.retrieveApplicationVariablesMap());
            if (envVariableMap.size() > mRuntimeConfig.countVariables()) {
                // number of environment variable tokens used in WSDLs 
                // is greater than the ones defined 
                mRuntimeConfig.updateApplicationVariablesMap(envVariableMap);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = ex.getLocalizedMessage();
            mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00316.SU_Deploy_Failed", new Object[] {mId, errMsg}));
            throw new JBIException(errMsg);
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
    public void init() throws JBIException {
        // Initialize the endpoints for this ServiceUnit.  Basically sets
        // up the static configuration information for each Endpoint.
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00301.SU_Init_Called", new Object[] { getServiceUnitId() }));

        Endpoint currEndpoint = null;
        Collection<Endpoint> endpoints = null;

        try {
            // Prepare for endpoint configuration map if deploy() hasn't been called yet
            if (mEndpointConfig == null) {
                mEndpointConfig = initializeEndpointConfig();
            }
            endpoints = mWSDLConfig.createEndpoints(mEndpointConfig.endpoints(),
                    mRuntimeConfig.retrieveApplicationVariablesMap(),
                    mRuntimeConfig.retrieveApplicationConfigurationsMap());

            if (endpoints.size() == 0) {
				if (mLogger.isLoggable(Level.SEVERE))
					mLogger.log(Level.WARNING, mMessages.getString("IMSBC-W00318.SU_No_Matching_Wsdl", mId));
				AlertsUtil.getAlerter().critical(mMessages.getString("IMSBC-W00318.SU_No_Matching_Wsdl", mId), 
										IMSBindingComponent.SHORT_DISPLAY_NAME, 
										null, 
										AlertsUtil.getServerType(),
										AlertsUtil.COMPONENT_TYPE_BINDING,
										NotificationEvent.OPERATIONAL_STATE_RUNNING, 
										NotificationEvent.EVENT_TYPE_ALERT,
										"IMSBC-W00318");  
            }

        // Initialize the state of all these endpoints.
        for (Endpoint endpoint : endpoints) {
        	currEndpoint = endpoint;
			Map encoderMapping = mWSDLConfig.getPartEncoderMapping(
					currEndpoint);
			currEndpoint.setMessagePartEncoderMapping(encoderMapping);

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
            }
            currEndpoint.setEndpointStatus(reporting.getEndpointStatus(uniqueName));
        }
	    mEndpoints.clear();
        mEndpoints.addAll(endpoints);
        endpoints.clear();
        } catch (Exception ex) {
            String errMsg = ex.getLocalizedMessage();
            if (currEndpoint != null) {
				String exMsg =  mMessages.getString("IMSBC-E00319.SU_Init_Failed", new Object[] { currEndpoint.getServiceName(),
								currEndpoint.getEndpointName(), errMsg });
                mLogger.log(Level.SEVERE, exMsg);

				AlertsUtil.getAlerter().critical(exMsg, 
										IMSBindingComponent.SHORT_DISPLAY_NAME, 
										null, 
										AlertsUtil.getServerType(),
										AlertsUtil.COMPONENT_TYPE_BINDING,
										NotificationEvent.OPERATIONAL_STATE_RUNNING, 
										NotificationEvent.EVENT_TYPE_ALERT,
										"IMSBC-E00319"); 
            }

            throw new JBIException(errMsg);
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
    public void start() throws JBIException {
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00302.SU_Start_Called", new Object[] { getServiceUnitId() }));
        HashSet activatedEndpoints = new HashSet();

        Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint = null;
        try {
            while (it.hasNext()) {
                currEndpoint = (Endpoint) it.next();
                if (currEndpoint.getEndpointType() == EndpointType.INBOUND) {
                    // "Activate" consumer "endpoints"
                    activateEndpoint(currEndpoint);
                    activatedEndpoints.add(currEndpoint);

                } else {
    				activateEndpoint(currEndpoint);
    				activatedEndpoints.add(currEndpoint);
                    if (mLogger.isLoggable(Level.INFO)) {
                        mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00321.SU_Outbound_Endpoint_Start", 
									new Object[] {currEndpoint.getEndpointName()}));
                    }

                }
            }
        } catch (Exception ex) {
            String errMsg = mMessages.getString("IMSBC-E00303.SU_Endpoint_Activation_Error", new Object[] {
                        currEndpoint.getServiceName().toString(), currEndpoint.getEndpointName(), ex.getLocalizedMessage() });
            if (currEndpoint != null) {
				if (mLogger.isLoggable(Level.SEVERE))
					mLogger.log(Level.SEVERE, errMsg);
					AlertsUtil.getAlerter().critical(errMsg, 
										IMSBindingComponent.SHORT_DISPLAY_NAME, 
										null, 
										AlertsUtil.getServerType(),
										AlertsUtil.COMPONENT_TYPE_BINDING,
										NotificationEvent.OPERATIONAL_STATE_RUNNING, 
										NotificationEvent.EVENT_TYPE_ALERT,
										"IMSBC-E00303");
            }
			if (mLogger.isLoggable(Level.INFO))
				mLogger.log(Level.INFO, mMessages.getString("IMSBC-E00304.SU_Deactivate_Endpoints_Error"));
            Iterator it2 = activatedEndpoints.iterator();
            while (it2.hasNext()) {
                Endpoint endpoint = (Endpoint) it2.next();
                deactivateEndpoint(endpoint);
            }
            throw new JBIException(errMsg);
        }
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-E00305.SU_Start_Succeeded", new Object[] { getServiceUnitId() }));
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
    public void stop() throws JBIException {
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00306.SU_Stop_Called", new Object[] { getServiceUnitId() }));

        Iterator it = mEndpoints.iterator();
        while (it.hasNext()) {
            Endpoint endpoint = (Endpoint)it.next();
			deactivateEndpoint(endpoint);
        }
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00307.SU_Stop_Succeeded", new Object[] { getServiceUnitId() }));
    }

    /**
     * Shuts down this ServiceUnit.  Simply moves all Endpoints to the
     * shutdown state.
     *
     * @exception    JBIException if the ServiceUnit fails to shutdown
     */
    public void shutdown() throws JBIException {
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00308.SU_Shutdown_Called", new Object[] { getServiceUnitId() }));

        Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint;

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
            } else {
                mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
                reporting.removeProvisioningEndpoints(new String[] { uniqueName });
            }
            // Dispose the encoders
            Map encoders = currEndpoint.getMessagePartEncoderMapping();
            for (Iterator it2 = encoders.values().iterator(); it2.hasNext();) {
                Encoder aEncoder = (Encoder) it2.next();
                // Dispose the encoder. This also
                // triggers the removal of its codegen artifacts
                aEncoder.dispose();
            }
        }
		// Clear the encoder cache
        mWSDLConfig.clearEncoderCache();
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00309.SU_Shutdown_Succeeded", new Object[] { getServiceUnitId() }));
    }

    /**
     * Retrieves the set of Endpoints handled by this ServiceUnit.
     *
     * @return       the set of Endpoints
     */
   public Collection<Endpoint> getEndpoints() {
        return Collections.unmodifiableCollection(mEndpoints);
    }

    ////////
    //
    //  ServiceUnitImpl Private Methods
    //
    ////////

    /**
     * Activates an individual Endpoint.
     *
     * @param        endpoint the Endpoint to activate 
     * @exception    JBIException if unable to find the requested
     * Endpoint
     */
    private void activateEndpoint(Endpoint endpoint) throws JBIException {

        String direction = "Outbound";
        if (endpoint.getEndpointType() == EndpointType.OUTBOUND) {
            ServiceEndpoint endpointReference = mContext.activateEndpoint(endpoint.getServiceName(),
                    endpoint.getEndpointName());
            endpoint.setServiceEndpoint(endpointReference);
        } else {
            direction = "Inbound";
            try {
                mInboundProcessorMgr.addInboundMessageProcessors(endpoint);
            } catch (IMSException ex) {
                mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00310.SU_Inbound_Msg_Processors_Start_Failed", new Object[] {
                        endpoint.getServiceName(), endpoint.getEndpointName(), ex }));

                String errMsg = mMessages.getString("IMSBC-E00310.SU_Inbound_Msg_Processors_Start_Failed",
                        new Object[] { endpoint.getServiceName().toString(), endpoint.getEndpointName(), ex });
                throw new JBIException(errMsg);
            }
        }
        endpoint.setState(EndpointState.RUNNING);
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00312.SU_Endpoint_Activated", new Object[] { direction,
					endpoint.getServiceName().toString(), endpoint.getEndpointName() }));
    }

    private void deactivateEndpoint(Endpoint endpoint) throws JBIException {

        String direction = "Outbound";
        if (endpoint.getEndpointType() == EndpointType.OUTBOUND) {
            ServiceEndpoint endpointReference = endpoint.getServiceEndpoint();
            mContext.deactivateEndpoint(endpointReference);
        } else {
            direction = "Inbound";
            mInboundProcessorMgr.removeInboundMessageProcessors(endpoint);
        }
        endpoint.setState(EndpointState.STOPPED);
		if (mLogger.isLoggable(Level.INFO))
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00313.SU_Endpoint_Deactivated", new Object[] { direction,
					endpoint.getServiceName().toString(), endpoint.getEndpointName() }));
    }

    private String getEndpointKey(Endpoint endpoint) {
        return getEndpointKey(endpoint.getServiceName().toString(), endpoint.getEndpointName(),
                endpoint.getEndpointType());
    }

    private String getEndpointKey(String serviceName, String endpointName, int endpointType) {
        return mId + serviceName + endpointName + EndpointImpl.endpointTypeToString(endpointType);
    }

    private EndpointConfiguration initializeEndpointConfig() throws Exception {
        EndpointConfiguration endpointConfig = null;
        if (mEndpointConfig == null) {
            endpointConfig = EndpointConfigurationFactory.getEndpointConfiguration(mSuPath);
        }

        return endpointConfig;
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

        Endpoint epoint = null;
        for (Endpoint endpoint : mEndpoints) {
            if (endpoint.getEndpointType() == endpointType && 
				endpoint.getServiceName().toString() == serviceName && 
				endpoint.getEndpointName() == endpointName) {        
				    epoint = endpoint;
					break;
			}
		}
        return epoint;
    }

    protected void deactivateOutboundEndpoint(Endpoint endpoint) throws JBIException {
        if (endpoint.getEndpointType() != EndpointType.OUTBOUND) {
            return;
        }
        
        ServiceEndpoint endpointReference = mContext.getEndpoint(
                endpoint.getServiceName(),
                endpoint.getEndpointName());
        mContext.deactivateEndpoint(endpointReference);
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00320.SU_Deactivate_Outbound_Endpoint",
                new Object[] {endpoint.getServiceName(), endpoint.getEndpointName()}));
        }
        
    }

    private void start (Endpoint endpoint) throws Exception {

        String direction = endpoint.getEndpointType()== Endpoint.EndpointType.OUTBOUND?"provisioning":"consuming";
        
        endpoint.setState(EndpointState.RUNNING);

       	if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, 
                        "ServiceUnitImpl_ENDPOINT_STARTED",
                         new Object[]{direction,
                                     endpoint.getEndpointName(),
                                     endpoint.getServiceName().toString(),
                                     mId});
        }
    }

    private void stop(Endpoint endpoint) throws Exception {

        String direction = endpoint.getEndpointType()== Endpoint.EndpointType.OUTBOUND?"provisioning":"consuming";
               
        endpoint.setState(EndpointState.STOPPED);

       	if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, 
                        "ServiceUnitImpl_ENDPOINT_STOPPED",
                        new Object[]{direction,
                                     endpoint.getEndpointName(),
                                     endpoint.getServiceName().toString(),
                                     mId});
        }
    }

	synchronized public void resume(Endpoint ep) throws Exception {
		start(ep);
		String msg = mMessages.getString("IMSBC-W00322.SU_Suspended_Endpoint", new Object[] { ep.toString() });
		mLogger.log(Level.WARNING, msg);
		AlertsUtil.getAlerter().warning(msg, 
								IMSBindingComponent.SHORT_DISPLAY_NAME, 
								mId, 
								AlertsUtil.getServerType(),
								AlertsUtil.COMPONENT_TYPE_BINDING,
								NotificationEvent.OPERATIONAL_STATE_RUNNING, 
								NotificationEvent.EVENT_TYPE_ALERT,
								"IMSBC-W00321");
	}

	synchronized  public void suspend(Endpoint ep) throws Exception {
		stop(ep);
		String msg = mMessages.getString("IMSBC-W00323.SU_Resumed_Endpoint", new Object[] { ep.toString() });
		mLogger.log(Level.WARNING, msg);
		AlertsUtil.getAlerter().warning(msg, 
								IMSBindingComponent.SHORT_DISPLAY_NAME, 
								mId, 
								AlertsUtil.getServerType(),
								AlertsUtil.COMPONENT_TYPE_BINDING,
								NotificationEvent.OPERATIONAL_STATE_RUNNING, 
								NotificationEvent.EVENT_TYPE_ALERT,
								"IMSBC-W00322");
	}

}
