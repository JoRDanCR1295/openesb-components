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

package com.sun.jbi.swiftbc;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.encoder.Encoder;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.swiftbc.Endpoint.EndpointState;
import com.sun.jbi.swiftbc.Endpoint.EndpointType;
import com.sun.jbi.swiftbc.packaging.EndpointConfiguration;
import com.sun.jbi.swiftbc.packaging.EndpointConfigurationFactory;
import com.sun.jbi.swiftbc.packaging.WSDLConfigurations;
import com.sun.jbi.internationalization.Messages;

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
 * @author S. Nageswara Rao
 */
public class ServiceUnitImpl implements ServiceUnit {
    private static Logger mLogger = Messages.getLogger(ServiceUnitImpl.class);

    private String mId;

    private String mSuPath;

    private ComponentContext mContext;

    private StatusProviderHelper mStatusProviderHelper;

    private RuntimeConfigurationMBean mRuntimeConfiguration;

    private InboundReceiver mInboundReceiver;

    private WSDLConfigurations mWSDLConfig;

	private EndpointConfiguration mEndpointConfig;

    private Collection mEndpoints;

    // private Collection mEndpointChangeListeners;

    public ServiceUnitImpl(String id, String suPath, ComponentContext context, RuntimeConfigurationMBean runtimeConfig,
            StatusProviderHelper statusProviderHelper, InboundReceiver inboundReceiver) {
        mId = id;
        mSuPath = suPath;
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mRuntimeConfiguration = runtimeConfig;
        mInboundReceiver = inboundReceiver;
        mEndpoints = new ArrayList();
        mWSDLConfig = new WSDLConfigurations(suPath);
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

	 /**
     * Deploy the ServiceUnit. 
     *
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    public void deploy() throws JBIException {
		 mLogger.log(Level.INFO, "SU_Called_SU_deploy");
		 try {
				mEndpointConfig = initializeEndpointConfig();
				Map envVariableMap = mWSDLConfig.parseForEnvironmentVariables(mEndpointConfig.endpoints(), mRuntimeConfiguration.retrieveEnvVariablesMap());
				if (envVariableMap.size() > mRuntimeConfiguration.retrieveEnvVariablesMap().size()) {
					// number of environment variable tokens used in File WSDLs 
					// is greater than the ones defined 
                    mRuntimeConfiguration.updateEnvVariablesMap(envVariableMap);
				}
		} catch (Exception ex) {
			ex.printStackTrace();
			String errMsg = ex.getLocalizedMessage();
			mLogger.log(Level.SEVERE, "SU_Failed_deploy_SU", new Object[] {mId, errMsg});
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
        mLogger.log(Level.INFO, "SU_Called_SU_init");
		Endpoint currEndpoint = null;
        HashSet activatedOutboundEndpoints = new HashSet();

		  try {
            // Prepare for endpoint configuration map if deploy() hasn't been called yet
            if (mEndpointConfig == null) {
                mEndpointConfig = initializeEndpointConfig();
            }
            mEndpoints = mWSDLConfig.parse(mEndpointConfig.endpoints(), mRuntimeConfiguration.retrieveEnvVariablesMap());
            if (mEndpoints.size() == 0) {
                mLogger.log(Level.WARNING, "SU_No_matching_WSDL", mId);
            }
            // Initialize the state of all these endpoints.
            Iterator it = mEndpoints.iterator();
            while (it.hasNext()) {
                currEndpoint = (Endpoint)it.next();
                // Initializes encoder map for the current endpoint.
                // This also triggers the encoder codegen if the
                // specified encoder is not cached.
                Map encoderMapping = mWSDLConfig.getPartEncoderMapping(currEndpoint.getDefinition(),
                                                                       currEndpoint.getServiceName().toString(),
                                                                       currEndpoint.getEndpointName(),
                                                                       currEndpoint.getEndpointType(),
                                                                       currEndpoint.getSwiftOperations(),
                                                                       currEndpoint.getOperationMsgExchangePattern());
                currEndpoint.setMessagePartEncoderMapping(encoderMapping);
                
                // Store the state of the Endpoint
                currEndpoint.setState(EndpointState.STOPPED);

                // Store the status...
                StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
                QName serviceName = currEndpoint.getServiceName();
                String portName = currEndpoint.getEndpointName();
                String uniqueName = null;
                if (currEndpoint.getEndpointType() == EndpointType.INBOUND) {
                    uniqueName = 
                        mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
                    reporting.addConsumingEndpoint(uniqueName);
                } else {
                    uniqueName = 
                        mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
                    reporting.addProvisioningEndpoint(uniqueName);
                    activateOutboundEndpoint(currEndpoint);
                    activatedOutboundEndpoints.add(currEndpoint);
                }
                currEndpoint.setEndpointStatus(reporting.getEndpointStatus(uniqueName));
            }
        } catch (Exception ex) {
            String errMsg = ex.getLocalizedMessage();
            if (currEndpoint != null) {
                mLogger.log(Level.SEVERE, "SU_Failed_init_SU", 
                    new Object[] {currEndpoint.getServiceName(), currEndpoint.getEndpointName(), errMsg}) ;
            }
            
            Iterator it2 = activatedOutboundEndpoints.iterator();
            while (it2.hasNext()) {
                deactivateOutboundEndpoint((Endpoint)it2.next());
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
        mLogger.log(Level.INFO, "SU_Called_SU_start");

        HashSet activatedInboundEndpoints = new HashSet();

        Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint = null;
        try {
            while (it.hasNext()) {
                currEndpoint = (Endpoint) it.next();
                startConsuming(currEndpoint);
                activatedInboundEndpoints.add(currEndpoint);
            }
        } catch (Exception ex) {
            String errMsg = ex.getLocalizedMessage();
            if (currEndpoint != null) {
                mLogger.log(Level.SEVERE, "SU_Failed_start_SU", new Object[] { currEndpoint.getServiceName(),
                        currEndpoint.getEndpointName(), errMsg });
            }
            Iterator it2 = activatedInboundEndpoints.iterator();
            while (it2.hasNext()) {
                stopConsuming((Endpoint) it2.next());
            }
            throw new JBIException(errMsg);
        }
        mLogger.log(Level.INFO, "SU_Complete_start_SU", getServiceUnitId());

    }

    /**
     * Stops this ServiceUnit. This involves deactivating all Endpoints that are part of this
     * ServiceUnit. All Endpoints are moved into the EndpointState.STOPPED state.
     * 
     * @exception JBIException if any Endpoint fails to deactivate
     */
    public void stop() throws JBIException {
        mLogger.log(Level.INFO, "SU_Call_stop");

		Endpoint endpoint = null;
        Iterator it = mEndpoints.iterator();

        while (it.hasNext()) {
            endpoint = (Endpoint) it.next();
            stopConsuming(endpoint);
        mLogger.log(Level.INFO, "SU_Complete_SU", getServiceUnitId());
    }
    }

    /**
     * Shuts down this ServiceUnit. Simply moves all Endpoints to the shutdown state.
     * 
     * @exception JBIException if the ServiceUnit fails to shutdown
     */
    public void shutdown() throws JBIException {

        mLogger.log(Level.INFO, "SU_Call_shutdown");

        Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint;

        while (it.hasNext()) {
            currEndpoint = (Endpoint) it.next();

            // Set the state of the endpoint
            currEndpoint.setState(EndpointState.SHUTDOWN);

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
				deactivateOutboundEndpoint(currEndpoint);
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
        }
        // Clear the encoder cache
        mWSDLConfig.clearEncoderCache();
        mLogger.log(Level.INFO, "SU_Complete_shutdown", getServiceUnitId());
    }

    /**
     * Retrieves the set of Endpoints handled by this ServiceUnit.
     * 
     * @return the set of Endpoints
     */
    public Collection getEndpoints() {
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
        ServiceEndpoint endpointReference = 
            mContext.activateEndpoint(endpoint.getServiceName(),
                                      endpoint.getEndpointName());
        endpoint.setServiceEndpoint(endpointReference);
        endpoint.setState(EndpointState.RUNNING);
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "SU_Activate_outbound_EP", 
                new Object[] {endpoint.getServiceName(), endpoint.getEndpointName()});
        }
        
    }

	 protected void deactivateOutboundEndpoint(Endpoint endpoint) throws JBIException {
        if (endpoint.getEndpointType() != EndpointType.OUTBOUND) {
            return;
        }
        
        ServiceEndpoint endpointReference = endpoint.getServiceEndpoint();
        mContext.deactivateEndpoint(endpointReference);
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "SU_Deactivate_outbound_EP",
                new Object[] {endpoint.getServiceName(), endpoint.getEndpointName()});
        }
		endpoint.setState(EndpointState.STOPPED);
        
    }

	 protected void startConsuming(Endpoint endpoint) throws JBIException {
    	if (endpoint.getEndpointType() != EndpointType.INBOUND) {
    	    return;
    	}
    	
        try {
            mInboundReceiver.addInboundMessageProcessor(endpoint);
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "SU_Failed_start_inbound_EP", endpoint.getEndpointName());
            throw new JBIException (ex.getLocalizedMessage(), ex);
        }
        endpoint.setState(EndpointState.RUNNING);
    }

	 protected void stopConsuming(Endpoint endpoint) throws JBIException {
        if (endpoint.getEndpointType() != EndpointType.INBOUND) {
            return;
        }
        try {
            mInboundReceiver.removeInboundMessageProcessor(endpoint);
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "SU_Failed_stop_inbound_EP", endpoint.getEndpointName());
            throw new JBIException (ex.getLocalizedMessage(), ex);
        }
        endpoint.setState(EndpointState.STOPPED);
        
    }

	private EndpointConfiguration initializeEndpointConfig() throws Exception {
    	EndpointConfiguration endpointConfig = null;
        if (mEndpointConfig == null) {
                endpointConfig =
                    EndpointConfigurationFactory.getEndpointConfiguration(mSuPath);
        }
        
        return endpointConfig;
    }

    /**
     * package protected method. Used solely for JUnit test purposes
     */
    void setEndpoints(List endpoints) {
        mEndpoints = endpoints;
    }
}
