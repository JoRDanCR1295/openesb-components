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

package com.sun.jbi.execbc;

import com.sun.encoder.Encoder;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.execbc.packaging.EndpointConfiguration;
import com.sun.jbi.execbc.packaging.EndpointConfigurationFactory;
import com.sun.jbi.execbc.util.OutputFilenameFormatter;
import com.sun.jbi.execbc.packaging.WSDLConfigurations;
import com.sun.jbi.execbc.Endpoint.EndpointType;
import com.sun.jbi.internationalization.Messages;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;


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
 * com.sun.jbi.execbc.packaging package should need to change.
 *
 */
public class ServiceUnitImpl implements ServiceUnit {
    private static Logger mLogger = Messages.getLogger(ServiceUnitImpl.class);
    
    private String mId;
    private String mSuPath;
    private ComponentContext mContext;
    private StatusProviderHelper mStatusProviderHelper;
    private RuntimeConfigurationMBean mRuntimeConfig;
    private InboundReceiver mInboundReceiver;
    private Collection mEndpoints;
    private WSDLConfigurations mWSDLConfig;
    private EndpointConfiguration mEndpointConfig;
    
    public ServiceUnitImpl(String id,
            String suPath,
            ComponentContext context,
            RuntimeConfigurationMBean runtimeConfig,
            StatusProviderHelper statusProviderHelper,
            InboundReceiver inboundReceiver) {
        mId = id;
        mSuPath = suPath;
        mContext = context;
        mRuntimeConfig = runtimeConfig;
        mStatusProviderHelper = statusProviderHelper;
        mInboundReceiver = inboundReceiver;
        mEndpoints = new ArrayList();
        mWSDLConfig = new WSDLConfigurations(suPath);
    }
    
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
     * @param        serviceUnitRootPath path to the ServiceUnit
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    public void deploy() throws JBIException {
        mLogger.log(Level.INFO, "SU_Called_SU_deploy");
        try {
            mEndpointConfig = initializeEndpointConfig();
            Map envVariableMap = mWSDLConfig.parseForEnvironmentVariables(mEndpointConfig.endpoints(), mRuntimeConfig.retrieveEnvVariablesMap());
            if (envVariableMap.size() > mRuntimeConfig.retrieveEnvVariablesMap().size()) {
                // number of environment variable tokens used in File WSDLs
                // is greater than the ones defined
                mRuntimeConfig.updateEnvVariablesMap(envVariableMap);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = ex.getLocalizedMessage();
            mLogger.log(Level.SEVERE, "SU_Failed_deploy_SU", new Object[] {mId, errMsg});
            throw new JBIException(errMsg);
        }
    }
    
    /**
     * Initializes the ServiceUnit.  Parses the serviceUnitRootPath
     * to create the Endpoint objects.  A ServiceUnit may have
     * mutiple WSDLs, each potentially with multiple endpoints.
     * <p>
     *
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    public void init() throws JBIException {
        // Initialize the endpoints for this ServiceUnit.  Basically sets
        // up the static configuration information for each Endpoint.
        mLogger.log(Level.INFO, "SU_Called_SU_init");
        Endpoint currEndpoint = null;
        HashSet activatedOutboundEndpoints = new HashSet();
        
        try {
            // Prepare for endpoint configuration map if deploy() hasn't been called yet
            if (mEndpointConfig == null) {
                mEndpointConfig = initializeEndpointConfig();
            }
            mEndpoints = mWSDLConfig.parse(mEndpointConfig.endpoints(), mRuntimeConfig.retrieveEnvVariablesMap());
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
                        currEndpoint.getExecOperations(),
                        currEndpoint.getOperationMsgExchangePattern());
                currEndpoint.setMessagePartEncoderMapping(encoderMapping);
                
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
     * Starts this ServiceUnit.  This involves activating
     * all Endpoints that are part of this ServiceUnit.  All Endpoints
     * are moved into the EndpointState.RUNNING state.
     * <p>
     *
     * @exception  JBIException upon error.
     * to activate
     */
    public void start() throws JBIException {
        mLogger.log(Level.INFO, "SU_Called_SU_start");
        HashSet activatedInboundEndpoints = new HashSet();
        
        Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint = null;
        
        try {
            while (it.hasNext()) {
                currEndpoint = (Endpoint)it.next();
                startConsuming(currEndpoint);
                activatedInboundEndpoints.add(currEndpoint);
            }
        } catch (Exception ex) {
            String errMsg = ex.getLocalizedMessage();
            if (currEndpoint != null) {
                mLogger.log(Level.SEVERE, "SU_Failed_start_SU",
                        new Object[] {currEndpoint.getServiceName(), currEndpoint.getEndpointName(), errMsg}) ;
            }
            Iterator it2 = activatedInboundEndpoints.iterator();
            while (it2.hasNext()) {
                stopConsuming((Endpoint)it2.next());
            }
            throw new JBIException(errMsg);
        }
        mLogger.log(Level.INFO, "SU_Complete_start_SU", getServiceUnitId());
    }
    
    /**
     * Stops this ServiceUnit.  This involves deactivating
     * all Endpoints that are part of this ServiceUnit.  All
     * Endpoints are moved into the EndpointState.STOPPED state.
     * <p>
     *
     * @exception    JBIException if any Endpoint fails
     * to deactivate
     */
    public void stop() throws JBIException {
        mLogger.log(Level.INFO, "SU_Call_stop");
        Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint;
        
        while (it.hasNext()) {
            currEndpoint = (Endpoint)it.next();
            stopConsuming(currEndpoint);
        }
        mLogger.log(Level.INFO, "SU_Complete_SU", getServiceUnitId());
    }
    
    /**
     * Shuts down this ServiceUnit.  Simply moves all Endpoints to the
     * shutdown state.
     *
     * @exception    JBIException if the ServiceUnit fails to shutdown
     */
    public void shutdown() throws JBIException {
        mLogger.log(Level.INFO, "SU_Call_shutdown");
        
        Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint;
        
        while (it.hasNext()) {
            currEndpoint = (Endpoint)it.next();
            // Set the state of the endpoint
            //currEndpoint.setState(EndpointState.SHUTDOWN);
            
            // Set the status of the endpoint
            StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
            QName serviceName = currEndpoint.getServiceName();
            String portName = currEndpoint.getEndpointName();
            String uniqueName = null;
            if (currEndpoint.getEndpointType() == EndpointType.INBOUND) {
                uniqueName =
                        mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
                reporting.removeConsumingEndpoints(new String[] {uniqueName});
            } else {
                uniqueName =
                        mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
                reporting.removeProvisioningEndpoints(new String[] {uniqueName});
                deactivateOutboundEndpoint(currEndpoint);
            }
            
            // Dispose the encoders
            Map encoders = currEndpoint.getMessagePartEncoderMapping();
            for (Iterator it2 = encoders.values().iterator(); it2.hasNext();) {
                Encoder aEncoder = (Encoder)it2.next();
                // Dispose the encoder. This also
                // triggers the removal of its codegen artifacts
                aEncoder.dispose();
            }
            
            // Dispose cached maps for file name patterns
            clearCachedMaps(currEndpoint);
        }
        
        // Clear the encoder cache
        mWSDLConfig.clearEncoderCache();
        mLogger.log(Level.INFO, "SU_Complete_shutdown", getServiceUnitId());
    }
    
    /**
     * Retrieves the set of Endpoints handled by this ServiceUnit.
     *
     * @return       the set of Endpoints
     */
    public Collection getEndpoints() {
        return Collections.unmodifiableCollection(mEndpoints);
    }
    
    ////////
    //
    //  ServiceUnitImpl Protected Methods
    //
    ////////
    
    protected void startConsuming(Endpoint endpoint) throws JBIException {
        if (endpoint.getEndpointType() != EndpointType.INBOUND) {
            return;
        }
        // for clustering support - more than one inbound polling an endpoint
        // this could also happen if we change the inbound processing threading model
        // e.g. to multiple inbound threads polling the same target directory
        // - for throttling up inbound processing volume
        // need to register the endpoint and corresponding locks in LockRegistry - a per process registry
        // 
        
        try {
            mInboundReceiver.addInboundMessageProcessor(endpoint);
        } catch (FaultException ex) {
            mLogger.log(Level.SEVERE, "SU_Failed_start_inbound_EP", endpoint.getEndpointName());
            throw new JBIException(ex.getLocalizedMessage(), ex);
        } catch (IBProcCreationException ex) {
            mLogger.log(Level.SEVERE, "SU_Failed_start_inbound_EP", endpoint.getEndpointName());
            throw new JBIException(ex.getLocalizedMessage(), ex);
        }
    }
    
    protected void activateOutboundEndpoint(Endpoint endpoint) throws JBIException {
        if (endpoint.getEndpointType() != EndpointType.OUTBOUND) {
            return;
        }
        ServiceEndpoint endpointReference =
                mContext.activateEndpoint(endpoint.getServiceName(),
                endpoint.getEndpointName());
        endpoint.setServiceEndpoint(endpointReference);
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "SU_Activate_outbound_EP",
                    new Object[] {endpoint.getServiceName(), endpoint.getEndpointName()});
        }
    }
    
    protected void stopConsuming(Endpoint endpoint) throws JBIException {
        if (endpoint.getEndpointType() != EndpointType.INBOUND) {
            return;
        }
        mInboundReceiver.removeInboundMessageProcessor(endpoint);
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
    }
    
    private void clearCachedMaps(Endpoint endpoint) {
        // clear resource maps for outbound endpoints if any;
        OutputFilenameFormatter.removeOutputFileNamePattern(
                endpoint.getServiceName().toString() + endpoint.getEndpointName());
    }
    
    private EndpointConfiguration initializeEndpointConfig() throws Exception {
        EndpointConfiguration endpointConfig = null;
        if (mEndpointConfig == null) {
            endpointConfig =
                    EndpointConfigurationFactory.getEndpointConfiguration(mSuPath);
        }
        
        return endpointConfig;
    }
    
    /** package protected method.
     * Used solely for JUnit test purposes
     */
    void setEndpoints(List endpoints) {
        mEndpoints = endpoints;
    }
}
