/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 *(the "License").  You may not use this file except
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

package com.sun.jbi.sapbc;

import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.sapbc.Endpoint.EndpointState;
import com.sun.jbi.sapbc.Endpoint.EndpointType;
import com.sun.jbi.sapbc.packaging.EndpointConfiguration;
import com.sun.jbi.sapbc.packaging.EndpointConfigurationFactory;
import com.sun.jbi.sapbc.packaging.WSDLConfigurations;
import java.util.ArrayList;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;


/**
 * ServiceUnitImpl represents a ServiceUnit deployed in a ServiceUnitManager.
 * <p>
 * The initialization of a ServiceUnit and its respective Endpoints depends on
 * two main configuration items: the endpoints.xml file and the WSDL(s)
 * representing the Service Unit.  Initialization of the ServiceUnit is done
 * through the Observer pattern when parsing each of the WSDL files.  This class
 * only cares about WSDLs containing the extensibility elements pertinent to
 * this Binding Component.
 * <p>
 * This is important since the actual representation of all the initialization
 * artifacts may change in the future.  In other words, endpoints.xml file may
 * file may , and the extensibility elements for each BC is different. However,
 * by encapsulating the initialization logic away from the model of
 * ServiceUnitManagers, ServiceUnits, and Endpoints, we can more easily change
 * model of ServiceUnitManagers, ServiceUnits, and Endpoints, we can more
 * how models are initialized without breaking too much.
 * <p>
 * If the artifacts change, only this class and the classes in the package
 * com.sun.jbi.sapbc.packaging should need to change.
 *
 */
class ServiceUnitImpl implements ServiceUnit {
    public ServiceUnitImpl(
            String id,
            String suPath,
            ComponentContext context,
            RuntimeConfigurationMBean runtimeConfig,
            StatusProviderHelper statusProviderHelper,
            InboundReceiver inboundReceiver) {
        mId = id;
        mSuPath = suPath;
        mContext = context;
        mParsedEndpoints = Collections.synchronizedList(new ArrayList());
        mRuntimeConfig = runtimeConfig;
        mStatusProviderHelper = statusProviderHelper;
        mInboundReceiver = inboundReceiver;
        mWSDLConfig = new WSDLConfigurations(suPath);
        mLogger = Messages.getLogger(ServiceUnitImpl.class);
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
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    public void deploy() throws JBIException {
        mLogger.log(Level.INFO, "ServiceUnit.Called_deploy", new Object[] {mSuPath, mId});
        try {
            if(mEndpointConfig == null)
                mEndpointConfig = initializeEndpointConfig();
            if(mWSDLConfig == null)
                mWSDLConfig = new WSDLConfigurations(mSuPath);
            
            //When the preprocess parameter is true, the code is only populating
            //the mRuntimeConfig parameter.  No parsed endpoints are actually
            //returned
            mWSDLConfig.parseEndpoints(mEndpointConfig.endpoints(),
                mRuntimeConfig.retrieveEnvVariablesMap(),
                true);

            Map envVariableMap = mWSDLConfig.getSAPEnvVars().getEnvVariableMap();
            if (envVariableMap.size() > mRuntimeConfig.retrieveEnvVariablesMap().size()) {
                // number of environment variable tokens used in SAP WSDLs
                // is greater than the ones defined
                mRuntimeConfig.updateEnvVariablesMap(envVariableMap);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = ex.getLocalizedMessage();
            mLogger.log(Level.SEVERE, "ServiceUnit.Failed_deploy", new Object[] {mId, errMsg});
            throw new JBIException(errMsg);
        }
    }
    
    /**
     * Initializes the ServiceUnit.  Parses the mSuPath
     * to create the Endpoint objects.  A ServiceUnit may have
     * mutiple WSDLs, each with multiple endpoints that we care about.
     * <p>
     * This method will initialize the WSDLs, transforming it into a
     * set of Endpoints
     *
     * TODO: What should happen if not all the Endpoints
     * can be activated?  Should I deactivate them or just leave
     * them?  For now, I'm going to assume that this method is
     * transactional.  Either all the Endpoints activate or none.
     * If any one fails to activate, the other activated Endpoints
     * will be deactivated.
     *
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    public void init() throws JBIException {
        // Initialize the endpoints for this ServiceUnit.  Basically sets
        // up the static configuration information for each Endpoint.
        mLogger.log(Level.INFO, "ServiceUnit.Called_init", new Object[] {mSuPath, mId});
        Endpoint currEndpoint = null;
        
        try {
            if (mEndpointConfig == null)
                mEndpointConfig = initializeEndpointConfig();
            if (mWSDLConfig == null)
                mWSDLConfig = new WSDLConfigurations(mSuPath);
            
            setEndpoints(mWSDLConfig.parseEndpoints(mEndpointConfig.endpoints(),
                    mRuntimeConfig.retrieveEnvVariablesMap(),
                    false));
                
            if (getEndpoints().size() == 0) {
                mLogger.log(Level.WARNING, "ServiceUnit.No_matching_WSDL", mId);
            }
            
            // Initialize the state of all these endpoints.
            for(Iterator it = getEndpoints().iterator(); it.hasNext(); ) {
                currEndpoint =(Endpoint)it.next();
                // Initializes encoder map for the current endpoint.
                // This also triggers the encoder codegen if the
                // specified encoder is not cached.
                // Map encoderMapping = mWSDLConfig.getPartEncoderMapping(currEndpoint.getDefinition(),
                //         currEndpoint.getServiceName().toString(),
                //         currEndpoint.getEndpointName(),
                //         currEndpoint.getEndpointType(),
                //         currEndpoint.getFileOperations(),
                //         currEndpoint.getOperationMsgExchangePattern());
                // currEndpoint.setMessagePartEncoderMapping(encoderMapping);
                
                // Store the state of the Endpoint
                currEndpoint.setState(EndpointState.SHUTDOWN);
                
                // Store the status...
                StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
                QName serviceName = currEndpoint.getServiceName();
                String portName = currEndpoint.getEndpointName();
                //debug mLogger.info("####debug ServiceUnitImpl.init service name ["+serviceName.toString()+"] portName ["+portName+"]");
                String uniqueName = null;
                
                if(currEndpoint.getEndpointType() == EndpointType.INBOUND) {
                    uniqueName = mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
                    reporting.addConsumingEndpoint(uniqueName);
                } else {
                    uniqueName =
                            mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
                    reporting.addProvisioningEndpoint(uniqueName);
                    activateOutboundEndpoint(currEndpoint);
                }
                currEndpoint.setEndpointStatus(reporting.getEndpointStatus(uniqueName));
            }
        } catch(Exception ex) {
            String errMsg = ex.getLocalizedMessage();
            if(currEndpoint != null) {
                mLogger.log(Level.SEVERE, "ServiceUnit.Failed_init",
                        new Object[] {currEndpoint.getServiceName(), currEndpoint.getEndpointName(), errMsg}) ;
            }
            
            Iterator it2 = getEndpoints().iterator();
            while(it2.hasNext()) {
                deactivateOutboundEndpoint((Endpoint)it2.next());
            }
            
            throw new JBIException(errMsg);
        }
        synchronized(this) {
            isInitialized = true;
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
     * @exception  JBIException upon error.
     * to activate
     */
    public void start() throws JBIException {
        mLogger.log(Level.INFO, "ServiceUnit.Called_start");
        
        HashSet activatedInboundEndpoints = new HashSet();
        
        Endpoint currEndpoint = null;
        try {
            for(Iterator it = getEndpoints().iterator(); it.hasNext(); ) {
                currEndpoint =(Endpoint) it.next();
                startConsuming(currEndpoint);
                activatedInboundEndpoints.add(currEndpoint);
            }
        } catch(Exception ex) {
            if(currEndpoint != null) {
                mLogger.log(
                        Level.SEVERE,
                        "ServiceUnit.Failed_start",
                        new Object[] {
                    currEndpoint.getServiceName(),
                    currEndpoint.getEndpointName(),
                    ex.getLocalizedMessage()
                });
            }
            for(Iterator it = activatedInboundEndpoints.iterator(); it.hasNext(); ) {
                stopConsuming((Endpoint) it.next());
            }
            throw new JBIException(ex.getMessage());
        }
        mLogger.log(Level.INFO, "ServiceUnit.Started", getServiceUnitId());
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
        mLogger.log(Level.INFO, "ServiceUnit.Called_stop");
        for(Iterator it = getEndpoints().iterator(); it.hasNext(); ) {
            stopConsuming((Endpoint) it.next());
        }
        mLogger.log(Level.INFO, "ServiceUnit.Stopped", getServiceUnitId());
    }
    
    /**
     * Shuts down this ServiceUnit.  Simply moves all Endpoints to the
     * shutdown state.
     *
     * @exception    JBIException if the ServiceUnit fails to shutdown
     */
    public void shutdown() throws JBIException {
        mLogger.log(Level.INFO, "ServiceUnit.Called_shutdown");
        
        Endpoint currEndpoint;
        
        for(Iterator it = getEndpoints().iterator(); it.hasNext(); ) {
            currEndpoint =(Endpoint)it.next();
            
            //Set the state of the endpoint
            currEndpoint.setState(EndpointState.SHUTDOWN);
            
            // Set the status of the endpoint
            StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
            QName serviceName = currEndpoint.getServiceName();
            String portName = currEndpoint.getEndpointName();
            String uniqueName = null;
            if(currEndpoint.getEndpointType() == EndpointType.INBOUND) {
                uniqueName = mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
                reporting.removeConsumingEndpoints(new String[] {uniqueName});
            } else {
                uniqueName = mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
                reporting.removeProvisioningEndpoints(new String[] {uniqueName});
                deactivateOutboundEndpoint(currEndpoint);
            }
        }
        synchronized(this) {
            isShutdown = true;
        }
        
        mLogger.log(Level.INFO, "ServiceUnit.Shutdown", getServiceUnitId());
    }
    
    public synchronized boolean isInitialized() {
        return isInitialized;
    }
    
    public synchronized boolean isShutdown() {
        return isShutdown;
    }
    
    /**
     * Retrieves the set of Endpoints handled by this ServiceUnit.
     *
     * @return       the set of Endpoints
     */
    public synchronized List getEndpoints() {
        return Collections.unmodifiableList(mParsedEndpoints);
    }
   
    void setEndpoints(List endpoints) {
        mParsedEndpoints = endpoints;
    }
    
    ////////
    //
    //  ServiceUnitImpl Protected Methods
    //
    ////////
    
    protected void startConsuming(Endpoint endpoint) throws JBIException {
        if(endpoint.getEndpointType() != EndpointType.INBOUND) {
            return;
        }
        
        try {
            mInboundReceiver.addInboundMessageProcessor(endpoint);
        } catch(FaultException ex) {
            mLogger.log(Level.SEVERE, "ServiceUnit.Failed_start_inbound_EP", endpoint.getEndpointName());
            throw new JBIException(ex.getLocalizedMessage(), ex);
        }
        /* JK Implement inbound receiver
        catch(IBProcCreationException ex)
        {
            mLogger.log(Level.SEVERE, "ServiceUnit.Failed_start_inbound_EP", endpoint.getEndpointName());
            throw new JBIException(ex.getLocalizedMessage(), ex);
        }
         */
    }
    
    protected void activateOutboundEndpoint(Endpoint endpoint) throws JBIException {
        if(mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "ServiceUnit.Activate_outbound",
                    new Object[] {endpoint.getServiceName(), endpoint.getEndpointName(), endpoint.getEndpointType()});
        }
        if(endpoint.getEndpointType() != EndpointType.OUTBOUND) {
            return;
        }
        ServiceEndpoint endpointReference =
                mContext.activateEndpoint(endpoint.getServiceName(),
                endpoint.getEndpointName());
        endpoint.setState(EndpointState.RUNNING);
        endpoint.setServiceEndpoint(endpointReference);
        if(mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "ServiceUnit.Activated_outbound",
                    new Object[] {endpoint.getServiceName(), endpoint.getEndpointName()});
        }
        
    }
    
    protected void stopConsuming(Endpoint endpoint) throws JBIException {
        if(endpoint.getEndpointType() != EndpointType.INBOUND) {
            return;
        }
        mInboundReceiver.removeInboundMessageProcessor(endpoint);
    }
    
    protected void deactivateOutboundEndpoint(Endpoint endpoint) throws JBIException {
        if(endpoint.getEndpointType() != EndpointType.OUTBOUND) {
            return;
        }
        
        ServiceEndpoint endpointReference = endpoint.getServiceEndpoint();
        endpoint.setState(EndpointState.STOPPED);
        mContext.deactivateEndpoint(endpointReference);
        
        if(mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "ServiceUnit.Deactivated_outbound",
                    new Object[] {endpoint.getServiceName(), endpoint.getEndpointName()});
        }
        
    }
    
    private EndpointConfiguration initializeEndpointConfig() throws Exception {
        return EndpointConfigurationFactory.getEndpointConfiguration(mSuPath);
    }
    
    private static Messages mMessages =
            Messages.getMessages(ServiceUnitImpl.class);
    
    private Logger mLogger;
    
    private String mId;
    private String mSuPath;
    private ComponentContext mContext;
    private boolean isInitialized = false;
    private boolean isShutdown;
    private StatusProviderHelper mStatusProviderHelper;
    private InboundReceiver mInboundReceiver;
    private RuntimeConfigurationMBean mRuntimeConfig;
    private List<Endpoint> mParsedEndpoints;
    private EndpointConfiguration mEndpointConfig = null;
    private WSDLConfigurations mWSDLConfig = null;
}
