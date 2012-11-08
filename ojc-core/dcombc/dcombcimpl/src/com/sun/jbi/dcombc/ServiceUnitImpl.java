/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/
package com.sun.jbi.dcombc;

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

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

//import com.sun.encoder.Encoder;

import com.sun.jbi.internationalization.Messages;

import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;

import com.sun.jbi.dcombc.mbeans.RuntimeConfigurationMBean;
import com.sun.jbi.dcombc.dcom.Channel;
import com.sun.jbi.dcombc.dcom.ChannelManager;
import com.sun.jbi.dcombc.Endpoint.EndpointState;
import com.sun.jbi.dcombc.Endpoint.EndpointType;
import com.sun.jbi.dcombc.packaging.EndpointConfiguration;
import com.sun.jbi.dcombc.packaging.EndpointConfigurationFactory;
import com.sun.jbi.dcombc.packaging.InvalidConfigurationException;
import com.sun.jbi.dcombc.packaging.WSDLConfigurations;
import com.sun.jbi.dcombc.extensions.DCOMOperation;

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
 * com.sun.jbi.dcombc.packaging package should need to change.
 *
 * @author Chandrakanth Belde
 */
public class ServiceUnitImpl implements ServiceUnit {
	/**
	 *
	 */
    private static final Messages mMessages = Messages.getMessages(ServiceUnitImpl.class);

    private static final Logger mLogger = Messages.getLogger(ServiceUnitImpl.class);

    private int PRODUCTION_ENDPOINT_CONFIG_TYPE = EndpointConfigurationFactory.END_POINT_CONFIG_TYPE_PORTMAP;

    private String mId;

    private ComponentContext mContext;

    private StatusProviderHelper mStatusProviderHelper;

    private Map mEndpoints;
    private String mSuPath;

    private ChannelManager mDCOMChannelMgr;

    private InboundReceiver mInboundProcessorMgr;

    private WSDLConfigurations mWSDLConfig;

    private RuntimeConfigurationMBean mRuntimeConfig;
    private EndpointConfiguration mEndpointConfig;
    public ServiceUnitImpl(String suId, String suPath, ComponentContext context,
		RuntimeConfigurationMBean runtimeConfig,
            StatusProviderHelper statusProviderHelper, ChannelManager dcomChannelMgr,
            InboundReceiver inboundProcessorMgr) {
        mId = suId;
        mSuPath = suPath;
        mContext = context;
		mRuntimeConfig = runtimeConfig;
        mStatusProviderHelper = statusProviderHelper;
        mWSDLConfig = new WSDLConfigurations(suPath);
        mEndpoints = Collections.synchronizedMap(new HashMap());
        mDCOMChannelMgr = dcomChannelMgr;
        mInboundProcessorMgr = inboundProcessorMgr;

        if (EndpointConfigurationFactory.isJBIRoutingEnabled()) {
            PRODUCTION_ENDPOINT_CONFIG_TYPE = EndpointConfigurationFactory.END_POINT_CONFIG_TYPE_SU_DESCRIPTOR;
        }
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
    public void init(String serviceUnitRootPath) throws JBIException {
        // Initialize the endpoints for this ServiceUnit.  Basically sets
        // up the static configuration information for each Endpoint.
        mLogger.log(Level.INFO, "ServiceUnitImpl.INIT_CALLED", new Object[] { getServiceUnitId() });

        try {
            EndpointConfiguration endpointConfig = EndpointConfigurationFactory.getEndpointConfiguration(
                    PRODUCTION_ENDPOINT_CONFIG_TYPE, serviceUnitRootPath);
            Collection endpoints = mWSDLConfig.parse(endpointConfig.endpoints(), mRuntimeConfig.retrieveEnvVariablesMap());

            Iterator it = endpoints.iterator();
            while (it.hasNext()) {
                Endpoint endpoint = (Endpoint) it.next();
                mEndpoints.put(getEndpointKey(endpoint), endpoint);
            }
        } catch (Exception ex) {
            throw new JBIException(ex);
        }

        // Initialize the state of all these endpoints.
        Iterator it = mEndpoints.values().iterator();
        while (it.hasNext()) {
            Endpoint endpoint = (Endpoint) it.next();

            // Store the state of the Endpoint
            endpoint.setState(EndpointState.SHUTDOWN);

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
        mLogger.log(Level.INFO, "ServiceUnitImpl.START_CALLED", new Object[] { getServiceUnitId() });
        HashSet activatedEndpoints = new HashSet();

        Iterator it = mEndpoints.values().iterator();
        Endpoint currEndpoint = null;
        try {
            while (it.hasNext()) {
                currEndpoint = (Endpoint) it.next();

                // Initializes encoder map for the current endpoint.
                // This also triggers the encoder codegen if the
                // specified encoder is not cached.
                // Map encoderMapping = mWSDLConfig.getPartEncoderMapping(currEndpoint);
                // currEndpoint.setMessagePartEncoderMapping(encoderMapping);

                if (currEndpoint.getEndpointType() == EndpointType.OUTBOUND) {
                    // For outbound (we are the provider), 
                    // the SE endpoint has to be activated first
                    // Activate the provider endpoints
                    activateEndpoint(currEndpoint);

                    activatedEndpoints.add(currEndpoint);

                    // Add DCOM producer "channels"
                    addChannels(currEndpoint);
                } else {
                    // For inbound (we are the consumer), 
                    // the DCOM "channel" has to be established first                    
                    // Add DCOM consumer "channels"
                    addChannels(currEndpoint);

                    // "Activate" consumer "endpoints"
                    activateEndpoint(currEndpoint);

                    activatedEndpoints.add(currEndpoint);
                }
            }
        } catch (Exception ex) {
            String errMsg = ex.getLocalizedMessage();
            if (currEndpoint != null) {
                mLogger.log(Level.SEVERE, "ServiceUnitImpl.ENDPOINT_ACTIVATION_ERROR", new Object[] {
                        currEndpoint.getServiceName().toString(), currEndpoint.getEndpointName(), ex });

                errMsg = mMessages.getString("ServiceUnitImpl.ENDPOINT_ACTIVATION_ERROR", new Object[] {
                        currEndpoint.getServiceName().toString(), currEndpoint.getEndpointName(), ex });
            }
            mLogger.log(Level.INFO, "ServiceUnitImpl.DEACTIVATE_ENDPOINTS_DUE_TO_ERROR");
            Iterator it2 = activatedEndpoints.iterator();
            while (it2.hasNext()) {
                Endpoint endpoint = (Endpoint) it2.next();
                deactivateEndpoint(endpoint);
                try {
                    removeChannels(endpoint);
                } catch (Exception ex2) {
                    mLogger.log(Level.WARNING, "ServiceUnitImpl.DCOM_CHANNELS_REMOVE_ERROR", new Object[] {
                            getServiceUnitId(), endpoint.getServiceName().toString(), endpoint.getEndpointName(), ex2 });
                }
            }
            throw new JBIException(errMsg);
        }

        mLogger.log(Level.INFO, "ServiceUnitImpl.START_SUCCEEDED", new Object[] { getServiceUnitId() });
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
        mLogger.log(Level.INFO, "ServiceUnitImpl.STOP_CALLED", new Object[] { getServiceUnitId() });

        Iterator it = mEndpoints.values().iterator();
        while (it.hasNext()) {
            Endpoint endpoint = (Endpoint) it.next();
            deactivateEndpoint(endpoint);
            try {
                removeChannels(endpoint);
            } catch (DCOMException ex) {
                mLogger.log(Level.WARNING, "ServiceUnitImpl.DCOM_CHANNELS_REMOVE_ERROR", new Object[] {
                        getServiceUnitId(), endpoint.getServiceName().toString(), endpoint.getEndpointName(), ex });
            }
        }

        mLogger.log(Level.INFO, "ServiceUnitImpl.STOP_SUCCEEDED", new Object[] { getServiceUnitId() });
    }

    /**
     * Shuts down this ServiceUnit.  Simply moves all Endpoints to the
     * shutdown state.
     *
     * @exception    JBIException if the ServiceUnit fails to shutdown
     */
    public void shutdown() throws JBIException {
        mLogger.log(Level.INFO, "ServiceUnitImpl.SHUTDOWN_CALLED", new Object[] { getServiceUnitId() });

        Iterator it = mEndpoints.values().iterator();
        Endpoint currEndpoint;

        while (it.hasNext()) {
            currEndpoint = (Endpoint) it.next();

            // Set the state of the endpoint
            currEndpoint.setState(EndpointState.SHUTDOWN);

            // Dispose the encoders
            //Map encoders = currEndpoint.getMessagePartEncoderMapping();
            /*for (Iterator it2 = encoders.values().iterator(); it2.hasNext();) {
                Encoder aEncoder = (Encoder) it2.next();
                // Dispose the encoder. This also
                // triggers the removal of its codegen artifacts
                aEncoder.dispose();
            } */
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
        }

        mLogger.log(Level.INFO, "ServiceUnitImpl.SHUTDOWN_SUCCEEDED", new Object[] { getServiceUnitId() });
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
    public Endpoint getEndpoint(String serviceName, String endpointName, int endpointType) {

        Endpoint endpoint = null;

        endpoint = (Endpoint) mEndpoints.get(getEndpointKey(serviceName, endpointName, endpointType));
        return endpoint;
    }

    /**
     * Retrieves the set of Endpoints handled by this ServiceUnit.
     *
     * @return       the set of Endpoints
     */
    public Collection getEndpoints() {
        return Collections.unmodifiableCollection(mEndpoints.values());
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
            } catch (DCOMException ex) {
                mLogger.log(Level.SEVERE, "ServiceUnitImpl.INBOUND_MSG_PROCESSORS_START_FAILED", new Object[] {
                        endpoint.getServiceName(), endpoint.getEndpointName(), ex });

                String errMsg = mMessages.getString("ServiceUnitImpl.INBOUND_MSG_PROCESSORS_START_FAILED",
                        new Object[] { endpoint.getServiceName().toString(), endpoint.getEndpointName(), ex });
                throw new JBIException(errMsg);
            }
        }
        endpoint.setState(EndpointState.RUNNING);

        mLogger.log(Level.INFO, "ServiceUnitImpl.ENDPOINT_ACTIVATED", new Object[] { direction,
                endpoint.getServiceName().toString(), endpoint.getEndpointName() });
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
        ServiceEndpoint endpointReference = endpoint.getServiceEndpoint();

        endpoint.setState(EndpointState.STOPPED);

        mLogger.log(Level.INFO, "ServiceUnitImpl.ENDPOINT_DEACTIVATED", new Object[] { direction,
                endpoint.getServiceName().toString(), endpoint.getEndpointName() });
    }

    private void addChannels(Endpoint endpoint) throws DCOMException {
        Map operations = endpoint.getDCOMOperations();
        Iterator iter = operations.keySet().iterator();
        List operationsList = new ArrayList();
        Channel dcomChannel = null;
        try {
            while (iter.hasNext()) {
                QName operation = (QName) iter.next();
                try {
                    dcomChannel = mDCOMChannelMgr.addChannel(endpoint, operation);
                } catch (DCOMException ce) {
                    mLogger.log(Level.WARNING, "ServiceUnitImpl.ADDCHANNEL_FAILED", new Object[] { mId,
                            endpoint.getServiceName().toString(), endpoint.getEndpointName(), operation.toString() });
                }
                operationsList.add(operation);
                try {
                    dcomChannel.open();
                } catch (DCOMException ex) {
                    if (endpoint.getEndpointType() == Endpoint.EndpointType.OUTBOUND) {
                        mLogger.log(Level.WARNING, "ServiceUnitImpl.OUTBOUND_CHANNEL_OPEN_FAILED",
                                new Object[] { mId, endpoint.getServiceName().toString(), endpoint.getEndpointName(),
                                        operation.toString() });
                    } else { // propagate error for inbound
                        throw ex;
                    }
                }
            }
        } catch (Exception ex) {
            try {
                // Remove channels added before failure (inclusive of failure).
                Iterator opsIter = operationsList.iterator();
                while (opsIter.hasNext()) {
                    QName op = (QName) opsIter.next();
                    Channel channel = mDCOMChannelMgr.removeChannel(endpoint, op);
                    channel.close();
                }
            } catch (DCOMException me) {
                throw me; // propogate
            }
        }

    }

    private void removeChannels(Endpoint endpoint) throws DCOMException {
        Map operations = endpoint.getDCOMOperations();
        Iterator iter = operations.keySet().iterator();
        int direction = endpoint.getEndpointType();
        while (iter.hasNext()) {
            QName operation = (QName) iter.next();
            if (direction == EndpointType.OUTBOUND) {
                // Use ServiceEndpoint name from JBI as part of key
                mDCOMChannelMgr.removeChannel(endpoint, operation);
            } else {
                // Use Endpoint name from portmap as part of key
                mDCOMChannelMgr.removeChannel(endpoint, operation);
            }
        }
    }

    private String getEndpointKey(Endpoint endpoint) {
        return getEndpointKey(endpoint.getServiceName().toString(), endpoint.getEndpointName(),
                endpoint.getEndpointType());
    }

    private String getEndpointKey(String serviceName, String endpointName, int endpointType) {
        return mId + serviceName + endpointName + EndpointImpl.endpointTypeToString(endpointType);
    }
/*    private EndpointConfiguration initializeEndpointConfig() throws Exception {
        EndpointConfiguration endpointConfig = null;
        if (mEndpointConfig == null) {
            endpointConfig =
                    EndpointConfigurationFactory.getEndpointConfiguration(mSuPath);
        }
        
        return endpointConfig;
    }
    
    *//**
     * Deploy the ServiceUnit. 
     *
     * @param        serviceUnitRootPath path to the ServiceUnit
     * @exception    JBIException if unable to initialize this ServiceUnit
     *//*
     public void deploy() throws JBIException {
         mLogger.log(Level.INFO, "ServiceUnitImpl_Called_SU_deploy");
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
            mLogger.log(Level.SEVERE, "ServiceUnitImpl_Called_SU_deploy", new Object[] {mId, errMsg});
            throw new JBIException(errMsg);
        }
     }*/
}
