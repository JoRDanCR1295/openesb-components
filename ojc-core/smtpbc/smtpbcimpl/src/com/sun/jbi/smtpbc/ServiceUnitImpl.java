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

/***************************************************************************
 *
 *          Copyright (c) 2005, SeeByeond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.smtpbc;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import com.sun.encoder.Encoder;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.smtpbc.Endpoint.EndpointState;
import com.sun.jbi.smtpbc.Endpoint.EndpointType;
import com.sun.jbi.smtpbc.mbeans.RuntimeConfigurationMBean;
import com.sun.jbi.smtpbc.packaging.EndpointConfiguration;
import com.sun.jbi.smtpbc.packaging.WSDLConfigurations;

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
 * com.sun.jbi.smtp.packaging package should need to change.
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public class ServiceUnitImpl implements ServiceUnit {



    private final String mId;
    private final ComponentContext mContext;
    private final StatusProviderHelper mStatusProviderHelper;

    private Collection mEndpoints;
    private final Collection mEndpointChangeListeners;
    private final WSDLConfigurations wsdlConfig;
    private String mSuPath;
    private EndpointConfiguration mEndpointConfig;
    private RuntimeConfigurationMBean mRuntimeConfig;
    
    private static Logger mLogger = Messages.getLogger(ServiceUnitImpl.class);

    public ServiceUnitImpl(final String id, final String serviceUnitRootPath,final ComponentContext context,
                           final StatusProviderHelper statusProviderHelper,
                           final Collection endpointChangeListeners, final RuntimeConfigurationMBean runtimeConfig) {
        mId = id;
        mContext = context;
        mStatusProviderHelper = statusProviderHelper;
        mEndpoints = new HashSet();
        mEndpointChangeListeners = endpointChangeListeners;
        wsdlConfig = new WSDLConfigurations(serviceUnitRootPath);
        mSuPath = serviceUnitRootPath;
        mRuntimeConfig = runtimeConfig;
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
    public void init(final String serviceUnitRootPath) throws JBIException {

        // Initialize the endpoints for this ServiceUnit.  Basically sets
        // up the static configuration information for each Endpoint.
        ServiceUnitImpl.mLogger.log(Level.INFO, "SU_Called_SU_init");
        try {
        	final EndpointConfiguration endpointConfig = initializeEndpointConfig(serviceUnitRootPath);
            

            mEndpoints = 
//             Collection<Endpoint> endpoints =
                wsdlConfig.parse(endpointConfig.portMaps(),mRuntimeConfig.retrieveEnvVariablesMap());
//             Iterator<Endpoint> it = endpoints.iterator();
//             while (it.hasNext()) {
//                 Endpoint endpoint = it.next();
//                 mEndpoints.put(endpoint.getEndpointName(),
//                                endpoint);
//             }


	        // Initialize the state of all these endpoints.
	        final Iterator it = mEndpoints.iterator();
	        while (it.hasNext()) {
	            final Endpoint endpoint = (Endpoint)it.next();
	
	            // Store the state of the Endpoint
	            endpoint.setState(EndpointState.SHUTDOWN);
	
	
	            // Initializes encoder map for the current endpoint.
	            // This also triggers the encoder codegen if the
	            // specified encoder is not cached.
	            final Map encoderMapping = wsdlConfig.getPartEncoderMapping(endpoint.getDefinition(),
	            		endpoint.getServiceName().toString(),
	            		endpoint.getEndpointName(),
	            		endpoint.getEndpointType(),
	            		endpoint.getSMTPOperations());
	            endpoint.setMessagePartEncoderMapping(encoderMapping);
	            
	            // Store the status...
	            final StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
	            final QName serviceName = endpoint.getServiceName();
	            final String portName = endpoint.getEndpointName();
	            String uniqueName = null;
	            if (endpoint.getEndpointType().equals(EndpointType.INBOUND)) {
	                uniqueName = mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
	                reporting.addConsumingEndpoint(uniqueName);
	            } else {
	                uniqueName = mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
	                reporting.addProvisioningEndpoint(uniqueName);
	            }
	            endpoint.setEndpointStatus(reporting.getEndpointStatus(uniqueName));
	
	            // Let all our EndpointChangeListeners know what's happening
	            final Iterator it2 = mEndpointChangeListeners.iterator();
	            while (it2.hasNext()) {
	                ((EndpointChangeListener)it2.next()).endpointInitialized(endpoint);
	            }
	        }
        } catch (final Exception ex) {
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
    public void start() throws JBIException {

        if (ServiceUnitImpl.mLogger.isLoggable(Level.INFO)) {
            ServiceUnitImpl.mLogger.log(Level.INFO, "SU_Called_SU_start", getServiceUnitId());
            ServiceUnitImpl.mLogger.log(Level.INFO, "SU_Activate_All_Endpoints");
        }

        final HashSet activatedEndpoints = new HashSet();

        final Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint = null;
        try {
            while (it.hasNext()) {
                currEndpoint = (Endpoint)it.next();

                activateEndpoint(currEndpoint);
                activatedEndpoints.add(currEndpoint);

                // Let all our EndpointChangeListeners know what's happening
                final Iterator it2 =
                    mEndpointChangeListeners.iterator();
                while (it2.hasNext()) {
                    ((EndpointChangeListener)it2.next()).endpointActivated(currEndpoint);
                }
            }
        } catch (final Exception ex) {
            final String errMsg = ex.getLocalizedMessage();
            if (currEndpoint != null) {
                ServiceUnitImpl.mLogger.log(Level.SEVERE, "SU_Failed_start_SU", 
                    new Object[] {currEndpoint.getServiceName(), currEndpoint.getEndpointName(), errMsg}) ;                
            }
            ServiceUnitImpl.mLogger.log(Level.INFO, "SU_Deactivate_Endpoints");
            final Iterator it2 = activatedEndpoints.iterator();
            while (it2.hasNext()) {
                deactivateEndpoint((Endpoint)it2.next());
            }
            throw new JBIException(ex);
        }
        if (ServiceUnitImpl.mLogger.isLoggable(Level.INFO)) {
            ServiceUnitImpl.mLogger.log(Level.INFO, "SU_Complete_start_SU", getServiceUnitId());
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
    public void stop() throws JBIException {
        if (ServiceUnitImpl.mLogger.isLoggable(Level.INFO)) {
            ServiceUnitImpl.mLogger.log(Level.INFO, "SU_Called_SU_stop", getServiceUnitId());
        }        
        final Iterator it = mEndpoints.iterator();
        while (it.hasNext()) {
            final Endpoint endpoint = (Endpoint)it.next();
            deactivateEndpoint(endpoint);

            // Let all our EndpointChangeListeners know what's happening
            final Iterator it2 =
                mEndpointChangeListeners.iterator();
            while (it2.hasNext()) {
                ((EndpointChangeListener)it2.next()).endpointDeactivated(endpoint);
            }
        }
        if (ServiceUnitImpl.mLogger.isLoggable(Level.INFO)) {
            ServiceUnitImpl.mLogger.log(Level.INFO, "SU_Complete_SU", getServiceUnitId());
        }
    }

    /**
     * Shuts down this ServiceUnit.  Simply moves all Endpoints to the
     * shutdown state.
     *
     * @exception    JBIException if the ServiceUnit fails to shutdown
     */
    public void shutdown() throws JBIException {
        if (ServiceUnitImpl.mLogger.isLoggable(Level.INFO)) {
            ServiceUnitImpl.mLogger.log(Level.INFO, "SU_Call_shutdown");
        }
        final Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint;

        while (it.hasNext()) {
            currEndpoint = (Endpoint)it.next();

            // Set the state of the endpoint
            currEndpoint.setState(EndpointState.SHUTDOWN);

            // Set the status of the endpoint
            final StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
            final QName serviceName = currEndpoint.getServiceName();
            final String portName = currEndpoint.getEndpointName();
            String uniqueName = null;
            if (currEndpoint.getEndpointType().equals(EndpointType.INBOUND)) {
                uniqueName = mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
                reporting.removeConsumingEndpoints(new String[] {uniqueName});
            } else {
                mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
                reporting.removeProvisioningEndpoints(new String[] {uniqueName});
            }

            // Let all our EndpointChangeListeners know what's happening
            final Iterator it2 =
                mEndpointChangeListeners.iterator();
            while (it2.hasNext()) {
                ((EndpointChangeListener)it2.next()).endpointShutdown(currEndpoint);
            }
            
            // Dispose the encoders
            final Map encoders = currEndpoint.getMessagePartEncoderMapping();
            for (final Iterator it3 = encoders.values().iterator(); it3.hasNext();) {
                final Encoder aEncoder = (Encoder)it3.next();
                // Dispose the encoder. This also
                // triggers the removal of its codegen artifacts
                aEncoder.dispose();
            }

        }
        wsdlConfig.clearEncoderCache();
        if (ServiceUnitImpl.mLogger.isLoggable(Level.INFO)) {
            ServiceUnitImpl.mLogger.log(Level.INFO, "SU_Complete_shutdown", getServiceUnitId());
        }
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

    protected void activateEndpoint(final Endpoint endpoint) throws JBIException {

        if (endpoint.getEndpointType().equals(EndpointType.OUTBOUND)) {
            final ServiceEndpoint endpointReference = 
                mContext.activateEndpoint(endpoint.getServiceName(),
                                          endpoint.getEndpointName());
            endpoint.setServiceEndpoint(endpointReference);
            if (ServiceUnitImpl.mLogger.isLoggable(Level.INFO)) {
                ServiceUnitImpl.mLogger.log(Level.INFO, "SU_Activate_outbound_EP", 
                    new Object[] {endpoint.getServiceName(), endpoint.getEndpointName()});
            }            
        }
        endpoint.setState(EndpointState.RUNNING);
    }

    protected void deactivateEndpoint(final Endpoint endpoint) throws JBIException {

        if (endpoint.getEndpointType().equals(EndpointType.OUTBOUND)) {
            final ServiceEndpoint endpointReference = endpoint.getServiceEndpoint();
            mContext.deactivateEndpoint(endpointReference);
            if (ServiceUnitImpl.mLogger.isLoggable(Level.INFO)) {
                ServiceUnitImpl.mLogger.log(Level.INFO, "SU_Deactivate_outbound_EP",
                    new Object[] {endpoint.getServiceName(), endpoint.getEndpointName()});
            }            
        }
        endpoint.setState(EndpointState.STOPPED);
    }
    
    /**
     * Deploy the ServiceUnit. 
     *
     * @param        serviceUnitRootPath path to the ServiceUnit
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
     public void deploy(final String serviceUnitRootPath) throws JBIException {
         mLogger.log(Level.INFO, "SU_Called_SU_deploy");
         try {
            mEndpointConfig = initializeEndpointConfig(serviceUnitRootPath);
            Map envVariableMap = wsdlConfig.parseForEnvironmentVariables(mEndpointConfig.portMaps(), mRuntimeConfig.retrieveEnvVariablesMap());
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
     
     private EndpointConfiguration initializeEndpointConfig(final String serviceUnitRootPath) throws Exception {
         if (mEndpointConfig == null) {
        	 mEndpointConfig =
                     EndpointConfiguration.newConfiguration(serviceUnitRootPath);
         }
         
         return mEndpointConfig;
     }

}
