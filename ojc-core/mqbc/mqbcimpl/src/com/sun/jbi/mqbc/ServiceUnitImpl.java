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

package com.sun.jbi.mqbc;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.encoder.Encoder;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.mqbc.Endpoint.EndpointType;
import com.sun.jbi.mqbc.extensions.MQBCAddress;
import com.sun.jbi.mqbc.extensions.MQBCOperation;
import com.sun.jbi.mqbc.mbeans.RuntimeConfigurationMBean;
import com.sun.jbi.mqbc.monitoring.EventLogger;
import com.sun.jbi.mqbc.monitoring.EventManagementFrameworkAlerter;
import com.sun.jbi.mqbc.packaging.EndpointConfiguration;
import com.sun.jbi.mqbc.packaging.EndpointConfigurationFactory;
import com.sun.jbi.mqbc.packaging.WSDLConfigurations;
import com.sun.jbi.mqbc.recovery.ConnectionInfoPersistException;
import com.sun.jbi.mqbc.recovery.ConnectionInfoPersister;
import com.sun.jbi.mqbc.recovery.MQConnectionInfoRecord;


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
 * com.sun.jbi.mqbc.packaging package should need to change.
 *
 *
 */
final class ServiceUnitImpl implements ServiceUnit {
    
    private EventLogger mLogger;


    private final String mId ;
    private final String mSuPath;
    private final MQComponentContext mContext;
    private final RuntimeConfigurationMBean mRuntimeConfig;
    private final StatusProviderHelper mStatusProviderHelper;
    private final ConnectionInfoPersister mConnPersister;
    
    private final Collection<Endpoint> mEndpoints;
    private final Collection<Endpoint> mSuspendedEndpoints;
    private final InboundReceiver mInboundReceiver;
    private final WSDLConfigurations mWsdlConfigSupport;
    private EndpointConfiguration mEndpointConfig;

    ServiceUnitImpl(String id,
                    String suPath,
                    MQComponentContext context,
                    RuntimeConfigurationMBean runtimeConfig,
                    StatusProviderHelper statusProviderHelper,
                    InboundReceiver inboundReceiver,
                    ConnectionInfoPersister connPersister) {
        assert id != null;
        assert suPath != null;
        assert context != null;
        assert runtimeConfig != null;
        assert statusProviderHelper != null;
        assert inboundReceiver != null;
        assert connPersister != null;
        mId = id;
        mSuPath = suPath;
        mContext = context;
        mRuntimeConfig = runtimeConfig;
        mStatusProviderHelper = statusProviderHelper;
        mEndpoints = new HashSet<Endpoint>();
        mSuspendedEndpoints = new HashSet<Endpoint>();
        mInboundReceiver = inboundReceiver;
        mWsdlConfigSupport = new WSDLConfigurations(suPath);
        mConnPersister = connPersister;
        mLogger = new EventLogger(Util.getLogger(mContext,
                getClass().getName()),
                EventManagementFrameworkAlerter.alerter);

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.fine(I18n.msg("Service unit id {0}, work root {1}.",
                    id,
                    suPath));
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
     * Deploy the ServiceUnit. 
     *
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
     public void deploy() throws JBIException {
         try {
             initializeEndpointConfig();
             Map<String, String[]> envVariableMap =
                     mWsdlConfigSupport.parseForEnvironmentVariables(
                             mRuntimeConfig.retrieveApplicationVariablesMap());
             if (envVariableMap.size() > mRuntimeConfig.countVariables()) {
                // number of environment variable tokens used in WSDLs 
                // is greater than the ones defined 
                mRuntimeConfig.updateApplicationVariablesMap(envVariableMap);
            }
        } catch (Exception ex) {
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
     * @exception    JBIException if unable to initialize this ServiceUnit
     */
    public void init() throws JBIException {
        
        // Initialize the endpoints for this ServiceUnit.  Basically sets
        // up the static configuration information for each Endpoint.
        Endpoint currEndpoint = null;
        Collection<Endpoint> endpoints;
        Collection<Endpoint> activatedOutboundEndpoints = new HashSet<Endpoint>();
        try {
            // Prepare for endpoint configuration map if deploy() hasn't been called yet
            initializeEndpointConfig();

            // Service Qualities
            mContext.getMessagingChannel().installServiceQualities(mId, mSuPath);
            
            endpoints = mWsdlConfigSupport.createEndpoints(mEndpointConfig.endpoints(),
                            mRuntimeConfig.retrieveApplicationVariablesMap(),
                            mRuntimeConfig.retrieveApplicationConfigurationsMap());
            if (endpoints.size() == 0) {
                mLogger.log(Level.SEVERE,
                        NotificationEvent.SEVERITY_TYPE_MAJOR,
                        NotificationEvent.OPERATIONAL_STATE_STARTED,
                        I18n.msg("1051: Service unit {0} initialized"
                                + " but configuration (WSDL) specifies no endpoints"
                                + " applicable to this service unit.", mId));
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg(
                            "Service unit {0} initializing {1} endpoints...",
                            mId,
                            endpoints.size()));
                }
            }
            
            // Initialize the state of all these endpoints.
            for (Endpoint endpoint : endpoints) {
                currEndpoint = endpoint;
                
                Map encoderMapping = mWsdlConfigSupport.getPartEncoderMapping(
                        currEndpoint);
                currEndpoint.setMessagePartEncoderMapping(encoderMapping);
                
                // Store the state of the Endpoint
                StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
                QName serviceName = currEndpoint.getServiceName();
                String portName = currEndpoint.getEndpointName();
                String uniqueName;
                if (currEndpoint.getEndpointType() == EndpointType.INBOUND) {
                    uniqueName =
                            mStatusProviderHelper
                                    .createConsumingEndpointIdentifier(
                                            serviceName,
                                            portName);
                    reporting.addConsumingEndpoint(uniqueName);
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.fine(I18n.msg("Consuming endpoint {0} init.",
                                uniqueName));
                    }
                } else {
                    uniqueName =
                            mStatusProviderHelper
                                    .createProvisioningEndpointIdentifier(
                                            serviceName,
                                            portName);
                    reporting.addProvisioningEndpoint(uniqueName);
                    activateOutboundEndpoint(currEndpoint);
                    activatedOutboundEndpoints.add(currEndpoint);
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.fine(I18n.msg("Provisioning endpoint {0} init.",
                                uniqueName));
                    }
                }
                currEndpoint.setEndpointStatus(reporting.getEndpointStatus(
                        uniqueName));
                persistEndpointForXA(currEndpoint);
            }
            
            synchronized (mEndpoints) {
                mEndpoints.clear();
                mEndpoints.addAll(endpoints);
            }
            endpoints.clear();
            
        } catch (Exception ex) {
            if (mLogger.isLoggable(Level.FINE)) {
                if (currEndpoint != null) {
                    mLogger.log(Level.FINE,
                            NotificationEvent.SEVERITY_TYPE_FATAL,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            I18n.msg(
                                    "Service Unit {0} initialization failed for endpoint {1},"
                                            + " deactivating endpoints already activated"
                                            + " before rethrowing exception.",
                                    mId,
                                    currEndpoint.getEndpointName()
                            ),
                            ex
                    );
                } else {
                    mLogger.log(Level.FINE,
                            NotificationEvent.SEVERITY_TYPE_FATAL,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            I18n.msg("Service Unit {0} initialization failed,"
                                    + " deactivating endpoints already activated"
                                    + " before rethrowing exception.", mId
                            ),
                            ex
                    );
                }
            }
            for (Endpoint activatedOutboundEndpoint : activatedOutboundEndpoints) {
                deactivateOutboundEndpoint(activatedOutboundEndpoint);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Endpoint {0} deactivated.",
                            activatedOutboundEndpoint.getEndpointName()));
                }
            }
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
       
        
        HashSet activatedInboundEndpoints = new HashSet();
        Endpoint currEndpoint = null;
        try {
            synchronized (mEndpoints) {
            for (Endpoint e : mEndpoints) {
                currEndpoint = e;
                if (e.getEndpointType() == EndpointType.INBOUND) {
                    startConsuming(e);
                    activatedInboundEndpoints.add(e);
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.fine(I18n.msg("Consuming endpoint {0} start.",
                                e.getEndpointName()));
                    }
                } else {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.fine(I18n.msg("Provisioning endpoint {0} start.",
                                e.getEndpointName()));
                    }
                }
            }
            }
        } catch (Exception ex) {
            if (mLogger.isLoggable(Level.FINE)) {
                if (currEndpoint != null) {
                    mLogger.log(Level.FINE,
                            NotificationEvent.SEVERITY_TYPE_FATAL,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            I18n.msg(
                                    "Service Unit {0} start failed for endpoint {1},"
                                            + " deactivating endpoints already activated"
                                            + " before rethrowing exception.",
                                    mId,
                                    currEndpoint.getEndpointName()
                            ),
                            ex
                    );
                } else {
                    mLogger.log(Level.FINE,
                            NotificationEvent.SEVERITY_TYPE_FATAL,
                            NotificationEvent.OPERATIONAL_STATE_RUNNING,
                            I18n.msg("Service Unit {0} start failed,"
                                    + " deactivating endpoints already activated"
                                    + " before rethrowing exception.", mId
                            ),
                            ex
                    );
                }
            }
            for (Object activatedInboundEndpoint : activatedInboundEndpoints) {
                Endpoint endpoint = (Endpoint) activatedInboundEndpoint;
                stopConsuming(endpoint);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Endpoint {0} stopped.",
                            endpoint.getEndpointName()));
                }
            }
            throw new JBIException(ex);
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
        synchronized (mEndpoints) {
        for (Endpoint endpoint : mEndpoints) {
            if (endpoint.getEndpointType() == EndpointType.INBOUND) {
                stopConsuming(endpoint);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Endpoint {0} stopped.",
                            endpoint.getEndpointName()));
                }
            }
            
        }
        }
    }
    
    /**
     * Shuts down this ServiceUnit.  Simply moves all Endpoints to the
     * shutdown state.
     *
     * @exception    JBIException if the ServiceUnit fails to shutdown
     */
    public void shutdown() throws JBIException {
        
        synchronized (mEndpoints) {
        for (Endpoint currEndpoint : mEndpoints) {
            
            // Set the state of the endpoint
           
            
            // Set the status of the endpoint
            StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
            QName serviceName = currEndpoint.getServiceName();
            String portName = currEndpoint.getEndpointName();
            String uniqueName = null;
            if (currEndpoint.getEndpointType()==EndpointType.INBOUND) {
                uniqueName = 
                        mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
                reporting.removeConsumingEndpoints(new String[] {uniqueName});
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Consuming endpoint {0} shut down.",
                            uniqueName));
                }
            } else {
                uniqueName = 
                        mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
                reporting.removeProvisioningEndpoints(new String[] {uniqueName});
                 deactivateOutboundEndpoint(currEndpoint);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.fine(I18n.msg("Provisioning endpoint {0} shut down.",
                            uniqueName));
                }
            }
            // Dispose the encoders
            Map encoders = currEndpoint.getMessagePartEncoderMapping();
            if (encoders != null)
                for (Iterator it2 = encoders.values().iterator(); it2.hasNext();) {
                Encoder aEncoder = (Encoder)it2.next();
                // Dispose the encoder. This also
                // triggers the removal of its codegen artifacts
                aEncoder.dispose();
                }  
        }
        }
    }
    
    /**
     * Retrieves the set of Endpoints handled by this ServiceUnit.
     *
     * @return       the set of Endpoints
     */
    public Collection<Endpoint> getEndpoints() {
        synchronized (mEndpoints) {
        return Collections.unmodifiableCollection(mEndpoints);
        }
    }

    /**
     * Determines if the specified endpoint is in a suspended state. A suspended
     * endpoint is a started endpoint, not stopped, not shut down, but not
     * accepting calls for its service.
     *
     * @param endpoint An endpoint managed by the service unit
     *
     * @return true if the endpoint is suspended, false otherwise.
     * @throws javax.jbi.JBIException if the specified endpoint is null, or is
     * not one managed by the service, or its state cannot be determined for
     * whatever reason.
     */
    public boolean isSuspended(Endpoint endpoint) throws JBIException {
        checkMine(endpoint);
        synchronized (mSuspendedEndpoints) {
            return mSuspendedEndpoints.contains(endpoint);
        }
    }
    
    private void checkMine(Endpoint endpoint) throws JBIException {
        assert endpoint != null;
        
        for (Endpoint aEndpoint : mEndpoints) {
            String aName = aEndpoint.getEndpointName();
            QName aServiceName = aEndpoint.getServiceName();
            if (aName.equals(endpoint.getEndpointName()) 
                    && aServiceName.equals(endpoint.getServiceName())) {
                return;
            }
        }
        throw new JBIException(I18n.msg(
                "1054: Unknown endpoint {0},{1} - not managed by service unit {2}",
                endpoint.getServiceName().toString(),
                endpoint.getEndpointName(),
                mId));
    }

    /**
     * Suspends the specified endpoint. The endpoint must be one that the
     * service unit is managing.  An endpoint that is suspended is started, not
     * stopped nor shut down, but not accepting calls for its service.
     *
     * @param endpoint An endpoint managed by the service unit
     *
     * @throws javax.jbi.JBIException if the specified endpoint is null, is not
     * one managed by the service unit, or its suspension cannot be effected for
     * whatever reason.
     */
    public void suspend(Endpoint endpoint) throws JBIException {
        checkMine(endpoint);
        synchronized (mSuspendedEndpoints) {
            if (mSuspendedEndpoints.contains(endpoint)) {
                throw new JBIException(I18n.msg(
                        "1055: Cannot suspend endpoint {0},{1} - already suspended",
                        endpoint.getServiceName().toString(),
                        endpoint.getEndpointName()));
            }
            mInboundReceiver.suspendInboundMessageProcessor(endpoint);
            mSuspendedEndpoints.add(endpoint);
        }
    }

    /**
     * Resumes (unsuspends) the specified endpoint. The endpoint must be one
     * that the service unit is managing. An endpoint that is resumed is started
     * and accepting calls for its service.
     *
     * @param endpoint An endpoint managed by the service unit
     *
     * @throws javax.jbi.JBIException if the specified endpoint is null, is not
     * one managed by the service unit, or its resumption cannot be effected for
     * whatever reason.
     */
    public void resume(Endpoint endpoint) throws JBIException {
        checkMine(endpoint);
        synchronized (mSuspendedEndpoints) {
            if (!mSuspendedEndpoints.contains(endpoint)) {
                throw new JBIException(I18n.msg(
                        "1056: Cannot resume endpoint {0},{1} - not suspended",
                        endpoint.getServiceName().toString(),
                        endpoint.getEndpointName()));
            }
            mInboundReceiver.resumeInboundMessageProcessor(endpoint);
            mSuspendedEndpoints.remove(endpoint);
        }
    }

    private void startConsuming(Endpoint endpoint) throws JBIException {
        if (endpoint.getEndpointType() != EndpointType.INBOUND) {
            return;
        }  	

        List<ServiceQuality> qos = mContext.getMessagingChannel()
                .getServiceQualitiesForEndpoint(endpoint.toEndpointData().toEndpointInfo());

        try {
            mInboundReceiver.addInboundMessageProcessor(this, endpoint, qos);
        } catch (Exception ex) {
            throw new JBIException(I18n.msg("1052: Service unit {0}:"
                    + " MEx processor for endpoint {1} failed initialization",
                    mId, endpoint.getEndpointName()), ex);
        }
    }
    
    private void stopConsuming(Endpoint endpoint) throws JBIException {
        if (endpoint.getEndpointType() != EndpointType.INBOUND) {
            return;
        }
        mInboundReceiver.removeInboundMessageProcessor(endpoint);
    }
    
    ////////
    //
    //  ServiceUnitImpl Protected Methods
    //
    ////////
    protected void activateOutboundEndpoint(Endpoint endpoint) throws JBIException {
        if (endpoint.getEndpointType() != EndpointType.OUTBOUND) {
            return;
        }
        ServiceEndpoint endpointReference =
                mContext.activateEndpoint(endpoint.getServiceName(),
                endpoint.getEndpointName());
        endpoint.setServiceEndpoint(endpointReference);
    }
    
    protected void deactivateOutboundEndpoint(Endpoint endpoint) throws JBIException {
        if (endpoint.getEndpointType() != EndpointType.OUTBOUND) {
            return;
        }
        
        ServiceEndpoint endpointReference = mContext.getEndpoint(
                endpoint.getServiceName(),
                endpoint.getEndpointName());
        if (endpointReference != null) {
            mContext.deactivateEndpoint(endpointReference);
        }
    }
    
     private void initializeEndpointConfig() throws Exception {
         assert mSuPath != null;
         synchronized (this) {
             if (mEndpointConfig == null) {
                 mEndpointConfig = EndpointConfigurationFactory.getEndpointConfiguration(mSuPath);
                 if (mLogger.isLoggable(Level.FINE)) {
                     mLogger.fine(I18n.msg("Loaded endpoint configuration -"
                             + " Service unit {0}: Work root path: {1}",
                             mId,
                             mSuPath));
                 }
             }
         }
    }

    private void persistEndpointForXA (Endpoint endpoint) throws Exception {
          MQBCOperation mqbcOperation = null;
          Map operations = endpoint.getMQOperations();
          Set entrySet = operations.entrySet();
          for (Iterator it = entrySet.iterator(); it.hasNext();) {
               Entry entry = (Entry) it.next();
               QName opname = (QName) entry.getKey();
               if(opname.getNamespaceURI() == null || opname.getNamespaceURI().length()==0) {
                   ServiceEndpoint serviceEndpoint = endpoint.getServiceEndpoint();
                   if (serviceEndpoint != null) {
                       opname = new QName(
                               serviceEndpoint.getServiceName().getNamespaceURI(),
                               opname.getLocalPart());
                   }
               }
               mqbcOperation =  (MQBCOperation)operations.get(opname);
               try {
                   if (mqbcOperation.getTransaction() && this.mConnPersister != null ) {
                       MQBCAddress mqbcAddr = endpoint.getMQAddress();
                       MQConnectionInfoRecord rec = new MQConnectionInfoRecord(
                               mqbcAddr.getHostName(),
                               mqbcAddr.getPortNumber(),
                               mqbcAddr.getQueueManagerName(),
                               mqbcAddr.getChannelName(),
                               mqbcAddr.getUserName(),
                               mqbcAddr.getPassword(),
                               mqbcAddr.getCipherSuite(),
                               mqbcAddr.getSslPeerName()
                       );
                       mConnPersister.persist(new MQConnectionInfoRecord [] {rec});
                     }
               } catch (ConnectionInfoPersistException ex) {
                   throw new Exception(I18n.msg("1053: Service Unit {0}:"
                           + " Configuration persistence failed initialization"
                           + " for endpoint {1}", mId,
                           endpoint.getEndpointName()));
               }
          }
    }
}
