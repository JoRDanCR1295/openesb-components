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

package com.sun.jbi.ftpbc;

import com.sun.jbi.ftpbc.Endpoint.EndpointType;
import com.sun.jbi.ftpbc.Endpoint.EndpointState;
import com.sun.jbi.ftpbc.extensions.FTPAddress;
import com.sun.jbi.ftpbc.extensions.FTPExtSerializer;
import com.sun.jbi.ftpbc.ftp.FtpFileConfigConstants;
import com.sun.jbi.ftpbc.packaging.EndpointConfiguration;
import com.sun.jbi.ftpbc.packaging.EndpointConfigurationFactory;
import com.sun.jbi.ftpbc.packaging.WSDLConfigurations;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.management.descriptor.ConfigurationException;
import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;

import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;

import com.sun.encoder.Encoder;

import javax.xml.namespace.QName;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;

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
 * com.sun.jbi.ftpbc.packaging package need to be changed.
 *
 */
public class ServiceUnitImpl implements ServiceUnit {
    private static final Messages mMessages =
            Messages.getMessages(ServiceUnitImpl.class);
    private static Logger mLogger = Messages.getLogger(ServiceUnitImpl.class);
    
    private String mId;
    private String mSuPath;
    private ComponentContext mContext;
    private StatusProviderHelper mStatusProviderHelper;
    private InboundReceiver mInboundReceiver;
    private Collection mEndpoints;
    private String mComponentName;
    private WSDLConfigurations mWSDLConfig;
    private EndpointConfiguration mEndpointConfig;
    private RuntimeConfigurationMBean mRuntimeConfig;
//    private Map<EndpointInfo, List<ServiceQuality>> mEndpointQos;
    private MessagingChannel mChannel;
    
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
        mComponentName = context.getComponentName();
        mStatusProviderHelper = statusProviderHelper;
        mInboundReceiver = inboundReceiver;
        mEndpoints = new ArrayList();
        mWSDLConfig = new WSDLConfigurations(suPath);
        mChannel = FTPBCComponentContext.getInstance().getBindingChannel();
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
     * Initializes the ServiceUnit.  Parses the serviceUnitRootPath
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
        if ( mLogger.isLoggable(Level.INFO ) )
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R003001.SU_Called_SU_init"));
        
        Endpoint currEndpoint = null;
        Map svcs = null;
        
//        DeploymentLookup lookUpUtil = new DeploymentLookup(mContext);
//        mEndpointQos = lookUpUtil.lookupServiceQualities(mSuPath);
//
//        if (mEndpointQos != null && mEndpointQos.size() > 0) {
//            for (Iterator itor = mEndpointQos.keySet().iterator(); itor.hasNext(); ) {
//                EndpointInfo endpointInfo = (EndpointInfo) itor.next();
//                ServiceQuality[] qos =
//                    (ServiceQuality[])((List)mEndpointQos.get(endpointInfo)).toArray(new ServiceQuality[0]);
//                mChannel.addServiceQualityToEndpoint(endpointInfo, qos);
//            }
//        }

        mChannel.installServiceQualities(mId, mSuPath);

        try {
            if ( mEndpointConfig == null )
                mEndpointConfig = initializeEndpointConfig();
            
            svcs = getEndpointIdentifiers(mSuPath);

            if ( svcs == null || svcs.size() == 0 ) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W009001.No_endpoints_to_create", new Object[] {mComponentName, mId}));
                return;
            }

           mEndpoints = mWSDLConfig.parse(mEndpointConfig.endpoints(), mRuntimeConfig.retrieveApplicationVariablesMap());
            
            if ( mEndpoints == null || mEndpoints.size() == 0 ) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W003001.SU_No_matching_WSDL", mId));
                return;
            }
            
            // Initialize the state of all these endpoints.
            Iterator it = mEndpoints.iterator();
            
            while (it.hasNext()) {
                currEndpoint = (Endpoint)it.next();
                // Store the state of the Endpoint
                currEndpoint.setState(EndpointState.SHUTDOWN);
                
                resolveAppConfig(currEndpoint, svcs, true);
            
                getQOSConfigurations(currEndpoint, (EndpointIdentifier)svcs.get(currEndpoint.getServiceName() + "|" + currEndpoint.getEndpointName()));
                
                // Initializes encoder map for the current endpoint.
                // This also triggers the encoder codegen if the
                // specified encoder is not cached.
                Map encoderMapping = mWSDLConfig.getPartEncoderMapping(currEndpoint.getDefinition(),
                        currEndpoint.getServiceName().toString(),
                        currEndpoint.getEndpointName(),
                        currEndpoint.getEndpointType(),
                        currEndpoint.getOperations(),
                        currEndpoint.getOperationMsgExchangePattern());
                
                currEndpoint.setMessagePartEncoderMapping(encoderMapping);
                currEndpoint.setServiceUnitID(mId);
                currEndpoint.setServiceUnit(this);
                
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
                    // follow the guidelines for Component/SU life cycle
                    // register and start provisioning here for OB
                    activateOBEndpoint(currEndpoint);
                }
                currEndpoint.setEndpointStatus(reporting.getEndpointStatus(uniqueName));
            }
        } catch (Exception ex) {
            String errMsg = ex.getLocalizedMessage();
            if (currEndpoint != null) {
                mLogger.log(Level.SEVERE, mMessages.getString("FTPBC-E001012.SU_Failed_init_SU", new Object[] {currEndpoint.getServiceName(), currEndpoint.getEndpointName(), errMsg})) ;
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
     * @exception  JBIException upon error.
     * to activate
     */
    public void start() throws JBIException {
        mLogger.log(Level.INFO, mMessages.getString("FTPBC-R003002.SU_Called_SU_start"));
        HashSet activatedEndpoints = new HashSet();
        
        Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint = null;
        try {
            while (it.hasNext()) {
                currEndpoint = (Endpoint)it.next();
                
                if (currEndpoint.getEndpointType() == EndpointType.INBOUND) {
                    startConsuming(currEndpoint);
                    activatedEndpoints.add(currEndpoint);
                }
            }
        } catch (Exception ex) {
            String errMsg = ex.getLocalizedMessage();
            if (currEndpoint != null) {
                mLogger.log(Level.SEVERE, mMessages.getString("FTPBC-E001013.SU_Failed_start_SU", new Object[] {currEndpoint.getServiceName(), currEndpoint.getEndpointName(), errMsg})) ;
            }
            // clean up a bit of half done IB activation
            Iterator it2 = activatedEndpoints.iterator();
            while (it2.hasNext()) {
                stopConsuming((Endpoint)it2.next());
            }
            throw new JBIException(errMsg);
        }
        mLogger.log(Level.INFO, mMessages.getString("FTPBC-R003003.SU_Complete_start_SU", getServiceUnitId()));
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
        mLogger.log(Level.INFO, mMessages.getString("FTPBC-R003004.SU_Call_stop"));
        Iterator it = mEndpoints.iterator();
        while (it.hasNext()) {
            Endpoint endpoint = (Endpoint)it.next();
            // follow the new Component/SU life cycle guide line
            // stop inbound consuming
            if (endpoint.getEndpointType() == EndpointType.INBOUND) {
                // need to handle throttling clean up
                stopConsuming(endpoint);
            }
        }
        mLogger.log(Level.INFO, mMessages.getString("FTPBC-R003005.SU_Complete_SU", getServiceUnitId()));
    }
    
    /**
     * Shuts down this ServiceUnit.  Simply moves all Endpoints to the
     * shutdown state.
     *
     * @exception    JBIException if the ServiceUnit fails to shutdown
     */
    public void shutdown() throws JBIException {
        if ( mLogger.isLoggable(Level.INFO) )
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R003006.SU_Call_shutdown"));
        
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

        Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint;
        
        while (it.hasNext()) {
            currEndpoint = (Endpoint)it.next();
            
            if (currEndpoint.getEndpointType() == EndpointType.OUTBOUND) {
                deactivateOBEndpoint(currEndpoint);
            }
            
            // Set the state of the endpoint
            currEndpoint.setState(EndpointState.SHUTDOWN);
            
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
        mLogger.log(Level.INFO, mMessages.getString("FTPBC-R003007.SU_Complete_shutdown", getServiceUnitId()));
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
    
    protected void activateOBEndpoint(Endpoint endpoint) throws JBIException {
        ServiceEndpoint endpointReference =
                mContext.activateEndpoint(endpoint.getServiceName(),
                endpoint.getEndpointName());
        endpoint.setServiceEndpoint(endpointReference);
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R003008.SU_Activate_outbound_EP", new Object[] {endpoint.getServiceName(), endpoint.getEndpointName()}));
        }
        endpoint.setState(EndpointState.RUNNING);
    }
    
    protected void deactivateOBEndpoint(Endpoint endpoint) throws JBIException {
        ServiceEndpoint endpointReference = endpoint.getServiceEndpoint();
        mContext.deactivateEndpoint(endpointReference);
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R003009.SU_Deactivate_outbound_EP", new Object[] {endpoint.getServiceName(), endpoint.getEndpointName()}));
        }
        endpoint.setState(EndpointState.STOPPED);
    }
    
    protected void startConsuming(Endpoint endpoint) throws JBIException {
        try {
            mInboundReceiver.addInboundMessageProcessor(endpoint);
        } catch (FaultException ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("FTPBC-E001014.SU_Failed_start_inbound_EP", endpoint.getEndpointName()));
            throw new JBIException(ex.getLocalizedMessage(), ex);
        } catch (IBProcCreationException ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("FTPBC-E001014.SU_Failed_start_inbound_EP", endpoint.getEndpointName()));
            throw new JBIException(ex.getLocalizedMessage(), ex);
        }
        endpoint.setState(EndpointState.RUNNING);
    }
    
    protected void stopConsuming(Endpoint endpoint) throws JBIException {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R003010.SU_Remove_inbound_proc", endpoint.getEndpointName()));
        }
        mInboundReceiver.removeInboundMessageProcessor(endpoint);
        endpoint.setState(EndpointState.STOPPED);
    }
    
    /** package protected method.
     * Used solely for JUnit test purposes
     */
    void setEndpoints(List endpoints) {
        mEndpoints = endpoints;
    }
    
    public void deploy() throws JBIException {
        if ( mLogger.isLoggable(Level.INFO) )
            mLogger.log(Level.INFO, mMessages.getString("FTPBC-R003011.SU_Called_SU_deploy"));
        
        try {
            mEndpointConfig = initializeEndpointConfig();
            Map appVariableMap = mWSDLConfig.parseForApplicationVariables(mEndpointConfig.endpoints(), mRuntimeConfig.retrieveApplicationVariablesMap());
            if (appVariableMap.size() > mRuntimeConfig.retrieveApplicationVariablesMap().size()) {
                // number of app variable tokens used in File WSDLs
                // is greater than the ones defined
                mRuntimeConfig.updateApplicationVariablesMap(appVariableMap);
            }

            EndpointIdentifier[] svcs = getEndpointIdentifierArray(mSuPath);

            if ( svcs == null || svcs.length == 0 ) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W009001.No_endpoints_to_create", new Object[] {mComponentName, mId}));
                return;
            }
            
            Map appConfigMap = mRuntimeConfig.retrieveApplicationConfigurationsMap();
            
            for (int ii = 0; ii < svcs.length; ii++) {
                EndpointIdentifier epDesc = svcs[ii];
                String configName = epDesc.getApplicationConfigurationName();

                // this application configuration name is not yet defined in the config MBean,
                // add it to the application configuration data structure
                if (configName != null && !"".equals(configName) &&
                    !appConfigMap.containsKey(configName)) {
                    appConfigMap.put(configName, null);    
                }
            }
            
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = ex.getLocalizedMessage();
            mLogger.log(Level.SEVERE, mMessages.getString("FTPBC-E001015.SU_Failed_deploy_SU", new Object[] {mId, errMsg}));
            throw new JBIException(errMsg);
        }
    }
    
    private EndpointConfiguration initializeEndpointConfig() throws Exception {
        EndpointConfiguration endpointConfig = null;
        if (mEndpointConfig == null) {
            endpointConfig =
                    EndpointConfigurationFactory.getEndpointConfiguration(mSuPath);
        }
        
        return endpointConfig;
    }

    private Map getEndpointIdentifiers(String suPath) throws ConfigurationException {
        Map eps = null;
        SUDescriptorSupport descSupport = 
            new SUDescriptorSupport(suPath);
        EndpointIdentifier[] svcs = descSupport.getServices();
        if ( svcs != null && svcs.length > 0 ) {
            eps = new HashMap();
            String epKey = null;
            for ( int i = 0; i < svcs.length; i++ ) {
                epKey = svcs[i].getServiceName() + "|" + svcs[i].getEndpointName();
                eps.put(epKey, svcs[i]);
            }
        }
        return eps;
    }
    
    private EndpointIdentifier[] getEndpointIdentifierArray(String suPath) throws ConfigurationException {
        SUDescriptorSupport descSupport = 
            new SUDescriptorSupport(suPath);
        return descSupport.getServices();
    }

    private void resolveAppConfig(Endpoint ep, Map svcs, boolean resolveAppCfg) throws Exception {
        Map appCfgs = mRuntimeConfig.retrieveApplicationConfigurationsMap();
        Map appVars = mRuntimeConfig.retrieveApplicationVariablesMap();
        EndpointIdentifier match = (EndpointIdentifier)svcs.get(ep.getServiceName() + "|" + ep.getEndpointName());
        if ( match != null ) {
            String appCfgName = match.getApplicationConfigurationName();
            if ( appCfgName != null && appCfgName.trim().length() > 0 ) {
                Map parms = (Map)appCfgs.get(appCfgName);
                if ( parms != null && parms.size() > 0 ) {
                    FTPAddress address = ep.getAddress();
                    if ( address == null )
                        throw new Exception("Missing ftp:address element in end point : service name=[" + ep.getServiceName() + "] endpoint name=[" + ep.getEndpointName() + "]");
                    String host = getParameterAndResolveAppVar(RuntimeConfiguration.FTP_HOST_NAME, parms, appVars);
                    Integer port = (Integer)parms.get(RuntimeConfiguration.FTP_HOST_PORT);
                    address.setURL("ftp://".concat(host).concat(":").concat(port.toString()));
                    address.parse();
                    address.getFTPURL().setHost(host);
                    address.getFTPURL().setPort(port.toString());
                    String user = getParameterAndResolveAppVar(RuntimeConfiguration.FTP_USER_ID, parms, appVars);
                    String password = getParameterAndResolveAppVar(RuntimeConfiguration.FTP_PASSWORD, parms, appVars);
                    address.getFTPURL().setUser(user);
                    address.getFTPURL().setPassword(password);
                    address.setUser(user);
                    address.setPassword(password);

                    // dir listing
                    address.setDirListStyle(getParameterAndResolveAppVar(RuntimeConfiguration.FTP_DIR_LIST_STYLE, parms, appVars));
                    address.setUseUserDefinedHeuristics(((Boolean)parms.get(RuntimeConfiguration.FTP_USE_USER_DEFINED_DIR_LIST_STYLE)).booleanValue());
                    address.setUserDefDirListStyle(getParameterAndResolveAppVar(RuntimeConfiguration.FTP_USER_DEFINED_DIR_LIST_STYLE, parms, appVars));
                    address.setUserDefDirListHeuristics(getParameterAndResolveAppVar(RuntimeConfiguration.FTP_USER_DEFINED_DIR_LIST_STYLE_CFG, parms, appVars));
                    
                    address.setPersistenceBaseLocation(getParameterAndResolveAppVar(RuntimeConfiguration.FTP_PERSISTENCE_BASE_DIR, parms, appVars));

                    String secType = getParameterAndResolveAppVar(RuntimeConfiguration.FTP_SEC_TYPE, parms, appVars);
                    address.setSecureFTPType(secType);
                    if ( secType != null & !secType.equals(FtpFileConfigConstants.FTP_SECURE_NONE)) {
                        // either implicitSSL or explicitSSL
                        address.setKeyStore(getParameterAndResolveAppVar(RuntimeConfiguration.FTP_KEY_STORE, parms, appVars));
                        address.setKeyStorePassword(getParameterAndResolveAppVar(RuntimeConfiguration.FTP_KEY_STORE_PASSWORD, parms, appVars));
                        address.setTrustStore(getParameterAndResolveAppVar(RuntimeConfiguration.FTP_TRUST_STORE, parms, appVars));
                        address.setTrustStorePassword(getParameterAndResolveAppVar(RuntimeConfiguration.FTP_TRUST_STORE_PASSWORD, parms, appVars));
                        address.setKeyAlias(getParameterAndResolveAppVar(RuntimeConfiguration.FTP_KEY_ALIAS, parms, appVars));
                        address.setKeyPassword(getParameterAndResolveAppVar(RuntimeConfiguration.FTP_KEY_PASSWORD, parms, appVars));
                        address.setEnableCCC(((Boolean)parms.get(RuntimeConfiguration.FTP_ENABLE_CCC)).booleanValue());
                    }
                } else if ( !resolveAppCfg ) {
                    // at deploy time - it is allowed that an SU references an app cfg
                    // that is not defined yet
                    appCfgs.put(appCfgName, null);
                } else {
                    // error - dangling app config reference
                    throw new Exception("Unresolved Application Configuration Reference: " + appCfgName);
                }
            }
        }
        else {
            // warn that inconsistency between Endpoint info parsed from jbi.xml and
            // collected from WSDLs...
            if ( mLogger.isLoggable(Level.WARNING) )
                mLogger.log(Level.WARNING, "Inconsistency between endpoint info from jbi.xml and wsdls, can not find end point: service name=[" + ep.getServiceName() + "] endpoint name = [" + ep.getEndpointName() + "]");
        }
    }

    private String getParameterAndResolveAppVar(String parmName, Map parms, Map appVars) throws Exception {
        return FTPExtSerializer.resolveVars(parmName, (String)parms.get(parmName), appVars);
    }

    private void getQOSConfigurations (Endpoint ep,
                                       EndpointIdentifier endpointIdentifier) throws DeploymentException {

        Map<EndpointInfo, List<ServiceQuality>> qosMap = mChannel.getLookup().getQoSAssembly(mId).getServiceQualities();

        if (qosMap != null && qosMap.size() > 0) {
            // Until there's a total transitioning to use the common-util, there's a need to
            // create EndpointInfo using EndpointIndentifier
            EndpointInfo endpointInfo = new EndpointInfo (false, 
                                                          endpointIdentifier.getEndpointName(), 
                                                          null,
                                                          endpointIdentifier.getServiceName(),
                                                          null);
            List<ServiceQuality> qoss = qosMap.get(endpointInfo);
            if (qoss == null) {
                // there is no QoS configuration on this endpoint
                return;
            }
            Iterator<ServiceQuality> qossIter = qoss.iterator();
            while (qossIter.hasNext()) {
                ServiceQuality qos = qossIter.next();
                // Gather throttling config
                if (qos instanceof ThrottlingConfig) {
                    ThrottlingConfig throttleConfig = (ThrottlingConfig)qos;
                    ep.setMaxConcurrencyLimit(throttleConfig.getMaxConcurrencyLimit());
                } else if (qos instanceof RedeliveryConfig) {   // get the redelivery configuration
                    RedeliveryConfig redeliveryConfig = (RedeliveryConfig)qos;
                    ep.setRedeliveryConfiguration(redeliveryConfig);
                }
                // no other QoS services at the moment...
            }
        }
    }

    public String getServiceUnitPath() {
        return mSuPath;
    }
}
