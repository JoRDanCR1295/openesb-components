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
package com.sun.jbi.filebc;

import com.sun.encoder.Encoder;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.filebc.Endpoint.EndpointType;
import com.sun.jbi.filebc.extensions.FileAddress;
import com.sun.jbi.filebc.packaging.EndpointConfiguration;
import com.sun.jbi.filebc.packaging.EndpointConfigurationFactory;
import com.sun.jbi.filebc.packaging.WSDLConfigurations;
import com.sun.jbi.filebc.util.AlertsUtil;
import com.sun.jbi.filebc.util.OutputFilenameFormatter;
import com.sun.jbi.filebc.util.WSDLUtilities;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.management.descriptor.ConfigurationException;
import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;

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

import javax.jbi.JBIException;
import javax.jbi.management.DeploymentException;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import javax.wsdl.WSDLException;

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
 * com.sun.jbi.filebc.packaging package should need to change.
 *
 */
public class ServiceUnitImpl implements ServiceUnit {

    private static Logger mLogger = Messages.getLogger(ServiceUnitImpl.class);
    private static Messages mMessages = Messages.getMessages(ServiceUnitImpl.class);
    private String mId;
    private String mSuPath;
    private ComponentContext mContext;
    private StatusProviderHelper mStatusProviderHelper;
    private RuntimeConfigurationMBean mRuntimeConfig;
    private InboundReceiver mInboundReceiver;
    private Collection mEndpoints;
    private WSDLConfigurations mWSDLConfig;
    private EndpointConfiguration mEndpointConfig;
    private MessagingChannel mChannel;

    //private Map<EndpointInfo, List<ServiceQuality>> mEndpointQos;
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
        mChannel = FileComponentContext.getInstance().getBindingChannel();
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
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "SU_Called_SU_deploy");
        }
        try {
            mEndpointConfig = initializeEndpointConfig();
            Map envVariableMap = mWSDLConfig.parseForEnvironmentVariables(mEndpointConfig.endpoints(), mRuntimeConfig.retrieveApplicationVariablesMap());
            if (envVariableMap.size() > mRuntimeConfig.retrieveApplicationVariablesMap().size()) {
                // number of environment variable tokens used in File WSDLs
                // is greater than the ones defined
                mRuntimeConfig.updateApplicationVariablesMap(envVariableMap);
            }
        } catch (Exception ex) {
            String errMsg = mMessages.getString("FILEBC-E00301.Service_unit_deploy_failed_exception",
                    new Object[]{mId, ex.getLocalizedMessage()});
            mLogger.log(Level.SEVERE, errMsg, ex);
            AlertsUtil.getAlerter().critical(errMsg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    mId,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00301");
            throw new JBIException(errMsg, ex);
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
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "SU_Called_SU_init");
        }
        Endpoint currEndpoint = null;
        HashSet activatedOutboundEndpoints = new HashSet();

        // for unit test only
        if (mChannel == null) {
            return;
        }

        mChannel.installServiceQualities(mId, mSuPath);

        Map<String, EndpointIdentifier> svcs = null;
        Logger logger = null;

        try {
            // Push the context
            logger = Logger.getLogger("com.sun.EnterContext");
            logger.fine(getServiceUnitId() + "-" + "init");

            svcs = getEndpointIdentifiers(mSuPath);

            if (svcs == null || svcs.size() == 0) {
                String msg = mMessages.getString("FILEBC-W00306.No_endpoints_to_create",
                        new Object[]{mContext.getComponentName(), mId});
                mLogger.log(Level.WARNING, "FILEBC-W00306.No_endpoints_to_create",
                        new Object[]{mContext.getComponentName(), mId});
                AlertsUtil.getAlerter().warning(msg,
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        mId,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-W00306");
                return;
            }

            Map appConfigObjectMap = mRuntimeConfig.retrieveApplicationConfigurationsMap();
            Map<String, String> mapEPAppConfigNames = new HashMap<String, String>();

            Iterator svcDescKeys = svcs.keySet().iterator();

            while (svcDescKeys.hasNext()) {
                EndpointIdentifier epDesc = svcs.get(svcDescKeys.next().toString());
                String configName = epDesc.getApplicationConfigurationName();

                // this application configuration name is not yet defined in the config MBean,
                // add it to the application configuration data structure
                if (configName != null && !"".equals(configName) &&
                        !appConfigObjectMap.containsKey(configName)) {
                    //TODO: should we add the new (not yet defined) config object?
//                    appConfigObjectMap.put(configName, new ApplicationConfigurationObject());
//                    mRuntimeConfig.updateApplicationConfigurationsMap(appConfigObjectMap);
                }

                if (configName != null && !"".equals(configName)) {
                    mapEPAppConfigNames.put(epDesc.getEndpointName(), configName);
                }
            }

            // Prepare for endpoint configuration map if deploy() hasn't been called yet
            if (mEndpointConfig == null) {
                mEndpointConfig = initializeEndpointConfig();
            }

            //mEndpoints = mWSDLConfig.parse(mEndpointConfig.endpoints(), mRuntimeConfig.retrieveEnvVariablesMap());
            mEndpoints = mWSDLConfig.parse(mEndpointConfig.endpoints(), mRuntimeConfig.retrieveApplicationVariablesMap());

            if (mEndpoints.size() == 0) {
                mLogger.log(Level.WARNING, "FILEBC-W00307.Service_unit_has_no_service",
                        new Object[]{mContext.getComponentName(), mId});
                AlertsUtil.getAlerter().warning(mMessages.getString("FILEBC-W00307.Service_unit_has_no_service", new Object[]{mContext.getComponentName(), mId}),
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        mId,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-W00307");
            }

            // Initialize the state of all these endpoints.
            Iterator it = mEndpoints.iterator();
            while (it.hasNext()) {
                currEndpoint = (Endpoint) it.next();
                resolveAppConfig(currEndpoint, svcs, true);

                // Initializes encoder map for the current endpoint.
                // This also triggers the encoder codegen if the
                // specified encoder is not cached.
                Map encoderMapping = mWSDLConfig.getPartEncoderMapping(currEndpoint.getDefinition(),
                        currEndpoint.getServiceName().toString(),
                        currEndpoint.getEndpointName(),
                        currEndpoint.getEndpointType(),
                        currEndpoint.getFileOperations(),
                        currEndpoint.getOperationMsgExchangePattern());
                currEndpoint.setMessagePartEncoderMapping(encoderMapping);

                // Application Configuration associated with each endpoint
                // is processed here.
                String configName = mapEPAppConfigNames.get(currEndpoint.getEndpointName());
                if (configName != null && !"".equals(configName)) {
                    // there is a app config object defined for the endpoint
                    // use the address defined for the app config object instead
                    // of the one defined in the WSDL
                    ApplicationConfigurationObject aco = (ApplicationConfigurationObject) appConfigObjectMap.get(configName);
                    if (aco == null) {
                        throw new JBIException(mMessages.getString("FILEBC-E00302.Application_config_object_not_defined", new Object[]{configName, currEndpoint.getEndpointName()}));
                    }
                    String fileDir = aco.getFileDirectory();
                    if (fileDir == null) {
                        throw new JBIException(mMessages.getString("FILEBC-E00303.Application_config_object_has_no_dir", configName));
                    }

                    // over-ride file address from WSDL.
//                    currEndpoint.getFileAddress().setFileDirectory(fileDir);
//                    currEndpoint.getFileAddress().setRelativePath(aco.getRelativePath());
//                    currEndpoint.getFileAddress().setPathRelativeTo(aco.getPathRelativeTo());
//                    currEndpoint.getFileAddress().setLockName(aco.getLockName());
//                    currEndpoint.getFileAddress().setWorkArea(aco.getWorkArea());
//                    currEndpoint.getFileAddress().setSeqName(aco.getSeqName());
//                    currEndpoint.getFileAddress().setPersistenceBaseLoc(aco.getPersistenceBaseLoc());
//                    currEndpoint.getFileAddress().setRecursive(aco.getRecursive());
//                    currEndpoint.getFileAddress().setExcludeRegex(aco.getRecursiveExclude());
                }

                getQOSConfigurations(currEndpoint);

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
            String errMsg;
            if (currEndpoint != null) {
                errMsg = mMessages.getString("FILEBC-E00304.Service_unit_init_failed_exception",
                        new Object[]{mId, currEndpoint.getServiceName(), currEndpoint.getEndpointName(), ex.getLocalizedMessage()});
                mLogger.log(Level.SEVERE, errMsg, ex);
                AlertsUtil.getAlerter().critical(errMsg,
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        mId,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-E00304");

            } else {
                errMsg = mMessages.getString("FILEBC-E00308.Service_unit_init_failed_exception",
                        new Object[]{mId, ex.getLocalizedMessage()});
                mLogger.log(Level.SEVERE, errMsg, ex);
                AlertsUtil.getAlerter().critical(errMsg,
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        mId,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-E00308");
            }

            Iterator it2 = activatedOutboundEndpoints.iterator();
            while (it2.hasNext()) {
                deactivateOutboundEndpoint((Endpoint) it2.next());
            }

            throw new JBIException(errMsg);
        } finally {
            // Pop the context
            if (logger != null) {
                Logger.getLogger("com.sun.ExitContext").fine(getServiceUnitId() + "-" + "init");
            }

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
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "SU_Called_SU_start");
        }
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
            String errMsg;
            if (currEndpoint != null) {
                errMsg = mMessages.getString("FILEBC-E00305.Service_unit_start_failed_exception",
                        new Object[]{mId, currEndpoint.getServiceName(), currEndpoint.getEndpointName(), ex.getLocalizedMessage()});
                mLogger.log(Level.SEVERE, errMsg, ex);
                AlertsUtil.getAlerter().critical(errMsg,
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        mId,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-E00305");
            } else {
                errMsg = mMessages.getString("FILEBC-E00306.Service_unit_start_failed_exception",
                        new Object[]{mId, ex.getLocalizedMessage()});
                mLogger.log(Level.SEVERE, errMsg, ex);
                AlertsUtil.getAlerter().critical(errMsg,
                        FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                        mId,
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING,
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "FILEBC-E00306");
            }
            Iterator it2 = activatedInboundEndpoints.iterator();
            while (it2.hasNext()) {
                stopConsuming((Endpoint) it2.next());
            }
            throw new JBIException(errMsg);
        }
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "SU_Complete_start_SU", getServiceUnitId());
        }
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
            currEndpoint = (Endpoint) it.next();
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
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "SU_Call_shutdown");
        }

        // for unit test only
        if (mChannel == null) {
            return;
        }
        mChannel.uninstallServiceQualities(mId);

        Iterator it = mEndpoints.iterator();
        Endpoint currEndpoint;

        while (it.hasNext()) {
            currEndpoint = (Endpoint) it.next();
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
                reporting.removeConsumingEndpoints(new String[]{uniqueName});
            } else {
                uniqueName =
                        mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
                reporting.removeProvisioningEndpoints(new String[]{uniqueName});
                deactivateOutboundEndpoint(currEndpoint);
            }

            // Dispose the encoders
            Map encoders = currEndpoint.getMessagePartEncoderMapping();
            for (Iterator it2 = encoders.values().iterator(); it2.hasNext();) {
                Encoder aEncoder = (Encoder) it2.next();
                // Dispose the encoder. This also
                // triggers the removal of its codegen artifacts
                aEncoder.dispose();
            }

            // Dispose cached maps for file name patterns
            clearCachedMaps(currEndpoint);
        }

        // Clear the encoder cache
        mWSDLConfig.clearEncoderCache();
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "SU_Complete_shutdown", getServiceUnitId());
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
            String errMsg = mMessages.getString("FILEBC-E00307.Message_processor_start_failed_exception",
                    new Object[]{endpoint.getEndpointName(), ex.getLocalizedMessage()});
            mLogger.log(Level.SEVERE, errMsg, ex);
            AlertsUtil.getAlerter().critical(errMsg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    mId,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00307");
            throw new JBIException(errMsg, ex);
        } catch (IBProcCreationException ex) {
            String errMsg = mMessages.getString("FILEBC-E00307.Message_processor_start_failed_exception",
                    new Object[]{endpoint.getEndpointName(), ex.getLocalizedMessage()});
            mLogger.log(Level.SEVERE, errMsg, ex);
            AlertsUtil.getAlerter().critical(errMsg,
                    FileBindingLifeCycle.SHORT_DISPLAY_NAME,
                    mId,
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING,
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "FILEBC-E00307");
            throw new JBIException(errMsg, ex);
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
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "SU_Activate_outbound_EP",
                    new Object[]{endpoint.getServiceName(), endpoint.getEndpointName()});
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

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "SU_Deactivate_outbound_EP",
                    new Object[]{endpoint.getServiceName(), endpoint.getEndpointName()});
        }

    }

    private void clearCachedMaps(Endpoint endpoint) {
        // clear resource maps for outbound endpoints if any;
        OutputFilenameFormatter.removeOutputFileNamePattern(endpoint.getEPUUID());
    }

    private EndpointConfiguration initializeEndpointConfig() throws Exception {
        EndpointConfiguration endpointConfig = mEndpointConfig;
        if (endpointConfig == null) {
            endpointConfig =
                    EndpointConfigurationFactory.getEndpointConfiguration(mSuPath);
        }

        return endpointConfig;
    }

    private void getQOSConfigurations(Endpoint endpoint) throws DeploymentException {
        // Until there's a total transitioning to use the common-util,
        // there's a need to
        // create EndpointInfo using EndpointIndentifier
        Map<EndpointInfo, List<ServiceQuality>> qosMap = mChannel.getLookup().getQoSAssembly(mId).getServiceQualities();

        if (qosMap != null && qosMap.size() > 0) {
            EndpointInfo endpointInfo = new EndpointInfo(false,
                    endpoint.getEndpointName(), null,
                    endpoint.getServiceName(), null);

            List<ServiceQuality> qoss = qosMap.get(endpointInfo);

            if (qoss == null) {
                // there is no QoS configuration on this endpoint
                return;
            }

            Iterator<ServiceQuality> qossIter = qoss.iterator();

            while (qossIter.hasNext()) {
                ServiceQuality qos = qossIter.next();

                if (qos instanceof ThrottlingConfig) {
                    ThrottlingConfig throttleConfig = (ThrottlingConfig) qos;
                    endpoint.setMaxConcurrencyLimit(throttleConfig.getMaxConcurrencyLimit());
                } else if (qos instanceof RedeliveryConfig) {
                    // get the redelivery configuration
                    RedeliveryConfig redeliveryConfig = (RedeliveryConfig) qos;
                    endpoint.setRedeliveryConfiguration(redeliveryConfig);
                }
                // no other QoS services at the moment...
            }
        }
    }

    /**
     * package protected method. Used solely for JUnit test purposes
     */
    void setEndpoints(List endpoints) {
        mEndpoints = endpoints;
    }

    /**
     * helper to resolve app vars referenced in app config
     * 
     * @param ep
     * @param svcs
     * @param resolveAppCfg
     * @throws Exception
     */
    private void resolveAppConfig(Endpoint ep, Map svcs, boolean resolveAppCfg) throws Exception {
        Map appCfgs = mRuntimeConfig.retrieveApplicationConfigurationsMap();
        Map appVars = mRuntimeConfig.retrieveApplicationVariablesMap();
        EndpointIdentifier match = (EndpointIdentifier) svcs.get(ep.getServiceName() + "|" + ep.getEndpointName());
        if (match != null) {
            String appCfgName = match.getApplicationConfigurationName();
            if (appCfgName != null && appCfgName.trim().length() > 0) {
                ApplicationConfigurationObject parms = (ApplicationConfigurationObject) appCfgs.get(appCfgName);
                if (parms != null) {
                    FileAddress address = ep.getFileAddress();
                    if (address == null) {
                        throw new Exception("Missing file:address element in endpoint : service name=[" + ep.getServiceName() + "] endpoint name=[" + ep.getEndpointName() + "]");
                    }
                    // note boolean param can not reference ${xyz}, so overwrite happens directly
                    // al the rest are string type
                    String appCfgParam = getParameterAndResolveAppVar(RuntimeConfiguration.APPLICATION_CONFIG_PROPERTY_FILEDIR, parms.getFileDirectory(), appVars);
                    address.setFileDirectory(appCfgParam);
                    //appCfgParam = getParameterAndResolveAppVar(RuntimeConfiguration.APPLICATION_CONFIG_PROPERTY_RELATIVEPATH, parms.getRelativePath(), appVars);
                    address.setRelativePath(parms.getRelativePath());
                    appCfgParam = getParameterAndResolveAppVar(RuntimeConfiguration.APPLICATION_CONFIG_PROPERTY_PATHRELATIVETO, parms.getPathRelativeTo(), appVars);
                    address.setPathRelativeTo(appCfgParam);
                    appCfgParam = getParameterAndResolveAppVar(RuntimeConfiguration.APPLICATION_CONFIG_PROPERTY_LOCKNAME, parms.getLockName(), appVars);
                    address.setLockName(appCfgParam);
                    appCfgParam = getParameterAndResolveAppVar(RuntimeConfiguration.APPLICATION_CONFIG_PROPERTY_WORKAREA, parms.getSeqName(), appVars);
                    address.setWorkArea(appCfgParam);
                    appCfgParam = getParameterAndResolveAppVar(RuntimeConfiguration.APPLICATION_CONFIG_PROPERTY_SEQNAME, parms.getWorkArea(), appVars);
                    address.setSeqName(appCfgParam);
                    appCfgParam = getParameterAndResolveAppVar(RuntimeConfiguration.APPLICATION_CONFIG_PROPERTY_PERSIST_BASE, parms.getPersistenceBaseLoc(), appVars);
                    address.setPersistenceBaseLoc(appCfgParam);
                    //appCfgParam = getParameterAndResolveAppVar(RuntimeConfiguration.APPLICATION_CONFIG_PROPERTY_REURSIVE, parms.getRecursive(), appVars);
                    address.setRecursive(parms.getRecursive());
                    appCfgParam = getParameterAndResolveAppVar(RuntimeConfiguration.APPLICATION_CONFIG_PROPERTY_EXCLUDE_REGEX, parms.getRecursiveExclude(), appVars);
                    address.setExcludeRegex(appCfgParam);
                } else if (!resolveAppCfg) {
                    // at deploy time - it is allowed that an SU references an app cfg
                    // that is not defined yet
                    appCfgs.put(appCfgName, null);
                } else {
                    // error - dangling app config reference
                    throw new Exception("Unresolved Application Configuration Reference: Application Configuration Name = [" + appCfgName + "], Endpoint = [" + ep.getUniqueName() + "]");
                }
            }
        } else {
            // warn that inconsistency between Endpoint info parsed from jbi.xml and
            // collected from WSDLs...
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, "Inconsistency between endpoint info from jbi.xml and wsdls, can not find end point: service name=[" + ep.getServiceName() + "] endpoint name = [" + ep.getEndpointName() + "]");
            }
        }
    }

    private String getParameterAndResolveAppVar(String parmName, String parmValue, Map appVars) throws Exception {
        return resolveVars(parmName, parmValue, appVars);
    }

    private String resolveVars(String name, String val, Map vars) throws WSDLException {
        String result = val;
        if (result != null && vars != null) {
            if (WSDLUtilities.hasMigrationAppVarRef(val)) {
                // attribute contains app var reference(s)
                Object[] refs = WSDLUtilities.getAppVariableNames(val);
                for (int i = 0; i < refs.length; i++) {
                    String[] varDesc = (String[]) vars.get(refs[i]);
                    if (varDesc == null || varDesc.length != 2) {
                        throw new WSDLException("INVALID_WSDL", mMessages.getString("FILEBC-E00309.Invalid_app_var_ref_no_def", new Object[]{refs[i], val, name}));
                    } else {
                        // check if the de-referenced value has ${ in it
                        String varVal = varDesc[0];
                        if (varVal == null) {
                            throw new WSDLException("INVALID_WSDL",
                                    mMessages.getString("FILEBC-E00310.Invalid_attr_value_NULL", new Object[]{refs[i], name}));
                        }
                        if (varVal.indexOf("${") >= 0) {
                            throw new WSDLException("INVALID_WSDL", mMessages.getString("FILEBC-E00311.Invalid_var_value_contains_var_ref", new Object[]{name, val, refs[i], varVal}));
                        }
                        result = result.replace("${" + refs[i] + "}", varVal);
                    }
                }
            }
        }
        if (result != null && WSDLUtilities.hasMigrationAppVarRef(result)) {
            // still has ref un-resolved
            throw new WSDLException("INVALID_WSDL", mMessages.getString("FILEBC-E00312.Invalid_attr_value_contains_unresolvable_ref", new Object[]{val, name}));
        }
        return result;
    }

    private Map<String, EndpointIdentifier> getEndpointIdentifiers(String suPath) throws ConfigurationException {
        Map<String, EndpointIdentifier> eps = null;
        SUDescriptorSupport descSupport =
                new SUDescriptorSupport(suPath);
        EndpointIdentifier[] svcs = descSupport.getServices();
        if (svcs != null && svcs.length > 0) {
            eps = new HashMap<String, EndpointIdentifier>();
            for (int i = 0; i < svcs.length; i++) {
                eps.put(svcs[i].getServiceName() + "|" + svcs[i].getEndpointName(), svcs[i]);
            }
        }
        return eps;
    }
}
