
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
 * @(#)LDAPBindingDeployer.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ldapbc;

import java.util.Collection;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.ldapbc.extensions.LDAPAddress;
import com.sun.jbi.ldapbc.extensions.LDAPBinding;
import com.sun.jbi.ldapbc.extensions.LDAPOperation;
import com.sun.jbi.ldapbc.extensions.LDAPOperationOutput;
import com.sun.jbi.ldapbc.util.AlertsUtil;
import com.sun.jbi.ldapbc.util.LdapConnection;
import com.sun.jbi.ldapbc.util.LdapHelper;
import com.sun.jbi.ldapbc.configuration.AppConfigAddressVisitor;
import com.sun.jbi.ldapbc.configuration.ApplicationConfigurationField;
import com.sun.jbi.ldapbc.configuration.Visitor;
import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;

import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.xml.sax.EntityResolver;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.MessagingException;

import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.PortType;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.exolab.castor.xml.schema.Schema;
import org.w3c.dom.Document;

/**
 *
 *LDAPBindingDeployer
 */
public class LDAPBindingDeployer implements ServiceUnitManager {

    private static final Messages mMessages = Messages.getMessages(LDAPBindingDeployer.class);
    private static final Logger mLogger = Messages.getLogger(LDAPBindingDeployer.class);
    private final Map<String, File> mDeployedIds;
    private final Map<String, EndpointImpl[]> mDeployedEndpoints;
    private final ComponentContext mContext;
    private final DeployHelper mHelper;
    private final LDAPBindingLifeCycle mLifeCycle;
    private String mId;

    /** Creates a new instance of DeployerMBean
     * @param context
     * @param lifeCycle 
     */
    public LDAPBindingDeployer(final ComponentContext context,
            final LDAPBindingLifeCycle lifeCycle) {
        mContext = context;
        mDeployedIds = new HashMap<String, File>();
        mDeployedEndpoints = new HashMap<String, EndpointImpl[]>();
        mHelper = new DeployHelper();
        mLifeCycle = lifeCycle;
    }

    /**
     * Initiate a BC Deployment.
     *
     * @param suId -
     *            ID of the ASA being deployed
     * @param asaFilePath 
     * @return 
     * @throws javax.jbi.management.DeploymentException
     */
    public String deploy(final String suId, final String asaFilePath)
            throws DeploymentException {
        String retMsg = null;
        final String taskName = "deploy";
        if (LDAPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            LDAPBindingDeployer.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00201.LDBD_Deploy_SU",
                    new Object[]{suId, asaFilePath}));
        }
        retMsg = createSuccessMessage(taskName, mContext.getComponentName());
        return retMsg;
    }

    /**
     * @param suId
     * @param suPath
     * @throws DeploymentException
     */
    public void init(final String suId, final String suPath)
            throws DeploymentException {
        final String taskName = "init";
        if (LDAPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            LDAPBindingDeployer.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00202.LDBD_Init_SU",
                    new Object[]{suId, suPath}));
        }

        try {
            // Prepare for start if the deployment hasn't been processed yet.
            if (mDeployedIds.get(suId) == null) {
                final StatusProviderHelper statusProviderHelper = mLifeCycle.getStatusProviderHelper();
                final RuntimeConfigurationMBean runtimeConfigMBean = mLifeCycle.getRuntimeConfigurationMBean();
                processDeployment(suId, suPath, statusProviderHelper, runtimeConfigMBean);
            }

            if (LDAPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
                LDAPBindingDeployer.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00203.LDBD_Initialize_SU_Succeeded", new Object[]{suId}));
            }
        } catch (final DeploymentException ex) {
            final String errMsg = LDAPBindingDeployer.mMessages.getString("LDAPBC-R00204.LDBD_Initialize_SU_Failed",
                    new Object[]{ex.getMessage()});
            LDAPBindingDeployer.mLogger.log(Level.SEVERE, errMsg);

            final String exMsg = createExceptionMessage(mContext.getComponentName(),
                    taskName, "FAILED", "LDAPBC_INIT_1", null, errMsg, ex);
            throw new DeploymentException(exMsg, ex);
        }
    }

    /**
     * Process a deployment, validating it and preparing to start it..
     *
     * @param suId
     *            the name of the Service Unit being initialized.
     * @param path
     *            the full path to the Service Unit artifact root directory.
     * @throws javax.jbi.management.DeploymentException
     *             if the Service Unit is not deployed, or is in an incorrect
     *             state.
     */
    void processDeployment(final String suId, final String path,
            final StatusProviderHelper statusProviderHelper, final RuntimeConfigurationMBean runtimeConfigMBean) throws DeploymentException {
        final String taskName = "processDeployment";

        try {
            final File asaDir = new File(path);
            mHelper.deploy(asaDir, suId, mContext, statusProviderHelper, runtimeConfigMBean);

            final EndpointImpl[] endPointsArr = mDeployedEndpoints.get(suId);

            mLifeCycle.activateEndpoints(endPointsArr);
            setEndpointsStatus(endPointsArr, EndpointImpl.STATUS_RUNNING);
        } catch (final FileNotFoundException ex) {
            final String exMsg = createExceptionMessage(mContext.getComponentName(),
                    taskName, "FAILED", "FBC_PROCESS_2", suId,
                    "Processing deployment error: " + ex.getMessage(), ex);
            throw new DeploymentException(exMsg);
        } catch (final IOException ex) {
            final String exMsg = createExceptionMessage(mContext.getComponentName(),
                    taskName, "FAILED", "FBC_PROCESS_3", suId,
                    "Processing deployment error: " + ex.getMessage(), ex);
            throw new DeploymentException(exMsg);
        } catch (final MessagingException ex) {
            final String exMsg = createExceptionMessage(mContext.getComponentName(),
                    taskName, "FAILED", "FBC_PROCESS_4", suId,
                    "Processing deployment error: " + ex.getMessage(), ex);
            throw new DeploymentException(exMsg);
        } catch (final Exception e) {
            final String exMsg = createExceptionMessage(mContext.getComponentName(),
                    taskName, "FAILED", "FBC_PROCESS_4", suId,
                    "Processing deployment error: " + e.getMessage(), e);
            throw new DeploymentException(exMsg);
        }
    }

    /**
     * @param suId
     * @throws DeploymentException
     */
    public void start(final String suId) throws DeploymentException {
        if (LDAPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            LDAPBindingDeployer.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00205.LDBD_Start_SU_Called", new Object[]{suId}));
        }

        final EndpointImpl[] endPointsArr = mDeployedEndpoints.get(suId);
        // establish LDAP connections
//        for (int i = 0; i < endPointsArr.length; i++) {
//            EndpointBean epb = endPointsArr[i];
//            LdapConnection conn = (LdapConnection) epb.getValueObj(EndpointBean.LDAP_CONNECTION);
//            conn.getConnection();
//        }
        setEndpointsStatus(endPointsArr, EndpointImpl.STATUS_RUNNING);

        if (LDAPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            LDAPBindingDeployer.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00206.LDBD_Start_SU_Succeeded"));
        }
    }

    /**
     * @param suId
     * @throws DeploymentException
     */
    public void stop(final String suId) throws DeploymentException {
        if (LDAPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            LDAPBindingDeployer.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00207.LDBD_Stop_SU_Called", new Object[]{suId}));
        }

        final EndpointImpl[] endPointsArr = mDeployedEndpoints.get(suId);
        for (int i = 0; i < endPointsArr.length; i++) {
            EndpointImpl epb = endPointsArr[i];
            LdapConnection conn = (LdapConnection) epb.getValueObj(EndpointImpl.LDAP_CONNECTION);
//            try {
            if (null != conn) {
                conn.closeConnection();
            }
//            } catch (NamingException ex) {
//                Logger.getLogger(LDAPBindingDeployer.class.getName()).log(Level.SEVERE, null, ex);
//            }
        }

        setEndpointsStatus(endPointsArr, EndpointImpl.STATUS_STOPPED);

        if (LDAPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            LDAPBindingDeployer.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00208.LDBD_Stop_SU_Succeeded", new Object[]{suId}));
        }
    }

    /**
     * @param suId
     * @throws DeploymentException
     */
    public void shutDown(final String suId) throws DeploymentException {
        final String taskName = "shutDown";

        if (LDAPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            LDAPBindingDeployer.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00209.LDBD_Shutdown_SU", new Object[]{suId}));
        }

        final File asaDir = mDeployedIds.get(suId);

        if (asaDir == null) {
            LDAPBindingDeployer.mLogger.info("Service unit " + mContext.getComponentName() +
                    " is not deployed.");
        } else {
            final EndpointImpl[] endPointsArr = mDeployedEndpoints.get(suId);

            try {
                mLifeCycle.deactivateEndpoints(endPointsArr);
            } catch (final MessagingException ex) {
                final String errMsg = LDAPBindingDeployer.mMessages.getString("LDAPBC-E00201.LDBD_Error_Shutdown_SU",
                        new Object[]{ex.getMessage()});
                LDAPBindingDeployer.mLogger.log(Level.SEVERE, errMsg);

                final String exMsg = createExceptionMessage(mContext.getComponentName(),
                        taskName, "FAILED", "LDAPBC_SHUTDOWN_1", null, errMsg,
                        ex);
                throw new DeploymentException(exMsg, ex);
            }

            setEndpointsStatus(endPointsArr, EndpointImpl.STATUS_SHUTDOWN);

            final StatusProviderHelper statusProviderHelper = mLifeCycle.getStatusProviderHelper();
            mHelper.undeploy(suId, asaDir, statusProviderHelper);
        }
    }

    /**
     * Cancel a Service Deployment. If the deployment is in use (has
     * dependencies), then this operation may fail.
     *
     * @param name -
     *            name of the service unit
     * @param root -
     *            root of the service unit
     * @return 
     */
    public String undeploy(final String name, final String root) throws DeploymentException {
        String retMsg = null;
        final String taskName = "undeploy";

        if (LDAPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            LDAPBindingDeployer.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00211.LDBD_Undeploy_SU",
                    new Object[]{name, root}));
        }

        retMsg = createSuccessMessage(taskName, mContext.getComponentName());

        return retMsg;
    }

    /**
     * Keep track of the running status of each endpoint. This allows for quick
     * look-up of the state and provides for extensibility where an individual
     * endpoint could be controlled rather than the whole SU.
     */
    void setEndpointsStatus(final EndpointImpl[] endpoints, final String status) {
        if (endpoints != null) {
            for (EndpointImpl element : endpoints) {
                element.setValue(EndpointImpl.STATUS, status);
            }
        }
    }

    private String createSuccessMessage(final String taskName, final String componentName) {
        final JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);

        final String retMsg = msgBuilder.createSuccessMessage(taskName);

        return retMsg;
    }

    private String createExceptionMessage(final String componentName,
            final String taskName, final String status, final String locToken, final String locParam,
            final String locMessage, final Throwable exObj) {
        final JBITaskMessageBuilder msgBuilder = new DefaultJBITaskMessageBuilder();
        msgBuilder.setComponentName(componentName);

        final String retMsg = msgBuilder.createExceptionMessage(taskName, locToken,
                locMessage, locParam, exObj);

        return retMsg;
    }

    public EndpointImpl[] getEndpoint(String serviceName, String endpointName, int endpointType) {

        EndpointImpl[] endpoint = null;

        endpoint = mDeployedEndpoints.get(getEndpointKey(serviceName, endpointName, endpointType));
        return endpoint;
    }

    private String getEndpointKey(EndpointImpl endpoint) {
        return getEndpointKey(endpoint.getServiceName().toString(), endpoint.getEndpointName(),
                endpoint.getEndpointType());
    }

    private String getEndpointKey(String serviceName, String endpointName, int endpointType) {
        return mId + serviceName + endpointName + EndpointImpl.endpointTypeToString(endpointType);
    }

    /**
     * Retrieves the set of Endpoints handled by this ServiceUnit.
     *
     * @return       the set of Endpoints
     */
    public Map<String, EndpointImpl[]> getEndpoints() {
        return mDeployedEndpoints;
    }

    synchronized public void resume(EndpointImpl ep) throws Exception {
        start(ep);
        sendAlert(ep, mMessages.getString("LDAPBC-R00213.LDBD_Resume_Endpoint"));
    }

    synchronized public void suspend(EndpointImpl ep) throws Exception {
        stop(ep);
        sendAlert(ep, mMessages.getString("LDAPBC-R00212.LDBD_Suspend_Endpoint"));
    }

    private void start(EndpointImpl endpoint) throws Exception {

        String direction = endpoint.getEndpointType() == EndpointImpl.OUTBOUND ? "provisioning" : "consuming";

        endpoint.setState(EndpointImpl.RUNNING);

    }

    private void stop(EndpointImpl endpoint) throws Exception {

        String direction = endpoint.getEndpointType() == EndpointImpl.OUTBOUND ? "provisioning" : "consuming";
        endpoint.setState(EndpointImpl.STOPPED);

    }

    private void sendAlert(EndpointImpl ep, String str) {
        String msg = mMessages.getString(str,
                new Object[]{ep.toString()});
        mLogger.log(Level.WARNING, msg);
        AlertsUtil.getAlerter().warning(msg, AlertsUtil.SUN_LDAP_BINDING, mId,
                AlertsUtil.getServerType(), AlertsUtil.COMPONENT_TYPE_BINDING,
                NotificationEvent.OPERATIONAL_STATE_RUNNING,
                NotificationEvent.EVENT_TYPE_ALERT, "LDAPBC-W0732");
    }

    /**
     * List all wsdl files in the currentDir and below
     * @author Administrator
     *
     */
    private class DeployHelper {

        /**
         *
         * @param asaDir
         * @param id
         * @param compContext
         * @param statusProviderHelper
         * @throws IOException
         * @throws DeploymentException
         * @throws javax.jbi.messaging.MessagingException
         */
        public void deploy(final File asaDir, final String id,
                final ComponentContext compContext,
                final StatusProviderHelper statusProviderHelper, final RuntimeConfigurationMBean runtimeConfiguration)
                throws IOException, DeploymentException,
                javax.jbi.messaging.MessagingException, Exception {
            final String taskName = "deploy";
            Map<String, String[]> envVariableMap =
                    LdapHelper.parseForEnvironmentVariables(asaDir, runtimeConfiguration.retrieveApplicationVariablesMap());
            if (envVariableMap.size() > runtimeConfiguration.countVariables()) {
                // number of environment variable tokens used in File WSDLs
                // is greater than the ones defined
                runtimeConfiguration.updateApplicationVariablesMap(envVariableMap);
            }
            // read all wsdl files to see if
            final FileToDefinitionInfo[] fileToDefs = readAllDefinitions(asaDir, id,
                    taskName, "FBC_DEPLOY_HELPER_4", envVariableMap);

            final List<EndpointImpl> endPoints = new ArrayList<EndpointImpl>();

            if (SUDescriptorSupport.TEMP_SWITCH_ENABLE_JBI_ROUTING) {
                LDAPBindingDeployer.mLogger.info(
                        "About to parse jbi.xml to resolve service end point.");

                EndpointIdentifier[] svcs = null;

                try {
                    final SUDescriptorSupport descSupport = new SUDescriptorSupport(asaDir.getAbsolutePath());
                    svcs = descSupport.getServices();
                } catch (final Exception ex) {
                    final String exMsg = createExceptionMessage(mContext.getComponentName(),
                            taskName, "FAILED", "FBC_DEPLOY_12_1", id,
                            "Error parsing SU descriptor from" +
                            asaDir.getAbsolutePath() + " for " + id, ex);
                    throw new DeploymentException(exMsg);
                }

                final int len = svcs.length;
                for (int i = 0; i < len; i++) {
                    Definition matchedDef = null;
                    File matchedWSDL = null;
                    LDAPBinding ldapBinding = null;
                    LDAPOperation[] ldapOperations = null;
                    LDAPAddress ldapAddress = null;

                    // Document result = null;
                    final EndpointIdentifier epDesc = svcs[i];

                    for (FileToDefinitionInfo element : fileToDefs) {
                        ldapBinding = LdapHelper.getLDAPBinding(element.getDefinition(),
                                epDesc.getServiceName().toString(),
                                epDesc.getEndpointName());

                        if (ldapBinding != null) {
                            matchedDef = element.getDefinition();
                            matchedWSDL = element.getFile();
                            break;
                        }
                    }

                    // If no ldap:binding is defined, the ldapbc does not
                    // service this wsdl
                    if (ldapBinding != null) {
                        ldapAddress = LdapHelper.getLDAPAddress(matchedDef,
                                epDesc.getServiceName().toString(),
                                epDesc.getEndpointName());

                        if (ldapAddress == null) {
                            final String msg = "Missing ldap:address in wsdl for this service " +
                                    epDesc.getServiceName().toString();
                            final String exMsg = createExceptionMessage(mContext.getComponentName(),
                                    taskName, "FAILED", "FBC_DEPLOY_HELPER_5",
                                    id, msg, null);
                            throw new DeploymentException(exMsg);
                        }

                        // if an application configuration object is defined for this
                        // endpoint, apply it.
                        String appConfName = epDesc.getApplicationConfigurationName();
                        Map<String, Collection<ApplicationConfigurationField>> appConfMap =
                                runtimeConfiguration.retrieveApplicationConfigurationsMap();
                        Collection<ApplicationConfigurationField> appConfObj = appConfMap.get(appConfName);
                        if (appConfObj != null) {
                            applyApplicationConfiguration(ldapAddress, appConfObj);
                        }


                        ldapOperations = LdapHelper.getLDAPOperations(matchedDef,
                                epDesc.getServiceName().toString(),
                                epDesc.getEndpointName());

                        if ((ldapOperations == null) ||
                                (ldapOperations.length == 0)) {
                            final String msg = "Missing ldap:operation definition(s) in wsdl for this service " +
                                    epDesc.getServiceName().toString();
                            final String exMsg = createExceptionMessage(mContext.getComponentName(),
                                    taskName, "FAILED", "FBC_DEPLOY_HELPER_7",
                                    id, msg, null);
                            throw new DeploymentException(exMsg);
                        }

                        // create endpoint bean for each operation
                        final EndpointImpl bcEndpoint = new EndpointImpl();

                        bcEndpoint.setValueObj(EndpointImpl.WSDL_FILE,
                                matchedWSDL);
                        bcEndpoint.setValueObj(EndpointImpl.DESCRIPTOR,
                                matchedDef); // result

                        final String endpoint = epDesc.getEndpointName();
                        final String endpointNameLocalPart = QName.valueOf(endpoint).getLocalPart();
                        bcEndpoint.setEndpointName(endpointNameLocalPart);
                        bcEndpoint.setServiceName(epDesc.getServiceName());
                        LdapConnection conn = ldapAddress;
                        bcEndpoint.setValueObj(EndpointImpl.LDAP_CONNECTION, conn);
                        bcEndpoint.setValue(EndpointImpl.ENDPOINT_NAME,
                                endpointNameLocalPart);
                        bcEndpoint.setValue(EndpointImpl.SERVICE_NAME,
                                epDesc.getServiceName().toString());
                        bcEndpoint.setValueObj(EndpointImpl.FULL_SERVICE_NAME,
                                epDesc.getServiceName());
                        bcEndpoint.setValue(EndpointImpl.STATUS,
                                EndpointImpl.STATUS_SHUTDOWN);

                        if (epDesc.isProvider()) {
                            bcEndpoint.setValue(EndpointImpl.ENDPOINT_TYPE,
                                    EndpointImpl.ENDPOINT_TYPE_OUTBOUND);
                            bcEndpoint.setEndpointType(1);
                        } else {
                            bcEndpoint.setValue(EndpointImpl.ENDPOINT_TYPE,
                                    EndpointImpl.ENDPOINT_TYPE_INBOUND);
                            bcEndpoint.setEndpointType(0);
                        }

                        final StatusReporting reporting = statusProviderHelper.getStatusReporter();
                        final QName serviceName = (QName) bcEndpoint.getValueObj(EndpointImpl.FULL_SERVICE_NAME);
                        final String portName = bcEndpoint.getValue(EndpointImpl.ENDPOINT_NAME);
                        String uniqueName = null;

                        if (bcEndpoint.getValue(EndpointImpl.ENDPOINT_TYPE).equals(EndpointImpl.ENDPOINT_TYPE_INBOUND)) {
                            uniqueName = statusProviderHelper.createConsumingEndpointIdentifier(serviceName,
                                    portName);
                            reporting.addConsumingEndpoint(uniqueName);
                        } else {
                            uniqueName = statusProviderHelper.createProvisioningEndpointIdentifier(serviceName,
                                    portName);
                            reporting.addProvisioningEndpoint(uniqueName);
                        }

                        final EndpointStatus stat = reporting.getEndpointStatus(uniqueName);
                        bcEndpoint.setEndpointStatus(stat);

                        //to set service description
                        final DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
                        final DocumentBuilder documentBuilder = docBuilderFactory.newDocumentBuilder();
                        Document serviceDescription = documentBuilder.parse(matchedWSDL);
                        bcEndpoint.setServiceDescription(serviceDescription);

                        endPoints.add(bcEndpoint);


                        final Map<String, OperationMetaData> operationNameToMetaData = new HashMap<String, OperationMetaData>();

                        for (final LDAPOperation ldapOperation : ldapOperations) {
                            // TODO: keep all the info separately in
                            // endpointbean.
                            final BindingOperation bindingOperation = LdapHelper.getParentBindingOperation(ldapOperation,
                                    matchedDef,
                                    epDesc.getServiceName().toString(),
                                    epDesc.getEndpointName());

                            if (bindingOperation == null) {
                                final String msg = "Misssing binding operation in wsdl for this service " +
                                        epDesc.getServiceName().toString();
                                final String exMsg = createExceptionMessage(mContext.getComponentName(),
                                        taskName, "FAILED",
                                        "FBC_DEPLOY_HELPER_11", id, msg, null);
                                throw new DeploymentException(exMsg);
                            }

                            final Binding binding = LdapHelper.getBinding(matchedDef,
                                    epDesc.getServiceName().toString(),
                                    epDesc.getEndpointName());
                            final PortType pt = binding.getPortType();

                            if (pt == null) {
                                final String msg = "Misssing binding portType in wsdl for this service " +
                                        epDesc.getServiceName().toString() +
                                        " portType : " + pt.getQName();
                                final String exMsg = createExceptionMessage(mContext.getComponentName(),
                                        taskName, "FAILED",
                                        "FBC_DEPLOY_HELPER_12", id, msg, null);
                                throw new DeploymentException(exMsg);
                            }

                            final Operation operation = LdapHelper.getOperation(matchedDef,
                                    pt.getQName().toString(),
                                    bindingOperation.getName());

                            if (operation == null) {
                                final String msg = "Misssing portType operation in wsdl for this service " +
                                        epDesc.getServiceName().toString() +
                                        " portType : " + pt.getQName() +
                                        "operation : " +
                                        bindingOperation.getName();
                                final String exMsg = createExceptionMessage(mContext.getComponentName(),
                                        taskName, "FAILED",
                                        "FBC_DEPLOY_HELPER_13", id, msg, null);
                                throw new DeploymentException(exMsg);
                            }

                            LDAPOperationOutput ldapOperationOutput = null;

                            if (bindingOperation.getBindingOutput() != null) {
                                ldapOperationOutput = LdapHelper.getLDAPOperationOutput(bindingOperation.getBindingOutput());
                            }

                            // create operation meta data
                            final OperationMetaData opMetaData = new OperationMetaData();
                            opMetaData.setBindingOperation(bindingOperation);
                            opMetaData.setLDAPOperation(ldapOperation);
                            opMetaData.setRetryCount(runtimeConfiguration.getRetryCount());
                            opMetaData.setRetryInterval(runtimeConfiguration.getRetryInterval());
                            opMetaData.setAllowConnectionPooling(runtimeConfiguration.getAllowConnectionPooling());
                            if(opMetaData.getAllowConnectionPooling().booleanValue()){
	                            opMetaData.setConnectionPoolPrefSize(runtimeConfiguration.getConnectionPoolPrefSize());
	                            opMetaData.setConnectionPoolMaxSize(runtimeConfiguration.getConnectionPoolMaxSize());
	                            opMetaData.setConnectionMaxIdleTimeout(runtimeConfiguration.getConnectionMaxIdleTimeout());
	                            opMetaData.setConnectionProtocol(runtimeConfiguration.getConnectionProtocol());
	                            opMetaData.setConnectionAuthentication(runtimeConfiguration.getConnectionAuthentication());
                            }
                            if (!ldapOperation.getOperationType().equals("addConnectionRequest")) {
                                //get input information
                                if (ldapOperation.getOperationType().equals(LDAPOperation.OPERATION_TYPE_SEARCH)) {
                                    Message inputMessage = operation.getInput().getMessage();
                                    if (inputMessage != null) {
                                        Part requestPart = inputMessage.getPart("request");
                                        String nsUri = requestPart.getElementName().getNamespaceURI();
                                        Schema mainXSD = LdapHelper.getSchema(nsUri, asaDir);
                                        if (mainXSD != null) {
                                            opMetaData.setSearchDN(LdapHelper.getDN(mainXSD));
                                            opMetaData.setReferral(LdapHelper.getReferral(mainXSD));
                                            opMetaData.setSearchControls(LdapHelper.getSearchControl(mainXSD));
                                            opMetaData.setRecordsPerPage(LdapHelper.getRecordsPerPage(mainXSD));
                                            opMetaData.setSortByAttribute(LdapHelper.getSortedByAttribute(mainXSD));
                                            opMetaData.setSortByType(LdapHelper.getSortedByType(mainXSD));
                                            LdapHelper.setQueryAttributes(mainXSD, opMetaData);
                                            List<String> resAttrs = LdapHelper.getResponseAttributes(mainXSD);
                                            if (null != resAttrs) {
                                                for (int k = 0; k < resAttrs.size(); k++) {
                                                    String attr = resAttrs.get(k);
                                                    ldapOperationOutput.addReturnAttr(attr);
                                                }
                                            }
                                        }
                                    }
                                } else if (ldapOperation.getOperationType().equals(LDAPOperation.OPERATION_TYPE_UPDATE)) {
                                    Message inputMessage = operation.getInput().getMessage();
                                    if (inputMessage != null) {
                                        Part requestPart = inputMessage.getPart("request");
                                        String nsUri = requestPart.getElementName().getNamespaceURI();
                                        Schema mainXSD = LdapHelper.getSchema(nsUri, asaDir);
                                        if (mainXSD != null) {
                                            opMetaData.setSearchDN(LdapHelper.getDN(mainXSD));
                                            opMetaData.setReferral(LdapHelper.getReferral(mainXSD));
                                            opMetaData.setSearchControls(LdapHelper.getSearchControl(mainXSD));
                                            LdapHelper.setQueryAttributes(mainXSD, opMetaData);
                                            LdapHelper.setUpdateAttributes(mainXSD, opMetaData);
                                        }
                                    }
                                } else if (ldapOperation.getOperationType().equals(LDAPOperation.OPERATION_TYPE_ADD)) {
                                    Message inputMessage = operation.getInput().getMessage();
                                    if (inputMessage != null) {
                                        Part requestPart = inputMessage.getPart("request");
                                        String nsUri = requestPart.getElementName().getNamespaceURI();
                                        Schema mainXSD = LdapHelper.getSchema(nsUri, asaDir);
                                        if (mainXSD != null) {
                                            opMetaData.setSearchDN(LdapHelper.getDN(mainXSD));
                                            opMetaData.setAddedReturnAttrs(LdapHelper.getAddedReturnAttrs(mainXSD));
                                        }
                                    }
                                } else if (ldapOperation.getOperationType().equals(LDAPOperation.OPERATION_TYPE_DELETE)) {
                                    Message inputMessage = operation.getInput().getMessage();
                                    if (inputMessage != null) {
                                        Part requestPart = inputMessage.getPart("request");
                                        String nsUri = requestPart.getElementName().getNamespaceURI();
                                        Schema mainXSD = LdapHelper.getSchema(nsUri, asaDir);
                                        if (mainXSD != null) {
                                            opMetaData.setSearchDN(LdapHelper.getDN(mainXSD));
                                            opMetaData.setSearchControls(LdapHelper.getSearchControl(mainXSD));
                                            LdapHelper.setQueryAttributes(mainXSD, opMetaData);
                                        }
                                    }
                                }
                            }
                            opMetaData.setLDAPOperationOutput(ldapOperationOutput);
                            operationNameToMetaData.put(bindingOperation.getName(), opMetaData);
                        }
                        bcEndpoint.setValueObj(EndpointImpl.OPERATION_NAME_TO_META_DATA,
                                operationNameToMetaData);
                    }
                }
            }

            // todo : should consider what happens if one endpoint fails to
            // activate
            // what type of cleanup should we do?
            final EndpointImpl[] endPointsArr = endPoints.toArray(new EndpointImpl[0]);
            mDeployedIds.put(id, asaDir);
            mDeployedEndpoints.put(id, endPointsArr);
        }

        /**
         *
         * @param asaId
         * @param asaDir
         * @param statusProviderHelper
         * @throws DeploymentException
         */
        public void undeploy(final String asaId, final File asaDir,
                final StatusProviderHelper statusProviderHelper)
                throws DeploymentException {
            final String taskName = "undeploy";

            if (LDAPBindingDeployer.mLogger.isLoggable(Level.INFO)) {
                LDAPBindingDeployer.mLogger.log(Level.INFO, mMessages.getString("LDAPBC-R00211.LDBD_Undeploy_SU",
                        new Object[]{asaId, asaDir}));
            }

            try {
                mDeployedIds.remove(asaId);

                final EndpointImpl[] endpoints = mDeployedEndpoints.remove(asaId);

                if (endpoints != null) {
                    final StatusReporting reporting = statusProviderHelper.getStatusReporter();

                    for (EndpointImpl element : endpoints) {
                        final QName serviceName = (QName) element.getValueObj(EndpointImpl.FULL_SERVICE_NAME);
                        final String portName = element.getValue(EndpointImpl.ENDPOINT_NAME);
                        String uniqueName = null;

                        if (element.getValue(
                                EndpointImpl.ENDPOINT_TYPE).equals(EndpointImpl.ENDPOINT_TYPE_INBOUND)) {
                            uniqueName = statusProviderHelper.createConsumingEndpointIdentifier(serviceName,
                                    portName);
                            reporting.removeConsumingEndpoints(new String[]{
                                        uniqueName
                                    });
                        } else {
                            uniqueName = statusProviderHelper.createProvisioningEndpointIdentifier(serviceName,
                                    portName);
                            reporting.removeProvisioningEndpoints(new String[]{
                                        uniqueName
                                    });
                        }
                    }
                }
            } catch (final Exception e) {
                final String exMsg = createExceptionMessage(mContext.getComponentName(),
                        taskName, "FAILED", "LDAPBC_UNDEPLOY_HELPER_1", asaId,
                        "Failed to undeploy " + e.getMessage(), e);
                throw new DeploymentException(exMsg, e);
            }
        }

        /**
         * Applies the configuration values represented by an application configuration object, to
         * an LDAPAddress object.
         * 
         * @param address The LDAPAddress to which to apply the configuration
         * @param appConfObj The application configuration object to apply
         */
        private void applyApplicationConfiguration(LDAPAddress address,
                Collection<ApplicationConfigurationField> appConfObj) {
            assert address != null;
            assert appConfObj != null;
            if (!appConfObj.isEmpty()) {
                Visitor addressVisitor = new AppConfigAddressVisitor(address);
                for (ApplicationConfigurationField field : appConfObj) {
                    field.accept(addressVisitor);
                }
            }
        }

        /**
         *
         * @param asaDir
         * @param id
         * @param taskName
         * @param locToken
         * @return
         * @throws DeploymentException
         */
        FileToDefinitionInfo[] readAllDefinitions(final File asaDir, final String id,
                final String taskName, final String locToken, final Map<String, String[]> envVariableMap) throws DeploymentException {
            final CatalogManager catalogManager = new CatalogManager();
            catalogManager.setCatalogFiles(asaDir.getAbsolutePath() +
                    File.separator + "xml-catalog.xml");
            catalogManager.setRelativeCatalogs(true);

            final EntityResolver resolver = new CatalogResolver(catalogManager);

            final List<File> wsdls = LdapHelper.listWSDLFiles(asaDir);
            final File[] wsdlFiles = wsdls.toArray(new File[0]);

            // read all wsdl files to see if
            FileToDefinitionInfo[] fileToDefs = null;

            if (wsdlFiles != null) {
                fileToDefs = new FileToDefinitionInfo[wsdlFiles.length];

                for (int i = 0; i < wsdlFiles.length; i++) {
                    try {
                        final Definition def = LdapHelper.readWSDL(wsdlFiles[i], resolver, envVariableMap);
                        fileToDefs[i] = new FileToDefinitionInfo(wsdlFiles[i],
                                def);
                    } catch (final Exception e) {
                        final String msg = "Unable to read WSDL file " +
                                wsdlFiles[i] + " : " + e.getMessage();
                        final String exMsg = createExceptionMessage(mContext.getComponentName(),
                                taskName, "FAILED", locToken, id, msg, e);
                        throw new DeploymentException(exMsg, e);
                    }
                }
            }

            return fileToDefs;
        }

        /**
         *
         * @author Administrator
         *
         */
        class FileToDefinitionInfo {

            private File mFile;
            private Definition mDefinition;

            FileToDefinitionInfo(final File file, final Definition definition) {
                mFile = file;
                mDefinition = definition;
            }

            public File getFile() {
                return mFile;
            }

            public Definition getDefinition() {
                return mDefinition;
            }
        }
    }
}
