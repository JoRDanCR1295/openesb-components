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
 * @(#)JDBCBindingDeployer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package org.glassfish.openesb.databasebc;


import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.DeploymentException;
import javax.jbi.messaging.MessagingException;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import javax.wsdl.Input;
import javax.wsdl.Output;
import javax.wsdl.OperationType;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.ibm.wsdl.factory.WSDLFactoryImpl;

import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.xml.sax.EntityResolver;

// Additional imports for replacing the ibm WSDL factory to Sun extensions
import com.sun.wsdl4j.ext.impl.WSDLFactoryEx;
import com.sun.wsdl4j.ext.impl.WSDLReaderEx;

import com.sun.jbi.management.descriptor.EndpointIdentifier;
import com.sun.jbi.management.descriptor.SUDescriptorSupport;
import com.sun.jbi.management.message.DefaultJBITaskMessageBuilder;
import com.sun.jbi.management.message.JBITaskMessageBuilder;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.descriptor.DeploymentLookup;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;

import org.glassfish.openesb.databasebc.extensions.SPOperationInput;
import org.glassfish.openesb.databasebc.extensions.SPOperationOutput;
import org.glassfish.openesb.databasebc.extensions.JDBCAddress;
import org.glassfish.openesb.databasebc.extensions.JDBCBinding;
import org.glassfish.openesb.databasebc.extensions.JDBCExtensionRegistry;
import org.glassfish.openesb.databasebc.extensions.JDBCOperation;
import org.glassfish.openesb.databasebc.extensions.JDBCOperationInput;
import org.glassfish.openesb.databasebc.extensions.JDBCOperationOutput;
import org.glassfish.openesb.databasebc.extensions.DBExtPreprocessDeserializer;
import org.glassfish.openesb.databasebc.util.Configuration.PortMap;
import org.glassfish.openesb.databasebc.mbeans.ApplicationConfigurationField;
import org.glassfish.openesb.databasebc.mbeans.AppConfigAddressVisitor;
import org.glassfish.openesb.databasebc.mbeans.Visitor;
import org.glassfish.openesb.databasebc.util.AlertsUtil;

/**
 *
 *JDBCBindingDeployer
 */
public class JDBCBindingDeployer implements ServiceUnitManager {

    private static final Messages mMessages = Messages.getMessages(JDBCBindingDeployer.class);
    private static final Logger mLogger = Messages.getLogger(JDBCBindingDeployer.class);
    public static final String SHORT_DISPLAY_NAME = "Database BC";
    public static final String catalogFileRelativePath="META-INF" + File.separator +"catalog.xml";
    private final Map<String, File> mDeployedIds;
    private final Map<String, EndpointBean[]> mDeployedEndpoints;
    private final ComponentContext mContext;
    private final DeployHelper mHelper;
    private final JDBCBindingLifeCycle mLifeCycle;
    private RuntimeConfigurationMBean mRuntimeConfig;  
    private Map<EndpointInfo, List<ServiceQuality>> mEndpointQos;
    private MessagingChannel mChannel;
    private HashMap colNamesTypes = new HashMap();
	private List mXsds = new ArrayList();

    /** Creates a new instance of DeployerMBean */
    JDBCBindingDeployer(final ComponentContext context,
                        final JDBCBindingLifeCycle lifeCycle,  RuntimeConfigurationMBean runtimeConfig) {
        mContext = context;
        mDeployedIds = new HashMap<String, File>();
        mDeployedEndpoints = new HashMap<String, EndpointBean[]>();
        mRuntimeConfig = runtimeConfig;
        mHelper = new DeployHelper();
        mLifeCycle = lifeCycle;
        mChannel = JDBCComponentContext.getInstance().getBindingChannel();
        
    }

    /**
     * Initiate a BC Deployment.
     *
     * @param suId -
     *            ID of the ASA being deployed
     * @param asaFilePath 
     * @return 
     * @throws javax.jbi.management.DeploymentException 
     * 
     */
    //@Override
    public String deploy(final String suId, final String asaFilePath)
            throws DeploymentException {
        String retMsg = null;
        final String taskName = "deploy";

        if (JDBCBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            JDBCBindingDeployer.mLogger.log(Level.INFO, "DBBC_R00201.BD_Deploy_SU",
                    new Object[]{suId, asaFilePath});
        }

        try {
            // Populate the Application Variables
            Map<String, String[]> envVariableMap =
                mHelper.parseForEnvironmentVariables(asaFilePath, mRuntimeConfig.retrieveApplicationVariablesMap());
            if (envVariableMap.size() > mRuntimeConfig.countVariables()) {
                // number of environment variable tokens used in WSDLs 
                // is greater than the ones defined 
                mRuntimeConfig.updateApplicationVariablesMap(envVariableMap);
            }
            
            retMsg = createSuccessMessage(taskName, mContext.getComponentName());
        } catch (Exception ex) {
            final String errMsg = JDBCBindingDeployer.mMessages.getString("DBBC_E00215.BD_Failed_deploy_SU",
                    ex.getMessage());
            AlertsUtil.getAlerter().critical(errMsg, 
                    JDBCBindingDeployer.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "DBBC_E00215");
            JDBCBindingDeployer.mLogger.log(Level.SEVERE, errMsg);

            final String exMsg = createExceptionMessage(mContext.getComponentName(),
                    taskName, "FAILED", "JDBCBC_DEPLOY_1", null, errMsg, ex);
            throw new DeploymentException(exMsg, ex);
        }
        return retMsg;
    }

    /**
     * @param suId
     * @param suPath
     * @throws DeploymentException
     */
    //@Override
    public void init(final String suId, final String suPath)
            throws DeploymentException {
        final String taskName = "init";
        if (JDBCBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            JDBCBindingDeployer.mLogger.log(Level.INFO, "DBBC_R00202.BD_Init_SU",
                    new Object[]{suId, suPath});
        }

        try {
            // Prepare for start if the deployment hasn't been processed yet.
            if (mDeployedIds.get(suId) == null) {
                final StatusProviderHelper statusProviderHelper = mLifeCycle.getStatusProviderHelper();
                processDeployment(suId, suPath, statusProviderHelper);
            }

            if (JDBCBindingDeployer.mLogger.isLoggable(Level.INFO)) {
                JDBCBindingDeployer.mLogger.log(Level.INFO, "DBBC_R00203.BD_Complete_init_SU", suId);
            }
        } catch (final DeploymentException ex) {
            final String errMsg = JDBCBindingDeployer.mMessages.getString("DBBC_E00204.BD_Failed_init_SU",
                    ex.getMessage());
            AlertsUtil.getAlerter().critical(errMsg, 
                    JDBCBindingDeployer.SHORT_DISPLAY_NAME, 
                    null, 
                    AlertsUtil.getServerType(),
                    AlertsUtil.COMPONENT_TYPE_BINDING,
                    NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                    NotificationEvent.EVENT_TYPE_ALERT,
                    "DBBC_E00204");
            JDBCBindingDeployer.mLogger.log(Level.SEVERE, errMsg);

            final String exMsg = createExceptionMessage(mContext.getComponentName(),
                    taskName, "FAILED", "JDBCBC_INIT_1", null, errMsg, ex);
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
    private void processDeployment(final String suId, final String path,
            final StatusProviderHelper statusProviderHelper) throws DeploymentException {
        final String taskName = "processDeployment";

        try {
            final File asaDir = new File(path);
            mHelper.deploy(asaDir, suId, mContext, statusProviderHelper);

            final EndpointBean[] endPointsArr = mDeployedEndpoints.get(suId);
			for (EndpointBean element : endPointsArr) {
				if (element.getValue(EndpointBean.ENDPOINT_TYPE)
                                .equals(EndpointBean.ENDPOINT_TYPE_OUTBOUND)) {
					mLifeCycle.activateEndpoint(element);
					setEndpointsStatus(endPointsArr, EndpointBean.STATUS_RUNNING);
				}
			}
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
    // @Override
    public void start(final String suId) throws DeploymentException {
        if (JDBCBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            JDBCBindingDeployer.mLogger.log(Level.INFO, "DBBC_R00205.BD_Starting_SU", suId);
        }

        final EndpointBean[] endPointsArr = mDeployedEndpoints.get(suId);
		if(endPointsArr != null){
			for (EndpointBean element : endPointsArr) {
					if (element.getValue(EndpointBean.ENDPOINT_TYPE)
									.equals(EndpointBean.ENDPOINT_TYPE_INBOUND)) {
					try{
						mLifeCycle.activateEndpoint(element);
						}catch (final MessagingException ex) {
						final String exMsg = createExceptionMessage(mContext.getComponentName(),
								"start", "FAILED", "FBC_PROCESS_4", suId,
								"Processing deployment error: " + ex.getMessage(), ex);
						throw new DeploymentException(exMsg);
					}catch (final Exception e) {
						final String exMsg = createExceptionMessage(mContext.getComponentName(),
								"start", "FAILED", "FBC_PROCESS_4", suId,
								"Processing deployment error: " + e.getMessage(), e);
						throw new DeploymentException(exMsg);
					}
				}
			}
		}
        setEndpointsStatus(endPointsArr, EndpointBean.STATUS_RUNNING);

        if (JDBCBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            JDBCBindingDeployer.mLogger.log(Level.INFO, "DBBC_R00207.BD_Complete_start_BC");
        }
    }

    /**
     * @param suId
     * @throws DeploymentException
     */
    //@Override
    public void stop(final String suId) throws DeploymentException {
        if (JDBCBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            JDBCBindingDeployer.mLogger.log(Level.INFO, "DBBC_R00208.BD_Stop_SU", suId);
        }

        final EndpointBean[] endPointsArr = mDeployedEndpoints.get(suId);

        setEndpointsStatus(endPointsArr, EndpointBean.STATUS_STOPPED);

        if (JDBCBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            JDBCBindingDeployer.mLogger.log(Level.INFO, "DBBC_R00210.BD_Complete_stop_SU", suId);
        }
    }

    /**
     * @param suId
     * @throws DeploymentException
     */
    //@Override
    public void shutDown(final String suId) throws DeploymentException {
        final String taskName = "shutDown";

        if (JDBCBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            JDBCBindingDeployer.mLogger.log(Level.INFO, "DBBC_R00211.BD_Shutdown_SU", suId);
        }

        final File asaDir = mDeployedIds.get(suId);

        if (asaDir == null) {
            JDBCBindingDeployer.mLogger.info("Service unit " + mContext.getComponentName() +
                    " is not deployed.");
        } else {
            final EndpointBean[] endPointsArr = mDeployedEndpoints.get(suId);

            try {
                mLifeCycle.deactivateEndpoints(endPointsArr);
            } catch (final MessagingException ex) {
                final String errMsg = mMessages.getString("DBBC_E00212.BD_Error_shutdown_SU",
                        ex.getMessage());
                AlertsUtil.getAlerter().critical(errMsg, 
                        JDBCBindingDeployer.SHORT_DISPLAY_NAME, 
                        null, 
                        AlertsUtil.getServerType(),
                        AlertsUtil.COMPONENT_TYPE_BINDING,
                        NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                        NotificationEvent.EVENT_TYPE_ALERT,
                        "DBBC_E00212");
                JDBCBindingDeployer.mLogger.log(Level.SEVERE, errMsg);

                final String exMsg = createExceptionMessage(mContext.getComponentName(),
                        taskName, "FAILED", "JDBCBC_SHUTDOWN_1", null, errMsg,
                        ex);
                throw new DeploymentException(exMsg, ex);
            }

            setEndpointsStatus(endPointsArr, EndpointBean.STATUS_SHUTDOWN);

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
     * @throws DeploymentException
     */
    //@Override
    public String undeploy(final String name, final String root) throws DeploymentException {
        String retMsg = null;
        final String taskName = "undeploy";

        if (JDBCBindingDeployer.mLogger.isLoggable(Level.INFO)) {
            JDBCBindingDeployer.mLogger.log(Level.INFO, "DBBC_R00213.BD_Undeploy_SU",
                    new Object[]{name, root});
        }

        retMsg = createSuccessMessage(taskName, mContext.getComponentName());

        return retMsg;
    }

    synchronized public void resume(EndpointBean ep) throws Exception {
        start(ep.getDeploymentId());
        //sendAlert(ep, mMessages.getString("JDBCBC-R00213.LDBD_Resume_Endpoint"));
    }

    synchronized public void suspend(EndpointBean ep) throws Exception {
        stop(ep.getDeploymentId());
        //sendAlert(ep, mMessages.getString("JDBCBC-R00212.LDBD_Suspend_Endpoint"));
    }

    
    
    /**
     * Determine the message exchange pattern. For handling 1.1 wsdls, map
     * transmission primitives to the closest message exchange pattern, taking
     * into account the endpoint direction direction inbound: request-response
     * and solicit-response -> in-out one-way and notification -> in-only
     * direction outbound: request-response and solicit-response -> out-in
     * one-way and notification -> out-only
     *
     * @param pm
     *            the endpoint configuration from the portmap
     * @param po
     *            the binding operation definition from the wsdl
     * @return the message exchange pattern, null if no mapping could be
     *         determined.
     */
    private String determineMEP(final PortMap pm, final BindingOperation bo) {
        String mep = null;
        final OperationType type = bo.getOperation().getStyle();

        if (pm.getDirection().equals(EndpointBean.ENDPOINT_TYPE_INBOUND)) {
            if (type.equals(OperationType.REQUEST_RESPONSE) ||
                    type.equals(OperationType.SOLICIT_RESPONSE)) {
                mep = "inout";
            } else if (type.equals(OperationType.ONE_WAY) ||
                    type.equals(OperationType.NOTIFICATION)) {
                mep = "inonly";
            }
        } else {
            if (type.equals(OperationType.REQUEST_RESPONSE) ||
                    type.equals(OperationType.SOLICIT_RESPONSE)) {
                mep = "outin";
            } else if (type.equals(OperationType.ONE_WAY) ||
                    type.equals(OperationType.NOTIFICATION)) {
                mep = "outonly";
            }
        }

        return mep;
    }

    /**
     * Keep track of the running status of each endpoint. This allows for quick
     * look-up of the state and provides for extensibility where an individual
     * endpoint could be controlled rather than the whole SU.
     */
    private void setEndpointsStatus(final EndpointBean[] endpoints, final String status) {
        if (endpoints != null) {
            for (EndpointBean element : endpoints) {
                element.setValue(EndpointBean.STATUS, status);
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
    
    public Map<String, EndpointBean[]> getEndpoints(){
    	return this.mDeployedEndpoints;
    }

    /**
     * List all wsdl files in the currentDir and below
     * @author Administrator
     *
     */
    private class DeployHelper {

        private List<File> listWSDLFiles(final File currentDir) {
            final List<File> cumulativeResults = new ArrayList<File>();
            final File[] filesInCurrentDir = currentDir.listFiles();

            for (File element : filesInCurrentDir) {

                if (element.isFile()) {

                    if (element.getName().toLowerCase().endsWith(".wsdl")) {
                        cumulativeResults.add(element);
                    }
                } else if (element.isDirectory()) {
                    final List<File> wsdlsInSubDirectories = listWSDLFiles(element);
                    cumulativeResults.addAll(wsdlsInSubDirectories);
                }
            }
            return cumulativeResults;
        }

	  private List<File> listXSDFiles(final File currentDir) {
            final List<File> cumulativeResults = new ArrayList<File>();
            final File[] filesInCurrentDir = currentDir.listFiles();

            for (File element : filesInCurrentDir) {

                if (element.isFile()) {

                    if (element.getName().toLowerCase().endsWith(".xsd")) {
                        cumulativeResults.add(element);
                    }
                } else if (element.isDirectory()) {
                    final List<File> wsdlsInSubDirectories = listXSDFiles(element);
                    cumulativeResults.addAll(wsdlsInSubDirectories);
                }
            }
            return cumulativeResults;
        }
        private Definition readWSDL(File f, EntityResolver resolver, Map envVariableMap) 
            throws WSDLException {
            
            WSDLFactoryEx wsdlFactory = new WSDLFactoryEx();
            WSDLReaderEx reader = wsdlFactory.newWSDLReaderEx();
            reader.setEntityResolver(resolver);
            reader.setExtensionRegistry(new JDBCExtensionRegistry(envVariableMap));
            Definition def = reader.readWSDL(f.getAbsolutePath());
            
            return def;
        }

        private Map readWSDLForEnvVariables(File f, EntityResolver resolver, Map<String, String[]> envVariableMap) 
            throws WSDLException {
            
            WSDLFactoryEx wsdlFactory = new WSDLFactoryEx();
            WSDLReaderEx reader = wsdlFactory.newWSDLReaderEx();
            reader.setEntityResolver(resolver);
            DBExtPreprocessDeserializer preProcessDeserializer = new DBExtPreprocessDeserializer(envVariableMap);
            reader.setExtensionRegistry(new JDBCExtensionRegistry(preProcessDeserializer));
            reader.readWSDL(f.getAbsolutePath());
            return preProcessDeserializer.getEnvVariableMap();
        }


        /**
         *
         * @param def
         * @param serviceName
         * @param endpointName
         * @return
         */
        private Binding getBinding(final Definition def, final String serviceName,
                final String endpointName) {
            final Service svc = def.getService(QName.valueOf(serviceName));

            if (svc == null) {
                return null;
            }

            final Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());

            if (port == null) {
                return null;
            } else {
                return port.getBinding();
            }
        }

        /**
         *
         * @param def
         * @param portTypeName
         * @param operationName
         * @return
         */
        private Operation getOperation(final Definition def, final String portTypeName,
                final String operationName) {
            Operation operation = null;
            final PortType pType = def.getPortType(QName.valueOf(portTypeName));

            if (pType != null) {
                operation = pType.getOperation(operationName, null, null);
            }

            return operation;
        }

        /**
         *
         * @param def
         * @param serviceName
         * @param endpointName
         * @return
         */
        private JDBCAddress getJDBCAddress(final Definition def, final String serviceName,
                final String endpointName) {
            JDBCAddress address = null;
            final Service svc = def.getService(QName.valueOf(serviceName));

            if (svc == null) {
                return null;
            }

            final Port port = svc.getPort(QName.valueOf(endpointName).getLocalPart());

            if (port != null) {
                final List extElems = port.getExtensibilityElements();

                // Look for jdbc:address
                final Iterator extIter = (extElems == null) ? null : extElems.iterator();

                while ((extIter != null) && extIter.hasNext() &&
                        (address == null)) {
                    final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();

                    if (JDBCAddress.class.isInstance(ee)) {
                        address = (JDBCAddress) ee;
                    }
                }
            }

            return address;
        }

        /**
         *
         * @param def
         * @param serviceName
         * @param endpointName
         * @return
         */
        private JDBCBinding getJDBCBinding(final Definition def, final String serviceName,
                final String endpointName) {
            JDBCBinding jdbcBinding = null;
            final Binding binding = getBinding(def, serviceName, endpointName);

            if (binding != null) {
                final List extElems = binding.getExtensibilityElements();

                // Look for jdbc:binding
                final Iterator extIter = (extElems == null) ? null : extElems.iterator();

                while ((extIter != null) && extIter.hasNext() &&
                        (jdbcBinding == null)) {
                    final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();

                    if (JDBCBinding.class.isInstance(ee)) {
                        jdbcBinding = (JDBCBinding) ee;
                    }
                }
            }

            return jdbcBinding;
        }

        /**
         *
         * @param def
         * @param serviceName
         * @param endpointName
         * @return
         */
        private JDBCOperation[] getJDBCOperations(final Definition def,
                final String serviceName, final String endpointName) {
            final List<ExtensibilityElement> jdbcOperations = new ArrayList<ExtensibilityElement>();
            final Binding binding = getBinding(def, serviceName, endpointName);

            if (binding != null) {
                final List bindingOperations = binding.getBindingOperations();
                final Iterator operIter = (bindingOperations == null) ? null
                        : bindingOperations.iterator();

                while ((operIter != null) && operIter.hasNext()) {
                    final BindingOperation oper = (BindingOperation) operIter.next();
                    final List extElems = oper.getExtensibilityElements();

                    // Look for file:operation entries
                    final Iterator extIter = (extElems == null) ? null
                            : extElems.iterator();

                    while ((extIter != null) && extIter.hasNext()) {
                        final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();

                        if (JDBCOperation.class.isInstance(ee)) {
                            jdbcOperations.add(ee);
                        }
                    }
                }
            }

            return jdbcOperations.toArray(new JDBCOperation[0]);
        }

        /**
         *
         * @param bindingInput
         * @return
         */
        private JDBCOperationInput getJDBCOperationInput(
                final BindingInput bindingInput) {
            JDBCOperationInput operationInput = null;
            final List extElems = bindingInput.getExtensibilityElements();

            // Look for jdbc:input entries
            final Iterator extIter = (extElems == null) ? null : extElems.iterator();

            while ((extIter != null) && extIter.hasNext()) {
                final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();

                if (JDBCOperationInput.class.isInstance(ee)) {
                    operationInput = (JDBCOperationInput) ee;

                    break;
                }
            }

            return operationInput;
        }

        /**
         *
         * @param bindingOutput
         * @return
         */
        private JDBCOperationOutput getJDBCOperationOutput(
                final BindingOutput bindingOutput) {
            JDBCOperationOutput operationOutput = null;

            final List extElems = bindingOutput.getExtensibilityElements();

            // Look for jdbc:output entries
            final Iterator extIter = (extElems == null) ? null : extElems.iterator();

            while ((extIter != null) && extIter.hasNext()) {
                final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();

                if (JDBCOperationOutput.class.isInstance(ee)) {
                    operationOutput = (JDBCOperationOutput) ee;

                    break;
                }
            }

            return operationOutput;
        }

        /**
         *
         * @param bindingInput
         * @return
         */
        private SPOperationInput getJDBCSPOperationInput(
                final BindingInput bindingInput) {
            SPOperationInput operationInput = null;
            final List extElems = bindingInput.getExtensibilityElements();

            // Look for jdbc:input entries
            final Iterator extIter = (extElems == null) ? null : extElems.iterator();

            while ((extIter != null) && extIter.hasNext()) {
                final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();

                if (SPOperationInput.class.isInstance(ee)) {
                    operationInput = (SPOperationInput) ee;

                    break;
                }
            }

            return operationInput;
        }

        /**
         *
         * @param bindingOutput
         * @return
         */
        private SPOperationOutput getJDBCSPOperationOutput(
                final BindingOutput bindingOutput) {
            SPOperationOutput operationOutput = null;

            final List extElems = bindingOutput.getExtensibilityElements();

            // Look for jdbc:output entries
            final Iterator extIter = (extElems == null) ? null : extElems.iterator();

            while ((extIter != null) && extIter.hasNext()) {
                final ExtensibilityElement ee = (ExtensibilityElement) extIter.next();

                if (SPOperationOutput.class.isInstance(ee)) {
                    operationOutput = (SPOperationOutput) ee;

                    break;
                }
            }

            return operationOutput;
        }

        /**
         *
         * @param jdbcOperation
         * @param def
         * @param serviceName
         * @param endpointName
         * @return
         */
        private BindingOperation getParentBindingOperation(
                final JDBCOperation jdbcOperation, final Definition def, final String serviceName,
                final String endpointName) {
            BindingOperation bindingOperation = null;

            final Binding binding = getBinding(def, serviceName, endpointName);

            if (binding != null) {
                final List l = binding.getBindingOperations();
                final Iterator it = l.iterator();

                while (it.hasNext()) {
                    final BindingOperation bOperation = (BindingOperation) it.next();
                    final List exElements = bOperation.getExtensibilityElements();

                    if ((exElements != null) &&
                            exElements.contains(jdbcOperation)) {
                        bindingOperation = bOperation;

                        break;
                    }
                }
            }

            return bindingOperation;
        }

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
        private void deploy(final File asaDir, final String id,
                final ComponentContext compContext,
                final StatusProviderHelper statusProviderHelper)
                throws IOException, DeploymentException,
                javax.jbi.messaging.MessagingException {
            final String taskName = "deploy";

            final DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder documentBuilder = null;

            try {
                documentBuilder = docBuilderFactory.newDocumentBuilder();
            } catch (final Exception ex) {
                final String msg = "Failed to obtain a DOM parser: " +
                        ex.getMessage();
                JDBCBindingDeployer.mLogger.severe(msg);

                final String exMsg = createExceptionMessage(mContext.getComponentName(),
                        taskName, "FAILED", "FBC_DEPLOY_HELPER_1", id, msg, null);
                throw new DeploymentException(exMsg);
            }

			DeploymentLookup lookUpUtil = new DeploymentLookup(compContext);
			mEndpointQos = lookUpUtil.lookupServiceQualities(asaDir.getAbsolutePath());

			if (mEndpointQos != null && mEndpointQos.size() > 0) {
				for (Iterator itor = mEndpointQos.keySet().iterator(); itor.hasNext(); ) {
					EndpointInfo endpointInfo = (EndpointInfo) itor.next();
					ServiceQuality[] qos = 
						(ServiceQuality[])((List)mEndpointQos.get(endpointInfo)).toArray(new ServiceQuality[0]);
						
					mChannel.addServiceQualityToEndpoint(endpointInfo, qos);
				}
			}

            // read all wsdl files to see if
            final FileToDefinitionInfo[] fileToDefs = readAllDefinitions(asaDir, id,
                    taskName, "FBC_DEPLOY_HELPER_4");
            mXsds = readAllXSDs(asaDir, id,
                    taskName, "FBC_DEPLOY_HELPER_4");

            final ArrayList<EndpointBean> endPoints = new ArrayList<EndpointBean>();

            if (SUDescriptorSupport.TEMP_SWITCH_ENABLE_JBI_ROUTING) {
                JDBCBindingDeployer.mLogger.info(
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
                    JDBCBinding jdbcBinding = null;
                    JDBCOperation[] jdbcOperations = null;
                    final BindingOperation[] bindingOperations = null;
                    JDBCAddress jdbcAddress = null;

                    // Document result = null;
                    final EndpointIdentifier epDesc = svcs[i];

                    for (FileToDefinitionInfo element : fileToDefs) {
                        jdbcBinding = getJDBCBinding(element.getDefinition(),
                                epDesc.getServiceName().toString(),
                                epDesc.getEndpointName());

                        if (jdbcBinding != null) {
                            matchedDef = element.getDefinition();
                            // this is the wsdl that contains the service
                            // endpoint, save it
                            matchedWSDL = element.getFile();

                            break;
                        }
                    }

                    // If no jdbc:binding is defined, the databasebc does not
                    // service this wsdl
                    if (jdbcBinding != null) {
                        // Prepare and cache the endpoint descriptor
                        /*
                         * try { result = documentBuilder.parse(matchedWSDL); }
                         * catch (Exception ex) { String msg = "Failed to parse
                         * WSDL to prepare endpoint descriptor for " +
                         * epDesc.getServiceName().toString() + " " +
                         * epDesc.getEndpointName() + ":" + ex.getMessage();
                         * String exMsg = createExceptionMessage(mContext
                         * .getComponentName(), taskName, "FAILED",
                         * "FBC_DEPLOY_HELPER_11", id, msg, ex); throw new
                         * DeploymentException(exMsg); }
                         */
                        jdbcAddress = getJDBCAddress(matchedDef,
                                epDesc.getServiceName().toString(),
                                epDesc.getEndpointName());

                        if (jdbcAddress == null) {
                            final String msg = "Missing jdbc:address in wsdl for this service " +
                                    epDesc.getServiceName().toString();
                            final String exMsg = createExceptionMessage(mContext.getComponentName(),
                                    taskName, "FAILED", "FBC_DEPLOY_HELPER_5",
                                    id, msg, null);
                            throw new DeploymentException(exMsg);
                        } else if (jdbcAddress.getJndiName() == null) {
                            final String msg = "Missing " +
                                    JDBCAddress.ATTR_JNDI_NAME +
                                    " in jdbc:address of wsdl for this service " +
                                    epDesc.getServiceName().toString();
                            final String exMsg = createExceptionMessage(mContext.getComponentName(),
                                    taskName, "FAILED", "FBC_DEPLOY_HELPER_6",
                                    id, msg, null);
                            throw new DeploymentException(exMsg);
                        }

                        if (JDBCBindingDeployer.mLogger.isLoggable(Level.FINEST)) {
                            mLogger.log(Level.INFO, "Deploying " + asaDir.getName() + " service Name = " + epDesc.getServiceName().toString() +
                                " and endPoint Name = " + epDesc.getEndpointName());
                        } else if (JDBCBindingDeployer.mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.INFO, "Deploying " + asaDir.getName() + " service Name = " + epDesc.getServiceName().toString());
                        } else if (mLogger.isLoggable(Level.INFO)) {
                            mLogger.log(Level.INFO, "Deploying " + asaDir.getName());
                        }

                        // Apply application configuration if applicable
                        String appConfName = epDesc.getApplicationConfigurationName();
                        Collection<ApplicationConfigurationField> appConfObj = 
                            mRuntimeConfig.retrieveApplicationConfigurationsMap().get(appConfName);
                        if (appConfObj != null) {
                            applyApplicationConfiguration(jdbcAddress, appConfObj);
                        }
                        
						jdbcOperations = getJDBCOperations(matchedDef,
                                epDesc.getServiceName().toString(),
                                epDesc.getEndpointName());

                        if ((jdbcOperations == null) ||
                                (jdbcOperations.length == 0)) {
                            final String msg = "Missing jdbc:operation definition(s) in wsdl for this service " +
                                    epDesc.getServiceName().toString();
                            final String exMsg = createExceptionMessage(mContext.getComponentName(),
                                    taskName, "FAILED", "FBC_DEPLOY_HELPER_7",
                                    id, msg, null);
                            throw new DeploymentException(exMsg);
                        }

                        // create endpoint bean for each operation
                        final EndpointBean bcEndpoint = new EndpointBean();

                        bcEndpoint.setDeploymentId(id);
                        bcEndpoint.setValueObj(EndpointBean.WSDL_FILE,
                                matchedWSDL);
                        bcEndpoint.setValueObj(EndpointBean.DESCRIPTOR,
                                matchedDef); // result
                        bcEndpoint.setValue(EndpointBean.JDBC_DATABASE_JNDI_NAME,
                                jdbcAddress.getJndiName());

                        bcEndpoint.setValue(EndpointBean.JDBC_DATABASE_DRIVER_CLASS,
                                jdbcAddress.getDriverClass());
                        bcEndpoint.setValue(EndpointBean.JDBC_DATABASE_URL,
                                jdbcAddress.getDBUrl());
                        bcEndpoint.setValue(EndpointBean.JDBC_DATABASE_USER,
                                jdbcAddress.getUserName());
                        bcEndpoint.setValue(EndpointBean.JDBC_DATABASE_PASSWORD,
                                jdbcAddress.getPassword());

                        final String endpoint = epDesc.getEndpointName();
                        final String endpointNameLocalPart = QName.valueOf(endpoint).getLocalPart();
                        bcEndpoint.setValue(EndpointBean.ENDPOINT_NAME,
                                endpointNameLocalPart);
                        bcEndpoint.setValue(EndpointBean.SERVICE_NAME,
                                epDesc.getServiceName().toString());
                        bcEndpoint.setValueObj(EndpointBean.FULL_SERVICE_NAME,
                                epDesc.getServiceName());
                        bcEndpoint.setValue(EndpointBean.STATUS,
                                EndpointBean.STATUS_SHUTDOWN);

                        if (epDesc.isProvider()) {
                            bcEndpoint.setValue(EndpointBean.ENDPOINT_TYPE,
                                    EndpointBean.ENDPOINT_TYPE_OUTBOUND);
                        } else {
                            bcEndpoint.setValue(EndpointBean.ENDPOINT_TYPE,
                                    EndpointBean.ENDPOINT_TYPE_INBOUND);
                        }

                        final StatusReporting reporting = statusProviderHelper.getStatusReporter();
                        final QName serviceName = (QName) bcEndpoint.getValueObj(EndpointBean.FULL_SERVICE_NAME);
                        final String portName = bcEndpoint.getValue(EndpointBean.ENDPOINT_NAME);
                        String uniqueName = null;

                        if (bcEndpoint.getValue(EndpointBean.ENDPOINT_TYPE).equals(EndpointBean.ENDPOINT_TYPE_INBOUND)) {
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
                        endPoints.add(bcEndpoint);

                        getQOSConfigurations(bcEndpoint, epDesc, mEndpointQos);    
                        
                        final Map<String, OperationMetaData> operationNameToMetaData = new HashMap<String, OperationMetaData>();

                        for (final JDBCOperation jdbcOperation : jdbcOperations) {
                            // TODO: keep all the info separately in
                            // endpointbean.
                            final BindingOperation bindingOperation = getParentBindingOperation(jdbcOperation,
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

                            final Binding binding = getBinding(matchedDef,
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

                            final Operation operation = getOperation(matchedDef,
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
                            Input input = operation.getInput();
				javax.wsdl.Message wsdlMessage = input.getMessage();
				Map parts = wsdlMessage.getParts();
				for (Iterator partIter = parts.values().iterator(); partIter.hasNext();) {
					Part aPart = (Part) partIter.next();
					QName type = (aPart.getElementName() != null) ? aPart
							.getElementName()
							: aPart.getTypeName();
					String partName = aPart.getName();
				// locate the XSD file based on the part type
					// namespace
					String xsdFileLoc = getXsdFileLocation(mXsds, type);
					colNamesTypes = org.glassfish.openesb.databasebc.util.XsdLocator.extractColTypes(new File(xsdFileLoc), colNamesTypes);                                
				}
                            
                            if (!(operation.getName().equalsIgnoreCase(JDBCOperations.OPERATION_TYPE_EXECUTE.toString()))) {
                                JDBCOperationInput jdbcOperationInput = null;

                                if (bindingOperation.getBindingInput() != null) {
                                    jdbcOperationInput = getJDBCOperationInput(bindingOperation.getBindingInput());
                                }

                                JDBCOperationOutput jdbcOperationOutput = null;

                                if (bindingOperation.getBindingOutput() != null) {
                                    jdbcOperationOutput = getJDBCOperationOutput(bindingOperation.getBindingOutput());
                                }

                                JDBCOperationInput jdbcSql = null;

                                if (jdbcOperationInput != null) {
                                    jdbcOperationInput.setJDBCSql(jdbcOperationInput);
                                }
                                jdbcSql = jdbcOperationInput.getJDBCSql();

                                final String jdbcSqlStr = jdbcSql.getSql().trim();
                                if(colNamesTypes!=null){
                                	jdbcOperationInput.setColNamesTypes(colNamesTypes);
                                }

                                if (jdbcSqlStr == null) {
                                    final String msg = "Missing sql under jdbc:input element in wsdl for this service " +
                                            epDesc.getServiceName().toString();
                                    final String exMsg = createExceptionMessage(mContext.getComponentName(),
                                            taskName, "FAILED",
                                            "JDBC_DEPLOY_HELPER_8", id, msg, null);
                                    throw new DeploymentException(exMsg);
                                }

                                JDBCBindingDeployer.mLogger.info("JDBC binding operation " +
                                        jdbcOperationInput.getOperationType() +
                                        " sql " + jdbcSqlStr);

                                // create operation meta data
                                final OperationMetaData opMetaData = new OperationMetaData();
                                opMetaData.setBindingOperation(bindingOperation);
                                opMetaData.setJDBCOperation(jdbcOperation);
                                opMetaData.setJDBCOperationInput(jdbcOperationInput);
                                opMetaData.setJDBCSql(jdbcSql);
                                opMetaData.setJDBCOperationOutput(jdbcOperationOutput);
                                opMetaData.setOperation(operation);
                                opMetaData.setDefinition(matchedDef);
                                operationNameToMetaData.put(bindingOperation.getName(),
                                        opMetaData);
                            } else { //process stored procedures
                                SPOperationInput jdbcSPOperationInput = null;

                                if (bindingOperation.getBindingInput() != null) {
                                    jdbcSPOperationInput = getJDBCSPOperationInput(bindingOperation.getBindingInput());
                                }

                                SPOperationOutput jdbcSPOperationOutput = null;

                                if (bindingOperation.getBindingOutput() != null) {
                                    jdbcSPOperationOutput = getJDBCSPOperationOutput(bindingOperation.getBindingOutput());
                                }

                                SPOperationInput spInput = null;

                                if (jdbcSPOperationInput != null) {
                                    jdbcSPOperationInput.setSPInput(jdbcSPOperationInput);
                                }
                                spInput = jdbcSPOperationInput.getSPInput();
                                final String jdbcSqlStr = jdbcSPOperationInput.getExecutionString().trim();
                                if (jdbcSqlStr == null) {
                                    final String msg = "Missing sql under jdbc:input element in wsdl for this service " +
                                            epDesc.getServiceName().toString();
                                    final String exMsg = createExceptionMessage(mContext.getComponentName(),
                                            taskName, "FAILED",
                                            "JDBC_DEPLOY_HELPER_8", id, msg, null);
                                    throw new DeploymentException(exMsg);
                                }

                                JDBCBindingDeployer.mLogger.info("JDBC binding operation " +
                                        jdbcSPOperationInput.getOperationType() +
                                        " sql " + jdbcSqlStr);

                                // create operation meta data
                                final OperationMetaData opMetaData = new OperationMetaData();
                                opMetaData.setBindingOperation(bindingOperation);
                                opMetaData.setJDBCOperation(jdbcOperation);
                                opMetaData.setJDBCSPOperationInput(jdbcSPOperationInput);
                                opMetaData.setSPInput(spInput);
                                opMetaData.setJDBCSPOperationOutput(jdbcSPOperationOutput);
                                opMetaData.setOperation(operation);
                                opMetaData.setDefinition(matchedDef);
                                operationNameToMetaData.put(bindingOperation.getName(),
                                        opMetaData);

                            }

                            bcEndpoint.setValueObj(EndpointBean.OPERATION_NAME_TO_META_DATA,
                                    operationNameToMetaData);
                        }
                    }
                }

            }
            // todo : should consider what happens if one endpoint fails to
            // activate
            // what type of cleanup should we do?
            final EndpointBean[] endPointsArr = endPoints.toArray(new EndpointBean[0]);
            mDeployedIds.put(id, asaDir);
            mDeployedEndpoints.put(id, endPointsArr);
        }

	protected String getXsdFileLocation(List mXsds, QName elemName) {
		if (elemName == null) {
			return null;
		}

		File aXsdFile = null;
		try {
			aXsdFile = org.glassfish.openesb.databasebc.util.XsdLocator.findXsdByElement(mXsds, elemName);
		} catch (Exception e) {
			
		}
		return aXsdFile != null ? aXsdFile.getAbsolutePath() : null;
	}
        /**
         *
         * @param asaId
         * @param asaDir
         * @param statusProviderHelper
         * @throws DeploymentException
         */
        private void undeploy(final String asaId, final File asaDir,
                final StatusProviderHelper statusProviderHelper)
                throws DeploymentException {
            final String taskName = "undeploy";

            if (JDBCBindingDeployer.mLogger.isLoggable(Level.INFO)) {
                JDBCBindingDeployer.mLogger.log(Level.INFO, "DBBC_R00213.BD_Undeploy_SU",
                        new Object[]{asaId, asaDir});
            }

            try {
                mDeployedIds.remove(asaId);

                final EndpointBean[] endpoints = mDeployedEndpoints.remove(asaId);

                if (endpoints != null) {
                    final StatusReporting reporting = statusProviderHelper.getStatusReporter();

                    for (EndpointBean element : endpoints) {
                        final QName serviceName = (QName) element.getValueObj(EndpointBean.FULL_SERVICE_NAME);
                        final String portName = element.getValue(EndpointBean.ENDPOINT_NAME);
                        String uniqueName = null;

                        if (element.getValue(
                                EndpointBean.ENDPOINT_TYPE).equals(EndpointBean.ENDPOINT_TYPE_INBOUND)) {
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
                        taskName, "FAILED", "JDBCBC_UNDEPLOY_HELPER_1", asaId,
                        "Failed to undeploy " + e.getMessage(), e);
                throw new DeploymentException(exMsg, e);
            }
        }

        private void applyApplicationConfiguration(JDBCAddress address,
                                                   Collection<ApplicationConfigurationField> appConfObj) {
            assert address != null;
            assert appConfObj != null;
            if (!appConfObj.isEmpty()) {
                Visitor addressVisitor = new AppConfigAddressVisitor(address);
                for (ApplicationConfigurationField field: appConfObj) {
                    field.accept(addressVisitor);
                }
            }
        }

        public Map<String, String[]> parseForEnvironmentVariables(String suPath, Map<String, String[]> envVariableMap)
            throws Exception {

            Map<String, String[]> envVariables =
                new HashMap<String, String[]>(envVariableMap);
        
            File catalog = new File(suPath + File.separator + catalogFileRelativePath);

            EntityResolver resolver = null;
        
            if ( catalog.exists() ) {
                CatalogManager catalogManager = new CatalogManager();
                catalogManager.setCatalogFiles(catalog.getAbsolutePath());
                catalogManager.setRelativeCatalogs(true);
                catalogManager.setUseStaticCatalog(false);        
                resolver = new CatalogResolver(catalogManager);
            }
            
            for (File file : listWSDLFiles(new File(suPath))) {
                envVariables.putAll(readWSDLForEnvVariables(file, resolver, envVariables));
            }
            
            return envVariables;
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
        private FileToDefinitionInfo[] readAllDefinitions(final File asaDir, final String id,
                final String taskName, final String locToken) throws DeploymentException {
	    File catalog = new File(asaDir.getAbsolutePath() + File.separator + catalogFileRelativePath);
            final CatalogManager catalogManager = new CatalogManager();
	    EntityResolver resolver = null;
        
            if ( catalog.exists() ) {
                catalogManager.setCatalogFiles(catalog.getAbsolutePath());
                catalogManager.setRelativeCatalogs(true);
                catalogManager.setUseStaticCatalog(false);        
                resolver = new CatalogResolver(catalogManager);
            }

            final List<File> wsdls = listWSDLFiles(asaDir);
            final File[] wsdlFiles = wsdls.toArray(new File[0]);

            // read all wsdl files to see if
            FileToDefinitionInfo[] fileToDefs = null;

            if (wsdlFiles != null) {
                fileToDefs = new FileToDefinitionInfo[wsdlFiles.length];

                for (int i = 0; i < wsdlFiles.length; i++) {
                    try {
                        final Definition def = readWSDL(wsdlFiles[i], resolver, mRuntimeConfig.retrieveApplicationVariablesMap());
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
		 private List readAllXSDs(final File asaDir, final String id,
                final String taskName, final String locToken) throws DeploymentException {
            final CatalogManager catalogManager = new CatalogManager();
            catalogManager.setCatalogFiles(asaDir.getAbsolutePath() + File.separator + "meta-inf" +
                    File.separator + "catalog.xml");
            catalogManager.setRelativeCatalogs(true);

            final EntityResolver resolver = new CatalogResolver(catalogManager);

            final List<File> xsds = listXSDFiles(asaDir);
            return xsds;
        }


        public void getQOSConfigurations (EndpointBean jdbcbcEndpoint,
                                          EndpointIdentifier endpointIdentifier,
                                          Map <EndpointInfo, List<ServiceQuality>> qosMap) {
            if (qosMap != null && qosMap.size() > 0) {
                // Until there's a total transitioning to use the common-util, there's a need to
                // create EndpointInfo using EndpointIndentifier
                EndpointInfo endpointInfo = new EndpointInfo (false, 
                                                              endpointIdentifier.getEndpointName(), 
                                                              null,
                                                              endpointIdentifier.getServiceName(),
                                                              null);
                List<ServiceQuality> qoss = qosMap.get(endpointInfo);
                Iterator<ServiceQuality> qossIter = qoss.iterator();
                while (qossIter.hasNext()) {
                    ServiceQuality qos = qossIter.next();
                    // Gather throttling config
                    if (qos instanceof ThrottlingConfig) {
                        ThrottlingConfig throttleConfig = (ThrottlingConfig)qos;
                        jdbcbcEndpoint.setMaxConcurrencyLimit(throttleConfig.getMaxConcurrencyLimit());
                    }
                }
            }
        }


        /**
         *
         * @author Administrator
         *
         */
        class FileToDefinitionInfo {

            private File mFile;
            private Definition mDefinition;

            private FileToDefinitionInfo(final File file, final Definition definition) {
                mFile = file;
                mDefinition = definition;
            }

            private File getFile() {
                return mFile;
            }

            private Definition getDefinition() {
                return mDefinition;
            }
        }
    }
}
