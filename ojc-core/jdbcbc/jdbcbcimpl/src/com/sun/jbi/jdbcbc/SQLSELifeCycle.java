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
 * @(#)SQLSELifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.MBeanNames;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.xml.sax.SAXException;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.jdbcbc.util.AlertsUtil;
import com.sun.jbi.jdbcbc.util.ReadWriteTextFile;
import javax.management.NotificationListener;
import javax.management.Notification;
import javax.management.AttributeChangeNotification;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;

/**
 * SE life cycle implementation of the jbi framework
 *
 */
public class SQLSELifeCycle implements ComponentLifeCycle, Component {
    private static final Messages mMessages = Messages.getMessages(SQLSELifeCycle.class);
    private static Logger mLogger = Messages.getLogger(SQLSELifeCycle.class);
    private static final Map<String,SQLSELifeCycle> initializedLifeCycles = new HashMap<String,SQLSELifeCycle>();

    // A short display name
    public static final String SHORT_DISPLAY_NAME = "SQL SE";
    private JDBCComponentContext mContext = JDBCComponentContext.getInstance();
    private OutboundReceiver mOutboundReceiver;
//    private InboundReceiver mInboundReceiver;
    private Thread mOutboundReceiverThread;
    Map<String,EndpointBean> mEndpoints = new HashMap<String,EndpointBean>();
    Map mEndpointMapping = new HashMap();
    private ObjectName mExtensionMBeanName;
    private MessagingChannel mChannel;
    private ObjectName mDeployerMBeanName;
    private SQLSEServiceUnitManager mSQLSUDeployer;
    private StatusProviderHelper mStatusProviderHelper;
    private SQLSERuntimeConfiguration mRuntimeConfig;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    private SQLMapEntryTable mSQLMapEntryTable;
    private ObjectName mProcessingExtensionMBeanName;

	// JBI MBean type
    static final String PROCESSING_EXTENSION = "ProcessingExtension"; // NOI18N

    // "Official" Performance Instrumentation Categories
    public static final String PERF_CAT_NORMALIZATION   = "Normalization"; // NOI18N
    public static final String PERF_CAT_DENORMALIZATION = "Denormalization"; // NOI18N
    public static final String [] SQLSE_PERF_CATEGORIES = 
                                       new String[] {PERF_CAT_NORMALIZATION,
                                                     PERF_CAT_DENORMALIZATION};

    public SQLSELifeCycle() {
    }

    //@Override
    public ComponentLifeCycle getLifeCycle() {
        return this;
    }

    //@Override
    public ServiceUnitManager getServiceUnitManager() {
        return mSQLSUDeployer;
    }

    private ObjectName getDeploymentMBeanName() {
        return mDeployerMBeanName;
    }

    //@Override
    public ObjectName getExtensionMBeanName() {
        return mExtensionMBeanName;
    }

    //@Override
    public void init(final ComponentContext jbiContext) throws JBIException {
        if (jbiContext == null) {
            throw new JBIException("Component Context is null");
        }

        mContext.setContext(jbiContext);
        Messages.registerContext(mContext.getContext());
        SQLSELifeCycle.mLogger = Messages.getLogger(SQLSELifeCycle.class);
        SQLSELifeCycle.initializedLifeCycles.put(mContext.getContext().getComponentName(), this);

        final MBeanServer mbServer = jbiContext.getMBeanServer();
        final MBeanNames mbnHndl = jbiContext.getMBeanNames();
		
        mProcessingExtensionMBeanName = mbnHndl.createCustomComponentMBeanName(PROCESSING_EXTENSION);

		
		try {
            //mChannel = mContext.getDeliveryChannel();
            mChannel = new BaseMessagingChannel(mContext.getContext());
            mContext.setBindingChannel(mChannel);
        } catch(MessagingException me) {
            String text = mMessages.getString("JDBCBC-E00141.No_binding_channel", me.getLocalizedMessage());
            mLogger.log(Level.SEVERE, text, me);
            AlertsUtil.getAlerter().critical(text, 
                                             JDBCBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "JDBCBC-E00141");
            throw me;
        }

        mSQLMapEntryTable = new SQLMapEntryTable();
        mSQLSUDeployer = new SQLSEServiceUnitManager(mContext, this,
                mSQLMapEntryTable);

        try {
            mStatusProviderHelper = new StatusProviderHelper(SQLSELifeCycle.SHORT_DISPLAY_NAME,
                    StatusProviderMBean.COMPONENT_TYPE_ENGINE,
                    jbiContext.getComponentName(), jbiContext.getMBeanServer());
            mStatusProviderHelper.registerMBean();

            if (SQLSELifeCycle.mLogger.isLoggable(Level.INFO)) {
                SQLSELifeCycle.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00121.SQLSELC_Register_mbean"),
                    jbiContext.getComponentName());
            }
        } catch (final Exception ex) {
            SQLSELifeCycle.mLogger.log(Level.WARNING, mMessages.getString("SQLSE_E00122.SQLSELC_Failed_register_mbean"), ex);
            throw new JBIException(SQLSELifeCycle.mMessages.getString(
                    "SQLSELC_Failed_register_mbean"), ex);
        }

		try {
            ObjectName statusMBeanObjName = mbnHndl.createCustomComponentMBeanName("Statistics");
            mStatusProviderHelper = new StatusProviderHelper(SHORT_DISPLAY_NAME, statusMBeanObjName, jbiContext.getMBeanServer());
            mStatusProviderHelper.registerMBean(SQLSE_PERF_CATEGORIES, new SQLSEPerformanceMeasurement());
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Registered Status Provider MBean for " + jbiContext.getComponentName());
            }            
        } catch (Exception ex) {
            String text = mMessages.getString("SQLSE_E00122.SQLSELC_Failed_register_mbean", ex.getMessage());
            AlertsUtil.getAlerter().critical(text, 
                                             SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_ENGINE,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "SQLSE_E00122");
            throw new JBIException(text, ex);
        }

        try {
            // add the details for the component configuration using the templates...
            String configData = "";
            String configSchema = "";
            String configDataFileLoc = jbiContext.getInstallRoot() + File.separator
                +"META-INF" + File.separator + "componentConfiguration.xml";
            File configDataFile = new File(configDataFileLoc);
            if (configDataFile.exists()) {
                configData = ReadWriteTextFile.getContents(configDataFile);
            }
            String configSchemaFileLoc = jbiContext.getInstallRoot() + File.separator
                +"META-INF" + File.separator + "componentConfiguration.xsd";
            File configSchemaFile = new File(configSchemaFileLoc);
            if (configSchemaFile.exists()) {
                configSchema = ReadWriteTextFile.getContents(configSchemaFile);
            }
            
            mRuntimeConfig = new SQLSERuntimeConfiguration(mContext.getContext().getWorkspaceRoot());
            
            // set the config schema and config data on the runtime configuration
            mRuntimeConfig.setConfigurationDisplayData(configData);
            mRuntimeConfig.setConfigurationDisplaySchema(configSchema);
            
            mRuntimeConfigHelper = new RuntimeConfigurationHelper(RuntimeConfigurationHelper.COMPONENT_TYPE_ENGINE,
                    jbiContext.getComponentName(), jbiContext.getMBeanServer());
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);

            if (SQLSELifeCycle.mLogger.isLoggable(Level.INFO)) {
                SQLSELifeCycle.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00123.SQLSELC_Register_config_mbean"),
                    jbiContext.getComponentName());
            }
            
            // Subscribe for changes to the configuration
            mRuntimeConfig.addNotificationListener(listener, null, null);
            
        } catch (final Exception ex) {
            SQLSELifeCycle.mLogger.log(Level.WARNING, mMessages.getString("SQLSE_E00124.SQLSELC_Failed_register_config_mbean"),
                ex);
            throw new JBIException(mMessages.getString(
                    "SQLSE_E00124.SQLSELC_Failed_register_config_mbean"), ex);
        }


        //try {
            mChannel = mContext.getBindingChannel();
        /*} catch (final MessagingException me) {
            mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_E00125.SQLSELC_Failed_DC"), me);
            String text = mMessages.getString("SQLSE_E00122.SQLSELC_Failed_register_mbean", me.getLocalizedMessage());
            AlertsUtil.getAlerter().critical(text, 
                                             SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_ENGINE,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "SQLSE_E00122");
            throw me;
        }*/

        
        
        
        
        try {
            startOutbound();
           // startInbound();
        } catch (final Exception ex) {
            mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_R00126.SQLSELC_Failed_start_SE"), ex);
            throw new JBIException(mMessages.getString(
                    "SQLSE_R00126.SQLSELC_Failed_start_SE") + ex.getMessage(), ex);
        }
    }

    //@Override
    public void shutDown() throws JBIException {
        SQLSELifeCycle.mLogger.log(Level.INFO, "SQLSELC_Shutdown_SE");

        try {
            stopOutbound();
            //stopInbound();
        } catch (final Exception ex) {
            mLogger.log(Level.SEVERE, "SQLSELC_Failed_stop_SE", ex);
            throw new JBIException(mMessages.getString("SQLSELC_Failed_stop_SE") +
                ex.getMessage(), ex);
        }

        if (mChannel != null) {
            mChannel.close();
        }

        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (final Exception ex) {
            mLogger.log(Level.WARNING,
                "Failed to un-register status provider MBean for " +
                mContext.getContext().getComponentName(), ex);
            throw new JBIException(
                "Failed to un-register status provider MBean for " +
                mContext.getContext().getComponentName(), ex);
        }

        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (final Exception ex) {
            mLogger.log(Level.WARNING, "SQLSELC_Failed_unregister_mbean",
                mContext.getContext().getComponentName());
            throw new JBIException(mMessages.getString(
                    "SQLSELC_Failed_unregister_mbean") +
                mContext.getContext().getComponentName(), ex);
        }

        SQLSELifeCycle.initializedLifeCycles.remove(mContext.getContext().getComponentName());
        SQLSELifeCycle.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00128.SQLSELC_Shutdown_SE_DONE") );
    }

    //@Override
    public void start() throws JBIException {
        SQLSELifeCycle.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00129.SQLSELC_started") );
    }

    //@Override
    public void stop() throws JBIException {
        SQLSELifeCycle.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00130.SQLSELC_stopped") );
    }

    //@Override
    public boolean isExchangeWithConsumerOkay(final ServiceEndpoint endpoint,
        final MessageExchange exchange) {
        // TODO: check whether operation on endpoint actually exists.
        return true;
    }

    //@Override
    public boolean isExchangeWithProviderOkay(final ServiceEndpoint endpoint,
        final MessageExchange exchange) {
        return false;
    }

    //@Override
    public Document getServiceDescription(final ServiceEndpoint endpoint) {
        Document result = null;

        // TODO: The document returned should be a stand-alone document (no imports or includes)
        // TODO: consider whether it should only return the abstract wsdl concerning the endpoint
        // TODO: Beware for service engines that they HAVE TO include a specific binding type defined for Service Engines
        final String uniqueName = EndpointBean.getUniqueName(endpoint.getServiceName()
                                                               .toString(),
                endpoint.getEndpointName(), EndpointBean.ENDPOINT_TYPE_OUTBOUND);
        final DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
        final DocumentBuilder documentBuilder = null;
        final EndpointBean foundEndpoint = mEndpoints.get(uniqueName);

        if (foundEndpoint == null) {
            SQLSELifeCycle.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00139.SQLSELC_FAILED_LOCATE_EP") );
        } else {
            try {
                final File matchedWSDL = (File) foundEndpoint.getValueObj(EndpointBean.WSDL_FILE);

                //result = (Document) foundEndpoint
                //		.getValueObj(EndpointBean.DESCRIPTOR);
                result = documentBuilder.parse(matchedWSDL);
            } catch (final SAXException ex) {
                mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00140.SQLSELC_FAILED_SAX") );
            } catch (final IOException exIO) {
                mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00141.SQLSELC_FAILED_IO") );

                //throw exIO;
            } catch (final Exception exception) {
                mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_R00142.SQLSELC_FAILED_Ex") );

                //throw exception;
            }
        }

        return result;
    }

    //@Override
    public ServiceEndpoint resolveEndpointReference(final DocumentFragment fragment) {
        // Currently we do not support dynamic endpoints
        return null;
    }

    StatusProviderHelper getStatusProviderHelper() {
        return mStatusProviderHelper;
    }

    private static SQLSELifeCycle getInstanceForId(final String componentName) {
        return SQLSELifeCycle.initializedLifeCycles.get(componentName);
    }

    private DeliveryChannel getDeliveryChannel() {
        return mChannel;
    }

    private EndpointBean getEndpointBeanForContext(final String context) {
        return (EndpointBean) mEndpointMapping.get(context);
    }

    protected void activateEndpoints(final EndpointBean[] endpoints) throws MessagingException {
        SQLSELifeCycle.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00135.SQLSELC_START_EP_ACTIVATE") );

        EndpointBean currEndpoint = null;

        for (EndpointBean element : endpoints) {
            currEndpoint = element;

            final String uniqueName = currEndpoint.getUniqueName();
            mEndpoints.put(uniqueName, currEndpoint);

            // For inbound endpoints, build an additional mapping from the URL context to the endpoint
            /*if (currEndpoint.getValue(EndpointBean.ENDPOINT_TYPE)
                                .equals(EndpointBean.ENDPOINT_TYPE_INBOUND)) {
                try {
                    mInboundReceiver.addInboundMessageProcessor(currEndpoint);
                } catch (final FaultException ex) {
                    SQLSELifeCycle.mLogger.log(Level.SEVERE,
                        "SQLSELC_SU_Failed_start_inbound_EP",
                        new Object[] { uniqueName, ex.getMessage() });
                } catch (final JBIException me) {
                    SQLSELifeCycle.mLogger.log(Level.SEVERE, "SQLSELC_FAILED_EP_ACTIVATE",
                        new Object[] { uniqueName, me.getMessage() });
                }
            } else
			*/
			if (EndpointBean.ENDPOINT_TYPE_OUTBOUND.equals(
                        currEndpoint.getValue(EndpointBean.ENDPOINT_TYPE))) {
                // Activate an outbound endpoint
                try {
                    final QName fullServiceName = (QName) currEndpoint.getValueObj(EndpointBean.FULL_SERVICE_NAME);
                    final ServiceEndpoint endpointReference = mContext.getContext().activateEndpoint(fullServiceName,
                            currEndpoint.getValue(EndpointBean.ENDPOINT_NAME));
                    currEndpoint.setValueObj(EndpointBean.ENDPOINT_REFERENCE,
                        endpointReference);

                    if (mLogger.isLoggable(Level.INFO)) {
                        mLogger.log(Level.INFO,
                            "Activated outbound endpoint " + uniqueName);
                    }
                } catch (final JBIException me) {
                    mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_R00138.SQLSELC_FAILED_EP_ACTIVATE"),
                        new Object[] { uniqueName, me.getMessage() });
                }
            }

            currEndpoint.setValue(EndpointBean.STATUS,
                EndpointBean.STATUS_RUNNING);
        }

        SQLSELifeCycle.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00319.SQLSESU_ENDPOINT_ACTIVATED"));
    }

    private void deactivateEndpoints(final EndpointBean[] endpoints)
        throws MessagingException {
        EndpointBean currEndpoint = null;
        SQLSELifeCycle.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00143.SQLSELC_START_EP_DEACTIVATE") );

        for (EndpointBean element : endpoints) {
            currEndpoint = element;

            final String uniqueName = currEndpoint.getUniqueName();
            mEndpoints.remove(uniqueName);

            /*if (currEndpoint.getValue(EndpointBean.ENDPOINT_TYPE)
                                .equals(EndpointBean.ENDPOINT_TYPE_INBOUND)) {
                try {
                    final ServiceEndpoint endpointReference = (ServiceEndpoint) currEndpoint.getValueObj(EndpointBean.ENDPOINT_REFERENCE);
                    mContext.deactivateEndpoint(endpointReference);
                } catch (final JBIException me) {
                    SQLSELifeCycle.mLogger.log(Level.SEVERE, "SQLSELC_FAILED_EP_DEACTIVATE",
                        new Object[] { uniqueName, me.getMessage() });
                }

                mInboundReceiver.removeInboundMessageProcessor(currEndpoint);
            } else */
			
			if(currEndpoint.getValue(EndpointBean.ENDPOINT_TYPE)
                                .equals(EndpointBean.ENDPOINT_TYPE_OUTBOUND)){
                try {
                    final ServiceEndpoint endpointReference = (ServiceEndpoint) currEndpoint.getValueObj(EndpointBean.ENDPOINT_REFERENCE);
                    mContext.getContext().deactivateEndpoint(endpointReference);
                } catch (final JBIException me) {
                    SQLSELifeCycle.mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_R00144.SQLSELC_FAILED_EP_DEACTIVATE"),
                        new Object[] { uniqueName, me.getMessage() });
                }
            }

            currEndpoint.setValue(EndpointBean.STATUS,
                EndpointBean.STATUS_STOPPED);
        }

        SQLSELifeCycle.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00145.SQLSELC_EP_DEACTIVATE") );
    }

    /*void startInbound() throws JBIException {
        mInboundReceiver = new InboundReceiver(mChannel, mEndpoints,
                mRuntimeConfig, mContext);
        SQLSELifeCycle.mLogger.log(Level.INFO, "SQLSELC_START_INBOUND");
    }*/

    private void startOutbound() throws JBIException {
        mOutboundReceiver = new OutboundReceiver(mChannel, mEndpoints,
                mRuntimeConfig, mContext);
        mOutboundReceiverThread = new Thread(mOutboundReceiver);
        mOutboundReceiverThread.start();
        SQLSELifeCycle.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00147.SQLSELC_START_OUTBOUND") );
    }

    /*void stopInbound() {
        if (mInboundReceiver != null) {
            mInboundReceiver.stopReceiving();
        }

        SQLSELifeCycle.mLogger.log(Level.INFO, "SQLSELC_STOP_INBOUND");
    }*/

    private void stopOutbound() {
        if (mOutboundReceiver != null) {
            mOutboundReceiver.stopReceiving();
        }

        SQLSELifeCycle.mLogger.log(Level.INFO, mMessages.getString("SQLSE_R00149.SQLSELC_STOP_OUTBOUND") );
    }

	/*private void populateEndpointWSDLInfo(EndpointStatus endpointStatus, Endpoint endpoint) throws Exception {
        // Set the resource info on the endpoint status if it is available
        Map importedResources = new HashMap();
        Transformer transformer = mTransformerPool.retrieve();
        if (endpointStatus != null) {
            javax.wsdl.Definition originalWsdlDef = endpoint.getServiceDescriptor();
            StringWriter originalWsdl = new StringWriter();
            WSDLFactory wsdlFactory = (WSDLFactory)WSDLFactory.newInstance();
            WSDLWriter writer = (WSDLWriter)wsdlFactory.newWSDLWriter();
            writer.writeWSDL(originalWsdlDef, originalWsdl);
            endpointStatus.setWSDLDefinition(originalWsdl.toString());
            
            for (Iterator imports = endpoint.getImportedWSDLDefinitions().values().iterator(); imports.hasNext();) {
                Definition aImportedWSDL = (Definition) imports.next();
                if (aImportedWSDL != null) {
                    StringWriter sWriter = new StringWriter();
                    writer.writeWSDL(aImportedWSDL, sWriter);
                    importedResources.put(aImportedWSDL.getTargetNamespace(), sWriter.toString());
                }
            }
            
            for (Iterator imports = endpoint.getImportedXSDSchemas().values().iterator(); imports.hasNext();) {
                Element aImportedSchema = (Element) imports.next();
                if (aImportedSchema != null) {
                    Source src = new DOMSource(aImportedSchema);
                    ByteArrayOutputStream baos = new ByteArrayOutputStream();
                    StreamResult dest = new StreamResult(baos);
                    transformer.transform(src, dest);
                    importedResources.put(aImportedSchema.getAttribute("targetNamespace"), baos.toString());
                }
            }
            endpointStatus.setWSDLImportedResources(importedResources);
            mTransformerPool.relinquish(transformer);
        }

    }*/

    // Inner class to handle configuration change notifications
    private NotificationListener listener = new NotificationListener() {
        public void handleNotification(Notification notification, Object obj) {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                if (attrName.equals(RuntimeConfiguration.CONFIG_THREADS)) {
                    Integer newVal = (Integer) (attrNotif.getNewValue());
                    try{
                        mRuntimeConfig.setThreads(newVal.intValue());
                    }catch(Exception me){
                         mLogger.log(Level.SEVERE, mMessages.getString("SQLSE_E00125.SQLSELC_Failed_DC"), me);
                        String text = mMessages.getString("SQLSE_E00122.SQLSELC_Failed_register_mbean", me.getLocalizedMessage());
                        AlertsUtil.getAlerter().critical(text, 
                                             SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_ENGINE,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "SQLSE_E00122");
                        //throw me;
                    }
                }
            }
        }
    };
    
}
