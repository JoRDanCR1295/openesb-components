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
 * @(#)HttpSoapBindingLifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.alerter.NotificationEvent;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.httpsoapbc.configuration.RuntimeConfiguration;
import com.sun.jbi.httpsoapbc.embedded.EmbeddedServerController;
import com.sun.jbi.httpsoapbc.jaxwssupport.JAXWSEndpointFactory;
import com.sun.jbi.httpsoapbc.management.HTTPManagement;
import com.sun.jbi.httpsoapbc.management.HTTPManagementMBean;
import com.sun.jbi.httpsoapbc.management.HTTPManagementMBeanHelper;
import com.sun.jbi.httpsoapbc.proxy.HttpProxy;
import com.sun.jbi.httpsoapbc.security.http.impl.BasicAuthenticator;
import com.sun.jbi.httpsoapbc.util.TransformerPool;
import com.sun.jbi.httpsoapbc.util.AlertsUtil;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusReporting;
import com.sun.jbi.internationalization.Messages;
import com.sun.xml.ws.api.server.WSEndpoint;

import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.JBIException;
import javax.jbi.management.MBeanNames;
import javax.jbi.messaging.MessagingException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.ObjectName;
import javax.management.ObjectInstance;
import javax.management.StandardMBean;
import javax.management.MBeanServer;
import javax.management.JMException;
import javax.management.NotificationListener;
import javax.management.Notification;
import javax.management.AttributeChangeNotification;
import javax.wsdl.Definition;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLWriter;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.Transformer;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;


import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.ByteBuffer;
import java.io.StringWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;
import java.util.Set;
import java.util.Collections;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.sun.jbi.httpsoapbc.async.AsyncResponseDispatcher;
import com.sun.jbi.httpsoapbc.DispatchPool;
import com.sun.jbi.httpsoapbc.embedded.EndpointsManagerHttpHandler;
import com.sun.jbi.httpsoapbc.embedded.GrizzlyEmbeddedWebContainer;
import java.util.HashSet;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.grizzly.http.server.NetworkListener;


/**
 * HTTP binding component ComponentLifeCycle implementation
 */
public class HttpSoapBindingLifeCycle
        implements ComponentLifeCycle, EndpointChangeListener {

    private static final String JBI_DESC_ID_NS = "http://www.sun.com/jbi/descriptor/identification/v1.0"; // NO i18n
    private static final String JBI_VERSIONINFO_TAG = "VersionInfo"; // NO i18n
    private static final String JBI_COMPONENTVER_ATTR = "component-version"; // NO i18n
    private static final String JBI_BUILDVER_ATTR = "build-number"; // NO i18n
    private static final String WS_ADDRESSING_NS = "http://schemas.xmlsoap.org/ws/2004/08/addressing";
    private static final Messages mMessages =
        Messages.getMessages(HttpSoapBindingLifeCycle.class);
    private Logger mLogger;

    // Default container port in case that it can not dynamically determine them. 
    public static final Integer DEFAULT_CONTAINER_PORT = new Integer(8282);
    public static final Integer DEFAULT_SSL_CONTAINER_PORT = new Integer(443);
    
    // A short display name
    public static final String SHORT_DISPLAY_NAME = "sun-http-binding";

    // ProcessingExtension MBean type
    static final String PROCESSING_EXTENSION = "ProcessingExtension"; // NOI18N

    // "Official" Performance Instrumentation Categories
    public static final String PERF_CAT_NORMALIZATION   = "Normalization"; // NOI18N
    public static final String PERF_CAT_DENORMALIZATION = "Denormalization"; // NOI18N
    public static final String [] HTTPSOAPBC_PERF_CATEGORIES = 
                                       new String[] {PERF_CAT_NORMALIZATION,
                                                     PERF_CAT_DENORMALIZATION};

    // JAX-WS actually expects a fixed reusable minimum pool size of 5.
    // So that's what we will set here.
    int mInboundCorePoolSize = 5;
    int mOutboundKeepAliveTime = 60 * 10;
    TimeUnit mOutboundTimeUnit = TimeUnit.SECONDS;
    // By using an unbounded queue the max pool size becomes irrelevant
    int mInboundMaxPoolSize = Integer.MAX_VALUE;
    

    private String mComponentName;
    private ComponentContext mContext;
    private OutboundReceiver mOutboundReceiver;
    private Thread mOutboundReceiverThread;
    private Map mEndpoints = new HashMap();
    private Map mPortReferenceCountMap = new HashMap();
    private Map mEndpointMapping = Collections.synchronizedMap(new HashMap());
    private ObjectName mExtensionMBeanName;
    private ObjectName mProcessingExtensionMBeanName;
    private ObjectName mDeployerMBeanName;
    private MessagingChannel mChannel;
    private EmbeddedServerController mEmbeddedController;
    
    private StatusProviderHelper mStatusProviderHelper;
    
    private RuntimeConfiguration mRuntimeConfig;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    
    private HTTPManagementMBeanHelper mManagementMBeanHelper;
    private HTTPManagementMBean mManagementMBean;
    
    private JAXWSEndpointFactory mJAXWSEndpointFactory;
    private HttpProxy mProxy;
    private BasicAuthenticator mBasicAuthenticator;
    private volatile ThreadPoolExecutor inboundThreadPool;
    private TransformerPool mTransformerPool;        
    // Inner class to handle configuration change notifications
    private NotificationListener listener = new NotificationListener() {
        public void handleNotification(Notification notification, Object obj) {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
                String attrName = attrNotif.getAttributeName();
                if (attrName.equals(RuntimeConfiguration.CONFIG_INBOUND_THREADS)) {
                    Integer newVal = (Integer) (attrNotif.getNewValue());
                    setThreads(newVal.intValue());
                }
            }
        }
    };
    
    // TO DO: this should be obsolete
    private List appserverContainerPorts = new ArrayList();
    
    /**
     * Internal handle to the Service Unit Manager Implementation.
     */
    private HttpSoapBindingDeployer mServiceUnitManager;

    public HttpSoapBindingLifeCycle() {
        mTransformerPool = new TransformerPool();
    }

    public ObjectName getDeploymentMBeanName() {
        return mDeployerMBeanName;
    }

    public ObjectName getExtensionMBeanName() {
        return mExtensionMBeanName;
    }

    public void init(ComponentContext jbiContext) throws JBIException {
        if (jbiContext ==  null) {
            throw new JBIException(mMessages.getString("HTTPBC-E00155.Null_component_context"));
        }
        mContext = jbiContext;
        mComponentName = jbiContext.getComponentName();
        
        HttpSoapComponentContext.getInstance().setContext(mContext);
        HttpSoapComponentContext.getInstance().setAssociatedLifeCycle(this);
        Messages.registerContext(mContext);
        mLogger = Messages.getLogger(getClass());

        mJAXWSEndpointFactory = new JAXWSEndpointFactory();
        mEmbeddedController = new EmbeddedServerController(this);
        
        MBeanServer mbServer = jbiContext.getMBeanServer();
        MBeanNames mbnHndl = jbiContext.getMBeanNames();
        mProcessingExtensionMBeanName = mbnHndl.createCustomComponentMBeanName(PROCESSING_EXTENSION);
        try {
            StandardMBean extensionBean = new StandardMBean(new Extension(), ExtensionMBean.class);
            if (!mbServer.isRegistered(mProcessingExtensionMBeanName)) {
                mbServer.registerMBean(extensionBean, mProcessingExtensionMBeanName);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Registered extension MBean with name "
                            + mProcessingExtensionMBeanName);
                }
            } else {
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Skipping registration of Extension MBean "
                            + mProcessingExtensionMBeanName
                            + " because it is already registered.");
                }
            }
        } catch (JMException ex) {
            String text = mMessages.getString("HTTPBC-W00121.Exception_during_extension_mbean_register", ex.getMessage());
            AlertsUtil.getAlerter().critical(text, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-W00121");
            throw new JBIException(text, ex);
        }
        
        try {
            ObjectName statusMBeanObjName = mbnHndl.createCustomComponentMBeanName("Statistics");
            mStatusProviderHelper = new StatusProviderHelper(SHORT_DISPLAY_NAME, statusMBeanObjName, jbiContext.getMBeanServer());
            mStatusProviderHelper.registerMBean(HTTPSOAPBC_PERF_CATEGORIES, new HttpSoapPerformanceMeasurement());
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Registered Status Provider MBean for " + jbiContext.getComponentName());
            }            
        } catch (Exception ex) {
            String text = mMessages.getString("HTTPBC-W00120.Exception_during_status_mbean_register", ex.getMessage());
            AlertsUtil.getAlerter().critical(text, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-W00120");
            throw new JBIException(text, ex);
        }
        
        try {
            KeyStoreUtilClient keystoreUtil = new KeyStoreUtilClient(mContext);
            mRuntimeConfig = new RuntimeConfiguration(mContext.getWorkspaceRoot(), keystoreUtil);
            ObjectName runtimeConfigMBeanObjName = mbnHndl.createCustomComponentMBeanName("Configuration");
            mRuntimeConfigHelper = new RuntimeConfigurationHelper(runtimeConfigMBeanObjName, jbiContext.getMBeanServer());
            mRuntimeConfigHelper.registerMBean(mRuntimeConfig);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Registered runtime configuration MBean for " + jbiContext.getComponentName());
            }
            
            // Apply existing configuration
            Integer threadCount = mRuntimeConfig.getInboundThreads();
            if (threadCount != null) {
                setThreads(threadCount.intValue());
            }

            if(inboundThreadPool == null) {
                synchronized(this) {
                    ThreadPoolQueue inBoundThreadpoolQ = new ThreadPoolQueue();
                    inboundThreadPool = new ThreadPoolExecutor(mInboundCorePoolSize, mInboundMaxPoolSize, mOutboundKeepAliveTime, mOutboundTimeUnit, inBoundThreadpoolQ, new DaemonThreadFactory());
                    inBoundThreadpoolQ.setThreadPoolExecutor(inboundThreadPool);
                }
            }
            
            // Subscribe for changes to the configuration
            mRuntimeConfig.addNotificationListener(listener, null, null);        

        } catch (Exception ex) {
            String text = mMessages.getString("HTTPBC-W00122.Exception_during_runtimecfg_mbean_register", ex.getLocalizedMessage());
            AlertsUtil.getAlerter().critical(text, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-W00122");
            throw new JBIException(text, ex);
        }
        
        
        try {
            ObjectName managementMBeanObjName = mbnHndl.createCustomComponentMBeanName("Administration");
            mManagementMBean = new HTTPManagement(this);
            mManagementMBeanHelper = new HTTPManagementMBeanHelper(managementMBeanObjName, jbiContext.getMBeanServer());
            mManagementMBeanHelper.registerMBean(mManagementMBean);
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Registered management MBean for " + jbiContext.getComponentName() + " with name " + managementMBeanObjName);
            }
        } catch (Exception ex) {
            String text = mMessages.getString("HTTPBC-W00126.Exception_during_management_mbean_register", ex.getMessage());
            AlertsUtil.getAlerter().critical(text, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-W00126");
            throw new JBIException(text, ex); 
        }


        // Move to here for now.  Relies on the mStatusProviderHelper being set first in
        // this object for the HttpSoapBindingDeployer to work properly.  We'll need to
        // fix this later.
        mServiceUnitManager = new HttpSoapBindingDeployer(jbiContext, mRuntimeConfig);
        mServiceUnitManager.addEndpointChangeListener(this);
    }

    public void shutDown() throws JBIException {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "HTTPBC-R00102.Shutting_down_binding", mComponentName);
        }

        if(mChannel != null) {
            mChannel.close();
        }
        try {
            MBeanServer mbServer = mContext.getMBeanServer();
            mbServer.unregisterMBean(mProcessingExtensionMBeanName);
        } catch (Exception ex) {
            String text = mMessages.getString("HTTPBC-W00124.Exception_during_extension_mbean_deregister", ex.getLocalizedMessage());
            mLogger.log(Level.WARNING, text, ex);
            AlertsUtil.getAlerter().warning(text, 
                                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "HTTPBC-W00124");
        }
        
        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (Exception ex) {
            String text = mMessages.getString("HTTPBC-W00123.Exception_during_status_mbean_deregister", ex.getLocalizedMessage());
            AlertsUtil.getAlerter().critical(text, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-W00123");
            throw new JBIException(text, ex);
        }
        
        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception ex) {
            String text = mMessages.getString("HTTPBC-W00125.Exception_during_runtimecfg_mbean_deregister", ex.getLocalizedMessage());
            AlertsUtil.getAlerter().critical(text, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-W00125");
            throw new JBIException(text, ex);
        }
        
        try {
            mManagementMBeanHelper.unregisterMBean();
        } catch (Exception ex) {
            String text = mMessages.getString("HTTPBC-W00127.Exception_during_management_mbean_deregister", ex.getLocalizedMessage());
            AlertsUtil.getAlerter().critical(text, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-W00127");
            throw new JBIException(text, ex);
        }
        
        // stopAll also stops the default HTTP/HTTPS ports
        mEmbeddedController.stopAll();   
        Messages.unregisterContext();
        
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "HTTPBC-R00103.Binding_shutdown", mComponentName);
        }
        
        AlertsUtil.getAlerter().info(mMessages.getString("HTTPBC-R00103.Binding_shutdown", mComponentName), 
                                     HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                     null, 
                                     AlertsUtil.getServerType(),
                                     AlertsUtil.COMPONENT_TYPE_BINDING,
                                     NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                     NotificationEvent.EVENT_TYPE_ALERT,
                                     "HTTPBC-R00103");
    }

    public void start() throws JBIException {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "HTTPBC-R00100.Binding_starting", mComponentName);
        }
        
        try {
            mChannel = new BaseMessagingChannel(mContext);
            HttpSoapComponentContext.getInstance().setBindingChannel(mChannel);
        } catch(MessagingException me) {
            String text = mMessages.getString("HTTPBC-E00141.No_binding_channel", me.getLocalizedMessage());
            mLogger.log(Level.SEVERE, text, me);
            AlertsUtil.getAlerter().critical(text, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-E00141");
            throw me;
        }
        
        try {
            // start the default HTTP/HTTPS ports if they are defined
            startDefaultHttpPorts();
            startOutbound();
            startAsyncResponseDispatcher();
            startDispatchPool();
        } catch (Exception ex) {
            String text = mMessages.getString("HTTPBC-E00101.Start_failed", new Object[] { mComponentName, ex.getLocalizedMessage() });
            AlertsUtil.getAlerter().critical(text, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-E00101");
            throw new JBIException(text, ex);
        }

        logComponentInfo();
    }

    public void stop() throws JBIException {
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, "HTTPBC-R00104.Stopping_binding", mComponentName);
        }
        try {
            AsyncResponseDispatcher.uninitialize();
            stopOutbound();
            DispatchPool.uninitialize();
            if (mLogger.isLoggable(Level.INFO)) {
                mLogger.log(Level.INFO, "HTTPBC-R00105.Binding_stopped", mComponentName);                
            }
            
            AlertsUtil.getAlerter().info(mMessages.getString("HTTPBC-R00105.Binding_stopped", mComponentName), 
                                         HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                         null, 
                                         AlertsUtil.getServerType(),
                                         AlertsUtil.COMPONENT_TYPE_BINDING,
                                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                         NotificationEvent.EVENT_TYPE_ALERT,
                                         "HTTPBC-R00105");        
        } catch (Exception ex) {
            String text = mMessages.getString("HTTPBC-E00104.Stopping_binding_failed", ex.getLocalizedMessage());
            AlertsUtil.getAlerter().critical(text, 
                                             HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                             null, 
                                             AlertsUtil.getServerType(),
                                             AlertsUtil.COMPONENT_TYPE_BINDING,
                                             NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                             NotificationEvent.EVENT_TYPE_ALERT,
                                             "HTTPBC-E00104");
            throw new JBIException(text, ex);
        }                
    }
    

    MessagingChannel getBindingChannel() {
        return mChannel;
    }
    
    Map getEndpointBeans() {
        return mEndpoints;
    }
    
    public StatusProviderHelper getStatusProviderHelper() {
        return mStatusProviderHelper;
    }
    
    public Endpoint getEndpointBeanForContext(String context, int port) {
        Endpoint match;
        final String search = port + ":" + context;
        
        // Attempt an exact match. This works for the case that I am dealing
        // with HTTP SOAP bindings.
        match = (Endpoint) mEndpointMapping.get(search); // NOI18N
        
        // Attempt an inexact match. This is necessary in the case that I am
        // dealing with non-SOAP HTTP bindings, where the location values to
        // which the Endpoints are mapped will probably hardly ever match
        // the request context, because the latter will usually be the
        // location concatenated with extra path information, and name-value
        // pairs (i.e., HTTP GET requests with either url-encoded or
        // url-replacement encoding).
        if (match == null) {
            Set <Map.Entry<String, Endpoint>> mapEntries = mEndpointMapping.entrySet();
            for (Map.Entry<String, Endpoint> entry : mapEntries) {
                String key = entry.getKey();

                // Provided a valid web service description, there should
                // always be only one match, because having multiple HTTP
                // bindings with the same location value is not (should not)
                // be permitted.
                if (search.startsWith(key)) {
                    match = entry.getValue();
                    break;
                }
            }
        }
        
        return match;
    }

    public void endpointDeployed(Endpoint endpoint) throws Exception {
        // Bug 6403494 & 6421363
        // Check to make sure this endpoint doesn't have the same
        // URL as another previously activated endpoint.        
        Iterator it = mEndpoints.values().iterator();
        URL endpointURL = endpoint.getEndpointUrl();
        String endpointName = endpoint.getUniqueName();
        while (it.hasNext()) {
            Endpoint activatedEndpoint = (Endpoint)it.next();
            // Only check for inbound endpoints.  Ignore outbound ones for now
            if (activatedEndpoint.isInbound() == true &&
                endpoint.isInbound() == true) {
                if (activatedEndpoint.getEndpointUrl().equals(endpointURL)) {
                    String deployedSAName = activatedEndpoint.getServiceUnitID().substring(0, 
                        activatedEndpoint.getServiceUnitID().indexOf("-" + SHORT_DISPLAY_NAME));
                    String msg = mMessages.getString("HTTPBC-E00142.Endpoint_already_started",
                        new Object[] { endpointName, deployedSAName, activatedEndpoint.getOriginalWSDL().getName(), endpointURL.toString() });
                    throw new Exception(msg);
                }
            }
        }
        
        mEndpoints.put(endpointName, endpoint);        
    } 
    
    public void endpointInitialized(Endpoint endpoint) throws Exception {        
        String uniqueEndpointName = endpoint.getUniqueName();
        
        mEndpoints.put(uniqueEndpointName, endpoint);        
        // Should this be done in the init() method of the endpoint?  I originally had
        // this code there, but it involved passing around the StatusProviderHelper around.
        // That didn't really make sense.  One could argue that providing status on an
        // endpoint is something that an Observer can do and isn't an integral part of what
        // constitutes an endpoint.
        StatusReporting reporting = mStatusProviderHelper.getStatusReporter();
        String portName = endpoint.getEndpointName();
        QName serviceName = endpoint.getServiceName();
        String uniqueName = null;
        
        if (endpoint.isInbound()) {
            uniqueName = mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
            reporting.addConsumingEndpoint(uniqueName);
        } else {
            uniqueName = mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
            reporting.addProvisioningEndpoint(uniqueName);
        }

        EndpointStatus stat = reporting.getEndpointStatus(uniqueName);
        endpoint.setEndpointStatus(stat);
    }

    public void endpointActivated(Endpoint aEndpoint) throws Exception {
    	final Endpoint endpoint = aEndpoint;
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Activating " + (endpoint.isInbound()? "inbound" : "outbound") + " endpoint - service name: [" + 
                                    endpoint.getServiceName() + "] endpoint name [" + endpoint.getEndpointName() + "]");
        }
        
        EndpointStatus status = endpoint.getEndpointStatus();
        
        // For inbound endpoints, build an additional mapping from the URL context to the endpoint
        String uniqueName = endpoint.getUniqueName();
        if (endpoint.isInbound()) {
            // For Inbound endpoints we are the consumer and only the provider (SE) activates the endpoint
            String context = endpoint.getUrlContext();
            int port = endpoint.getUrlPort();
            String protocol = endpoint.getEndpointUrl().getProtocol();
            
            if (context == null ) {
                throw new Exception(mMessages.getString("HTTPBC-E00143.Endpoint_no_context", uniqueName));
            } else {
                context = "".equals(context) ? "/" : context;
                mEndpointMapping.put(port + ":" + context, endpoint); // NOI18N
                // Since the same port number can be used for multiple endpoints,
                // we should keep a reference counter for active endpoints using the port
                incrementPortReferenceCount(port);
                if (!appserverContainerPorts.contains(new Integer(port))) {
                    try {
                    	boolean enableClientAuth = false;
                    	if ("https".equalsIgnoreCase(protocol) && port == mRuntimeConfig.getHttpsDefaultPort().intValue()) { // setting only applies to default https port right now
                           enableClientAuth = mRuntimeConfig.getClientAuthEnabled().booleanValue(); 
                        }
                        if (mLogger.isLoggable(Level.FINE)) {
                            mLogger.log(Level.FINE, "About to start port " + Integer.valueOf(port) + ", protocol is: " + protocol + 
                                                    ((enableClientAuth)? " with client authentication enabled..." : "...") );
                        }
                        mEmbeddedController.startServer(port, protocol, enableClientAuth);
                    } catch (Throwable ex) {
                    	// make sure the port is really stopped
                        mEmbeddedController.stopServer(port); 
                        // make sure that the endpoint entry is removed from the cache and deactivated
                        mEndpoints.remove(uniqueName);
                        endpointDeactivated(endpoint);
                        throw new Exception(ex);
                    }
                } else {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Port " + Integer.valueOf(port) + " is already started.");
                    }
                }
            }
            
            try {
                WSEndpoint wsendpoint = mJAXWSEndpointFactory.createWSEndpoint(port, context, endpoint);
                wsendpoint.setExecutor(inboundThreadPool);
                
                // register the external endpoint with a ServiceEndpoint to be
                // used as a proxy for the external consumer to access 
                // the internal endpoint
                DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();  // expensive to create a DocumentBuilder everytime?
                DocumentBuilder builder = factory.newDocumentBuilder();
                final Document document  = builder.newDocument();
                ServiceEndpoint sep = new ServiceEndpoint() {
                    public QName getServiceName() {
                        return endpoint.getServiceName();
                    }
                    
                    public String getEndpointName() {    	
                        return endpoint.getEndpointName();
                    }
                    
                    public DocumentFragment getAsReference(QName operation) {
                        DocumentFragment fragment = document.createDocumentFragment();
                        
                        // create the wsa:Address element
                        Element address = document.createElementNS(WS_ADDRESSING_NS, "wsa:Address");
                        address.setTextContent(endpoint.getEndpointUrl().toString());
                        fragment.appendChild(address);
                        
                        // create the wsa:ServiceName element
                        Element serviceName = document.createElementNS(WS_ADDRESSING_NS, "wsa:ServiceName");
                        String serviceNs = endpoint.getServiceName().getNamespaceURI();
                        String serviceLocalName = endpoint.getServiceName().getLocalPart();
                        serviceName.setAttributeNS("http://www.w3.org/2000/xmlns/","xmlns:ns0", serviceNs);
                        serviceName.setTextContent("ns0:" + serviceLocalName);
                        Attr port = document.createAttribute("PortName");
                        port.setValue(endpoint.getEndpointName());
                        serviceName.setAttributeNode(port);
                        fragment.appendChild(serviceName);
                        
                        return fragment;
                    }
                    
                    public QName[] getInterfaces() {
                        return new QName[] { endpoint.getInterfaceName() };
                    }   
                };
                
                ServiceEndpoint[] seps = mContext.getExternalEndpointsForService(endpoint.getServiceName());
                if (seps == null || seps.length == 0) {
                    mContext.registerExternalEndpoint(sep);
                    // add the external endpoint reference in the Endpoint construct
                    endpoint.setEndpointReference(sep);
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Registered an external endpoint with service name: " + endpoint.getServiceName() + 
                                                " and endpoint name: " + endpoint.getEndpointName());
                    }
                }
                
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Inbound endpoint " + uniqueName + " prepared, mapped to context " + context);
                }
            } catch (Exception e) {
            	// remove the endpoint from the local cache
                mEndpoints.remove(uniqueName);
                endpointDeactivated(endpoint);
                throw e;
            }
        } 
        
        //populateEndpointWSDLInfo(status, endpoint);
        
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Activated " + (endpoint.isInbound()? "inbound" : "outbound") + "endpoint - service name: [" + 
                                    endpoint.getServiceName() + "] endpoint name [" + endpoint.getEndpointName() + "]");
        }
    }
    
    public void endpointDeactivated(Endpoint endpoint) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Deactivating endpoints");
        }
        
        String uniqueName = endpoint.getUniqueName();
        // For inbound endpoints, Remove the additional mapping from the URL context to the endpoint
        if (endpoint.isInbound()) {
            String context = endpoint.getUrlContext();
            int port = endpoint.getUrlPort();
            if (context == null ) {
                mLogger.log(Level.SEVERE, "HTTPBC-E00143.Endpoint_no_context", uniqueName);
            } else {
                mEndpointMapping.remove(port + ":" + context); // NOI18N
                // decrement the endpoint reference count in the map
                // stop the port if there are no more activated endpoints referencing the port.
                int refCount = decrementPortReferenceCount(port);
                if (refCount == 0) {
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Port " + port + " is no longer referenced by any active endpoint. About to stop this port...");
                    }
                    mEmbeddedController.stopServer(port);
                }
            }
            
            inboundThreadPool.shutdownNow();
            if (endpoint.getWSEndpoint() != null) {
                endpoint.getWSEndpoint().dispose();
            }
            
            //de-register the external endpoint
            ServiceEndpoint sep = endpoint.getEndpointReference();
            if (sep != null) {
                mContext.deregisterExternalEndpoint(sep);
            }
        }   

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Endpoints deactivated");
        }
    }
    
    public boolean resumeActivatedEndpoint(String endpointName) {
    	// return false immediately if this is an outbound endpoint ID
    	if (endpointName.endsWith("," + StatusProviderHelper.PROVISIONING_ID)) {
    	    String warningMsg = mMessages.getString("HTTPBC-W01306.Will_not_resume_for_outbound_endpoint", endpointName);
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, warningMsg);
            }
            
            AlertsUtil.getAlerter().info(warningMsg, 
                                         HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                         null, 
                                         AlertsUtil.getServerType(),
                                         AlertsUtil.COMPONENT_TYPE_BINDING,
                                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                         NotificationEvent.EVENT_TYPE_ALERT,
                                         "HTTPBC-W01306"); 
            
    	    return false;
    	}
    	
    	// find the endpoint associated with the unique name
    	final String consumingEndpointName = 
    	    (endpointName.endsWith("," + StatusProviderHelper.CONSUMING_ID))? endpointName : endpointName + "," + StatusProviderHelper.CONSUMING_ID;
        Endpoint endpoint = (Endpoint)mEndpoints.get(consumingEndpointName);
        if (endpoint == null) {
            String warningMsg = mMessages.getString("HTTPBC-W01303.Cannot_find_consumer_endpoint_for_resume", endpointName);
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, warningMsg);
            }
            
            AlertsUtil.getAlerter().info(warningMsg, 
                                         HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                         null, 
                                         AlertsUtil.getServerType(),
                                         AlertsUtil.COMPONENT_TYPE_BINDING,
                                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                         NotificationEvent.EVENT_TYPE_ALERT,
                                         "HTTPBC-W01303"); 
            
            return false;
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Resuming endpoint - service name: [" + 
                                    endpoint.getServiceName() + "] endpoint name [" + endpoint.getEndpointName() + "]");
        }
        
        // For Inbound endpoints we are the consumer and only the provider (SE) activates the endpoint
        String context = endpoint.getUrlContext();
        int port = endpoint.getUrlPort();
        if (context != null ) {    // context cannot be null if we reach here
            context = "".equals(context) ? "/" : context;
            mEndpointMapping.put(port + ":" + context, endpoint); // NOI18N
        }
        
        return true;
    }
    
    public boolean suspendActivatedEndpoint(String endpointName) {
    	// return false immediately if this is an outbound endpoint ID
    	if (endpointName.endsWith("," + StatusProviderHelper.PROVISIONING_ID)) {
    	    String warningMsg = mMessages.getString("HTTPBC-W01307.Will_not_suspend_for_outbound_endpoint", endpointName);
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, warningMsg);
            }
            
            AlertsUtil.getAlerter().info(warningMsg, 
                                         HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                         null, 
                                         AlertsUtil.getServerType(),
                                         AlertsUtil.COMPONENT_TYPE_BINDING,
                                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                         NotificationEvent.EVENT_TYPE_ALERT,
                                         "HTTPBC-W01307"); 
            
    	    return false;
    	}
    	
    	// find the endpoint associated with the unique name
    	final String consumingEndpointName = 
    	    (endpointName.endsWith("," + StatusProviderHelper.CONSUMING_ID))? endpointName : endpointName + "," + StatusProviderHelper.CONSUMING_ID;
    	Endpoint endpoint = (Endpoint)mEndpoints.get(consumingEndpointName);
    	if (endpoint == null) {
    	    String warningMsg = mMessages.getString("HTTPBC-W01304.Cannot_find_consumer_endpoint_for_suspend=HTTPBC-E01304", endpointName);
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, warningMsg);
            }
            
            AlertsUtil.getAlerter().info(warningMsg, 
                                         HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                         null, 
                                         AlertsUtil.getServerType(),
                                         AlertsUtil.COMPONENT_TYPE_BINDING,
                                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                         NotificationEvent.EVENT_TYPE_ALERT,
                                         "HTTPBC-W01304"); 
            
            return false;
    	}
    	
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Suspending endpoint - service name: [" + 
                                    endpoint.getServiceName() + "] endpoint name [" + endpoint.getEndpointName() + "]");
        }
        
        // For Inbound endpoints we are the consumer and only the provider (SE) activates the endpoint
        String context = endpoint.getUrlContext();
        int port = endpoint.getUrlPort();
        if (context != null ) {    // context cannot be null if we reach here
            context = "".equals(context) ? "/" : context;
            mEndpointMapping.remove(port + ":" + context); // NOI18N
        }
        return true;
    }

    public boolean isEndpointActive(String endpointName) {
    	// return true immediately if this is an outbound endpoint ID
    	if (endpointName.endsWith("," + StatusProviderHelper.PROVISIONING_ID) && mEndpoints.containsKey(endpointName)) {
    	    return true;
    	}
    	
    	boolean isActive = false;
    	// find the endpoint associated with the unique name
    	final String consumingEndpointName = 
    	    (endpointName.endsWith("," + StatusProviderHelper.CONSUMING_ID))? endpointName : endpointName + "," + StatusProviderHelper.CONSUMING_ID;
    	Endpoint endpoint = (Endpoint)mEndpoints.get(consumingEndpointName);
    	if (endpoint == null) {
    	    String warningMsg = mMessages.getString("HTTPBC-W01305.Cannot_check_is_active_no_endpoint", endpointName);
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, warningMsg);
            }
            
            AlertsUtil.getAlerter().info(warningMsg, 
                                         HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                         null, 
                                         AlertsUtil.getServerType(),
                                         AlertsUtil.COMPONENT_TYPE_BINDING,
                                         NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                         NotificationEvent.EVENT_TYPE_ALERT,
                                         "HTTPBC-W01305"); 
            
            return false;
    	}
    	
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Checking if endpoint - service name: [" + 
                                    endpoint.getServiceName() + "] endpoint name [" + endpoint.getEndpointName() + "] is active...");
        }
        
        // For Inbound endpoints we are the consumer and only the provider (SE) activates the endpoint
        String context = endpoint.getUrlContext();
        int port = endpoint.getUrlPort();
        if (context != null ) {    // context cannot be null if we reach here
            context = "".equals(context) ? "/" : context;
            isActive = mEndpointMapping.containsKey(port + ":" + context);
        }
        
        return isActive;
    }
    
    public String[] getActiveConsumingEndpoints() {
    	List endpointNames = new ArrayList();
        for (Iterator it = mEndpointMapping.values().iterator(); it.hasNext(); ) {
            Endpoint aEndpoint = (Endpoint) it.next();
            endpointNames.add(aEndpoint.getUniqueName());
        }
        
        return (String[])endpointNames.toArray(new String[0] );
    }

    public String[] getInactiveConsumingEndpoints() {
        List endpointNames = new ArrayList();
        for (Iterator it = mEndpoints.keySet().iterator(); it.hasNext(); ) {
            String uniqueName = (String)it.next();
            if (!isEndpointActive(uniqueName)) {
                endpointNames.add(uniqueName);
            }
        }
        
        return (String[])endpointNames.toArray(new String[0]);
    }
    
    public void endpointShutdown(Endpoint endpoint) throws Exception {

        String uniqueName = endpoint.getUniqueName();
        mEndpoints.remove(uniqueName);

        StatusReporting reporting = mStatusProviderHelper.getStatusReporter();   
        QName serviceName = endpoint.getServiceName();
        String portName = endpoint.getEndpointName();
        if (endpoint.isInbound()) {
            uniqueName = mStatusProviderHelper.createConsumingEndpointIdentifier(serviceName, portName);
            reporting.removeConsumingEndpoints(new String[] {uniqueName});
        } else {
            uniqueName = mStatusProviderHelper.createProvisioningEndpointIdentifier(serviceName, portName);
            reporting.removeProvisioningEndpoints(new String[] {uniqueName});
        }

    }


    void startOutbound() {
        mOutboundReceiver = new OutboundReceiver(mChannel, mEndpoints, mRuntimeConfig, mManagementMBean);
        mOutboundReceiverThread = new Thread(mOutboundReceiver);
        mOutboundReceiverThread.start();        
        mProxy = new HttpProxy(mRuntimeConfig);
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Outbound processing started; HttpProxy instance " + mProxy);
        }
        mProxy.register();
        mBasicAuthenticator = new BasicAuthenticator();
        mBasicAuthenticator.register();
    }    
    
    void stopOutbound() {
        mOutboundReceiver.stopReceiving();
        mProxy.unregister();
        mBasicAuthenticator.unregister();
    }
    
    void determineServerPorts(MBeanServer mbServer) {
        final String HTTP_LISTENER_OBJECT_NAME_PATTERN = "*:j2eeType=X-HTTPListenerConfig,*";
        // This is sun appserver 8 specific code to determine which ports it is listening on
        ObjectName pattern = null;
        try {
            // Get MBeans corresponding to http listeners, e.g.
            //   "amx:X-ConfigConfig=server-config,X-HTTPServiceConfig=na,j2eeType=X-HTTPListenerConfig,name=http-listener-1"
            //   "amx:X-ConfigConfig=server-config,X-HTTPServiceConfig=na,j2eeType=X-HTTPListenerConfig,name=http-listener-2"
            //   "amx:X-ConfigConfig=server-config,X-HTTPServiceConfig=na,j2eeType=X-HTTPListenerConfig,name=admin-listener"
            pattern = new ObjectName(HTTP_LISTENER_OBJECT_NAME_PATTERN);
        } catch (Exception ex) {
            String text = mMessages.getString("HTTPBC-W00130.Mbean_search_pattern_invalid_continue", HTTP_LISTENER_OBJECT_NAME_PATTERN);
            mLogger.log(Level.WARNING, text, ex);
        }
        
        boolean useDefaults = false;
        java.util.Set listenerConfigs = mbServer.queryMBeans(pattern, null);
        if (listenerConfigs != null && listenerConfigs.size() > 0) {
            java.util.Iterator iter = listenerConfigs.iterator();
            boolean failure = false;
            while(iter.hasNext()) {
                ObjectInstance instance = (ObjectInstance) iter.next();
                try {                    
                    String portStr = (String) mbServer.getAttribute(instance.getObjectName(), "Port"); // NOI18N
                    Integer port = Integer.valueOf(portStr);
                    if (mLogger.isLoggable(Level.FINE)) {
                        mLogger.log(Level.FINE, "Container listening port found: " + port);
                    }
                    appserverContainerPorts.add(port);
                } catch (Exception ex) {
                    mLogger.log(Level.WARNING, "HTTPBC-W00131.Listen_port_query_failed", ex);
                    failure = true;
                }
            }
            // If there was an issue with retrieving the ports, use the defaults.
            if (failure && appserverContainerPorts.size() == 0) {
                useDefaults = true;
            }
        } else {
            useDefaults = true;
        }

        if (useDefaults) {
            // Add default port(s)
            String defaultPortsDisplayString = DEFAULT_CONTAINER_PORT + " " + DEFAULT_SSL_CONTAINER_PORT; // NOI18N
            appserverContainerPorts.add(DEFAULT_CONTAINER_PORT);
            appserverContainerPorts.add(DEFAULT_SSL_CONTAINER_PORT);
        }
        
    }
    
    /**
     * If an endpoint is listed as both inbound and outbound it means that it is serviced by a JBI component
     * In that case this BC does not need to act as an (outbound) proxy as the NMR can directly invoke the JBI endpoint
     * @return whether there is an inbound endpoint with the same server and endpoint name
     */
    boolean hasCorrespodingInboundEndpoint(HttpSoapEndpoint[] endpoints, QName fullServiceName, String endpointName) {
         boolean hasCorresponding = false;
         int endpointCount = 0;
         while (endpointCount < endpoints.length && !hasCorresponding) {
             if (endpoints[endpointCount].isInbound()
                 && fullServiceName.equals(endpoints[endpointCount].getServiceName())
                 && endpointName.equals(endpoints[endpointCount].getEndpointName())) {
                 hasCorresponding = true;
             }
             endpointCount++;
         }
         return hasCorresponding;
    }
  
    public ServiceUnitManager getServiceUnitManager() {
        return mServiceUnitManager;
    }
    
    public ByteBuffer queryResource(String context, Endpoint endpoint) throws Exception {
        return mServiceUnitManager.queryResource(context, endpoint);        
    }
    
    public Set<Endpoint> getRegisteredEndpoints() {
        return new HashSet<Endpoint>(mEndpoints.values());
    }
    
    private void logComponentInfo() {
        StringBuffer msgBuf = new StringBuffer(
                mMessages.getString("HTTPBC-R00101.Binding_started_detailed",
                mComponentName));
        msgBuf.append('\n');
        
        // Identification information
        String installRoot = mContext.getInstallRoot();
        identity(msgBuf, installRoot);
        
        // Runtime configuration
        mRuntimeConfig.dump(msgBuf);
        if (mLogger.isLoggable(Level.INFO)) {
            mLogger.log(Level.INFO, msgBuf.toString());
        }
        
        AlertsUtil.getAlerter().info(msgBuf.toString(), 
                                     HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                     null, 
                                     AlertsUtil.getServerType(),
                                     AlertsUtil.COMPONENT_TYPE_BINDING,
                                     NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                     NotificationEvent.EVENT_TYPE_ALERT,
                                     "HTTPBC-R00101");                
    }
    
    private void identity(StringBuffer buf, String idDescPath) {
        final String JBI_XML_PATH = idDescPath.concat("/META-INF/jbi.xml");
        String specVersion = "unknown";
        String buildNumber = "unknown";
        File desc = new File(JBI_XML_PATH);
        try {
            DocumentBuilderFactory aFactory = DocumentBuilderFactory.newInstance();
            aFactory.setIgnoringComments(true);
            aFactory.setNamespaceAware(true);
            DocumentBuilder aDocumentBuilder = aFactory.newDocumentBuilder();
            Document document = aDocumentBuilder.parse(JBI_XML_PATH);
            NodeList versioninfoNodes = document.getElementsByTagNameNS(JBI_DESC_ID_NS, JBI_VERSIONINFO_TAG);
            for (int i = 0; i < versioninfoNodes.getLength(); ++i) {
                Node versioninfoNode = versioninfoNodes.item(i);
                if (versioninfoNode.hasAttributes()) {
                    NamedNodeMap attrMap = versioninfoNode.getAttributes();
                    Node specVerAttrNode = attrMap.getNamedItem(JBI_COMPONENTVER_ATTR);
                    if (specVerAttrNode != null && specVerAttrNode.getNodeType() == Node.ATTRIBUTE_NODE) {
                        specVersion = specVerAttrNode.getNodeValue();
                    }
                    Node buildVerAttrNode = attrMap.getNamedItem(JBI_BUILDVER_ATTR);
                    if (buildVerAttrNode != null && buildVerAttrNode.getNodeType() == Node.ATTRIBUTE_NODE) {
                        buildNumber = buildVerAttrNode.getNodeValue();
                    }
                    break;
                }
            }
        } catch (ParserConfigurationException e) {
            String msg = mMessages.getString("HTTPBC-E00154.JBI_xml_docbuilder_error", JBI_XML_PATH);
            mLogger.log(Level.WARNING, msg, e);
            AlertsUtil.getAlerter().warning(msg, 
                                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "HTTPBC-E00154");
        } catch (SAXException e) {
            String msg = mMessages.getString("HTTPBC-E00152.JBI_xml_ident_parse_error", JBI_XML_PATH);
            mLogger.log(Level.WARNING, msg, e);
            AlertsUtil.getAlerter().warning(msg, 
                                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "HTTPBC-E00152");
        } catch (IOException e) {
            String msg = mMessages.getString("HTTPBC-E00153.JBI_xml_ident_io_error", JBI_XML_PATH);
            mLogger.log(Level.WARNING, msg, e);
            AlertsUtil.getAlerter().warning(msg, 
                                            HttpSoapBindingLifeCycle.SHORT_DISPLAY_NAME, 
                                            null, 
                                            AlertsUtil.getServerType(),
                                            AlertsUtil.COMPONENT_TYPE_BINDING,
                                            NotificationEvent.OPERATIONAL_STATE_RUNNING, 
                                            NotificationEvent.EVENT_TYPE_ALERT,
                                            "HTTPBC-E00153");
        }
        buf.append(JBI_COMPONENTVER_ATTR + ": ").append(specVersion).append('\n');
        buf.append(JBI_BUILDVER_ATTR + ": ").append(buildNumber).append('\n');
    }

    /**
     * Set the number or processing threads to use
     */
    public void setThreads(int threadCount) {
        mInboundMaxPoolSize = threadCount;
        if (inboundThreadPool != null) {
            inboundThreadPool.setMaximumPoolSize(threadCount);
        }
    }

    private void startAsyncResponseDispatcher() {
        AsyncResponseDispatcher.intitalize(mRuntimeConfig);
    }

    private void startDispatchPool() {
        DispatchPool.intitalize(mRuntimeConfig);
    }

    private static class DaemonThreadFactory implements ThreadFactory {
        private final AtomicInteger threadNumber = new AtomicInteger(1);
        
        public Thread newThread(Runnable r) {
            Thread daemonThread = new Thread(r, "HTTPBC-JAXWS-Engine-"+threadNumber.getAndIncrement());
            daemonThread.setDaemon(Boolean.TRUE);
            return daemonThread;
        }
    }
    
    //bug:862 comment it out for now until new monitoring api is defined
//    private void populateEndpointWSDLInfo(EndpointStatus endpointStatus, Endpoint endpoint) throws Exception {
//        // Set the resource info on the endpoint status if it is available
//        Map importedResources = new HashMap();
//        Transformer transformer = mTransformerPool.retrieve();
//        if (endpointStatus != null) {
//            javax.wsdl.Definition originalWsdlDef = endpoint.getServiceDescriptor();
//            StringWriter originalWsdl = new StringWriter();
//            WSDLFactory wsdlFactory = (WSDLFactory)WSDLFactory.newInstance();
//            WSDLWriter writer = (WSDLWriter)wsdlFactory.newWSDLWriter();
//            writer.writeWSDL(originalWsdlDef, originalWsdl);
//            endpointStatus.setWSDLDefinition(originalWsdl.toString());
//            
//            //we don't use namespace here because the namespace is not unique
//            /*for (Iterator imports = endpoint.getImportedWSDLDefinitions().values().iterator(); imports.hasNext();) {
//                Definition aImportedWSDL = (Definition) imports.next();
//                if (aImportedWSDL != null) {
//                    StringWriter sWriter = new StringWriter();
//                    writer.writeWSDL(aImportedWSDL, sWriter);
//                    importedResources.put(aImportedWSDL.getTargetNamespace(), sWriter.toString());
//                }
//            }*/
//            
//            for (String loc: endpoint.getImportedURL2WSDL().keySet()) {
//                Definition aImportedWSDL =  endpoint.getImportedURL2WSDL().get(loc);               
//                if (aImportedWSDL != null) {
//                    StringWriter sWriter = new StringWriter();
//                    writer.writeWSDL(aImportedWSDL, sWriter);
//                    importedResources.put(loc, sWriter.toString());
//                } else {
//                	String wsdl = readURL(loc);
//                	if (wsdl != null) {
//                		importedResources.put(loc, wsdl);
//                	}                	
//                }
//            }
//            
//            /*for (Iterator imports = endpoint.getImportedXSDSchemas().values().iterator(); imports.hasNext();) {
//                Element aImportedSchema = (Element) imports.next();
//                if (aImportedSchema != null) {
//                    Source src = new DOMSource(aImportedSchema);
//                    ByteArrayOutputStream baos = new ByteArrayOutputStream();
//                    StreamResult dest = new StreamResult(baos);
//                    transformer.transform(src, dest);
//                    importedResources.put(aImportedSchema.getAttribute("targetNamespace"), baos.toString());
//                }
//            }*/
//            
//            for (String loc: endpoint.getImportedURL2XSD().keySet()) {
//                Element aImportedSchema = (Element) endpoint.getImportedURL2XSD().get(loc);
//                if (aImportedSchema != null) {
//                    Source src = new DOMSource(aImportedSchema);
//                    ByteArrayOutputStream baos = new ByteArrayOutputStream();
//                    StreamResult dest = new StreamResult(baos);
//                    transformer.transform(src, dest);
//                    importedResources.put(loc, baos.toString());
//                } else {
//                	String xsd = readURL(loc);
//                	if (xsd != null) {
//                		importedResources.put(loc, xsd);
//                	}
//                }
//           }
//            endpointStatus.setWSDLImportedResources(importedResources);
//            mTransformerPool.relinquish(transformer);
//        }
//    }
    
    private String readURL(String url) throws Exception {
    	URL loc = new URL(url);
    	BufferedReader in = new BufferedReader(
    				new InputStreamReader(
    				loc.openStream()));

    	String inputLine;
    	StringBuffer result = new StringBuffer();
    	while ((inputLine = in.readLine()) != null) {
    	    result.append(inputLine);
    	    result.append(System.getProperty("line.separator"));
    	}
    	in.close();
    	return result.toString();   	
    }
    
    private void startDefaultHttpPorts() {
        try {
            Integer httpDefaultPort = mRuntimeConfig.getHttpDefaultPort();
            Integer httpsDefaultPort = mRuntimeConfig.getHttpsDefaultPort();
            if (httpDefaultPort != null && httpDefaultPort.intValue() != -1) { 
                // we have a valid HTTP port default
                mEmbeddedController.startServer(httpDefaultPort.intValue(), "http", false);
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Started default HTTP port (a.k.a. HttpDefaultPort) " + httpDefaultPort);
                }
            }
            
            /** SJSAS EE version doesn't use JKS, it uses NSS keystore format. 
                HTTP BC does not support NSS now and an "unsupported" Exception will be
                thrown if the server uses NSS. We don't want the component 
                start up to fail because of this, so we will not start the default HTTPS
                port for now until this is resolved.
            if (httpsDefaultPort != null && httpsDefaultPort.intValue() != -1) { 
                // we have a valid HTTP port default
                mEmbeddedController.startServer(httpsDefaultPort.intValue(), "https", mRuntimeConfig.getClientAuthEnabled());
                if (mLogger.isLoggable(Level.FINE)) {
                    mLogger.log(Level.FINE, "Started default HTTPs port (a.k.a. HttpsDefaultPort) " + httpsDefaultPort);
                }
            }
            **/
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.log(Level.FINE, "Default HTTPS port will not be started automatically when HTTP BC starts up...");
            }
        } catch (Throwable e) {
            // To code this more defensively, we will not try to raise this exception now, in case for any invalid configurations.
            // See INF 112403 for more details
            if (mLogger.isLoggable(Level.WARNING)) {
	        mLogger.log(Level.WARNING, mMessages.getString("HTTPBC-E00156.Failed_to_start_default_ports", e.getMessage()));
	    }
	}
    }

    private void incrementPortReferenceCount(int port) {
    	// the default HTTP/HTTPS ports will be left alone.
        if (port != mRuntimeConfig.getHttpDefaultPort().intValue() &&
            port != mRuntimeConfig.getHttpsDefaultPort().intValue()) {
            int refCount = 0;
            if (mPortReferenceCountMap.containsKey(new Integer(port))) {
                // the same port number is used for another endpoint
                // increment the port count 
                refCount = (Integer) mPortReferenceCountMap.get(new Integer(port));
            } 
            mPortReferenceCountMap.put(new Integer(port), new Integer(++refCount));
        }
    }
    
    private int decrementPortReferenceCount(int port) {
    	int refCount = 0;
        // the default HTTP/HTTPS ports will be left alone
    	if (port != mRuntimeConfig.getHttpDefaultPort().intValue() &&
            port != mRuntimeConfig.getHttpsDefaultPort().intValue()) {
            if (mPortReferenceCountMap.containsKey(new Integer(port))) {
                // the same port number is used for another endpoint
                // decrement the port count 
                refCount = (Integer) mPortReferenceCountMap.get(new Integer(port));
                if (refCount == 1) { // this is the last endpoint using the endpoint 
                    // remove the entry from the map
                    mPortReferenceCountMap.remove(new Integer(port));
                } else {
                    mPortReferenceCountMap.put(new Integer(port), new Integer(refCount - 1));
                }
            }
        }
        
        return --refCount;
    }
}