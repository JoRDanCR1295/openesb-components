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
 * @(#)WorkflowSELifeCycle.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow;

import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;
import javax.management.ObjectName;
import javax.naming.NamingException;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLWriter;

import org.w3c.dom.Document;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.component.endpoint.EndpointManager;
import com.sun.jbi.component.endpoint.impl.DefaultEndpointManager;
import com.sun.jbi.component.lifecycle.impl.AbstractComponentManager;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.crl.mep.AcceptManager;
import com.sun.jbi.crl.mep.ExchangeRouter;
import com.sun.jbi.crl.mep.impl.DefaultAcceptManager;
import com.sun.jbi.crl.mep.impl.DefaultListenerContext;
import com.sun.jbi.crl.mep.impl.PatternRoleKey;
import com.sun.jbi.crl.mep.impl.SimpleProcessorFactory;
import com.sun.jbi.eManager.provider.StatusProviderHelper;
import com.sun.jbi.eManager.provider.StatusProviderMBean;
import com.sun.jbi.engine.workflow.process.WorkflowSEExchangeContext;
import com.sun.jbi.engine.workflow.process.WorkflowSEInOnlyConsumer;
import com.sun.jbi.engine.workflow.process.WorkflowSEInOutProvider;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.engine.workflow.util.WSDLMergeUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.impl.Util;

public class WorkflowSELifeCycle extends AbstractComponentManager implements
        NotificationListener {
    private static final Messages MESSAGES = Messages
            .getMessages(WLMSERuntimeConfiguration.class);

    private static final Logger mLogger = Logger
            .getLogger(WorkflowSELifeCycle.class.getName());

    private ComponentContext mContext;

    private DeliveryChannel mChannel;

    private WorkflowMapEntryTable mWorkflowMapEntryTable = new WorkflowMapEntryTable();

    private StatusProviderHelper mStatusProviderHelper;

    private Hashtable mWsdlMap = new Hashtable();

    private WorkflowEngine mEngine;

    private RuntimeConfigurationHelper mRuntimeConfigHelper;

    private WLMSERuntimeConfiguration mConfigMBean;

    private WorkflowSEServiceUnitManager mServiceUnitManager;

    public WorkflowSELifeCycle() {
    }

    /**
     * Initialize the component. This performs initialization required by the
     * component but does not make it ready to process messages. This method is
     * called once for each life cycle of the component
     * 
     * @param context -
     *            the component's context.
     * @throws javax.jbi.JBIException
     *             if the component is unable to initialize.
     */
    public void init(ComponentContext context) throws javax.jbi.JBIException {
        super.init(context);
        mLogger.info(I18n.loc("WLM-5003: Initializing WLM  service engine"));

        mEngine = new WorkflowEngine();
        mEngine.init();
        WorkflowSEInOutProvider inOutProvider = new WorkflowSEInOutProvider(
                mEngine);

        ExchangeRouter router = getAcceptManager().getExchangeRouter();
        router.register(PatternRoleKey.IN_OUT_PROVIDER, // new
                                                        // XsltseInOutProvider());
                new SimpleProcessorFactory(inOutProvider));

        WorkflowSEInOnlyConsumer inOnlyConsumer = new WorkflowSEInOnlyConsumer(
                mEngine, getAcceptManager());
        router.register(PatternRoleKey.IN_ONLY_CONSUMER,
                new SimpleProcessorFactory(inOnlyConsumer));

        try {
            mStatusProviderHelper = new StatusProviderHelper(
                    "WorkflowSE MBean",
                    StatusProviderMBean.COMPONENT_TYPE_ENGINE, context
                            .getComponentName(), context.getMBeanServer());
            mStatusProviderHelper.registerMBean();
            if (mLogger.isLoggable(Level.FINE)) {
                mLogger.fine(I18n.loc(
                        "WLM-3010: Registered Status Provider MBean for {0}",
                        context.getComponentName()));
            }
        } catch (Exception e) {
            mLogger.log(Level.WARNING, I18n
                    .loc("WLM-6008: Failed to register status provider MBean"),
                    e);
            throw new JBIException(I18n
                    .loc("WLM-6008: Failed to register status provider MBean"),
                    e);
        }
        try {
            mContext = context;
            mChannel = context.getDeliveryChannel();

            // See "Prepare Installation Context" section in JBI 1.0 pr spec
            // 6.4.2.1.1
            mRuntimeConfigHelper = new RuntimeConfigurationHelper(context
                    .getMBeanNames().createCustomComponentMBeanName(
                            "Configuration"), context.getMBeanServer());

            mConfigMBean = new WLMSERuntimeConfiguration(context);
            mRuntimeConfigHelper.registerMBean(mConfigMBean);

            ComponentConfig configProp = mConfigMBean.getConfiguration();
            mLogger.fine(I18n.loc(
                    "WLM-3011: workflowse config properties: \n {0}",
                    configProp.toString()));

            WorkflowSEServiceUnitManager serviceUnitManager = (WorkflowSEServiceUnitManager) getServiceUnitManager();
            serviceUnitManager.initialize(mWorkflowMapEntryTable, mContext,
                    mChannel, mStatusProviderHelper, mWsdlMap, mEngine);

            initializeContext(context);
//           mEngine.recover();
        } catch (NamingException nameEx) {
            String msg = I18n.loc("WLM-7011: Cannot get datasource");
            mLogger.log(Level.SEVERE, msg, nameEx); 
        } catch (Exception ex) {
            throw new javax.jbi.JBIException(I18n
                    .loc("WLM-6009: WLM service engine failed to initialize"),
                    ex);
        }

        // Register handle configuration change notifications
        mConfigMBean.addNotificationListener(this, null, null);

        mLogger
                .info(I18n
                        .loc("WLM-5004: Initialized Workflow service engine successfully"));
    }

    /**
     * Get the JMX ObjectName for any additional MBean for this component. If
     * there is none, return null.
     * 
     * @return ObjectName the JMX object name of the additional MBean or null if
     *         there is no additional MBean.
     */
    public ObjectName getExtensionMBeanName() {
        return null;
    }

    /**
     * Start the component. This makes the component ready to process messages.
     * This method is called after init() completes when the JBI implementation
     * is starting up, and when the component is being restarted after a
     * previous call to shutDown(). If stop() was called previously but
     * shutDown() was not, then start() can be called again without another call
     * to init().
     * 
     * @throws javax.jbi.JBIException
     *             if the component is unable to start.
     */
    public void start() throws javax.jbi.JBIException {
        mLogger.info(I18n.loc("WLM-5005: Starting WLM service engine"));
        super.start();
        mEngine.start();
        mLogger.info(I18n
                .loc("WLM-5006: Started WLM service engine successfully"));
    }

    /**
     * Stop the component. This makes the component stop accepting messages for
     * processing. After a call to this method, start() can be called again
     * without first calling init().
     * 
     * @throws javax.jbi.JBIException
     *             if the component is unable to stop.
     */
    public void stop() throws javax.jbi.JBIException {
        mLogger.info(I18n.loc("WLM-5007: Stopping WLM service engine"));
        super.stop();
        mEngine.stop();
        mLogger.info(I18n
                .loc("WLM-5008: Stopped WLM service engine successfully"));
    }

    /**
     * Shut down the component. This performs cleanup before the component is
     * terminated. Once this method has been called, init() must be called
     * before the component can be started again with a call to start().
     * 
     * @throws javax.jbi.JBIException
     *             if the component is unable to shut down.
     */
    public void shutDown() throws javax.jbi.JBIException {
        mLogger.info(I18n.loc("WLM-5009: Shutting down WLM service engine"));
        super.shutDown();
        try {
            mStatusProviderHelper.unregisterMBean();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            String msg = I18n
                    .loc("WLM-6010: Failed to unregister status provider MBean");
            mLogger.log(Level.WARNING, msg, e);
            throw new JBIException(msg, e);

        }
        mEngine.shutdown();
        mLogger.info(I18n
                .loc("WLM-5010: Shut down WLM service engine successfully"));
    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initAcceptManager() */
    protected AcceptManager initAcceptManager() {
        // initialize AcceptManager
        AcceptManager amgr = new DefaultAcceptManager(this);
        DefaultListenerContext ctx = (DefaultListenerContext) amgr
                .getListenerContext();
        ctx.setExchangeContext(new WorkflowSEExchangeContext(ctx,
                mWorkflowMapEntryTable));
        return amgr;
    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initEndpointManager() */
    protected EndpointManager initEndpointManager() {
        return new DefaultEndpointManager(null);
    }

    /** @see com.sun.jbi.component.lifecycle.impl.AbstractComponentManager#initServiceUnitManager(javax.jbi.component.ComponentContext) */
    protected ServiceUnitManager initServiceUnitManager() {
        if (mServiceUnitManager != null) {
            return mServiceUnitManager;
        }
        mServiceUnitManager = new WorkflowSEServiceUnitManager(this);
        return mServiceUnitManager;
    }

    /**
     * @see javax.management.NotificationListener#handleNotification(javax.management.Notification,
     *      java.lang.Object)
     */
    public void handleNotification(Notification notification, Object obj) {

        if (notification instanceof AttributeChangeNotification) {

            AttributeChangeNotification attrNotif = (AttributeChangeNotification) notification;
            String attrName = attrNotif.getAttributeName();
            Object value = attrNotif.getNewValue();
            try {
                if (EnginePropertyConstants.MAXIMUM_THREADCOUNT
                        .equals(attrName)) {
                    int newThreadCount = Integer.valueOf((String) attrNotif
                            .getNewValue());
                    mEngine.setMaxThreadCount(newThreadCount);

                } else if (EnginePropertyConstants.DATASOURCE_JNDI
                        .equals(attrName)) {
                    // dynamically change task manager This is not working
                    // because of fail to find db script I think due to JMX
                    // invocation in
                    // a different classloader
                    // initializeContext(mContext);
                } else if (EnginePropertyConstants.DATASOURCE_TYPE
                        .equals(attrName)) {
                    // initializeContext(mContext);
                } else if (EnginePropertyConstants.TEST_MODE.equals(attrName)) {
                    // initializeContext(mContext);
               }
//                    else if (EnginePropertyConstants.PERSISTENCE_ENABLED
//                        .equals(attrName)) {
//                    // dynamically change task manager
//                }

            } catch (Exception ex) {
                mLogger
                        .log(
                                Level.WARNING,
                                I18n
                                        .loc(
                                                "WLM-6011: Failed to handle changed runtime configuration {0} value {1}",
                                                attrName, value), ex);
            }
        }
    }

    private void initializeContext(ComponentContext context)
            throws JBIException, NamingException {
        EngineContext engineContext = new EngineContext();
        engineContext.setInitialContext(context.getNamingContext());
        engineContext.setWorkflowMapEntryTable(mWorkflowMapEntryTable);
        engineContext.setConfig(mConfigMBean.getConfiguration());
        mEngine.setContext(engineContext);
    }

    /**
     * Retrieves a DOM representation containing metadata which describes the
     * service provided by this component, through the given endpoint. The
     * result can use WSDL 1.1 or WSDL 2.0.
     * 
     * @param endpoint -
     *            the service endpoint.
     * @return the description for the specified service endpoint.
     */
    public org.w3c.dom.Document getServiceDescription(ServiceEndpoint endpoint) {
        Document wsdlDefDoc = null;
        WSDLWriter writer = null;
        Definition newDef = null;

        WorkflowMapEntry entry = mServiceUnitManager.getEntryTable()
                .findWorkflowEntry(endpoint);
        if (entry == null) 
            return null;
        Definition wsdlDef = entry.getWsdl();
 
        try {
            WSDLFactory factory = WSDLFactory.newInstance();
            newDef = factory.newDefinition();
            writer = factory.newWSDLWriter();
        } catch (WSDLException e) {
            // TODO Auto-generated catch block
            String msg = I18n.loc("WLM-6102: Not able to create WSDLWriter");
            throw new RuntimeException(msg, e);
        }

        try {

            if (wsdlDef != null) {
                WSDLMergeUtil.mergeWSDL(newDef, wsdlDef);
                wsdlDefDoc = writer.getDocument(newDef);
                wsdlDefDoc.setDocumentURI(newDef.getDocumentBaseURI());
            } else if (entry.getTaskModel() != null) {
                Task task = entry.getTaskModel();
                if (task != null) {
                    wsdlDef = Util.getWSDLDefinition(task);
                    if (wsdlDef != null) {
                        WSDLMergeUtil.mergeWSDL(newDef, wsdlDef);
                        wsdlDefDoc = writer.getDocument(newDef);
                        wsdlDefDoc.setDocumentURI(newDef.getDocumentBaseURI());
                    }
                }
            }

        } catch (Exception e) {
            String msg = I18n
                    .loc(
                            "WLM-6103: Not able to get WSDL definition for service end point : service : {0}, endpoint: {1}",
                            endpoint.getServiceName(), endpoint
                                    .getEndpointName());
            throw new RuntimeException(msg, e);
        }
        return wsdlDefDoc;
    }

}
