package com.sun.jbi.restbc.jbiadapter;

import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.Component;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;
import javax.jbi.management.MBeanNames;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.management.ObjectName;
import javax.net.ssl.KeyManager;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

import com.sun.grizzly.SSLConfig;
import com.sun.grizzly.http.servlet.ServletAdapter;
import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.ConfigPersistence;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;
import com.sun.jbi.restbc.jbiadapter.inbound.CharsetResponseFilter;
import com.sun.jbi.restbc.jbiadapter.inbound.ContentLengthResponseFilter;
import com.sun.jbi.restbc.jbiadapter.inbound.InboundDelegator;
import com.sun.jbi.restbc.jbiadapter.inbound.InboundHttpListener;
import com.sun.jbi.restbc.jbiadapter.mbeans.RuntimeConfig;
import com.sun.jbi.restbc.jbiadapter.security.X509KeyManagerImpl;
import com.sun.jbi.restbc.jbiadapter.security.X509TrustManagerImpl;
import com.sun.jersey.spi.container.servlet.ServletContainer;

/**
 * RestComponent.java
 *
 * @author Edward Chou
 */
public class RestComponent implements Component, ComponentLifeCycle {

    /*
     * 1-20
     */
    private static final Logger logger = Logger.getLogger(RestComponent.class.getName());
    
    
    // SSL system properties
    private static final String KEYSTORE_PROP = "javax.net.ssl.keyStore"; // NOI18N
    private static final String KEYSTORE_PASS_PROP = "javax.net.ssl.keyStorePassword"; // NOI18N
    private static final String TRUSTSTORE_PROP = "javax.net.ssl.trustStore"; // NOI18N
    private static final String TRUSTSTORE_PASS_PROP = "javax.net.ssl.trustStorePassword"; // NOI18N
    
    private static final String KEYSTORE_PATH = "keystore.jks";  // NOI18N
    private static final String TRUSTSTORE_PATH = "truststore.jks"; // NOI18N
    
    private Map<String, InboundHttpListener> listeners = new HashMap<String, InboundHttpListener> ();
    
    private ComponentContext context;
    private RestSUManager suManager;
    private RuntimeConfig runtimeConfig;
    private Receiver receiver;
    private ComponentConfig compCfg;
    //private MBeanHelper mbeanHelper;
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
    
    private InboundDelegator inboundDelegator;
    
    // security
    private SSLContext sslContext = null;
    
    ////////
    //
    //  Component Interface Methods
    //
    ////////

    /* (non-Javadoc)
     * @see javax.jbi.component.Component#getLifeCycle()
     */
    public ComponentLifeCycle getLifeCycle() {
        return this;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.Component#getServiceDescription(javax.jbi.servicedesc.ServiceEndpoint)
     */
    public Document getServiceDescription(ServiceEndpoint se) {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.Component#getServiceUnitManager()
     */
    public ServiceUnitManager getServiceUnitManager() {
        return suManager;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.Component#isExchangeWithConsumerOkay(javax.jbi.servicedesc.ServiceEndpoint, javax.jbi.messaging.MessageExchange)
     */
    public boolean isExchangeWithConsumerOkay(ServiceEndpoint se, MessageExchange me) {
        // TODO Auto-generated method stub
        return false;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.Component#isExchangeWithProviderOkay(javax.jbi.servicedesc.ServiceEndpoint, javax.jbi.messaging.MessageExchange)
     */
    public boolean isExchangeWithProviderOkay(ServiceEndpoint se, MessageExchange me) {
        // TODO Auto-generated method stub
        return false;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.Component#resolveEndpointReference(org.w3c.dom.DocumentFragment)
     */
    public ServiceEndpoint resolveEndpointReference(DocumentFragment df) {
        // TODO Auto-generated method stub
        return null;
    }

    ////////
    //
    //  ComponentLifeCycle Interface Methods
    //
    ////////
    
    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentLifeCycle#getExtensionMBeanName()
     */
    public ObjectName getExtensionMBeanName() {
        // TODO Auto-generated method stub
        return null;
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentLifeCycle#init(javax.jbi.component.ComponentContext)
     */
    public void init(ComponentContext context) throws JBIException {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1001: ComponentLifeCycle.init() called.");//NOI18N
            logger.finest(msg);
        }
        
        this.context = context;
        this.suManager = new RestSUManager(this, context);
        this.inboundDelegator = InboundDelegator.getInstance(this);
        
        compCfg = ComponentConfig.parse(context.getInstallRoot());
        ConfigPersistence.loadConfig(compCfg, context.getWorkspaceRoot());
        runtimeConfig = new RuntimeConfig(context, compCfg);
        
        //mbeanHelper = new MBeanHelper(context);
        //mbeanHelper.registerMBean(RuntimeConfigurationMBean.CONFIGURATION_EXTENSION, runtimeConfig);
        
        try {
            MBeanNames mbeanNames = context.getMBeanNames();
            ObjectName runtimeConfigMBeanObjName = mbeanNames.createCustomComponentMBeanName("Configuration");
            mRuntimeConfigHelper = new RuntimeConfigurationHelper(runtimeConfigMBeanObjName, context.getMBeanServer());
            mRuntimeConfigHelper.registerMBean(runtimeConfig);
        } catch (Exception e) {
            throw new JBIException(e);
        }
        initSSLContext();
        
        // start receiving from NMR
        receiver = new Receiver(this, context, suManager, runtimeConfig);
        receiver.startReceiving();
        
        // send alert
        String[] info = I18n.locStr("RESTBC-5001: Initialized {0} successfully",
                context.getComponentName());
        logger.info(info[2]);
        I18n.alertInfo(info);
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentLifeCycle#shutDown()
     */
    public void shutDown() throws JBIException {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1002: ComponentLifeCycle.shutdown() called.");//NOI18N
            logger.finest(msg);
        }
        
        if (context != null) {
            context.getDeliveryChannel().close();
        }
        
        shutdownMBeans();
        
        // send alert
        String[] info = I18n.locStr("RESTBC-5002: Shut down {0} successfully",
                context.getComponentName());
        logger.info(info[2]);
        I18n.alertInfo(info);
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentLifeCycle#start()
     */
    public void start() throws JBIException {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1003: ComponentLifeCycle.start() called.");//NOI18N
            logger.finest(msg);
        }
        
        try {
            startHttpListener();
            startHttpsListener();
            
        } catch (Throwable t) {
            String msg = I18n.loc("RESTBC-7003: {0} failed to start {1}",
                    context.getComponentName(), t.getMessage());
            logger.log(Level.SEVERE, msg, t);
            throw new JBIException(msg, t);
        }
        
        // send alert
        String[] info = I18n.locStr("RESTBC-5003: Start {0} successfully",
                context.getComponentName());
        logger.info(info[2]);
        I18n.alertInfo(info);
    }

    /* (non-Javadoc)
     * @see javax.jbi.component.ComponentLifeCycle#stop()
     */
    public void stop() throws JBIException {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1004: ComponentLifeCycle.stop() called.");//NOI18N
            logger.finest(msg);
        }
        try {
            if (receiver != null) {
                receiver.stopReceiving();
            }
            
            stopListeners();
            
        } catch (Throwable t) {
            String msg = I18n.loc("RESTBC-7004: {0} failed to stop {1}",
                    context.getComponentName(), t.getMessage());
            logger.log(Level.SEVERE, msg, t);
            throw new JBIException(msg, t);
        }
        
        // send alert
        String[] info = I18n.locStr("RESTBC-5004: Stop {0} successfully",
                context.getComponentName());
        logger.info(info[2]);
        I18n.alertInfo(info);
    }
    
    ////////
    //
    //  other methods
    //
    ////////
    
    private void initSSLContext() {
        try {
            String installRoot = context.getInstallRoot();
            File installRootFile = new File(installRoot);
            if (!installRootFile.isDirectory()) {
                String msg = I18n.loc("RESTBC-6001: Unable to initialize SSLContext, install root is not a directory {0}",
                        installRootFile.toString());
                logger.log(Level.WARNING, msg);
                return;
            }
            
            // keystore file
            File keystoreFile = null;
            String keystoreSysProp = System.getProperty(KEYSTORE_PROP);
            if (keystoreSysProp == null || keystoreSysProp.length() == 0) {
                keystoreFile = new File(installRootFile, KEYSTORE_PATH);
            } else {
                keystoreFile = new File(keystoreSysProp);
            }
            if (!keystoreFile.isFile()) {
                String msg = I18n.loc("RESTBC-6002: Unable to initialize SSLContext, keystore file is not a file {0}",
                        keystoreFile.toString());
                logger.log(Level.WARNING, msg);
                return;
            }
            
            // truststore file
            File truststoreFile = null;
            String truststoreSysProp = System.getProperty(TRUSTSTORE_PROP);
            if (truststoreSysProp == null || truststoreSysProp.length() == 0) {
                truststoreFile = new File(installRootFile, TRUSTSTORE_PATH);
            } else {
                truststoreFile = new File(truststoreSysProp);
            }
            if (!truststoreFile.isFile()) {
                String msg = I18n.loc("RESTBC-6003: Unable to initialize SSLContext, truststore file is not a file {0}",
                        truststoreFile.toString());
                logger.log(Level.WARNING, msg);
                return;
            }
            
            // keystore password
            char[] keystorePassword = null;
            String keystorePasswordSysProp = System.getProperty(KEYSTORE_PASS_PROP);
            if (keystorePasswordSysProp == null || keystorePasswordSysProp.length() == 0) {
                keystorePassword = runtimeConfig.getKeystorePassword().toCharArray();
            } else {
                keystorePassword = keystorePasswordSysProp.toCharArray();
            }
            
            // truststore password
            char[] truststorePassword = null;
            String truststorePasswordSysProp = System.getProperty(TRUSTSTORE_PASS_PROP);
            if (truststorePasswordSysProp == null || truststorePasswordSysProp.length() == 0) {
                truststorePassword = runtimeConfig.getTruststorePassword().toCharArray();
            } else {
                truststorePassword = truststorePasswordSysProp.toCharArray();
            }
            
            String msg = I18n.loc("RESTBC-4001: Initializing Keystore at location {0}", keystoreFile.toString());
            logger.info(msg);
            KeyManager[] keyManager = new KeyManager[] {
                new X509KeyManagerImpl(keystoreFile, keystorePassword)
            };
            
            String msg2 = I18n.loc("RESTBC-4002: Initializing Truststore at location {0}", truststoreFile.toString());
            logger.info(msg2);
            TrustManager[] trustManager = new TrustManager[] {
                new X509TrustManagerImpl(truststoreFile, truststorePassword)
            };
            
            sslContext = SSLContext.getInstance("SSL");  // NOI18N
            sslContext.init(keyManager, trustManager, null);
            
        } catch (Exception e) {
            String msg = I18n.loc("RESTBC-6004: Unable to initialize SSLContext {0}",
                    e.getMessage());
            logger.log(Level.WARNING, msg, e);
            return;
        }
    }
    
    private void shutdownMBeans() throws JBIException {
        //mbeanHelper.unregisterMBean(RuntimeConfigurationMBean.CONFIGURATION_EXTENSION);
        try {
            mRuntimeConfigHelper.unregisterMBean();
        } catch (Exception e) {
            throw new JBIException(e);
        }
    }
    
    /*
     * start HTTP listener
     */
    private void startHttpListener() throws Exception {
        InboundHttpListener defaultHttpListener = new InboundHttpListener(InboundHttpListener.DEFAULT_LISTENER, 
                runtimeConfig.getDefaultHttpListenerPort(), runtimeConfig.getDefaultHttpListenerThreads(), null);
        
        // install root JAXRS-POJO
        ServletAdapter adapter = new ServletAdapter();
        final Map<String, String> initParams = new HashMap<String, String>();

        initParams.put("com.sun.jersey.config.property.resourceConfigClass", "com.sun.jbi.restbc.jbiadapter.inbound.JerseyRootResourceApplication");
        initParams.put("com.sun.jersey.spi.container.ContainerResponseFilters", 
                CharsetResponseFilter.class.getName() + ';' +
                ContentLengthResponseFilter.class.getName());
        
        for (Map.Entry<String, String> e : initParams.entrySet()) {
            adapter.addInitParameter(e.getKey(), e.getValue());
        }
        
        adapter.setServletInstance(ServletContainer.class.newInstance());
        
        adapter.setContextPath("/");
        
        defaultHttpListener.registerContext("/", adapter);
        
        defaultHttpListener.start();
        listeners.put(defaultHttpListener.getListenerName(), defaultHttpListener);
        
        String msg = I18n.loc("RESTBC-4003: Started listener {0} at port {1}", defaultHttpListener.getListenerName(), defaultHttpListener.getPort());
        logger.info(msg);
    }
    
    /*
     * start HTTPS listener
     */
    private void startHttpsListener() throws Exception {
        String installRoot = context.getInstallRoot();
        File installRootFile = new File(installRoot);
        if (!installRootFile.isDirectory()) {
            String msg = I18n.loc("RESTBC-6005: Unable to initialize SSLConfig, install root is not a directory {0}",
                    installRootFile.toString());
            logger.log(Level.WARNING, msg);
            return;
        }
        
        // keystore file
        File keystoreFile = null;
        String keystoreSysProp = System.getProperty(KEYSTORE_PROP);
        if (keystoreSysProp == null || keystoreSysProp.length() == 0) {
            keystoreFile = new File(installRootFile, KEYSTORE_PATH);
        } else {
            keystoreFile = new File(keystoreSysProp);
        }
        if (!keystoreFile.isFile()) {
            String msg = I18n.loc("RESTBC-6006: Unable to initialize SSLConfig, keystore file is not a file {0}",
                    keystoreFile.toString());
            logger.log(Level.WARNING, msg);
            return;
        }
        
        // truststore file
        File truststoreFile = null;
        String truststoreSysProp = System.getProperty(TRUSTSTORE_PROP);
        if (truststoreSysProp == null || truststoreSysProp.length() == 0) {
            truststoreFile = new File(installRootFile, TRUSTSTORE_PATH);
        } else {
            truststoreFile = new File(truststoreSysProp);
        }
        if (!truststoreFile.isFile()) {
            String msg = I18n.loc("RESTBC-6007: Unable to initialize SSLConfig, truststore file is not a file {0}",
                    truststoreFile.toString());
            logger.log(Level.WARNING, msg);
            return;
        }
        
        // keystore password
        String keystorePassword = null;
        String keystorePasswordSysProp = System.getProperty(KEYSTORE_PASS_PROP);
        if (keystorePasswordSysProp == null || keystorePasswordSysProp.length() == 0) {
            keystorePassword = runtimeConfig.getKeystorePassword();
        } else {
            keystorePassword = keystorePasswordSysProp;
        }
        
        // truststore password
        String truststorePassword = null;
        String truststorePasswordSysProp = System.getProperty(TRUSTSTORE_PASS_PROP);
        if (truststorePasswordSysProp == null || truststorePasswordSysProp.length() == 0) {
            truststorePassword = runtimeConfig.getTruststorePassword();
        } else {
            truststorePassword = truststorePasswordSysProp;
        }
        
        SSLConfig sslConfig = new SSLConfig();
        sslConfig.setKeyStoreFile(keystoreFile.getAbsolutePath());
        sslConfig.setKeyStorePass(keystorePassword);
        sslConfig.setTrustStoreFile(truststoreFile.getAbsolutePath());
        sslConfig.setTrustStorePass(truststorePassword);
        
        InboundHttpListener defaultHttpsListener = new InboundHttpListener(InboundHttpListener.DEFAULT_LISTENER_SSL, 
                runtimeConfig.getDefaultHttpsListenerPort(), runtimeConfig.getDefaultHttpsListenerThreads(), sslConfig);
        
        // install root JAXRS-POJO
        ServletAdapter adapter = new ServletAdapter();
        final Map<String, String> initParams = new HashMap<String, String>();

        initParams.put("com.sun.jersey.config.property.resourceConfigClass", "com.sun.jbi.restbc.jbiadapter.inbound.JerseyRootResourceApplication");
        initParams.put("com.sun.jersey.spi.container.ContainerResponseFilters", "com.sun.jbi.restbc.jbiadapter.inbound.CharsetResponseFilter");
        
        for (Map.Entry<String, String> e : initParams.entrySet()) {
            adapter.addInitParameter(e.getKey(), e.getValue());
        }
        
        adapter.setServletInstance(ServletContainer.class.newInstance());
        
        adapter.setContextPath("/");
        
        defaultHttpsListener.registerContext("/", adapter);
        
        defaultHttpsListener.start();
        listeners.put(defaultHttpsListener.getListenerName(), defaultHttpsListener);
        
        String msg = I18n.loc("RESTBC-4003: Started listener {0} at port {1}", defaultHttpsListener.getListenerName(), defaultHttpsListener.getPort());
        logger.info(msg);
    }
    
    private void stopListeners() {
        Iterator<Map.Entry<String, InboundHttpListener>> iter = listeners.entrySet().iterator();
        while (iter.hasNext()) {
            Map.Entry<String, InboundHttpListener> entry = iter.next();
            InboundHttpListener listener = entry.getValue();
            iter.remove();
            try {
                listener.stop();
            } catch (Exception e) {
                String msg = I18n.loc("RESTBC-7005: {0} failed to stop HTTP listener {1}, {2}",
                        context.getComponentName(), listener.getListenerName(), e.getMessage());
                logger.log(Level.SEVERE, msg, e);
                continue;
            }
        }
    }
    
    public InboundHttpListener getInboundHttpListener(String listenerName) {
        return listeners.get(listenerName);
    }
    
    public String getInboundHttpListenerNameByPort(int port) {
        for (InboundHttpListener listener : listeners.values()) {
            if (listener.getPort() == port) {
                return listener.getListenerName();
            }
        }
        
        return null;
    }
    
    public SSLContext getSslContext() {
        return sslContext;
    }

    public RuntimeConfig getRuntimeConfig() {
        return runtimeConfig;
    }
    
    public RestSUManager getRestServiceUnitManager() {
        return suManager;
    }
    
    public ComponentContext getComponentContext() {
        return context;
    }
    
}
