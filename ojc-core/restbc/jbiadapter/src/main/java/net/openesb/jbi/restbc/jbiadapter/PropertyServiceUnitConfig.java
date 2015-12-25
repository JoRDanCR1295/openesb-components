package net.openesb.jbi.restbc.jbiadapter;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import net.openesb.jbi.restbc.jbiadapter.descriptor.EndpointIdentifier;
import net.openesb.jbi.restbc.jbiadapter.descriptor.RestSUDescriptor;
import net.openesb.jbi.restbc.jbiadapter.util.PropertiesUtil;

/**
 * PropertyServiceUnitConfig.java
 *
 * @author Edward Chou
 */
public class PropertyServiceUnitConfig implements ServiceUnitConfig {
    
    /*
     * 131-140
     */
    private final static Logger logger = Logger.getLogger(PropertyServiceUnitConfig.class.getName());
    
    private final static String SERVICE_NAME_PROP = "service-name";
    private final static String ENDPOINT_NAME_PROP = "endpoint-name";
    
    private ServiceUnit serviceUnit;
    private RestComponent component;
    private ComponentContext context;
    private RestSUDescriptor suDescriptor;
    private Properties configProps = new Properties();
    private ServiceEndpoint endpointRef = null;
    
    private String serviceName;
    private QName serviceNameQName;
    private String endpointName;
    private EndpointIdentifier endpointIdentifier;
    
    private OutboundConfiguration outboundConfig = null;
    private InboundConfiguration inboundConfig = null;
    private boolean isOutbound = true;

    public PropertyServiceUnitConfig(ServiceUnit serviceUnit, RestComponent component, ComponentContext context, RestSUDescriptor suDescriptor, File configFile) throws Exception {
        this.serviceUnit = serviceUnit;
        this.component = component;
        this.context = context;
        this.suDescriptor = suDescriptor;
        
        init(configFile);
    }
    
    public RestComponent getComponent() {
        return component;
    }

    private void init(File configFile) throws Exception {
        InputStream is = null;
        try {
            is = new FileInputStream(configFile);
            configProps.load(is);
        } finally {
            try {
                is.close();
            } catch (IOException ioe) {
                // ignore
            }
        }
        
        serviceName = PropertiesUtil.safeGetProperty(configProps, SERVICE_NAME_PROP);
        if (serviceName.length() == 0) {
            String msg = I18n.loc("RESTBC-7134: {0} property cannot be empty.", SERVICE_NAME_PROP);
            logger.log(Level.SEVERE, msg);
            throw new Exception(msg);
        }
        serviceNameQName = QName.valueOf(serviceName);
        endpointName = PropertiesUtil.safeGetProperty(configProps, ENDPOINT_NAME_PROP);
        if (endpointName.length() == 0) {
            String msg = I18n.loc("RESTBC-7134: {0} property cannot be empty.", ENDPOINT_NAME_PROP);
            logger.log(Level.SEVERE, msg);
            throw new Exception(msg);
        }
        
    }
    
    public void parseOutboundConfiguration() throws Exception {
        isOutbound = true;
        outboundConfig = new OutboundConfiguration(configProps, serviceUnit, null, null, endpointIdentifier, component.getRuntimeConfig());
    }
    
    public void parseInboundConfiguration() throws Exception {
        isOutbound = false;
        inboundConfig = new InboundConfiguration(configProps, serviceUnit, null, null, serviceNameQName, endpointName, endpointIdentifier, component.getRuntimeConfig());
        // verfity that listener-name from inbound configuration matches an existing http-listener
        if (component.getInboundHttpListener(inboundConfig.getHttpListenerName()) == null) {
            String msg = I18n.loc("RESTBC-7131: http-listener-name={0} specified in the inbound configuration for PropertyConfiguration does not match an existing HTTP Listener.", inboundConfig.getHttpListenerName());
            logger.log(Level.SEVERE, msg);
            throw new Exception(msg);
        }
    }
    
    public boolean isOutbound() {
        return isOutbound;
    }
    
    public OutboundConfiguration getOutboundConfiguration() {
        return outboundConfig;
    }
    
    public InboundConfiguration getInboundConfiguration() {
        return inboundConfig;
    }
    

    public void start() throws Exception {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1131: PropertyServiceUnitConfig.start()");//NOI18N
            logger.finest(msg);
        }
        
        if (isOutbound) {
            try {
                String msg = I18n.loc("RESTBC-4131: activating endpoint for PropertyServiceUnitConfig: serviceName={0}, endpointName={1}", serviceName, endpointName);//NOI18N
                logger.log(Level.INFO, msg);
                endpointRef = context.activateEndpoint(serviceNameQName, endpointName);

            } catch (JBIException jbiEx) {
                String msg = I18n.loc("RESTBC-7132: unable to activate endpoint for PropertyServiceUnitConfig: serviceName={0}, endpointName={1}, {2}", serviceName, endpointName, jbiEx.getMessage());
                logger.log(Level.SEVERE, msg, jbiEx);
                throw jbiEx;
            }
        }
    }

    public void stop() throws Exception {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1132: PropertyServiceUnitConfig.stop()");//NOI18N
            logger.finest(msg);
        }
        
        if (isOutbound) {
            try {
                String msg = I18n.loc("RESTBC-4132: deactivating endpoint for PropertyServiceUnitConfig: serviceName={0}, endpointName={1}", endpointRef.getServiceName(), endpointRef.getEndpointName());//NOI18N
                logger.log(Level.INFO, msg);
                context.deactivateEndpoint(endpointRef);
            } catch (JBIException jbiEx) {
                String msg = I18n.loc("RESTBC-7133: unable to deactivate endpoint for PropertyServiceUnitConfig: serviceName={0}, endpointName={1}, {2}", endpointRef.getServiceName(), endpointRef.getEndpointName(), jbiEx.getMessage());
                logger.log(Level.SEVERE, msg, jbiEx);
                throw jbiEx;
            }
        }
    }
    
    public void shutdown() throws Exception {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1133: PropertyServiceUnitConfig.shutdown()");//NOI18N
            logger.finest(msg);
        }
        if (isOutbound) {
            endpointRef = null;
        }
    }
    
    /**
     * @return the serviceName
     */
    public QName getServiceName() {
        return serviceNameQName;
    }

    /**
     * @return the endpointName
     */
    public String getEndpointName() {
        return endpointName;
    }

    /**
     * @return the endpointIdentifier
     */
    public EndpointIdentifier getEndpointIdentifier() {
        return endpointIdentifier;
    }

    /**
     * @param endpointIdentifier the endpointIdentifier to set
     */
    public void setEndpointIdentifier(EndpointIdentifier endpointIdentifier) {
        this.endpointIdentifier = endpointIdentifier;
    }


}
