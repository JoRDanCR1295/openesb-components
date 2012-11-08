package com.sun.jbi.restbc.jbiadapter.wsdl;

import java.io.ByteArrayInputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.ws.rs.core.HttpHeaders;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.jbi.restbc.jbiadapter.I18n;
import com.sun.jbi.restbc.jbiadapter.InboundConfiguration;
import com.sun.jbi.restbc.jbiadapter.OutboundConfiguration;
import com.sun.jbi.restbc.jbiadapter.RestComponent;
import com.sun.jbi.restbc.jbiadapter.ServiceUnit;
import com.sun.jbi.restbc.jbiadapter.ServiceUnitConfig;
import com.sun.jbi.restbc.jbiadapter.descriptor.EndpointIdentifier;
import com.sun.jbi.restbc.jbiadapter.util.PathUtil;

/**
 * WSDLEndpoint.java
 *
 * @author Edward Chou
 */
public class WSDLEndpoint implements ServiceUnitConfig {
    
    /*
     * 121-130
     */
    private final static Logger logger = Logger.getLogger(WSDLEndpoint.class.getName());
    
    private ServiceUnit serviceUnit;
    private ComponentContext context;
    private RestComponent component;
    
    private Definition definition;
    private boolean isOutbound = true;
    private Document doc;
    private RestAddress restAddress;
    private RestBinding restBinding;
    
    private QName serviceName;
    private String endpointName;
    private EndpointIdentifier ei;
    private ServiceEndpoint endpointRef = null;
    
    private Map<QName, RestOperation> operationsMap = new HashMap<QName, RestOperation> ();
    private Map<RestOperation, OutboundConfiguration> outboundConfigs = new HashMap<RestOperation, OutboundConfiguration> ();
    private Map<RestOperation, InboundConfiguration> inboundConfigs = new HashMap<RestOperation, InboundConfiguration> ();
    
    
    public WSDLEndpoint(ServiceUnit serviceUnit, ComponentContext context, RestComponent component, QName serviceName, String endpointName, EndpointIdentifier ei) {
        this.serviceUnit = serviceUnit;
        this.context = context;
        this.component = component;
        this.serviceName = serviceName;
        this.endpointName = endpointName;
        this.ei = ei;
    }
    
    public void setDefinition(Definition definition) {
        this.definition = definition;
    }
    
    public Definition getDefinition() {
        return definition;
    }
    
    public void setIsOutbound(boolean isOutbound) {
        this.isOutbound = isOutbound;
    }
    
    public boolean isOutbound() {
        return isOutbound;
    }
    
    public void setServiceDescription(Document doc) {
        this.doc = doc;
    }
    
    public void setRestAddress(RestAddress restAddress) {
        this.restAddress = restAddress;
    }
    
    public void setRestBinding(RestBinding restBinding) {
        this.restBinding = restBinding;
    }
    
    public void setRestOperations(Map<QName, RestOperation> map) throws Exception {
        this.operationsMap.putAll(map);
        
        for (RestOperation op : operationsMap.values()) {
            Element el = op.getElement();
            String propertyString =  el.getTextContent();
            Properties p = new Properties();
            ByteArrayInputStream bais = new ByteArrayInputStream(propertyString.getBytes("UTF-8"));
            p.load(bais);
            
            if (isOutbound) {
                OutboundConfiguration outboundConfig = new OutboundConfiguration(p, serviceUnit, op, definition, ei, component.getRuntimeConfig());
                outboundConfigs.put(op, outboundConfig);
            } else {
                InboundConfiguration inboundConfig = new InboundConfiguration(p, serviceUnit, op, definition, serviceName, endpointName, ei, component.getRuntimeConfig());
                // verfity that listener-name from inbound configuration matches an existing http-listener
                if (component.getInboundHttpListener(inboundConfig.getHttpListenerName()) == null) {
                    String msg = I18n.loc("RESTBC-7121: http-listener-name={0} specified in the inbound configuration for WSDL operation {1} does not match an existing HTTP Listener.", inboundConfig.getHttpListenerName(), op.getOperation().toString());
                    logger.log(Level.SEVERE, msg);
                    throw new Exception(msg);
                }
                
                inboundConfigs.put(op, inboundConfig);
            }
        }
    }
    
    public OutboundConfiguration findOutboundConfig(QName operationName)  {
        RestOperation restOp = operationsMap.get(operationName);
        if (restOp != null) {
            return outboundConfigs.get(restOp);
        }
        return null;
    }
    
    public InboundConfiguration findInboundConfig(String listenerName, HttpHeaders headers, String method, String path) {
        for (InboundConfiguration inboundConfig : inboundConfigs.values()) {
            if (inboundConfig.getHttpListenerName().equals(listenerName)) {
                if (PathUtil.matchInboundConfiguration(inboundConfig, headers, method, path)) {
                    return inboundConfig;
                }
            }
        }
        return null;
    }
    
    public void start() throws Exception {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1121: WSDLEndpoint.start()");//NOI18N
            logger.finest(msg);
        }
        
        if (isOutbound) {
            try {
                String msg = I18n.loc("RESTBC-4121: activating endpoint for WSDLEndpoint: serviceName={0}, endpointName={1}", serviceName, endpointName);//NOI18N
                logger.log(Level.INFO, msg);
                endpointRef = context.activateEndpoint(serviceName, endpointName);
                
            } catch (JBIException jbiEx) {
                String msg = I18n.loc("RESTBC-7122: unable to activate endpoint for WSDLEndpoint: serviceName={0}, endpointName={1}, {2}", serviceName, endpointName, jbiEx.getMessage());
                logger.log(Level.SEVERE, msg, jbiEx);
                throw jbiEx;
            }
        }
    }

    public void stop() throws Exception {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1122: WSDLEndpoint.stop()");//NOI18N
            logger.finest(msg);
        }
        
        if (isOutbound) {
            try {
                String msg = I18n.loc("RESTBC-4122: deactivating endpoint for WSDLEndpoint: serviceName={0}, endpointName={1}", endpointRef.getServiceName(), endpointRef.getEndpointName());//NOI18N
                logger.log(Level.INFO, msg);
                context.deactivateEndpoint(endpointRef);
            } catch (JBIException jbiEx) {
                String msg = I18n.loc("RESTBC-7123: unable to deactivate endpoint for WSDLEndpoint: serviceName={0}, endpointName={1}, {2}", endpointRef.getServiceName(), endpointRef.getEndpointName(), jbiEx.getMessage());
                logger.log(Level.SEVERE, msg, jbiEx);
                throw jbiEx;
            }
        }
    }

    public void shutdown() throws Exception {
        if (logger.isLoggable(Level.FINEST)) {
            String msg = I18n.lf("RESTBC-1123: WSDLEndpoint.shutdown()");//NOI18N
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
        return serviceName;
    }

    /**
     * @return the endpointName
     */
    public String getEndpointName() {
        return endpointName;
    }

}
