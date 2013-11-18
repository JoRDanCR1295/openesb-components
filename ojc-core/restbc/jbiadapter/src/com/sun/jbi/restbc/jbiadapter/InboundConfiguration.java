package com.sun.jbi.restbc.jbiadapter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.ws.rs.core.MediaType;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

import com.sun.jbi.restbc.jbiadapter.descriptor.EndpointIdentifier;
import com.sun.jbi.restbc.jbiadapter.inbound.InboundHttpListener;
import com.sun.jbi.restbc.jbiadapter.mbeans.RuntimeConfig;
import com.sun.jbi.restbc.jbiadapter.util.JsonUtil;
import com.sun.jbi.restbc.jbiadapter.util.PropertiesUtil;
import com.sun.jbi.restbc.jbiadapter.wsdl.RestOperation;
import com.sun.jersey.api.uri.UriTemplate;

/**
 * InboundConfiguration.java
 *
 * @author Edward Chou
 */
public class InboundConfiguration {
    
    private final static String HTTP_LISTENER_NAME_PROP = "http-listener-name";
    private final static String PATH_PROP = "path";
    private final static String METHOD_PROP = "method";
    private final static String CONSUME_TYPES_PROP = "consume-types";
    private final static String PRODUCE_TYPES_PROP = "produce-types";
    private final static String FORWARD_AS_ATTACHMENT_PROP = "forward-as-attachment";
    private final static String TIMEOUT_PROP = "timeout";
    private final static String MSG_TYPE = "message-type";
    private final static String STRIP_NAMESPACES = "strip-namespaces";
    
    private String httpListenerName;
    private String path;
    private UriTemplate pathTemplate;
    private String method;
    private List<String> consumeTypes = new ArrayList<String> ();
    private List<MediaType> consumeMediaTypes = new ArrayList<MediaType> ();
    private List<String> produceTypes = new ArrayList<String> ();
    private List<MediaType> produceMediaTypes = new ArrayList<MediaType> ();
    private boolean forwardAsAttachment = false;
    private long timeout = 60000;
    private String msgType;
    // This property is only used in the case of a JSON output
    private boolean stripNamespaces = false;
    
    private ServiceUnit serviceUnit;
    private RestOperation restOp;
    private Definition definition;
    private QName serviceName;
    private String endpointName;
    private EndpointIdentifier endpointIdentifier;
    
    public InboundConfiguration(Properties p, 
            ServiceUnit serviceUnit, 
            RestOperation restOp, 
            Definition definition, 
            QName serviceName, 
            String endpointName, 
            EndpointIdentifier endpointIdentifier,
            RuntimeConfig runtimeConfig) throws Exception {
        this.serviceUnit = serviceUnit;
        this.restOp = restOp;
        this.definition = definition;
        this.serviceName = serviceName;
        this.endpointName = endpointName;
        this.endpointIdentifier = endpointIdentifier;
        httpListenerName = PropertiesUtil.safeGetProperty(p, HTTP_LISTENER_NAME_PROP, InboundHttpListener.DEFAULT_LISTENER);
        
        String appConfigName = endpointIdentifier.getApplicationConfigurationName();
        if (appConfigName != null && appConfigName.length() > 0) {
            Map appConfigMap = runtimeConfig.retrieveApplicationConfigurationsMap();
            path = (String) appConfigMap.get(appConfigName);
            if (path == null) {
                throw new Exception("cannot resolve Application Configuration named: " + appConfigName);
            }
        } else {
            path = PropertiesUtil.safeGetProperty(p, PATH_PROP);
        }
        pathTemplate = new UriTemplate(path);
        method = PropertiesUtil.safeGetProperty(p, METHOD_PROP, "GET");
        consumeTypes = JsonUtil.parseJsonList(PropertiesUtil.safeGetProperty(p, CONSUME_TYPES_PROP));
        for (String s : consumeTypes) {
            consumeMediaTypes.add(MediaType.valueOf(s));
        }
        produceTypes = JsonUtil.parseJsonList(PropertiesUtil.safeGetProperty(p, PRODUCE_TYPES_PROP));
        for (String s: produceTypes) {
            produceMediaTypes.add(MediaType.valueOf(s));
        }
        forwardAsAttachment = Boolean.parseBoolean(PropertiesUtil.safeGetProperty(p, FORWARD_AS_ATTACHMENT_PROP, "false"));
        timeout = Long.parseLong(PropertiesUtil.safeGetProperty(p, TIMEOUT_PROP, "60000"));
        msgType = PropertiesUtil.safeGetProperty(p, MSG_TYPE);
        
        stripNamespaces = Boolean.parseBoolean(PropertiesUtil.safeGetProperty(p, STRIP_NAMESPACES, "false"));
    }
    
    /**
     * @return the serviceUnit
     */
    public ServiceUnit getServiceUnit() {
        return serviceUnit;
    }

    /**
     * @return the restOp
     */
    public RestOperation getRestOp() {
        return restOp;
    }

    /**
     * @return the definition
     */
    public Definition getDefinition() {
        return definition;
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
    
    /**
     * @return the httpListenerName
     */
    public String getHttpListenerName() {
        return httpListenerName;
    }

    /**
     * @return the pathTemplate
     */
    public UriTemplate getPathTemplate() {
        return pathTemplate;
    }

    /**
     * @return the method
     */
    public String getMethod() {
        return method;
    }

    /**
     * @return the consumeMediaTypes
     */
    public List<MediaType> getConsumeMediaTypes() {
        return Collections.unmodifiableList(consumeMediaTypes);
    }

    /**
     * @return the produceMediaTypes
     */
    public List<MediaType> getProduceMediaTypes() {
        return Collections.unmodifiableList(produceMediaTypes);
    }

    /**
     * @return the forwardAsAttachment
     */
    public boolean isForwardAsAttachment() {
        return forwardAsAttachment;
    }

    /**
     * @return the timeout
     */
    public long getTimeout() {
        return timeout;
    }

    /**
     * @return the msgType
     */
    public String getMsgType() {
        return msgType;
    }

    /**
     * @return the endpointIdentifier
     */
    public EndpointIdentifier getEndpointIdentifier() {
        return endpointIdentifier;
    }

    /**
     * @return the stripNamespaces
     */
    public boolean isStripNamespaces() {
        return stripNamespaces;
    }
}
