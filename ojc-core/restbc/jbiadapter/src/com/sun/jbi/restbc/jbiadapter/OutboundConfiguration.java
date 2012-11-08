package com.sun.jbi.restbc.jbiadapter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.ws.rs.core.MediaType;
import javax.wsdl.Definition;

import com.sun.jbi.restbc.jbiadapter.descriptor.EndpointIdentifier;
import com.sun.jbi.restbc.jbiadapter.mbeans.RuntimeConfig;
import com.sun.jbi.restbc.jbiadapter.util.JsonUtil;
import com.sun.jbi.restbc.jbiadapter.util.PropertiesUtil;
import com.sun.jbi.restbc.jbiadapter.wsdl.RestOperation;

/**
 * OutboundConfiguration.java
 *
 * @author Edward Chou
 */
public class OutboundConfiguration {
    
    private final static String URL_PROP = "url";
    private final static String METHOD_PROP = "method";
    private final static String CONTENT_TYPE_PROP = "content-type";
    private final static String ACCEPT_TYPES_PROP = "accept-types";
    private final static String ACCEPT_LANGUAGES_PROP = "accept-languages";
    private final static String HEADERS_PROP = "headers";
    private final static String PARAM_STYLE_PROP = "param-style";
    private final static String PARAMS_PROP = "params";
    private final static String FORWARD_AS_ATTACHMENT_PROP = "forward-as-attachment";
    private final static String BASICAUTH_USERNAME_PROP = "basic-auth-username";
    private final static String BASICAUTH_PASSWORD_PROP = "basic-auth-password";
    private final static String MSG_TYPE = "message-type";
    
    private String url;
    private String method;
    private String contentType;
    private List<String> acceptTypes = new ArrayList<String> ();
    private List<MediaType> acceptMediaTypes = new ArrayList<MediaType> ();
    private List<String> acceptLanguages = new ArrayList<String> ();
    private Map<String, String> headers = new HashMap<String, String> ();
    private String paramStyle;
    private Map<String, String> params = new HashMap<String, String> ();
    private boolean forwardAsAttachment = false;
    private String basicAuthUsername;
    private String basicAuthPassword;
    private String msgType;
    
    private ServiceUnit serviceUnit;
    private RestOperation restOp;
    private Definition definition;
    private EndpointIdentifier endpointIdentifier;
    
    public OutboundConfiguration(Properties p, 
            ServiceUnit serviceUnit, 
            RestOperation restOp, 
            Definition definition, 
            EndpointIdentifier endpointIdentifier,
            RuntimeConfig runtimeConfig) throws Exception {
        this.serviceUnit = serviceUnit;
        this.restOp = restOp;
        this.definition = definition;
        this.endpointIdentifier = endpointIdentifier;
        
        String appConfigName = endpointIdentifier.getApplicationConfigurationName();
        if (appConfigName != null && appConfigName.length() > 0) {
            Map appConfigMap = runtimeConfig.retrieveApplicationConfigurationsMap();
            url = (String) appConfigMap.get(appConfigName);
            if (url == null) {
                throw new Exception("cannot resolve Application Configuration named: " + appConfigName);
            }
        } else {
            url = PropertiesUtil.safeGetProperty(p, URL_PROP);
        }
        method = PropertiesUtil.safeGetProperty(p, METHOD_PROP, "GET");
        contentType = PropertiesUtil.safeGetProperty(p, CONTENT_TYPE_PROP);
        acceptTypes = JsonUtil.parseJsonList(PropertiesUtil.safeGetProperty(p, ACCEPT_TYPES_PROP));
        for (String s : acceptTypes) {
            acceptMediaTypes.add(MediaType.valueOf(s));
        }
        acceptLanguages = JsonUtil.parseJsonList(PropertiesUtil.safeGetProperty(p, ACCEPT_LANGUAGES_PROP));
        headers = JsonUtil.parseJsonPairs(PropertiesUtil.safeGetProperty(p, HEADERS_PROP));
        paramStyle = PropertiesUtil.safeGetProperty(p, PARAM_STYLE_PROP, "QUERY");
        params = JsonUtil.parseJsonPairs(PropertiesUtil.safeGetProperty(p, PARAMS_PROP));
        forwardAsAttachment = Boolean.parseBoolean(PropertiesUtil.safeGetProperty(p, FORWARD_AS_ATTACHMENT_PROP, "false"));
        
        // basic authentication
        basicAuthUsername = PropertiesUtil.safeGetProperty(p, BASICAUTH_USERNAME_PROP);
        basicAuthUsername = PropertiesUtil.applyApplicationVariables(basicAuthUsername, runtimeConfig.retrieveApplicationVariablesMap());
        basicAuthPassword = PropertiesUtil.safeGetProperty(p, BASICAUTH_PASSWORD_PROP);
        basicAuthPassword = PropertiesUtil.applyApplicationVariables(basicAuthPassword, runtimeConfig.retrieveApplicationVariablesMap());
        
        msgType = PropertiesUtil.safeGetProperty(p, MSG_TYPE);
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
     * @return the url
     */
    public String getUrl() {
        return url;
    }

    /**
     * @return the method
     */
    public String getMethod() {
        return method;
    }

    /**
     * @return the contentType
     */
    public String getContentType() {
        return contentType;
    }

    /**
     * @return the acceptMediaTypes
     */
    public List<MediaType> getAcceptMediaTypes() {
        return Collections.unmodifiableList(acceptMediaTypes);
    }

    /**
     * @return the acceptLanguages
     */
    public List<String> getAcceptLanguages() {
        return Collections.unmodifiableList(acceptLanguages);
    }

    /**
     * @return the headers
     */
    public Map<String, String> getHeaders() {
        return Collections.unmodifiableMap(headers);
    }

    /**
     * @return the paramStyle
     */
    public String getParamStyle() {
        return paramStyle;
    }

    /**
     * @return the params
     */
    public Map<String, String> getParams() {
        return Collections.unmodifiableMap(params);
    }

    /**
     * @return the forwardAsAttachment
     */
    public boolean isForwardAsAttachment() {
        return forwardAsAttachment;
    }

    /**
     * @return the basicAuthUsername
     */
    public String getBasicAuthUsername() {
        return basicAuthUsername;
    }

    /**
     * @return the basicAuthPassword
     */
    public String getBasicAuthPassword() {
        return basicAuthPassword;
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
    
}
