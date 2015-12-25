package net.openesb.jbi.restbc.jbiadapter;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.jbi.messaging.NormalizedMessage;
import javax.mail.util.ByteArrayDataSource;
import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.ClientRequestFilter;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

import net.openesb.jbi.restbc.jbiadapter.descriptor.Filter;
import net.openesb.jbi.restbc.jbiadapter.util.BeanUtil;
import net.openesb.jbi.restbc.jbiadapter.util.JbiMessageUtil;
import net.openesb.jbi.restbc.jbiadapter.util.JsonUtil;
import net.openesb.jbi.restbc.jbiadapter.util.MediaTypeUtil;
import net.openesb.jbi.restbc.jbiadapter.util.NMPropertiesUtil;
import net.openesb.jbi.restbc.jbiadapter.util.PathUtil;
import net.openesb.jbi.restbc.jbiadapter.util.PropertiesUtil;
import net.openesb.jbi.restbc.jbiadapter.wsdl.RestOperation;

import org.glassfish.jersey.client.authentication.HttpAuthenticationFeature;
import org.glassfish.jersey.uri.UriTemplate;

/**
 * JerseyClientWrapper.java
 *
 * @author Edward Chou
 */
public final class JerseyClientWrapper {

    /*
     * 81-100
     */
    private static final Logger logger = Logger.getLogger(JerseyClientWrapper.class.getName());
    private static JerseyClientWrapper instance = null;

    private JerseyClientWrapper() throws Exception {
    }

    public static synchronized JerseyClientWrapper getInstance() throws Exception {
        if (instance == null) {
            instance = new JerseyClientWrapper();
        }
        return instance;
    }

    public Invocation buildClientRequest(
            RestComponent component,
            NormalizedMessage requestMsg,
            OutboundConfiguration outboundConfig) throws Exception {

        // build URL
        String url = PropertiesUtil.safeGetProperty(requestMsg, NMProps.NM_URL_PROP);
        if (url.length() == 0) {
            url = outboundConfig.getUrl();
        }
        if (url.length() == 0) {
            throw new Exception("invalid HTTP request URL: " + url);
        }

        // build URL template from Path Parameters
        Map<String, String> pathParams = NMPropertiesUtil.getDynamicNMProperties(requestMsg, NMProps.NM_PATH_PARAMS_PROP);

        UriTemplate uriTemplate = new UriTemplate(url);
        String resultURL = uriTemplate.createURI(pathParams);

        UriBuilder uriBuilder = UriBuilder.fromUri(resultURL);

        // build params
        String paramStyle = PropertiesUtil.safeGetProperty(requestMsg, NMProps.NM_PARAM_STYLE_PROP);
        if (paramStyle.length() == 0) {
            paramStyle = outboundConfig.getParamStyle();
        }

        Map<String, String> params = NMPropertiesUtil.getDynamicNMProperties(requestMsg, NMProps.NM_PARAMS_PROP);

        // merge the parameters defined in the service unit
        for (Map.Entry<String, String> param : outboundConfig.getParams().entrySet()) {
            if (!params.containsKey(param.getKey())) {
                params.put(param.getKey(), param.getValue());
            }
        }
        // build the URI with params
        for (Map.Entry<String, String> param : params.entrySet()) {
            if (paramStyle.equalsIgnoreCase("MATRIX")) {
                uriBuilder.matrixParam(param.getKey(), param.getValue());
            } else {
                uriBuilder.queryParam(param.getKey(), param.getValue());
            }
        }

        // build method
        String requestMethod = PropertiesUtil.safeGetProperty(requestMsg, NMProps.NM_METHOD_PROP);
        if (requestMethod.length() == 0) {
            requestMethod = outboundConfig.getMethod();
        }

        // build contentType
        String contentType = PropertiesUtil.safeGetProperty(requestMsg, NMProps.NM_CONTENT_TYPE_PROP);
        if (contentType.length() == 0) {
            contentType = outboundConfig.getContentType();
        }

        // build date
        String date = PropertiesUtil.safeGetProperty(requestMsg, NMProps.NM_DATE_PROP);

        // build acceptTypes
        String acceptTypesStr = PropertiesUtil.safeGetProperty(requestMsg, NMProps.NM_ACCEPT_TYPES_PROP);
        List<String> acceptTypes = null;
        if (acceptTypesStr.length() > 0) {
            acceptTypes = JsonUtil.parseJsonList(acceptTypesStr);
        }
        if (acceptTypes == null) {
            acceptTypes = new ArrayList<String>();
        }
        List<MediaType> acceptMediaTypes = new ArrayList<MediaType>();
        for (String s : acceptTypes) {
            acceptMediaTypes.add(MediaType.valueOf(s));
        }

        // merge the acceptTypes defined in the service unit
        for (MediaType acceptType : outboundConfig.getAcceptMediaTypes()) {
            if (!acceptMediaTypes.contains(acceptType)) {
                acceptMediaTypes.add(acceptType);
            }
        }

        // build acceptLanguages
        String acceptLanguagesStr = PropertiesUtil.safeGetProperty(requestMsg, NMProps.NM_ACCEPT_LANGUAGES_PROP);
        List<String> acceptLanguages = null;
        if (acceptLanguagesStr.length() > 0) {
            acceptLanguages = JsonUtil.parseJsonList(acceptLanguagesStr);
        }
        if (acceptLanguages == null) {
            acceptLanguages = new ArrayList<String>();
        }
        // merge the acceptLanguages defined in the service unit
        for (String s : outboundConfig.getAcceptLanguages()) {
            if (!acceptLanguages.contains(s)) {
                acceptLanguages.add(s);
            }
        }

        Map<String, String> headers = NMPropertiesUtil.getDynamicNMProperties(requestMsg, NMProps.NM_HEADERS_PROP);
        // merge the headers defined in the service unit
        for (Map.Entry<String, String> header : outboundConfig.getHeaders().entrySet()) {
            if (!headers.containsKey(header.getKey())) {
                headers.put(header.getKey(), header.getValue());
            }
        }

        // create ClientRequest.Builder
        Client client = ClientBuilder.newClient();
        WebTarget target = client.target(uriBuilder.build().toString());
        target = register(target, requestMsg, outboundConfig);
        
        
        Invocation.Builder invocationBuilder = target.request();
        
        boolean isContentTypeSet = false;
        if (contentType.length() > 0) {
        	invocationBuilder.header("Content-Type", contentType);
            isContentTypeSet = true;
        }

        boolean isDateSet = false;
        if (date.length() > 0) {
        	invocationBuilder.header("Date", date);
            isDateSet = true;
        }

        boolean isAcceptSet = false;
        for (MediaType acceptType : acceptMediaTypes) {
        	invocationBuilder.accept(acceptType);
            isAcceptSet = true;
        }

        boolean isAcceptLanguageSet = false;
        for (String acceptLanguage : acceptLanguages) {
        	invocationBuilder.acceptLanguage(acceptLanguage);
            isAcceptLanguageSet = true;
        }


        for (Map.Entry<String, String> header : headers.entrySet()) {
            // filter out headers that may conflict with the well-known headers set previously already
            if (header.getKey().equalsIgnoreCase("Content-Type") && isContentTypeSet) { // NOI18N
                String msg = I18n.loc("RESTBC-6081: Skipping Content-Type specified in Headers NM property, since Content-Type is already specified.");//NOI18N
                logger.warning(msg);
                continue;
            }
            if (header.getKey().equalsIgnoreCase("Accept") && isAcceptSet) { // NOI18N
                String msg = I18n.loc("RESTBC-6082: Skipping Accept specified in Headers NM property, since Accept is already specified.");//NOI18N
                logger.warning(msg);
                continue;
            }
            if (header.getKey().equalsIgnoreCase("Accept-Language") && isAcceptLanguageSet) { // NOI18N
                String msg = I18n.loc("RESTBC-6083: Skipping Accept-Language specified in Headers NM property, since Accept-Language is already specified.");//NOI18N
                logger.warning(msg);
                continue;
            }
            if (header.getKey().equalsIgnoreCase("Date") && isDateSet) { // NOI18N
                String msg = I18n.loc("RESTBC-6084: Skipping Date specified in Headers NM property, since Date is already specified.");//NOI18N
                logger.warning(msg);
                continue;
            }

            invocationBuilder.header(header.getKey(), header.getValue());
        }
        

        URI uri = uriBuilder.build();
        Invocation invocation = null;

        // set security if necessary
        if (uri.getScheme() != null && uri.getScheme().equalsIgnoreCase("https")) { // NOI18N

            SSLContext sslContext = component.getSslContext();  // get SSLContext if truststore and keystore are available
            if (sslContext != null) {
                HostnameVerifier hostnameVerifier = null;
                if (component.getRuntimeConfig().isEnableHostnameVerifier() == false) {
                    hostnameVerifier = new HostnameVerifier() {

                        public boolean verify(String hostname, SSLSession session) {
                            return true;
                        }
                    };
                }
                //LDE : todo
                //HTTPSProperties prop = new HTTPSProperties(hostnameVerifier, sslContext);
                //WebTarget.getProperties().put(HTTPSProperties.PROPERTY_HTTPS_PROPERTIES, prop);

            } else {
                String msg = I18n.loc("RESTBC-7081: SSLContext is unavailable to make HTTPS Request");//NOI18N
                logger.severe(msg);
                throw new Exception(msg);
            }

        }

        // set entity
        if (requestMethod.equalsIgnoreCase("GET") || requestMethod.equalsIgnoreCase("DELETE") || requestMethod.equalsIgnoreCase("HEAD")) { // NOI18N
            if (logger.isLoggable(Level.FINEST)) {
                String msg = I18n.lf("RESTBC-1081: ignored reading payload for {0} method", requestMethod);//NOI18N
                logger.finest(msg);
            }
            invocation = invocationBuilder.build(requestMethod);
            return invocation;
        }

        // use the entity as payload
        Object entity = requestMsg.getProperty(NMProps.NM_ENTITY_PROPS);
        if (entity != null) {
            if (entity instanceof String) {
                if (logger.isLoggable(Level.FINEST)) {
                    String msg = I18n.lf("RESTBC-1082: sending entity located in NM property for the request as a String object");//NOI18N
                    logger.finest(msg);
                }
                String entityString = (String) entity;
                invocation = invocationBuilder.build(requestMethod, Entity.text(entityString));
                //clientRequest.setEntity(entityString);
                return invocation;
            } else if (entity instanceof org.w3c.dom.Node) {
                if (logger.isLoggable(Level.FINEST)) {
                    String msg = I18n.lf("RESTBC-1083: sending entity located in NM property for the request as a DOMSource object");//NOI18N
                    logger.finest(msg);
                }
                org.w3c.dom.Node entityNode = (org.w3c.dom.Node) entity;
                DOMSource domSource = new DOMSource(entityNode);
                if (isContentTypeSet && PathUtil.isJSONMediaType(contentType)) {
                    String xmlPayloadAsString = JbiMessageUtil.convertXmlToString(domSource, outboundConfig.isStripNamespaces());
                    net.openesb.jbi.restbc.jbiadapter.org.json.JSONObject jsonObject =
                            net.openesb.jbi.restbc.jbiadapter.org.json.XML.toJSONObject(xmlPayloadAsString);
                    if (jsonObject != null) {
                    	invocation =invocationBuilder.build(requestMethod, Entity.json(jsonObject.toString()));
                    } else {
                    	invocation =invocationBuilder.build(requestMethod, Entity.json(xmlPayloadAsString));
                    }
                } else {
                	invocation =invocationBuilder.build(requestMethod, Entity.xml(JbiMessageUtil.convertXmlToString(domSource)));
                }
                return invocation;
            } else if (entity instanceof Source) {
                if (logger.isLoggable(Level.FINEST)) {
                    String msg = I18n.lf("RESTBC-1084: sending entity located in NM property for the request as a Source object");//NOI18N
                    logger.finest(msg);
                }
                Source entitySource = (Source) entity;
                if (isContentTypeSet && PathUtil.isJSONMediaType(contentType)) {
                    String xmlPayloadAsString = JbiMessageUtil.convertXmlToString(entitySource, outboundConfig.isStripNamespaces());
                    net.openesb.jbi.restbc.jbiadapter.org.json.JSONObject jsonObject =
                            net.openesb.jbi.restbc.jbiadapter.org.json.XML.toJSONObject(xmlPayloadAsString);
                    if (jsonObject != null) {
                    	invocation =invocationBuilder.build(requestMethod, Entity.json(jsonObject.toString()));
                    } else {
                    	invocation =invocationBuilder.build(requestMethod, Entity.json(xmlPayloadAsString));
                    }
                } else {
                	invocation =invocationBuilder.build(requestMethod, Entity.xml(JbiMessageUtil.convertXmlToString(entitySource)));
                }
                return invocation;
            }
        }

        // use JBI payload
        Object requestPayload = JbiMessageUtil.getPayloadFromWrappedMsg(requestMsg);
        if (requestPayload != null) {
            if (requestPayload instanceof Source) {
                Source xmlPayload = (Source) requestPayload;
                if (isContentTypeSet && PathUtil.isJSONMediaType(contentType)) {
                    String xmlPayloadAsString = JbiMessageUtil.convertXmlToString(xmlPayload, outboundConfig.isStripNamespaces());
                    net.openesb.jbi.restbc.jbiadapter.org.json.JSONObject jsonObject =
                            net.openesb.jbi.restbc.jbiadapter.org.json.XML.toJSONObject(xmlPayloadAsString);
                    if (jsonObject != null) {
                    	invocation = invocationBuilder.build(requestMethod, Entity.json(jsonObject.toString()));
                    } else {
                    	invocation = invocationBuilder.build(requestMethod, Entity.json((xmlPayloadAsString)));
                    }
                } else {
                	invocation = invocationBuilder.build(requestMethod, Entity.xml(JbiMessageUtil.convertXmlToString(xmlPayload)));
                }
            } else {
                DataHandler streamPayload = (DataHandler) requestPayload;
                //LDE : todo
                //invocation = invocationBuilder.build(requestMethod, streamPayload.getInputStream());
            }
        }

        return invocation;
  
    }

    public void buildNormalizedReplyMessage(NormalizedMessage replyMsg,
            String method,
            Response clientResponse,
            OutboundConfiguration outboundConfig) throws Exception {

 

        replyMsg.setProperty(NMProps.NM_RESPONSE_STATUS_PROP, clientResponse.getStatus());

        if (clientResponse.getLocation() != null) {
            replyMsg.setProperty(NMProps.NM_RESPONSE_URL_PROP, clientResponse.getLocation().toString());
        }

        if (clientResponse.getHeaderString("Content-Type") != null) {
            replyMsg.setProperty(NMProps.NM_RESPONSE_CONTENT_TYPE_PROP, MediaTypeUtil.mediaTypeToString(clientResponse.getMediaType()));
        }

        Map<String, String> headers = NMPropertiesUtil.multivaluedMapToMap(clientResponse.getStringHeaders());
        replyMsg.setProperty(NMProps.NM_RESPONSE_HEADERS_PROP, JsonUtil.buildJson(headers));
        NMPropertiesUtil.setDynamicNMProperties(replyMsg, NMProps.NM_RESPONSE_HEADERS_PROP, headers);

        //LDE : todo
        InputStream inputStream = (InputStream)clientResponse.getEntity();

        RestOperation restOp = outboundConfig.getRestOp();
        Definition definition = outboundConfig.getDefinition();
        if (restOp != null && definition != null) {
            // has WSDL configuration

            Source replyContent = null;
            if (inputStream != null && clientResponse.getMediaType() != null && !method.equalsIgnoreCase("head")) {
                MediaType mediaType = clientResponse.getMediaType();
                if (PathUtil.isXMLMediaType(mediaType) && !outboundConfig.isForwardAsAttachment()) {
                    // this is XML content and don't forward as attachment
                    replyContent = JbiMessageUtil.createJbiWrappedMsg(inputStream, restOp, definition, true);
                } else if (PathUtil.isJSONMediaType(mediaType) && !outboundConfig.isForwardAsAttachment()) {
                    // this is JSON content, convert to XML and don't forward as attachment
                    StringBuilder sb = new StringBuilder();
                    BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
                    String currentString = reader.readLine();
                    while (currentString != null) {
                        sb.append(currentString);
                        currentString = reader.readLine();
                    }
                    net.openesb.jbi.restbc.jbiadapter.org.json.JSONObject jsonObject = new net.openesb.jbi.restbc.jbiadapter.org.json.JSONObject(sb.toString());
                    String xmlString = net.openesb.jbi.restbc.jbiadapter.org.json.XML.toString(jsonObject);
                    replyContent = JbiMessageUtil.createJbiWrappedMsg(xmlString, restOp, definition, false);
                } else {
                    // treat as attachment
                    DataSource ds = new ByteArrayDataSource(inputStream, MediaTypeUtil.mediaTypeToString(mediaType));
                    DataHandler dataHandler = new DataHandler(ds);
                    String uuid = UUID.randomUUID().toString();

                    replyContent = JbiMessageUtil.createJbiAttachmentWrapper(uuid, restOp, definition, true);
                    replyMsg.addAttachment(uuid, dataHandler);
                }

            } else {
                replyContent = JbiMessageUtil.createEmptyJbiWrappedMsg(restOp, definition, true);
            }
            replyMsg.setContent(replyContent);
        } else {
            // no WSDL configuration
            Source replyContent = null;
            QName msgType = null;
            try {
                msgType = QName.valueOf(outboundConfig.getMsgType());
            } catch (IllegalArgumentException iae) {
                // ignore
            }
            if (inputStream != null && clientResponse.getMediaType() != null && !method.equalsIgnoreCase("head")) {
                MediaType mediaType = clientResponse.getMediaType();
                if (PathUtil.isXMLMediaType(mediaType) && !outboundConfig.isForwardAsAttachment()) {
                    // this is XML content and don't forward as attachment
                    replyContent = JbiMessageUtil.createJbiWrappedMsg(msgType, inputStream);
                } else if (PathUtil.isJSONMediaType(mediaType) && !outboundConfig.isForwardAsAttachment()) {
                    // this is JSON content, convert to XML and don't forward as attachment
                    StringBuilder sb = new StringBuilder();
                    BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));
                    String currentString = reader.readLine();
                    while (currentString != null) {
                        sb.append(currentString);
                        currentString = reader.readLine();
                    }
                    net.openesb.jbi.restbc.jbiadapter.org.json.JSONObject jsonObject = new net.openesb.jbi.restbc.jbiadapter.org.json.JSONObject(sb.toString());
                    String xmlString = net.openesb.jbi.restbc.jbiadapter.org.json.XML.toString(jsonObject);
                    replyContent = JbiMessageUtil.createJbiWrappedMsg(msgType, new ByteArrayInputStream(xmlString.getBytes()));

                } else {
                    // treat as attachment
                    DataSource ds = new ByteArrayDataSource(inputStream, MediaTypeUtil.mediaTypeToString(clientResponse.getMediaType()));
                    DataHandler dataHandler = new DataHandler(ds);
                    String uuid = UUID.randomUUID().toString();

                    replyContent = JbiMessageUtil.createJbiAttachmentWrapper(msgType, uuid);
                    replyMsg.addAttachment(uuid, dataHandler);
                }

            } else {
                replyContent = JbiMessageUtil.createJbiWrappedMsg(msgType, null);
            }
            replyMsg.setContent(replyContent);

        }

    }
    
 
    public WebTarget register(WebTarget webTarget,
            NormalizedMessage requestMsg,
            OutboundConfiguration outboundConfig) throws Exception {       
        
    	//LDE : what?
        //Client client = new Client(new URLConnectionClientHandler(new HttpProxyURLConnectionFactory()));

        // add basic auth filter
        String basicAuthUsername = PropertiesUtil.safeGetProperty(requestMsg, NMProps.NM_BASICAUTH_USERNAME_PROP);
        if (basicAuthUsername.length() == 0) {
            basicAuthUsername = outboundConfig.getBasicAuthUsername();
        }

        String basicAuthPassword = PropertiesUtil.safeGetProperty(requestMsg, NMProps.NM_BASICAUTH_PASSWORD_PROP);
        if (basicAuthPassword.length() == 0) {
            basicAuthPassword = outboundConfig.getBasicAuthPassword();
        }

        if (basicAuthUsername.length() > 0 && basicAuthPassword.length() > 0) {
            webTarget.register(HttpAuthenticationFeature.basic(basicAuthUsername, basicAuthPassword));
        }

        //LDE : todo
        //if (logger.isLoggable(Level.FINEST)) {
        //    logger.finest(getClientRequestAsString(request.));
        //}

        // apply all custom filters
        for (Filter filter : outboundConfig.getEndpointIdentifier().getFilters()) {
            try {
                Class<?> filterClass = outboundConfig.getServiceUnit().loadFilterClass(filter.getClassName());
                if (!ClientRequestFilter.class.isAssignableFrom(filterClass)) {
                    String msg = I18n.lf("RESTBC-6085: filter class {0} is not a subclass of com.sun.jersey.api.client.filter.ClientFilter, skipping ...", filter.getClassName());//NOI18N
                    logger.warning(msg);
                    continue;
                }
                Object filterObject = filterClass.newInstance();

                for (Map.Entry<String, String> entry : filter.getProps().entrySet()) {
                    String key = entry.getKey();
                    String val = entry.getValue();
                    BeanUtil.setProperty(filterObject, key, val);
                }

                webTarget.register((ClientRequestFilter) filterObject);

            } catch (Exception e) {
                String msg = I18n.lf("RESTBC-6086: unable to instantiate filter class {0} skipping ..., {1}", filter.getClassName(), e);//NOI18N
                logger.warning(msg);
                continue;
            }
        }


        //LDE toto
        //if (logger.isLoggable(Level.FINEST)) {
        //    logger.finest(getClientResponseAsString(response));
        //}

        return webTarget;
    }
/*
    private String getClientRequestAsString(ClientRequest request) {
        StringBuilder sb = new StringBuilder();
        try {
            sb.append("Client Request: \n");
            sb.append("  URI = " + request.getUri()+ "\n");
            sb.append("  Method = " + request.getMethod() + "\n");
            sb.append("  Headers = " + request.getHeaders()+ "\n");
        } catch (Exception e) {
            logger.log(Level.WARNING, "error logging client request", e);
        }
        return sb.toString();
    }

    private String getClientResponseAsString(ClientResponse response) {
        StringBuilder sb = new StringBuilder();
        try {
            sb.append("Client Response: \n");
            sb.append("  Status = " + response.getStatus()+ " \n");
            sb.append("  Type = " + MediaTypeUtil.mediaTypeToString(response.getMediaType()) + " \n");
            sb.append("  Headers = " + response.getHeaders() + " \n");
        } catch (Exception e) {
            logger.log(Level.WARNING, "error logging client response", e);
        }
        return sb.toString();
    }*/
}