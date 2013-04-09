package com.sun.jbi.restbc.jbiadapter.inbound;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.mail.util.ByteArrayDataSource;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;
import javax.ws.rs.core.Response.ResponseBuilder;
import javax.wsdl.Definition;
import javax.wsdl.OperationType;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jbi.restbc.jbiadapter.I18n;
import com.sun.jbi.restbc.jbiadapter.InboundConfiguration;
import com.sun.jbi.restbc.jbiadapter.NMProps;
import com.sun.jbi.restbc.jbiadapter.RestComponent;
import com.sun.jbi.restbc.jbiadapter.util.JbiMessageUtil;
import com.sun.jbi.restbc.jbiadapter.util.JsonUtil;
import com.sun.jbi.restbc.jbiadapter.util.MediaTypeUtil;
import com.sun.jbi.restbc.jbiadapter.util.NMPropertiesUtil;
import com.sun.jbi.restbc.jbiadapter.util.PathUtil;
import com.sun.jbi.restbc.jbiadapter.util.PropertiesUtil;
import com.sun.jbi.restbc.jbiadapter.wsdl.RestOperation;
import com.sun.jersey.api.uri.UriTemplate;

/**
 * InboundDelegator.java
 *
 * @author Edward Chou
 */
public class InboundDelegator {

    /*
     * 111-130
     */
    private static final Logger logger = Logger.getLogger(InboundDelegator.class.getName());
    
    private static InboundDelegator instance = null;
    
    private RestComponent component;
    
    private InboundDelegator(RestComponent component) {
        this.component = component;
    }
    
    public static synchronized InboundDelegator getInstance(RestComponent component) {
        if (instance == null) {
            instance = new InboundDelegator(component);
        }
        return instance;
    }
    
    public static synchronized InboundDelegator getInstance() {
        return instance;
    }
    
    
    public ResponseBuilder delegateRequest(
            String method, 
            UriInfo uriInfo, 
            HttpHeaders headers, 
            SecurityContext security, 
            InputStream payload) throws Exception {
        
        URI requestURI = uriInfo.getRequestUri();
        int port = requestURI.getPort();
        String listenerName = component.getInboundHttpListenerNameByPort(port);
        String path = requestURI.getPath();
        
        InboundConfiguration inboundConfig = 
            component.getRestServiceUnitManager().findInboundConfiguration(listenerName, headers, method, path);
        if (inboundConfig == null) {
            String msg = I18n.loc("RESTBC-7111: Request URL {0} does not match any ServiceUnit with listenerName={1}", requestURI.toString(), listenerName);//NOI18N
            logger.severe(msg);
            throw new Exception(msg);
        }
        
        QName serviceName = inboundConfig.getServiceName();
        String endpointName = inboundConfig.getEndpointName();
        
        ComponentContext componentContext = component.getComponentContext();
        ServiceEndpoint serviceEndpoint = componentContext.getEndpoint(serviceName, endpointName);
        if (serviceEndpoint == null) {
            throw new Exception();
        }
        
        MessageExchange msgEx = createMessageExchange(componentContext, inboundConfig.getRestOp().getMep());
        String exchangeId = msgEx.getExchangeId();
        
        QName bindingOpQName = new QName(serviceEndpoint.getServiceName().getNamespaceURI(),
                inboundConfig.getRestOp().getBindingOperation().getName());
        
        msgEx.setEndpoint(serviceEndpoint);
        msgEx.setOperation(bindingOpQName);
        
        NormalizedMessage requestMsg = msgEx.createMessage();
        
        RestOperation restOp = inboundConfig.getRestOp();
        Definition definition = inboundConfig.getDefinition();
        if (restOp != null && definition != null) {
            // has WSDL configuration
            
            Source requestContent = null;
            if (payload != null && headers.getMediaType() != null) {
                MediaType mediaType = headers.getMediaType();
                if (PathUtil.isXMLMediaType(mediaType) && !inboundConfig.isForwardAsAttachment())  {
                    // this is XML content and don't forward as attachment
                    requestContent = JbiMessageUtil.createJbiWrappedMsg(payload, restOp, definition, false);
                } else if (PathUtil.isJSONMediaType(mediaType) && !inboundConfig.isForwardAsAttachment()) {
                    // this is JSON content, convert to XML and don't forward as attachment
                    StringBuilder sb = new StringBuilder();
                    BufferedReader reader = new BufferedReader(new InputStreamReader(payload));
                    String currentString = reader.readLine();
                    while (currentString != null) {
                        sb.append(currentString);
                        currentString = reader.readLine();
                    }
                    com.sun.jbi.restbc.jbiadapter.org.json.JSONObject jsonObject = new com.sun.jbi.restbc.jbiadapter.org.json.JSONObject(sb.toString());
                    String xmlString = com.sun.jbi.restbc.jbiadapter.org.json.XML.toString(jsonObject);
                    requestContent = JbiMessageUtil.createJbiWrappedMsg(xmlString, restOp, definition, false);
                } else {
                    // treat as attachment
                    DataSource ds = new ByteArrayDataSource(payload, MediaTypeUtil.mediaTypeToString(mediaType));
                    DataHandler dataHandler = new DataHandler(ds);
                    String uuid = UUID.randomUUID().toString();
                    
                    requestContent = JbiMessageUtil.createJbiAttachmentWrapper(uuid, restOp, definition, false); 
                    requestMsg.addAttachment(uuid, dataHandler);
                }
                
            } else {
                requestContent = JbiMessageUtil.createEmptyJbiWrappedMsg(restOp, definition, false);
            }
            requestMsg.setContent(requestContent);
        } else {
            // no WSDL configuration
            Source requestContent = null;
            QName msgType = null;
            try {
                msgType = QName.valueOf(inboundConfig.getMsgType());
            } catch (IllegalArgumentException iae) {
                // ignore
            }
            if (payload != null && headers.getMediaType() != null) {
                MediaType mediaType = headers.getMediaType();
                if (PathUtil.isXMLMediaType(mediaType) && !inboundConfig.isForwardAsAttachment())  {
                    // this is XML content and don't forward as attachment
                    requestContent = JbiMessageUtil.createJbiWrappedMsg(msgType, payload);
                } else if (PathUtil.isJSONMediaType(mediaType) && !inboundConfig.isForwardAsAttachment()) {
                    // this is JSON content, convert to XML and don't forward as attachment
                    StringBuilder sb = new StringBuilder();
                    BufferedReader reader = new BufferedReader(new InputStreamReader(payload));
                    String currentString = reader.readLine();
                    while (currentString != null) {
                        sb.append(currentString);
                        currentString = reader.readLine();
                    }
                    com.sun.jbi.restbc.jbiadapter.org.json.JSONObject jsonObject = new com.sun.jbi.restbc.jbiadapter.org.json.JSONObject(sb.toString());
                    String xmlString = com.sun.jbi.restbc.jbiadapter.org.json.XML.toString(jsonObject);
                    requestContent = JbiMessageUtil.createJbiWrappedMsg(msgType, new ByteArrayInputStream(xmlString.getBytes()));
                } else {
                    // treat as attachment
                    DataSource ds = new ByteArrayDataSource(payload, MediaTypeUtil.mediaTypeToString(mediaType));
                    DataHandler dataHandler = new DataHandler(ds);
                    String uuid = UUID.randomUUID().toString();
                    
                    requestContent = JbiMessageUtil.createJbiAttachmentWrapper(msgType, uuid); 
                    requestMsg.addAttachment(uuid, dataHandler);
                }
                
            } else {
                requestContent = JbiMessageUtil.createJbiWrappedMsg(msgType, null);
            }
            requestMsg.setContent(requestContent);
        }
        
        
        // populate NM properties
        requestMsg.setProperty(NMProps.NM_URL_PROP, requestURI.toString());
        requestMsg.setProperty(NMProps.NM_METHOD_PROP, method);
        if (payload != null && headers.getMediaType() != null) {
            requestMsg.setProperty(NMProps.NM_CONTENT_TYPE_PROP, MediaTypeUtil.mediaTypeToString(headers.getMediaType()));
        }
        
        // querys
        Map<String, String> queryMap = NMPropertiesUtil.multivaluedMapToMap(uriInfo.getQueryParameters());
        requestMsg.setProperty(NMProps.NM_PARAMS_PROP, JsonUtil.buildJson(queryMap));
        NMPropertiesUtil.setDynamicNMProperties(requestMsg, NMProps.NM_PARAMS_PROP, queryMap);
        requestMsg.setProperty(NMProps.NM_PARAM_STYLE_PROP, "QUERY");
        
        // path parameters
        UriTemplate pathTemplate = inboundConfig.getPathTemplate();
        Map<String, String> pathParamMap = new HashMap<String, String> ();
        boolean matchedPath = pathTemplate.match(path, pathParamMap);
        if (!matchedPath) {
            // must match path when got here, since we already did successful match before here
            String msg = I18n.loc("RESTBC-7112: Unexcepted error: unable to match request path with a inbound configuration");//NOI18N
            logger.severe(msg);
            throw new Exception(msg);
        }
        requestMsg.setProperty(NMProps.NM_PATH_PARAMS_PROP, JsonUtil.buildJson(pathParamMap));
        NMPropertiesUtil.setDynamicNMProperties(requestMsg, NMProps.NM_PATH_PARAMS_PROP, pathParamMap);
        
        // headers
        Map<String, String> headerMap = NMPropertiesUtil.multivaluedMapToMap(headers.getRequestHeaders());
        requestMsg.setProperty(NMProps.NM_HEADERS_PROP, JsonUtil.buildJson(headerMap));
        NMPropertiesUtil.setDynamicNMProperties(requestMsg, NMProps.NM_HEADERS_PROP, headerMap);
        
        // accept-types
        List<String> acceptTypeList = new ArrayList<String> ();
        for (MediaType entry : headers.getAcceptableMediaTypes()) {
            acceptTypeList.add(MediaTypeUtil.mediaTypeToString(entry));
        }
        requestMsg.setProperty(NMProps.NM_ACCEPT_TYPES_PROP, JsonUtil.buildJson(acceptTypeList));
        
        // accept-languages
        List<String> acceptLanguageList = new ArrayList<String> ();
        for (Locale entry : headers.getAcceptableLanguages()) {
            acceptLanguageList.add(entry.toString());
        }
        requestMsg.setProperty(NMProps.NM_ACCEPT_LANGUAGES_PROP, JsonUtil.buildJson(acceptLanguageList));
        
        if (msgEx instanceof InOnly) {
            InOnly inonly = (InOnly) msgEx;
            inonly.setInMessage(requestMsg);
        } else if (msgEx instanceof InOut) {
            InOut inout = (InOut) msgEx;
            inout.setInMessage(requestMsg);
        }
        
        
        // invoke
        MessagingChannel channel = new BaseMessagingChannel(componentContext);
        msgEx.setProperty(ServiceQuality.MESSAGE_ID, msgEx.getExchangeId());
        boolean success = channel.sendSync(msgEx, inboundConfig.getTimeout());
        
        if (!success) {
            String msg = I18n.loc("RESTBC-7113: Inbound request timed-out after {0} ms", inboundConfig.getTimeout());//NOI18N
            logger.severe(msg);
            throw new Exception(msg);
        }
        
        if (msgEx instanceof InOnly) {
            InOnly inonly = (InOnly) msgEx;
            if (inonly.getStatus() == ExchangeStatus.DONE) {
                return null;
            } else if (inonly.getStatus() == ExchangeStatus.ERROR) {
                String msg = I18n.loc("RESTBC-7114: Error during inbound request {0}", inonly.getError());//NOI18N
                logger.severe(msg);
                throw new Exception(msg);
            } else {
                String msg = I18n.loc("RESTBC-7115: Incorrect return status for inbound request");//NOI18N
                logger.severe(msg);
                throw new Exception(msg);
            }
            
        } else if (msgEx instanceof InOut) {
            InOut inout = (InOut) msgEx;
            if (inout.getStatus() == ExchangeStatus.ACTIVE) {
                ResponseBuilder responseBuilder = Response.ok();
                
                NormalizedMessage replyMsg = inout.getOutMessage();
                Object responsePayload = JbiMessageUtil.getPayloadFromWrappedMsg(replyMsg);
                
                boolean isContentTypeSet = false;
                if (responsePayload != null) { // NOI18N
                    if (responsePayload instanceof Source) {
                        Source xmlPayload = (Source) responsePayload;
                        
                        List<MediaType> acceptableMediaTypes = headers.getAcceptableMediaTypes();
                        if (acceptableMediaTypes.size() == 0) {
                            // if accept header not present, return entity as XML
                            if (!method.equalsIgnoreCase("head")) {
                                responseBuilder.entity(JbiMessageUtil.convertXmlToString(xmlPayload));
                            } else {
                                responseBuilder.entity("");
                            }
                            responseBuilder.type(MediaType.APPLICATION_XML_TYPE);
                            isContentTypeSet = true;
                        } else {
                            for (MediaType acceptableMediaType : acceptableMediaTypes) {
                                if (PathUtil.isXMLMediaType(acceptableMediaType)) {
                                    if (!method.equalsIgnoreCase("head")) {
                                        responseBuilder.entity(JbiMessageUtil.convertXmlToString(xmlPayload));
                                    } else {
                                        responseBuilder.entity("");
                                    }
                                    String retMediaType = acceptableMediaType.isWildcardType() ? "application" : acceptableMediaType.getType();
                                    String retMediaSubType = acceptableMediaType.isWildcardSubtype() ? "xml" : acceptableMediaType.getSubtype();
                                    responseBuilder.type(new MediaType(retMediaType, retMediaSubType));
                                    isContentTypeSet = true;
                                    break;
                                } else if (PathUtil.isJSONMediaType(acceptableMediaType)) {
                                    if (!method.equalsIgnoreCase("head")) {
                                        String xmlPayloadAsString = JbiMessageUtil.convertXmlToString(xmlPayload);
                                        com.sun.jbi.restbc.jbiadapter.org.json.JSONObject jsonObject = 
                                            com.sun.jbi.restbc.jbiadapter.org.json.XML.toJSONObject(xmlPayloadAsString);
                                        if (jsonObject != null) {
                                            responseBuilder.entity(jsonObject.toString());
                                        } else {
                                            responseBuilder.entity(xmlPayloadAsString);
                                        }
                                    } else {
                                        responseBuilder.entity("");
                                    }
                                    String retMediaType = acceptableMediaType.isWildcardType() ? "application" : acceptableMediaType.getType();
                                    String retMediaSubType = acceptableMediaType.isWildcardSubtype() ? "json" : acceptableMediaType.getSubtype();
                                    responseBuilder.type(new MediaType(retMediaType, retMediaSubType));
                                    isContentTypeSet = true;
                                    break;
                                } else {
                                    if (!method.equalsIgnoreCase("head")) {
                                        responseBuilder.entity(JbiMessageUtil.convertXmlToString(xmlPayload));
                                    } else {
                                        responseBuilder.entity("");
                                    }
                                    break;
                                }
                            }
                        }
                    } else {
                        if (!method.equalsIgnoreCase("head")) {
                            DataHandler streamPayload = (DataHandler) responsePayload;
                            responseBuilder.entity(streamPayload.getInputStream());
                        } else {
                            responseBuilder.entity("");
                        }
                    }
                    
                    if (!isContentTypeSet) {
                        String contentType = PropertiesUtil.safeGetProperty(replyMsg, NMProps.NM_RESPONSE_CONTENT_TYPE_PROP);
                        if (contentType.length() > 0) {
                            responseBuilder.type(contentType);
                            isContentTypeSet = true;
                        } else {
                            List<MediaType> produceTypes = inboundConfig.getProduceMediaTypes();
                            if (produceTypes.size() > 0) {
                                responseBuilder.type(produceTypes.get(0));
                                isContentTypeSet = true;
                            }
                        }
                    }
                }
                
                Map<String, String> responseHeaderMap = NMPropertiesUtil.getDynamicNMProperties(replyMsg, NMProps.NM_RESPONSE_HEADERS_PROP);
                
                // set the headers on the response
                for (Map.Entry<String, String> header : responseHeaderMap.entrySet()) {
                    if (header.getKey().equalsIgnoreCase("Content-Type")) { // NOI18N
                        if (responsePayload == null || isContentTypeSet) {
                            // skip content-type
                            continue;
                        }
                    }
                    responseBuilder.header(header.getKey(), header.getValue());
                }
                
                // set the status if needed
                String statusStr = PropertiesUtil.safeGetProperty(replyMsg, NMProps.NM_RESPONSE_STATUS_PROP);
                if (statusStr.length() > 0) {
                    responseBuilder.status(Integer.parseInt(statusStr));
                }
                
                // set location if needed
                String locationStr = PropertiesUtil.safeGetProperty(replyMsg, NMProps.NM_RESPONSE_URL_PROP);
                if (locationStr.length() > 0) {
                    responseBuilder.contentLocation(new URI(locationStr));
                }
                
                inout.setStatus(ExchangeStatus.DONE);
                channel.send(inout);
                
                return responseBuilder;
            } else if (inout.getStatus() == ExchangeStatus.ERROR) {
                String msg = I18n.loc("RESTBC-7114: Error during inbound request {0}", inout.getError());//NOI18N
                logger.severe(msg);
                throw new Exception(msg);
            } else {
                String msg = I18n.loc("RESTBC-7115: Incorrect return status for inbound request");//NOI18N
                logger.severe(msg);
                throw new Exception(msg);
            }
        }
        
        return null;
    }
    
    private MessageExchange createMessageExchange(ComponentContext componentContext, OperationType mep) throws Exception {
        MessageExchangeFactory factory = componentContext.getDeliveryChannel().createExchangeFactory();
        if (OperationType.REQUEST_RESPONSE.equals(mep)) {
            return factory.createInOutExchange();
        } else if (OperationType.ONE_WAY.equals(mep)) {
            return factory.createInOnlyExchange();
        } else {
            String msg = I18n.loc("RESTBC-7116: Unsupported MEP type {0}", mep);//NOI18N
            logger.severe(msg);
            throw new Exception(msg);
        }
    }
    
}
