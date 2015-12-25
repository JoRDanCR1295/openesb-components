package net.openesb.jbi.restbc.jbiadapter.util;

import java.util.HashMap;
import java.util.List;

import javax.ws.rs.core.MediaType;

import net.openesb.jbi.restbc.jbiadapter.InboundConfiguration;

import org.glassfish.jersey.uri.UriTemplate;


/**
 * PathUtil.java
 *
 * @author Edward Chou
 */
public class PathUtil {

    /**
     * matches a given InboundConfiguration against method, request path, produce types, and consume types.
     * 
     * @param inboundConfig
     * @param headers
     * @param method
     * @param path
     * @return true if match is successful, false otherwise.
     */
    public static boolean matchInboundConfiguration(
            InboundConfiguration inboundConfig, 
            MediaType contentType,
            List<MediaType> acceptMediaTypes,
            String method, 
            String path) {
        
        // match method
        if (!inboundConfig.getMethod().equalsIgnoreCase(method)) {
            return false;
        }
        
        // match path
        UriTemplate pathTemplate = inboundConfig.getPathTemplate();
        if (!pathTemplate.match(path, new HashMap<String, String> ())) {
            return false;
        }
        
        // match consume types
        if (contentType != null) {
            boolean matchConsumeType = false;
            for (MediaType consumeType : inboundConfig.getConsumeMediaTypes()) {
                if (consumeType.isCompatible(contentType)) {
                    matchConsumeType = true;
                    break;
                }
            }
            if (!matchConsumeType) {
                return false;
            }
        }
        
        // match produce types
        boolean matchProduceType = false;
        if (acceptMediaTypes.size() == 0) {
            matchProduceType = true;
        }
        for (MediaType acceptMediaType : acceptMediaTypes) {
            for (MediaType produceType : inboundConfig.getProduceMediaTypes()) {
                if (produceType.isCompatible(acceptMediaType)) {
                    matchProduceType = true;
                    break;
                }
            }
        }
        if (!matchProduceType) {
            return false;
        }
        
        return true;
    }
    
    
    /**
     * check if given MediaType belongs to a XML family of media types, such as
     * application/xml, or application/yyy+xml, or xxx/yyy+xml
     * 
     * @param type
     * @return true if given MediaType belongs to XML family, false otherwise
     */
    public static boolean isXMLMediaType(MediaType type) {
        if (type.isWildcardSubtype()) {
            return true;
        }
        if (type.getSubtype().equalsIgnoreCase("xml")) {
            return true;
        }
        if (type.getSubtype().toLowerCase().endsWith("+xml")) {
            return true;
        }
        
        return false;
    }
    
    public static boolean isXMLMediaType(String type) {
        return isXMLMediaType(MediaType.valueOf(type));
    }
    
    /**
     * check if given MediaType belongs to a JSON family of media types, such as
     * application/json, or application/yyy+json, or xxx/yyy+json
     * 
     * @param type
     * @return true if given MediaType belongs to JSON family, false otherwise
     */
    public static boolean isJSONMediaType(MediaType type) {
        if (type.isWildcardSubtype()) {
            return true;
        }
        if (type.getSubtype().equalsIgnoreCase("json")) {
            return true;
        }
        if (type.getSubtype().toLowerCase().endsWith("+json")) {
            return true;
        }
        
        return false;
    }
    
    public static boolean isJSONMediaType(String type) {
        return isJSONMediaType(MediaType.valueOf(type));
    }
    
}

