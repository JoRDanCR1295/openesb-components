package net.openesb.jbi.restbc.jbiadapter.util;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.jbi.messaging.NormalizedMessage;
import javax.ws.rs.core.MultivaluedMap;

/**
 * NMPropertiesUtil.java
 *
 * @author Edward Chou
 */
public class NMPropertiesUtil {


    /*
     * convert javax.ws.rs.core.MultivaluedMap to java.util.Map
     */
    public static Map<String, String> multivaluedMapToMap(MultivaluedMap<String, String> multivaluedMap) {
        Map<String, String> map = new HashMap<String, String> ();
        for (Entry<String, List<String>> entry : multivaluedMap.entrySet()) {
            String key = entry.getKey();
            String value = "";
            for (String valuePart : entry.getValue()) {
                value = (value.length() == 0) ? value : ", " + value;
                value = value + valuePart;
            }
            map.put(key, value);
        }
        return map;
    }
    
    /*
     * set dynamic NMProps to a NormalizedMessage, using given prefix
     */
    public static void setDynamicNMProperties(NormalizedMessage msg, String prefix, Map<String, String> props) {
        for (Map.Entry<String, String> entry : props.entrySet()) {
            msg.setProperty(prefix + "." + entry.getKey(), entry.getValue());
        }
    }
    
    /*
     * 
     */
    public static Map<String, String> getDynamicNMProperties(NormalizedMessage msg, String prefix) {
        Map<String, String> props = new HashMap<String, String> ();
        
        // get all props with given prefix
        for (Object obj : msg.getPropertyNames()) {
            String s = (String) obj;
            if (!s.startsWith(prefix + ".")) {
                continue;
            }
            
            String key = s.substring((prefix+".").length());
            String val = PropertiesUtil.safeGetProperty(msg, s);
            if (val.length() > 0) {
                props.put(key, val);
            }
        }
        
        // get JSON formatted map of properties
        String jsonStr = PropertiesUtil.safeGetProperty(msg, prefix);
        Map<String, String> jsonMap = null;
        if (jsonStr.length() > 0) {
            jsonMap = JsonUtil.parseJsonPairs(jsonStr);
        }
        if (jsonMap == null) {
            return props;
        }
        
        // merge props
        for (Map.Entry<String, String> entry : jsonMap.entrySet()) {
            String key = entry.getKey();
            if (!props.containsKey(key)) {
                props.put(key, entry.getValue());
            }
        }
        
        return props;
    }
    
}
