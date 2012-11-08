package com.sun.jbi.restbc.jbiadapter.util;

import java.util.Map;
import java.util.Properties;

import javax.jbi.messaging.NormalizedMessage;

/**
 * PropertiesUtil.java
 *
 * @author Edward Chou
 */
public class PropertiesUtil {

    /**
     * Always returns the value as a trimmed String, never returns null.
     * 
     * @param p
     * @param name
     * @return
     */
    public static String safeGetProperty(Properties p, String name) {
        return p.getProperty(name, "").trim();
    }
    
    public static String safeGetProperty(Properties p, String name, String defaultValue) {
        String val = p.getProperty(name, "").trim();
        if (val.length() == 0) {
            return defaultValue;
        }
        return val;
    }

    public static String safeGetProperty(NormalizedMessage msg, String name) {
        Object o = msg.getProperty(name);
        return (o == null) ? "" : o.toString().trim();
    }
    
    
    public static String applyApplicationVariables(String s, Map appVariables) {
        String resultStr = s;
        Map<String, String[]> map = (Map<String, String[]>) appVariables;
        for (Map.Entry<String, String[]> entry : map.entrySet()) {
            String key = entry.getKey();
            String val = entry.getValue()[0];
            resultStr = resultStr.replaceAll("\\$\\{" + key + "\\}", val);
        }
        
        return resultStr;
    }
    
}
