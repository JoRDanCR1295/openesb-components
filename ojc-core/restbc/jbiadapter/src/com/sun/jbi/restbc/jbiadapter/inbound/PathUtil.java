package com.sun.jbi.restbc.jbiadapter.inbound;

/**
 * PathUtil.java
 *
 * @author Edward Chou
 */
public class PathUtil {

    
    public static String normalizePath(String path) {
        // compact all slashes
        String s  = path.replaceAll("/++", "/");
        
        // remove starting and ending slashes
        if (s.startsWith("/")) {
            s = s.substring(1);
        }
        if (s.endsWith("/")) {
            s = s.substring(0, s.length() - 1);
        }
        
        return s.toLowerCase();
    }
}
