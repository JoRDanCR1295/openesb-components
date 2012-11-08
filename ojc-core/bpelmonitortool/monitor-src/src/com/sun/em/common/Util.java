/*
 *
 * Copyright 2007 Sun Microsystems, Inc. All rights reserved.
 */

package com.sun.em.common;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author ylee
 */
public class Util {
    
    public final static String SYSTEM_TYPE = "SYSTEM_TYPE";
    public final static String SERVERS = "SERVERS";
    public final static String HOSTANDPORT = "HOSTANDPORT";
    public final static String PROJECT = "PROJECT";
    public final static String DEPLOYMENT = "DEPLOYMENT";
    public final static String CMAP = "CMAP";
    public final static String COMPONENT = "COMPONENT";
    public final static String HOSTNAME = "HOSTNAME";
    public final static String PORT = "PORT";
    public final static String LINK = "LINK";
    
    
    /** Creates a new instance of Util */
    public Util() {
    }
    
    
    public static String replaceInvalidChars(String value) {
        return value.replaceAll("\\.","_");
    }
    
    public static String fixupName(String name, String prefix) {
        if ( name.startsWith(prefix) ) {
            return name.substring(prefix.length()+1);
        } else {
            return name;
        }
    }
    
    
    
    public static String convertType(Object type) {
        String ctype = "";
        if ( type instanceof Boolean ) {
            ctype = type.toString();
        } else {
            ctype = type + "";
        }
        return ctype;
    }
    
    
    
    public static String trimRight(String str, String token) {
        String result="";
        if (str!=null ) {
            result = str.substring(0,str.lastIndexOf(token));
        }
        return result;
    }
    
    
    public static String trimLeft(String str,String token) {
        String result=str;
        if ( str!=null ) {
            int index = str.indexOf(token);
            //result = index>=0 ? str.substring(index+token.length()) : str;
            if ( index>=0 ) {
                result = str.substring(index+token.length());
            }
        }
        return result;
    }
    
    
    public static String getNamespace(String endpoint,String token) {
        String result="";
        if ( endpoint!=null ) {
            result = endpoint.substring(0,endpoint.indexOf(token));
        }
        return result;
    }
    
    /**
     *  strip off the path from file name
     *  @param fullPathName
     *  @return stripped path
     */
    public static String stripPath(String fullPathName) {
        String name = fullPathName;
        if ( fullPathName!=null ) {
            File f = new File(fullPathName);
            name = f.getName();
        }
        return name;
    }
    
    /**
     * get parameter signatures based on the object type class
     *
     * @param params
     * @return
     */
    public static String[] getParamSigs(Object[] params) {
        if (params == null || params.length == 0) {
            return null;
        }
        String[] sigs = new String[params.length];
        for (int i = 0; i < params.length; i++) {
            if (params[i] == null) {
                sigs[i] = "java.lang.Object";
            } else {
                sigs[i] = params[i].getClass().getName();
            }
        }
        return sigs;
    }
    
    public static void splitHostAndPort(String host, StringBuffer hostname,
            StringBuffer port) {
        // host into hostname and port
        String[] args = host.split(":");
        if (args != null) {
            switch (args.length) {
            case 2:
                port.append(args[1]);
            case 1:
                hostname.append(args[0]);
                break;
            default:
                hostname.append(host);
                break;
            }
        }
    }
    
    
    /**
     * split component path into its components
     *
     * @todo - does not account for sub-projects
     * @param path -
     *            e.g
     *            e51x|Servers|hostname:port|Project1|Deployment1|CMap1|Service
     *            e51x|Servers|hostname:port|Project1|Deployment1|CMap1|File1|File_Service1
     *            x -
     *            e51x|Servers|hostname:port|Project1|Project2|Deployment1|CMap1|Service
     * @return
     */
    public static Map parseComponentPath(String path) {
        Map map = new HashMap();
        String[] args = path.split("\\|");
        if (args != null) {
            switch (args.length) {
            default:
            case 8:
                map.put(LINK, args[7]);
            case 7:
                map.put(COMPONENT, args[6]);
            case 6:
                map.put(CMAP, args[5]);
            case 5:
                map.put(DEPLOYMENT, args[4]);
            case 4:
                map.put(PROJECT, args[3]);
            case 3:
                map.put(HOSTANDPORT, args[2]);
                StringBuffer hostname = new StringBuffer();
                StringBuffer port = new StringBuffer();
                splitHostAndPort(args[2], hostname, port);
                map.put(HOSTNAME, hostname.toString());
                map.put(PORT, port.toString());
            case 2:
                map.put(SERVERS, args[1]);
            case 1:
                map.put(SYSTEM_TYPE, args[0]);
            case 0:
            }
        }
        return map;
    }
    
    
    /**
     * get project path from fully qualified component path
     *
     * @warn - doest not work with cmlinks
     *
     * @param path
     *            e.g e51x|Servers| <hostname>:
     *            <port>|Project|Deployment|CMap|Service e51x|Servers|
     *            <hostname>: <port>|Project1|Project2|Deployment|CMap|Service
     * @return
     */
    public static String getMetaDataProjectPath(String path) {
        StringBuffer projectPath = new StringBuffer();
        String[] args = path.split("\\|");
        if (args != null) {
            for (int i = 3; i < args.length - 3; i++) {
                projectPath.append(args[i] + "|");
            }
            // append the last component
            projectPath.append(args[args.length - 1]);
        } else {
            System.out.println("path:" + path + " is invalid.");
        }
        return projectPath.toString();
    }
    
    public static String getProjectPath(String path) {
        StringBuffer projectPath = new StringBuffer();
        String[] args = path.split("\\|");
        String separator = "";
        if (args != null) {
            for (int i = 3; i < args.length - 3; i++) {
                projectPath.append(separator + args[i]);
                if ("".equals(separator)) {
                    separator = "|";
                }
            }
        } else {
            System.out.println("path:" + path + " is invalid.");
        }
        return projectPath.toString();
    }
    
    public static String getProjectPathFromCmapPath(String path) {
        StringBuffer projectPath = new StringBuffer();
        String[] args = path.split("\\|");
        String separator = "";
        if (args != null) {
            for (int i = 3; i < args.length - 2; i++) {
                projectPath.append(separator + args[i]);
                if ("".equals(separator)) {
                    separator = "|";
                }
            }
        } else {
            System.out.println("path:" + path + " is invalid.");
        }
        return projectPath.toString();
    }
    
    public static String getProjectName(String path) {
        String projectPath = getProjectPath(path);
        return projectPath;
    }
    
    /**
     * get project path from fully qualified component path
     *
     * @warn - doest not work with cmlinks
     *
     * @param path
     *            e.g e51x|Servers| <hostname>:
     *            <port>|Project|Deployment|CMap|Service e51x|Servers|
     *            <hostname>: <port>|Project1|Project2|Deployment|CMap|Service
     */
    public static String getDeploymentName(String path) {
        String deploymentName = null;
        String[] args = path.split("\\|");
        if (args != null) {
            if ( args.length>3 ) {
                deploymentName = args[args.length - 3];
            }
        } else {
            System.out.println("path:" + path + " is invalid.");
        }
        return deploymentName;
    }
    
    public static boolean pathIsIS(String path) {
        if (path == null) {
            return false;
        }
        return path.split("\\|").length == 3;
    }
    
    /**
     * get the linkName from the fully qualified component path
     *
     * @param path
     *            e.g.
     *            e51x|Servers|ylee-d600xp.stc.com:18000|Project2|Deployment1|CMap1|File1|File1_Service1
     * @return
     */
    public static String getLinkName(String path) {
        int idx = path.lastIndexOf(GenericConstants.COMPONENT_SEPARATOR);
        String linkName = path.substring(idx + 1);
        return linkName;
    }
    
    /**
     * get metadata path from fully qualified component path. this path is used
     * as the key into the metadata map
     *
     * @param path
     *            e.g e51x|Servers| <hostname>:
     *            <port>|Project|Deployment|CMap|Service
     */
    public static String getMetadataPath(String path) {
        String metadataPath = "";
        Map map = parseComponentPath(path);
        metadataPath = map.get(SYSTEM_TYPE) + "|" + map.get(SERVERS) + "|"
                + map.get(HOSTANDPORT) + "|" + getProjectPath(path) + "|"
                + getDeploymentName(path);
        return metadataPath;
    }
    
    public static String getHostAndPort(String path) {
        Map map = parseComponentPath(path);
        return (String) map.get(HOSTANDPORT);
    }
    
    /**
     * extract path from fully qualified component path
     *
     * @param path
     *            e.g e51x|Servers| <hostname>:
     *            <port>|Project|Deployment|CMap|Service e51x|Servers|
     *            <hostname>: <port>|Project1|Project2|Deployment|CMap|Service
     * @return
     */
    public static String extractPath(String path) {
        int index = path.lastIndexOf("|");
        return path.substring(0, index);
    }
    
    /**
     * extract component from fully qualified component path
     *
     * @param path
     *            e.g e51x|Servers| <hostname>:
     *            <port>|Project|Deployment|CMap|Service e51x|Servers|
     *            <hostname>: <port>|Project1|Project2|Deployment|CMap|Service
     * @return
     */
    public static String extractComponent(String path) {
        if (path != null) {
            int index = path.lastIndexOf("|");
            return path.substring(index + 1);
        } else {
            return path;
        }
    }
    
}
