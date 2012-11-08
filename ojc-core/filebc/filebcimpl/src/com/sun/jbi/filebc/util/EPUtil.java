/*
 * EPUtil.java
 * 
 * Created on May 9, 2007, 6:09:31 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.jbi.filebc.util;

import com.sun.jbi.filebc.Endpoint;
import com.sun.jbi.filebc.extensions.FileAddress;
import java.io.File;

import javax.xml.namespace.QName;

/**
 *
 * @author jfu
 */
public class EPUtil {

    public static final String FILEBINDINGNAME = "-sun-file-binding";

    private EPUtil() {}
    
    public static String getFileDirectory(Endpoint aEndpoint) {
        String fileDir;

        FileAddress fileAddr = aEndpoint.getFileAddress();
        if (fileAddr.getRelativePath() == null || !fileAddr.getRelativePath()) {
            fileDir = fileAddr.getFileDirectory();
        } else {
            // open esb issue: 1841, fileAddr.getPathRelativeTo() already resolved any well-known system dirs - User Home etc.
            fileDir = /*System.getProperty(*/ fileAddr.getPathRelativeTo()/*)*/ + File.separator + fileAddr.getFileDirectory();
        }

        return fileDir;
    }

    public static String getPersistenceBaseDirectory(Endpoint aEndpoint) {
        String persistBaseDir = aEndpoint.getFileAddress().getPersistenceBaseLoc();
        return persistBaseDir;
    }

    public static String getWorkAreaBaseDir(Endpoint endpoint) {
        String workDir = endpoint.getServiceUnitID();
        int pos = workDir.lastIndexOf(FILEBINDINGNAME);
        if (pos > -1) {
            workDir = workDir.substring(0, pos);
        }
        int unique = endpoint.getServiceName().hashCode() + endpoint.getEndpointName().hashCode();

        workDir = toFilepath(workDir + "_" +
                endpoint.getServiceName().getLocalPart() +
                "_" + endpoint.getEndpointName().trim());
        if (workDir.length() > 50) {
            workDir = workDir.substring(0, 49) + unique;
        } else {
            workDir = workDir + "_" + unique;
        }
        return workDir;

    }

    /*
     * / \ | ? *
     * Characters whose integer representations are in the range from zero through 31 are not allowed.
     */
    private static String toFilepath(String path) {
        int length = path.length();
        StringBuffer buf = new StringBuffer();
        char c;
        for (int i = 0; i < length; i++) {
            c = path.charAt(i);
            if (c == '/' || c == '\\' || c == '|' || c == '?' || c == '*' || c == '.' || c == ':') {
                c = '_';
            }
            buf.append(c);
        }
        return buf.toString();
    }

    public static String getWorkAreaBaseDir(QName serviceQName, String endpointName) {
        String ns = nsToFilePath(serviceQName.getNamespaceURI());
        String serviceName = serviceQName.getLocalPart();
        String epName = endpointName;

        String epDirPath = ns.length() > 0 ? ns + File.separator + serviceName + File.separator + epName
                : serviceName + File.separator + epName;
        return "sun-file-binding" + File.separator + epDirPath;

    }

    /*
     *   
     * Convert Namespace string to a valid file path 
     */
    private static String nsToFilePath(String ns) {
        if (ns == null || (ns.trim().length() == 0)) {
            return "";
        }

        ns = ns.replace("://", "-");
        ns = ns.replace("/", "-");
        ns = ns.replace(".", "-");

        return ns;
    }
}
