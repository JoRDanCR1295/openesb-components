/*
 * EPUtil.java
 * 
 * Created on May 9, 2007, 6:09:31 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.jbi.ftpbc.util;

import com.sun.jbi.ftpbc.Endpoint;

import javax.xml.namespace.QName;

import java.io.File;
import java.util.UUID;

/**
 *
 * @author jfu
 */
public class EPUtil {

    public static final String FTPBINDINGNAME = "-sun-ftp-binding";

    private EPUtil() {}
    
    public static String getWorkAreaBaseDir(Endpoint endpoint) {
        String workDir = endpoint.getServiceUnitID();
        int pos = workDir.lastIndexOf(FTPBINDINGNAME);
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
    public static String toFilepath(String path) {
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

    public static String fromQName2UniqueString(QName qname) {
        String prefix = EPUtil.toFilepath(qname.getLocalPart());
        String uuid = UUID.nameUUIDFromBytes(qname.toString().getBytes()).toString();
        String result = null;
        if (prefix.length() > 12) {
            result = prefix.substring(0, 12).concat(uuid);
        } else {
            result = prefix.concat(uuid);
        }
        return result;
    }

    public static String getWorkAreaBaseDir(QName serviceQName, String endpointName) {
        String ns = nsToFilePath(serviceQName.getNamespaceURI());
        String serviceName = serviceQName.getLocalPart();
        String epName = endpointName;

        String epDirPath = ns.length() > 0 ? ns + File.separator + serviceName + File.separator + epName
                : serviceName + File.separator + epName;
        return FTPBINDINGNAME + File.separator + epDirPath;

    }

    /*
     *   
     * Convert Namespace string to a valid file path 
     */
    public static String nsToFilePath(String ns) {
        if (ns == null || (ns.trim().length() == 0)) {
            return "";
        }

        ns = ns.replace("://", "-");
        ns = ns.replace("/", "-");
        ns = ns.replace(".", "-");

        return ns;
    }
}
