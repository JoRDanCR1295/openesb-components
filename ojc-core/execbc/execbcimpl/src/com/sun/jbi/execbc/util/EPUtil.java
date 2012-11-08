/*
 * EPUtil.java
 * 
 * Created on May 9, 2007, 6:09:31 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.execbc.util;

import com.sun.jbi.execbc.Endpoint;
import com.sun.jbi.execbc.extensions.ExecAddress;

import java.io.File;

/**
 *
 * @author jfu
 */
public class EPUtil {
//Not needed for Exec BC
//    public static String getFileDirectory(Endpoint aEndpoint) {
//        String fileDir;
//        
//        ExecAddress fileAddr = aEndpoint.getExecAddress();
//        if (!fileAddr.getRelativePath()) {
//            fileDir = fileAddr.getFileDirectory();
//        } else {
//            fileDir = System.getProperty(fileAddr.getPathRelativeTo()) + File.separator + fileAddr.getFileDirectory();
//        }
//        
//        return fileDir;
//    }
}
