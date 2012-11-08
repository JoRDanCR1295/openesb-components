/*
 * Copyright (c) 2007 Sun Microsystems, Inc.
 * All Rights Reserved.
 *
 * This program, and all the routines referenced herein,
 * are the proprietary properties and trade secrets of
 * Sun Microsystems.
 *
 * Except as provided for by license agreement, this
 * program shall not be duplicated, used, or disclosed
 * without  written consent signed by an officer of
 * Sun Microsystems.
 */
package com.sun.caps.management.common;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.InetAddress;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.caps.management.common.jbi.JBIResultXmlBuilder;


/**
 * This object provides utility methods common for client or server side code for tools.
 * @author  graj
 */
public class Util {
    /**
     * resource bundle
     */
    private static I18NBundle I18NBUNDLE = null;
    
    /** gives the I18N bundle
     *@return I18NBundle object
     */
    public static I18NBundle getCommonI18NBundle() {
        // lazzy initialize the JBI Client
        if (I18NBUNDLE == null) {
            I18NBUNDLE = new I18NBundle("com.sun.caps.management.common");
        }
        return I18NBUNDLE;
    }
    
    /**
     * prints the string to debug output
     * @param msg text to print
     */
    public static void logDebug(String msg) {
    }
    
    /**
     * logs the message
     * @param logger logger
     * @param ex exception
     */
    public static void logDebug(Logger logger, Exception ex) {
        logger.log(Level.FINER, ex.getMessage(), ex);
    }
    
    /**
     * logs the message
     * @param ex exception
     */
    public static void logDebug(Exception ex) {
    }
    
    /**
     * This method creates a new object of specified type using the reflection apis. It
     * instantiate the object using its constructor that takes String as a argument.
     * This is mainly used to convert the string value to a specified object type.
     * @param type the fully qualified name of the desired class
     * @param arg String value to be passed to the class constructor
     * @throws java.lang.ClassNotFoundException on error
     * @throws java.lang.NoSuchMethodException on error
     * @throws java.lang.InstantiationException on error
     * @throws java.lang.IllegalAccessException on error
     * @throws java.lang.reflect.InvocationTargetException on error
     * @return constrcuted object of the type specified with the string value
     */
    @SuppressWarnings("unchecked")
    public static Object newInstance(String type, String arg)
    throws ClassNotFoundException, NoSuchMethodException,
    InstantiationException, IllegalAccessException,
    InvocationTargetException {
        Class objClass = null;
        Object instance = null;
        
        // check for the primitive types
        // boolean, byte, char, short, int, long, float, double and void
        if (type.equals("boolean")) {
            objClass = Boolean.class;
        } else if (type.equals("byte")) {
            objClass = Byte.class;
        } else if (type.equals("char")) {
            objClass = Character.class;
        } else if (type.equals("short")) {
            objClass = Short.class;
        } else if (type.equals("int")) {
            objClass = Integer.class;
        } else if (type.equals("long")) {
            objClass = Long.class;
        } else if (type.equals("float")) {
            objClass = Float.class;
        } else if (type.equals("double")) {
            objClass = Double.class;
        } else if (type.equals("void")) {
            objClass = Void.class;
        } else {
            objClass = Class.forName(type);
        }
        
        Class[] argClass = new Class[] { String.class };
        Constructor objConstructor = objClass.getConstructor(argClass);
        String[] argValue = { arg };
        instance = objConstructor.newInstance(argValue);
        return instance;
    }
    
    /**
     * This method checks that the host name is a localhost or a remote host. This method
     * compares the host name on which this method is getting executes with the host name
     * that is passed to this method. if the comparison matches it returns true.
     * @param host host name to check for local or remote host.
     * @throws java.net.UnknownHostException on errror.
     * @return true if the host name passed is a localhost else false.
     */
    public static boolean isLocalHost(String host)
    throws java.net.UnknownHostException {
        
        String localhostName = "localhost";
        String loopbackIP = "127.0.0.1";
        
        String remoteHost = null;
        
        if (host != null) {
            remoteHost = host.trim().toLowerCase();
        }
        
        logDebug("Checking for Remote HOST " + remoteHost);
        
        if (remoteHost == null || remoteHost.length() == 0
                || remoteHost.equalsIgnoreCase(localhostName)
                || remoteHost.equalsIgnoreCase(loopbackIP)) {
            return true;
        }
        
        InetAddress localInetAddr = InetAddress.getLocalHost();
        String localHostIP = localInetAddr.getHostAddress();
        
        // logDebug("Local Host IP " + localHostIP);
        
        InetAddress[] remoteInetAddrs = InetAddress.getAllByName(remoteHost);
        // logDebug("Remote InetAddress Size " + remoteInetAddrs.length);
        
        for (int i = 0; i < remoteInetAddrs.length; ++i) {
            String remoteHostIP = remoteInetAddrs[i].getHostAddress();
            // logDebug("Comapring with Remote Host IP : " + remoteHostIP);
            if (localHostIP.equalsIgnoreCase(remoteHostIP)) {
                return true;
            }
        }
        return false;
    }
    
    /**
     * This method will return the return the host address information for the given
     * host name in the format of <host name> / <host ip address>.
     * @param host the name of the host (ip address string or name of host).
     * @throws java.net.UnknownHostException on error.
     * @return the host name in the format of <host name> / <host ip address>
     */
    public static String getHostName(String aHostName)
    throws java.net.UnknownHostException {
        InetAddress ia = null;
        if ((aHostName.equalsIgnoreCase("localhost")) || (aHostName.equals(""))) {
            ia = InetAddress.getLocalHost();
        } else {
            ia = InetAddress.getByName(aHostName);
        }
        String hostName = ia.getHostName();
        String hostAddress = ia.getHostAddress();
        String returnStr = hostName + "/" + hostAddress;
        return returnStr;
    }

    /**
     * return the signatures of the params
     * @param params    the paramerters
     * @return signatures of the parameters
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
 
    
    /**
     * Creates a management message string and populates the exception
     * 
     * @param i18nBundle
     *            i18n bundle object
     * @param i18nKey
     *            key to look for i18n msg
     * @param args
     *            i18n args
     * @param taskId
     *            task id
     * @param sourceException -
     *            the source exception to propagate
     * @return Exception object created with a valid XML Management Message
     */
    public static Exception createManagementException(I18NBundle i18NBundle, String bundleKey, String[] args,
            Exception sourceException, String taskID) {
        Exception exception = null;
        I18NBundle theBundle = i18NBundle;
        if(null == i18NBundle) {
            theBundle = getCommonI18NBundle();
        } 
        
        String xmlManagementMessage = JBIResultXmlBuilder.createJbiResultXml(
                theBundle, bundleKey, args,taskID,JBIResultXmlBuilder.FAILED_RESULT,
                JBIResultXmlBuilder.ERROR_MSG_TYPE,  sourceException);
        exception = new Exception(xmlManagementMessage);
        return exception;
    }

    
}
