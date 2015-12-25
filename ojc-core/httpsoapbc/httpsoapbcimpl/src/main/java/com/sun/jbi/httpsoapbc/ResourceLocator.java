/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)ResourceLocator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.UnknownHostException;
import java.net.URI;
import java.net.URL;

/**
 *
 *
 * @version      
 *
 */
public class ResourceLocator {

    private URL mLocation;

    public ResourceLocator(int port, String context) throws MalformedURLException {
        String protocol = "http";
        String host = "localhost";
        try {
            host = InetAddress.getLocalHost().getCanonicalHostName();
        } catch (UnknownHostException uhe) {
            // Ignore on purpose.  Host already initialized if this error happens.
        }
        // Clean up the context of extra slashes
        while (context.indexOf("//") != -1) {
            context = context.replace("//", "/");
        }
        mLocation = new URL(protocol, host, port, context);
    }
    
    public ResourceLocator(String protocol, int port, String context) throws MalformedURLException {
        String host = "localhost";
        try {
            host = InetAddress.getLocalHost().getCanonicalHostName();
        } catch (UnknownHostException uhe) {
            // Ignore on purpose.  Host already initialized if this error happens.
        }
        // Clean up the context of extra slashes
        while (context.indexOf("//") != -1) {
            context = context.replace("//", "/");
        }
        mLocation = new URL(protocol, host, port, context);
    }

    public ResourceLocator(int port,
                           String serviceUnitId,
                           String resourceLocation) throws MalformedURLException {
        String protocol = "http";
        String host = "localhost";
        try {
            host = InetAddress.getLocalHost().getCanonicalHostName();
        } catch (UnknownHostException uhe) {
            // Ignore on purpose.  Host already initialized if this error happens.
        }
        mLocation = new URL(protocol, host, port, "/" + serviceUnitId + "/" + resourceLocation);
    }
    
    public ResourceLocator(String protocol,
                           int port,
                           String serviceUnitId,
                           String resourceLocation) throws MalformedURLException {
        String host = "localhost";
        try {
            host = InetAddress.getLocalHost().getCanonicalHostName();
        } catch (UnknownHostException uhe) {
            // Ignore on purpose.  Host already initialized if this error happens.
        }
        mLocation = new URL(protocol, host, port, "/" + serviceUnitId + "/" + resourceLocation);
    }

    public URL toURL() {
        return mLocation;
    }

    public String toString() {
    	String location = null;
    	try {
            location =  mLocation.toURI().normalize().toString();
        } catch (Exception e) {
            // oh well, just return what we have then...
            location = mLocation.toString();
        }
        
        return location;
    }

    public String getServiceUnitId() {
        String path = mLocation.getFile();
        // The first character is / so ignore it.
        try {
            path = path.substring(1, path.length());
            return path.substring(0, path.indexOf("/"));
        } catch (Exception ex) {
            return null;
        }
    }

    public String getResourceLocation() {
        String path = mLocation.getFile();
        // The first character is / so ignore it.
        try {
            path = path.substring(1, path.length());        
            
            // get only the relative path
            String relativePath = path.substring(path.indexOf("/") + 1,
                                  path.length());
            
            return new URI(relativePath).normalize().toString();
        } catch (Exception ex) {
            return null;
        }
    }
}
