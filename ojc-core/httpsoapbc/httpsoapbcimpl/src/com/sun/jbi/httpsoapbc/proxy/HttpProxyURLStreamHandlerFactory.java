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
 * @(#)HttpProxyURLStreamHandlerFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.proxy;

import com.sun.jbi.internationalization.Messages;

import java.net.Proxy;
import java.net.URL;
import java.net.URLStreamHandler;
import java.net.URLStreamHandlerFactory;
import java.util.logging.Level;
import java.util.logging.Logger;

/*
 * It applies to per-connection-Proxy to http URL connection.
 * It is better to use HttpProxySelector (instead of this factory class)
 * if possible.
 *
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */
public class HttpProxyURLStreamHandlerFactory implements URLStreamHandlerFactory  {
    private static final Messages mMessages =
        Messages.getMessages(HttpProxyURLStreamHandlerFactory.class);
    private static final Logger mLog =
        Messages.getLogger(HttpProxyURLStreamHandlerFactory.class);

    private HttpProxy hp;
    private static boolean registerAlready = false;
    private static boolean registerSucceed = false;
    
    private static String propertyHttpProxyHost = null;
    private static String propertyHttpProxyPort = null;
    private static String propertyHttpNonProxyHosts = null;
    private static String propertySocksProxyHost = null;
    private static String propertySocksProxyPort = null;
    private static String propertySocksNonProxyHosts = null;
    
    /** Creates a new instance of HttpProxyURLStreamHandlerFactory */
    public HttpProxyURLStreamHandlerFactory(HttpProxy hp) {
        this.hp = hp;
    }

    public synchronized void register() {
        if (registerAlready) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "HttpProxyURLStreamHandlerFactory already registered. Register success flag: " + registerSucceed);
            }
            return;
        }
        
        try {
            if (!this.hp.isUseJVMProxySettings()) {
                URL.setURLStreamHandlerFactory(this);
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "HttpProxyURLStreamHandlerFactory registered");
                }
            }
            
            registerSucceed = true;
        } catch (Error e) {
            // factory is set by other components already, for example, glassfish startup may set 
            // HttpsURLStreamHandlerFactory, or DirContextURLStreamHandlerFactory.
            // It is also possible that it is set by this class' previous loading.
            //
            // Then if possible, use URL constructors with URLStreamHandler or URL menthod openConnection(Proxy);
            // otherwise, use system properties to hint JVM
            // So set system properties for backward safety
            if (mLog.isLoggable(Level.WARNING)) {
                mLog.log(Level.WARNING, "HTTPBC-W01105.URLStreamHandlerFactory_already_set");
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "HTTPBC-W01105.URLStreamHandlerFactory_already_set", e);
                }
            }
            this.setProperties();
            
        } catch (Throwable t) {
            // Security error
            // So set system properties for backward safety
            if (mLog.isLoggable(Level.WARNING)) {
                mLog.log(Level.WARNING, "HTTPBC-W01106.URLStreamHandlerFactory_registration_security_error");
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "HTTPBC-W01106.URLStreamHandlerFactory_registration_security_error", t);
                }
            }
            this.setProperties();
            
        }
        
        registerAlready = true;
    }

    public static boolean isRegisterSucceed() {
        return registerSucceed;
    }
    
    public static void unregister() {
        if (!registerSucceed) {
            // restore the old system properties
            if (null != propertyHttpProxyHost) {
                System.setProperty("http.proxyHost", propertyHttpProxyHost);
            }
            
            if (null != propertyHttpProxyPort) {
                System.setProperty("http.proxyPort", propertyHttpProxyPort);
            }
            
            if (null != propertyHttpNonProxyHosts) {
                System.setProperty("http.nonProxyHosts", propertyHttpNonProxyHosts);
            }
            
            if (null != propertySocksProxyHost) {
                System.setProperty("socksProxyHost", propertySocksProxyHost);
            }
            
            if (null != propertySocksProxyPort) {
                System.setProperty("socksProxyPort", propertySocksProxyPort);
            }
            
            if (null != propertySocksNonProxyHosts) {
                System.setProperty("socksNonProxyHosts", propertySocksNonProxyHosts);
            }
            
        }
        
        registerAlready = false;
        registerSucceed = false;
        
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "HttpProxyURLStreamHandlerFactory unregistered");
        }
    }
    
    private void setProperties() {
        if (Proxy.Type.HTTP.equals(this.hp.getProxyType())) {
            propertyHttpProxyHost = System.getProperty("http.proxyHost", "");
            propertyHttpProxyPort = System.getProperty("http.proxyPort", "");
            propertyHttpNonProxyHosts = System.getProperty("http.nonProxyHosts", "");
            
            System.setProperty("http.proxyHost", this.hp.getProxyHost());
            System.setProperty("http.proxyPort", "" + this.hp.getProxyPort());
            System.setProperty("http.nonProxyHosts", this.hp.getNonProxyHosts());
            
        } else if (Proxy.Type.SOCKS.equals(this.hp.getProxyType())) {
            
            propertySocksProxyHost = System.getProperty("socksProxyHost", "");
            propertySocksProxyPort = System.getProperty("socksProxyPort", "");
            propertySocksNonProxyHosts = System.getProperty("socksNonProxyHosts", "");
            
            System.setProperty("socksProxyHost", this.hp.getProxyHost());
            System.setProperty("socksProxyPort", "" + this.hp.getProxyPort());
            System.setProperty("socksNonProxyHosts", this.hp.getNonProxyHosts());
        } else {
            
        }
    }
    
    /**
     * Creates a new <code>URLStreamHandler</code> instance with the specified
     * protocol.
     * 
     * 
     * @param protocol   the protocol ("<code>ftp</code>",
     *                     "<code>http</code>", "<code>nntp</code>", etc.).
     * @return a <code>URLStreamHandler</code> for the specific protocol.
     * @see java.net.URLStreamHandler
     */
    public URLStreamHandler createURLStreamHandler(String protocol) {
        if ("http".equalsIgnoreCase(protocol) &&
            registerSucceed) {
            return new HttpProxyURLStreamHandler(this.hp);
        } else {
            return null;
        }
    }
    
}
