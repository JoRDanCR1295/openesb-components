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
 * @(#)HttpProxySelector.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.proxy;

import com.sun.jbi.internationalization.Messages;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.net.Proxy;
import java.net.ProxySelector;
import java.net.SocketAddress;
import java.net.URI;

/*
 * It is the ProxySelector used by HttpSoapBC.
 *
 * 
 * 
 * 
 *
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */

public class HttpProxySelector extends ProxySelector {
    private static final Messages mMessages =
        Messages.getMessages(HttpProxySelector.class);
    private static final Logger mLog =
        Messages.getLogger(HttpProxySelector.class);

    private HttpProxy hp;
    private static ProxySelector oldDefault = ProxySelector.getDefault();
    private static boolean registerAlready = false;
    
    /** Creates a new instance of HttpProxySelector */
    public HttpProxySelector(HttpProxy hp) {
        this.hp = hp;
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "The current ProxySelector is [" + oldDefault + "]");
        }
        
    }

    public void register() {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "Registering HttpProxySelector...");
        }
        
        if (registerAlready) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "HttpProxySelector registered already: " + oldDefault);
            }
            return;
        }
        
        try {
            if (!this.hp.isUseJVMProxySettings()) {
                ProxySelector.setDefault(this);
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log(Level.FINE, "HttpProxySelector registered");
                }
            }
        } catch (Throwable t) {
            // Security error
            if (mLog.isLoggable(Level.WARNING)) {
                mLog.log(Level.WARNING, "HTTPBC-W01120.Self_registration_failed", t);
            }
        }
        
        registerAlready = true;
    }
    
    public static void unregister() {
        ProxySelector.setDefault(oldDefault);
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "Unregistering HttpProxySelector...");
        }
        registerAlready = false;
    }
    
    /**
     * Selects all the applicable proxies based on the protocol to
     * access the resource with and a destination address to access
     * the resource at.
     * The format of the URI is defined as follow:
     * <UL>
     * <LI>http URI for http connections</LI>
     * <LI>https URI for https connections
     * <LI>ftp URI for ftp connections</LI>
     * <LI><code>socket://host:port</code><br>
     *     for tcp client sockets connections</LI>
     * </UL>
     * 
     * 
     * @param uri
     * 		The URI that a connection is required to
     * @return a List of Proxies. Each element in the
     * 		the List is of type 
     *          {@link java.net.Proxy Proxy};
     *          when no proxy is available, the list will
     *          contain one element of type
     *          {@link java.net.Proxy Proxy}
     *          that represents a direct connection.
     * @throws IllegalArgumentException if either argument is null
     */
    public List<Proxy> select(URI uri) {
        if (uri == null) {
            throw new IllegalArgumentException(mMessages.getString("HTTPBC-E01103.Null_uri"));
        }
        
        String host = uri.getHost();
        if (null == host) {
            // delegate to default for special handling
            return oldDefault.select(uri);
        }
        
	String protocol = uri.getScheme();
        
        if ("socket".equalsIgnoreCase(protocol) &&
            Proxy.Type.HTTP.equals(this.hp.getProxyType())) {
            // socket:// only supports DIRECT and SOCKS type, not support HTTP proxy, 
            // so just delegate to default
            return oldDefault.select(uri);
        }
        
        if (!"http".equalsIgnoreCase(protocol) &&
            !"https".equalsIgnoreCase(protocol) &&
            !"ftp".equalsIgnoreCase(protocol) &&
            !"gopher".equalsIgnoreCase(protocol) &&
            !"socket".equalsIgnoreCase(protocol)) {
            // a new protocol? delegate to default
            return oldDefault.select(uri);
        }
        
        ArrayList<Proxy> list = new ArrayList<Proxy>(1);
        String nonProxyHosts = this.hp.getNonProxyHosts();
        if (null == nonProxyHosts || 0 == nonProxyHosts.length()) {
            // overwrite the default behavior here: don't alwasys bypass localhost/loopback
            list.add(this.hp.getProxy());
            return list;
        }

        nonProxyHosts = nonProxyHosts.replaceAll(" ", "").toLowerCase();
        host = host.toLowerCase();
        if (nonProxyHosts.startsWith(host + "|") ||
            nonProxyHosts.endsWith("|" + host) ||
            nonProxyHosts.contains("|" + host + "|")) {
            list.add(Proxy.NO_PROXY);
        } else {
            list.add(this.hp.getProxy());
        }
        return list;
    }

    /**
     * Called to indicate that a connection could not be established
     * to a proxy/socks server. An implementation of this method can
     * temporarily remove the proxies or reorder the sequence of
     * proxies returned by select(String, String), using the address
     * and they kind of IOException given.
     * 
     * 
     * @param uri
     *          The URI that the proxy at sa failed to serve.
     * @param sa
     * 		The socket address of the proxy/SOCKS server
     * @param ioe
     * 		The I/O exception thrown when the connect failed.
     * @throws IllegalArgumentException if either argument is null
     */
    public void connectFailed(URI uri, SocketAddress sa, IOException ioe) {
        if (uri == null || sa == null || ioe == null) {
            throw new IllegalArgumentException(mMessages.getString("HTTPBC-E01102.Null_arguments"));
        }
        
        if (this.hp.getProxy().address().equals(sa)) {
            if (mLog.isLoggable(Level.WARNING)) {
                String logMsg = mMessages.getString("HTTPBC-W01125.Proxy_connection_failed", new Object[] {this.hp.getProxy(), uri});
                if (mLog.isLoggable(Level.WARNING)) {
                    mLog.log(Level.WARNING, logMsg, ioe);
                } else {
                    mLog.log(Level.WARNING, logMsg);
                }
            }
            //??throw new RuntimeException("HttpProxySelector.connectFailed(): Failed to connect to proxy {" + this.hp.getProxy() + "} when serving URI {" + uri + "}. ", ioe);
        } else {
            oldDefault.connectFailed(uri, sa, ioe);
        }
    }
    
}
