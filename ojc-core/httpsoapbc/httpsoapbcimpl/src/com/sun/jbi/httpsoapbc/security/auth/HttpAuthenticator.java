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
 * @(#)HttpAuthenticator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.security.auth;


import com.sun.jbi.httpsoapbc.proxy.HttpProxy;
import com.sun.jbi.httpsoapbc.security.api.HTTPBasicAuthCredential;
import com.sun.jbi.httpsoapbc.security.http.impl.BasicAuthenticator;
import com.sun.jbi.internationalization.Messages;

import java.net.Authenticator;
import java.net.Authenticator.RequestorType;
import java.net.InetSocketAddress;
import java.net.PasswordAuthentication;
import java.net.Proxy;
import java.util.logging.Level;
import java.util.logging.Logger;

/*
 * It is used for server or proxy authentication.
 *
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */
// JDK class SocksSocketImpl and HttpURLConnection handler call back on Authenticator
// TODO: the targetServerAuth is not fully integrated yet.
public class HttpAuthenticator extends Authenticator {
    private static final Messages mMessages =
        Messages.getMessages(HttpAuthenticator.class);
    private static final Logger mLog =
            Messages.getLogger(HttpAuthenticator.class);
    
    private static boolean registered = false;
    
    private static HttpProxy hp = null;
    
    private static BasicAuthenticator basicAuth = null;
    
    /** Creates a new instance of HttpAuthenticator */
    public HttpAuthenticator() {
        this.register();
    }
    
    public static void registerHttpProxy(HttpProxy proxy) {
        hp = proxy;
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "HttpProxy registered");
        }
    }
    
    public static void unregisterHttpProxy() {
        hp = null;
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "HttpProxy unregistered");
        }
    }

    public static void registerBasicAuthenticator(BasicAuthenticator ba){
        basicAuth = ba;
        if(mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "BasicAuthenticator registered");
        }
    }
    
    public static void unregisterBasicAuthenticator(){
        basicAuth = null;
        if(mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "BasicAuthenticator unregistered");
        }
    }

    public void register() {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "Registering HttpAuthenticator...");
        }
        
        if (registered) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "HttpAuthenticator already registered");
            }
            return;
        }
        
        try {
            // allow re-register
            Authenticator.setDefault(this);
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE, "HttpAuthenticator registered");
            }
        } catch (Throwable t) {
            // Security error
            if (mLog.isLoggable(Level.WARNING)) {
                mLog.log(Level.WARNING, "HTTPBC-W01005.Self_registration_failed", t);
            }
        }
        
        registered = true;
    }
    
    public static void unregister() {
        registered = false;
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, "HttpAuthenticator unregistered");
        }
    }
    
    /**
     * Called when password authorization is needed.  Subclasses should
     * override the default implementation, which returns null.
     *
     * @return The PasswordAuthentication collected from the
     * 		user, or null if none is provided.
     */
    protected PasswordAuthentication getPasswordAuthentication() {
        if (!registered) {
            return super.getPasswordAuthentication();
        }
        
        // for PROXY auth
        if (null != hp &&
            hp.getProxyPort() == getRequestingPort()) {
            
            if ("SOCKS5".equalsIgnoreCase(getRequestingProtocol()) &&  // for SOCKS PROXY auth
                Proxy.Type.SOCKS.equals(hp.getProxyType()) ||
                    
                "http".equalsIgnoreCase(getRequestingProtocol()) &&  // for HTTP PROXY auth
                Proxy.Type.HTTP.equals(hp.getProxyType()) &&
                RequestorType.PROXY.equals(getRequestorType())) {
                
                InetSocketAddress isa = (InetSocketAddress) this.hp.getProxy().address();
                if (isa.getAddress().equals(getRequestingSite()) ||
                    hp.getProxyHost().equalsIgnoreCase(getRequestingHost())) {
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.log(Level.FINE, "Proxy auth. RequestingProtocol [" + getRequestingProtocol() + "], RequestorType [" + getRequestorType() + "], RequestingHost [" + getRequestingHost() + "], RequestingPort [" + getRequestingPort() + "], RequestingSite [" + getRequestingSite() + "], RequestingURL [" + getRequestingURL() + "], RequestingScheme [" + getRequestingScheme() + "], RequestingPrompt [" + getRequestingPrompt() + "].");
                    }
                    return new PasswordAuthentication(hp.getProxyUserName(), hp.getProxyPassword());
                }
            }
            
        }
        
        return super.getPasswordAuthentication();
    }
    
}
