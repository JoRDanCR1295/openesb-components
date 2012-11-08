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
 * @(#)HttpProxyURLStreamHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.proxy;

import com.sun.jbi.httpsoapbc.security.auth.HttpAuthenticator;
import com.sun.jbi.internationalization.Messages;
import sun.net.www.protocol.http.Handler; 

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.util.logging.Level;
import java.util.logging.Logger;

/*
 * It applies  to per-connection-Proxy to http URL connection.
 *
 * @author Harry Liu (harry.liu@sun.com)
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.
 */
//public class HttpProxyURLStreamHandler extends URLStreamHandler { //too much impl logic
public class HttpProxyURLStreamHandler extends Handler {
    private static final Messages mMessages =
        Messages.getMessages(HttpProxyURLStreamHandler.class);
    private static final Logger mLog =
        Messages.getLogger(HttpProxyURLStreamHandler.class);

    private HttpProxy hp;
    
    /** Creates a new instance of HttpProxyURLStreamHandler */
    public HttpProxyURLStreamHandler(HttpProxy hp) {
        super();
        this.hp = hp;
    }

    public void register() {
        if (!this.hp.isUseJVMProxySettings()) {
            new HttpAuthenticator().registerHttpProxy(this.hp);
        }
    }

    /**
     * Opens a connection to the object referenced by the
     * <code>URL</code> argument.
     * This method should be overridden by a subclass.
     * 
     * <p>If for the handler's protocol (such as HTTP or JAR), there
     * exists a public, specialized URLConnection subclass belonging
     * to one of the following packages or one of their subpackages:
     * java.lang, java.io, java.util, java.net, the connection
     * returned will be of that subclass. For example, for HTTP an
     * HttpURLConnection will be returned, and for JAR a
     * JarURLConnection will be returned.
     * 
     * 
     * @param u   the URL that this connects to.
     * @return a <code>URLConnection</code> object for the <code>URL</code>.
     * @exception IOException  if an I/O error occurs while opening the
     *               connection.
     */
    protected URLConnection openConnection(URL u) throws IOException {
        if (this.hp.isUseJVMProxySettings()) {
            return super.openConnection(u);
        }
        
        if (!"http".equalsIgnoreCase(u.getProtocol())) {
            //? throw new IOException("Invalid usage, only http protocol is expected by this handler.");
            return super.openConnection(u); 
        }
        
        String nonProxyHosts = this.hp.getNonProxyHosts();
        if (null != nonProxyHosts && 0 != nonProxyHosts.length()) {
            nonProxyHosts = nonProxyHosts.replaceAll(" ", "").toLowerCase();
            String host = u.getHost();
            if (null == host ||
                nonProxyHosts.startsWith(host.toLowerCase() + "|") ||
                nonProxyHosts.endsWith("|" + host.toLowerCase()) ||
                nonProxyHosts.contains("|" + host.toLowerCase() + "|")) {
                return super.openConnection(u);
            }

        }

        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, mMessages.getString("Proxy is passed in."));
        }
        return super.openConnection(u, this.hp.getProxy());
        
    }
    
}
