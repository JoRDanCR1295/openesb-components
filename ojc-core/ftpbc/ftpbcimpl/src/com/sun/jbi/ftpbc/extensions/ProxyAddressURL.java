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
 * @(#)ProxyAddressURL.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc.extensions;

import com.sun.jbi.internationalization.Messages;

import java.util.List;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author jfu
 */
public class ProxyAddressURL implements AddressURL {

    private static Messages mMessages = Messages.getMessages(ProxyAddressURL.class);
    private static Logger mLogger = Messages.getLogger(ProxyAddressURL.class);
    private String scheme;
    private String user;
    private String password;
    private String host;
    private String port;
    private String url;

    public ProxyAddressURL(String url) {
        this.url = url;
    }

    public String getScheme() {
        return scheme;
    }

    public void setScheme(String scheme) {
        this.scheme = scheme;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public String getPort() {
        return port;
    }

    public void setPort(String port) {
        this.port = port;
    }

    public boolean parse() throws Exception {

        if (url == null || url.trim().length() == 0) {
            throw new Exception(mMessages.getString("FTPBC-E001026.FTPAddress.MISSING_PROXY_URL"));
        }

        // if still the place holder
        if (url.startsWith(PROXY_URL_PLACEHOLDER)) {
            throw new Exception(mMessages.getString("FTPBC-E001027.FTPAddress.REPLACE_PROXY_URL_PLACEHOLDER_WITH_REAL_URL"));
        }

        if (!url.startsWith(SOCKS4_URL_PREFIX) && !url.startsWith(SOCKS5_URL_PREFIX)) {
            throw new Exception(mMessages.getString("FTPBC-E001028.FTPAddress.MALFORMED_PROXY_URL", url));
        }

        scheme = url.startsWith(SOCKS4_URL_PREFIX) ? "socks4" : "socks5";

        if (url.length() > SOCKS4_URL_PREFIX.length()) {
            String rest = url.substring(SOCKS4_URL_PREFIX.length());
            if (rest.indexOf(URL_PATH_DELIM) >= 0) {
                throw new Exception(mMessages.getString("FTPBC-E001029.FTPAddress.INVALID_PROXY_URL_PATH_NOT_ALLOWED", url));
            }

            int l = rest.trim().length();
            int i = 0;
            StringBuffer cur = new StringBuffer();
            int at = 0;
            int col = 0;
            List comps = new Vector();
            while (i < l) {
                char c = rest.charAt(i);
                switch (c) {
                    case '\\':
                        if (i + 1 < l) {
                            cur.append(rest.charAt(i + 1));
                            i = i + 2;
                        } else {
                            cur.append(c);
                            i++;
                        }
                        break;
                    case ':':
                        col++;
                        if (col > 1 || cur.length() == 0 /* :password and :port are invalid */) {
                            // in each part: either user:password
                            // or host:port, there can be at most 1
                            // ':' delimiter;
                            throw new Exception(mMessages.getString("FTPBC-E001028.FTPAddress.MALFORMED_PROXY_URL", url));
                        }
                        comps.add(cur.toString());
                        cur = new StringBuffer();
                        i++;
                        break;
                    case '@':
                        at++;
                        if (at > 1) {
                            // at most 1 '@' as delimiter;
                            throw new Exception(mMessages.getString("FTPBC-E001028.FTPAddress.MALFORMED_PROXY_URL", url));
                        }
                        // previously collected belongs to user_password
                        comps.add(cur.toString());
                        cur = new StringBuffer();
                        col = 0;
                        switch (comps.size()) {
                            case 1:
                                this.user = (String) comps.get(0);
                                break;
                            case 2:
                                this.user = (String) comps.get(0);
                                this.password = (String) comps.get(1);
                                break;
                            default:
                                throw new Exception(mMessages.getString("FTPBC-E001028.FTPAddress.MALFORMED_PROXY_URL", url));
                        }
                        comps = new Vector();
                        i++;
                        break;
                    default:
                        cur.append(c);
                        i++;
                }
            }

            if (cur != null && cur.length() > 0) {
                comps.add(cur.toString());
            }

            switch (comps.size()) {
                case 1:
                    this.host = (String) comps.get(0);
                    break;
                case 2:
                    this.host = (String) comps.get(0);
                    this.port = (String) comps.get(1);
                    boolean goodPort = true;
                    if (port != null && port.trim().length() > 0) {
                        // must be a positive int
                        try {
                            int pt = Integer.parseInt(port);
                            if (pt <= 0) {
                                goodPort = false;
                            }
                        } catch (Exception e) {
                            goodPort = false;
                        }
                    }

                    if (!goodPort) {
                        throw new Exception(mMessages.getString("FTPBC-E001010.FTPAddress.INVALID_PORT_IN_URL", url));
                    }

                    break;
                default:
                    throw new Exception(mMessages.getString("FTPBC-E001028.FTPAddress.MALFORMED_PROXY_URL", url));
            }

            if (host == null || host.trim().length() == 0) {
                throw new Exception(mMessages.getString("FTPBC-E001030.FTPAddress.MALFORMED_PROXY_URL_HOST_REQUIRED", url));
            }
        } else {
            throw new Exception(mMessages.getString("FTPBC-E001028.FTPAddress.MALFORMED_PROXY_URL", url));
        }
        return true;
    }

    public void validate() throws Exception {
        if (this.host == null || this.host.trim().length() == 0) {
            throw new Exception(mMessages.getString("FTPBC-E001031.FTPProxy.NO_PROXY_HOST", this.url));
        }
        int proxyPort = 1080;
        if (this.port != null &&
                this.port.trim().length() > 0) {
            try {
                proxyPort = Integer.parseInt(this.port);
            } catch (Exception e) {
                // error
                throw new Exception(mMessages.getString("FTPBC-E001032.FTPProxy.INVALID_PORT", this.port));
            }
        } else {
            // warning - default used
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W001003.FTPProxy.PORT_DEFAULT", new Object[]{new Integer(proxyPort)}));
            }
        }
        if (this.scheme != null && this.scheme.trim().length() > 0) {
            if (!(this.scheme.equalsIgnoreCase("socks4") || this.scheme.equalsIgnoreCase("socks5"))) {
                throw new Exception(mMessages.getString("FTPBC-E001033.FTPProxy.INVALID_PROXY_PROTOCOL", this.url));
            }
        } else {
            throw new Exception(mMessages.getString("FTPBC-E001034.FTPProxy.MISSING_PROTOCOL_SCHEME", this.url));
        }
    }

    public String getURL() {
        return url;
    }
}
