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
 * @(#)FTPAddressURL.java 
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
public class FTPAddressURL implements AddressURL {

    private static Messages mMessages =
            Messages.getMessages(FTPAddressURL.class);
    private static Logger mLogger = Messages.getLogger(FTPAddressURL.class);
    private String scheme;
    private String user;
    private String password;
    private String host;
    private String port;
    private String url;

    public FTPAddressURL(String url) {
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
        // if missing
        if (url == null || url.trim().length() == 0) {
            throw new Exception(mMessages.getString("FTPBC-E001005.FTPAddress.MISSING_FTP_URL"));
        }

        // if still the place holder
        if (url.startsWith(FTP_URL_PLACEHOLDER)) {
            throw new Exception(mMessages.getString("FTPBC-E001006.FTPAddress.REPLACE_FTP_URL_PLACEHOLDER_WITH_REAL_URL"));
        }

        if (!url.startsWith(FTP_URL_PREFIX)) {
            throw new Exception(mMessages.getString("FTPBC-E001007.FTPAddress.INVALID_FTP_URL_PREFIX", url));
        }

        scheme = "ftp";
        if (url.length() > FTP_URL_PREFIX.length()) {
            String rest = url.substring(FTP_URL_PREFIX.length());
            if (rest.indexOf(URL_PATH_DELIM) >= 0) {
                throw new Exception(mMessages.getString("FTPBC-E001008.FTPAddress.INVALID_FTP_URL_PATH_NOT_ALLOWED", url));
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
                            throw new Exception(mMessages.getString("FTPBC-E001009.FTPAddress.MALFORMED_FTP_URL", url));
                        }
                        comps.add(cur.toString());
                        cur = new StringBuffer();
                        i++;
                        break;
                    case '@':
                        at++;
                        if (at > 1) {
                            // at most 1 '@' as delimiter;
                            throw new Exception(mMessages.getString("FTPBC-E001009.FTPAddress.MALFORMED_FTP_URL", url));
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
                                throw new Exception(mMessages.getString("FTPBC-E001009.FTPAddress.MALFORMED_FTP_URL", url));
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
                    throw new Exception(mMessages.getString("FTPBC-E001009.FTPAddress.MALFORMED_FTP_URL", url));
            }

            if (host == null || host.trim().length() == 0) {
                throw new Exception(mMessages.getString("FTPBC-E001011.FTPAddress.MALFORMED_FTP_URL_HOST_REQUIRED", url));
            }
        } else {
            throw new Exception(mMessages.getString("FTPBC-E001009.FTPAddress.MALFORMED_FTP_URL", url));
        }
        return true;
    }

    public void validate() throws Exception {
        int p = 21;
        if (this.port != null && port.trim().length() > 0) {
            try {
                p = Integer.parseInt(port);
            } catch (Exception e) {
                // error
                throw new Exception(mMessages.getString("FTPBC-E001012.FTPAddress.INVALID_FTP_PORT", port));
            }
        } else {
            // warning - default to 21
            if (mLogger.isLoggable(Level.WARNING)) {
                mLogger.log(Level.WARNING, mMessages.getString("FTPBC-W001002.FTPAddress.FTP_DEFAULT_PORT", new Object[]{new Integer(p)}));
            }
        }
    }
}
