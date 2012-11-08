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
 * @(#)AddressURL.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.ftpbc.extensions;

/**
 *
 * @author jfu
 */
public interface AddressURL {

    /**
     *
     */
    public static final String FTP_URL_PLACEHOLDER = "ftp://[ftp_user]:[ftp_password]@[ftp_host]:[ftp_port]";
    /**
     *
     */
    public static final String PROXY_URL_PLACEHOLDER = "[proxy_protocol]://[proxy_user]:[proxy_password]@[proxy_host]:[proxy_port]";
    /**
     *
     */
    public static final String FTP_URL_PREFIX = "ftp://";
    /**
     *
     */
    public static final String SOCKS4_URL_PREFIX = "socks4://";
    /**
     *
     */
    public static final String SOCKS5_URL_PREFIX = "socks5://";
    /**
     *
     */
    public static final String URL_LOGIN_HOST_DELIM = "@";
    /**
     *
     */
    public static final String URL_COLON_DELIM = ":";
    /**
     *
     */
    public static final String URL_PATH_DELIM = "/";

    /**
     *
     * @return
     */
    public String getScheme();

    /**
     *
     * @param scheme
     */
    public void setScheme(String scheme);

    /**
     *
     * @return
     */
    public String getUser();

    /**
     *
     * @param user
     */
    public void setUser(String user);

    /**
     *
     * @return
     */
    public String getPassword();

    /**
     *
     * @param password
     */
    public void setPassword(String password);

    /**
     *
     * @return
     */
    public String getHost();

    /**
     *
     * @param host
     */
    public void setHost(String host);

    /**
     *
     * @return
     */
    public String getPort();

    /**
     *
     * @param port
     */
    public void setPort(String port);

    /**
     *
     * @return
     * @throws java.lang.Exception
     */
    public boolean parse() throws Exception;

    /**
     *
     * @throws java.lang.Exception
     */
    public void validate() throws Exception;
}
