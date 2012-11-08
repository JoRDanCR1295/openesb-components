
package com.sun.jbi.restbc.jbiadapter.security;

/**
 *
 * @author Alexander Lomov
 */
public class ProxySettings {
    
    
    private String proxyHost;
    private String proxyPort;
    private String proxyUser;
    private String proxyPassword;
    
    
    private static final String HTTP = "http";
    private static final String HTTP_PROXY_HOST = "http.proxyHost";
    private static final String HTTP_PROXY_PORT = "http.proxyPort";
    private static final String HTTP_PROXY_USER = "http.proxyUser";
    private static final String HTTP_PROXY_PASSWORD = "http.proxyPassword";
    
    private static final String HTTPS = "https";
    private static final String HTTPS_PROXY_HOST = "https.proxyHost";
    private static final String HTTPS_PROXY_PORT = "https.proxyPort";
    private static final String HTTPS_PROXY_USER = "https.proxyUser";
    private static final String HTTPS_PROXY_PASSWORD = "https.proxyPassword";

    private ProxySettings() {
    }

    private ProxySettings(String proxyHost, String proxyPort, String proxyUser, String proxyPassword) {
        this.proxyHost = proxyHost;
        this.proxyPort = proxyPort;
        this.proxyUser = proxyUser;
        this.proxyPassword = proxyPassword;
    }
    /**
     * HTTP scheme is assumed.
     * 
     * @return ProxySettings for HTTP proxy
     */
    
    public static ProxySettings getProxySettings() {
        return getProxySettings(HTTP);
    }
    
    public static ProxySettings getProxySettings(String uriScheme) {
        ProxySettings ps = null;
        String host;
        String port;
        String user;
        String password;
        if (uriScheme.equalsIgnoreCase(HTTP)) {
            host = System.getProperty(HTTP_PROXY_HOST);
            port = System.getProperty(HTTP_PROXY_PORT);
            user = System.getProperty(HTTP_PROXY_USER, "80");
            password = System.getProperty(HTTP_PROXY_PASSWORD);
            ps = new ProxySettings(host, port, user, password);
        } else if (uriScheme.equalsIgnoreCase(HTTPS)) {
            host = System.getProperty(HTTPS_PROXY_HOST);
            port = System.getProperty(HTTPS_PROXY_PORT, "443");
            user = System.getProperty(HTTPS_PROXY_USER);
            password = System.getProperty(HTTPS_PROXY_PASSWORD);
            ps = new ProxySettings(host, port, user, password);
        }
        return ps;
    }

    public String getProxyHost() {
        return proxyHost;
    }

    public String getProxyPassword() {
        return proxyPassword;
    }

    public String getProxyPort() {
        return proxyPort;
    }

    public String getProxyUser() {
        return proxyUser;
    }
    
    public String getProxyUrl() {
        if (proxyHost == null || proxyHost.equalsIgnoreCase(""))
            return "";
        StringBuilder sb = new StringBuilder(proxyHost);
        if (proxyPort != null && !proxyPort.equalsIgnoreCase(""))
            sb.append(":").append(proxyPort);
        return sb.toString();
    }
    
    public boolean isProxyUrlSet() {
        return (proxyHost != null && !proxyHost.equalsIgnoreCase(""));
    }
    
    public boolean isProxyUserSet() {
        return (proxyUser != null && !proxyUser.equalsIgnoreCase(""));
    }
    
    public boolean isProxyPasswordSet() {
        return (proxyPassword != null && !proxyPassword.equalsIgnoreCase(""));
    }
    
}
