package com.sun.jbi.restbc.jbiadapter;

import com.sun.jbi.restbc.jbiadapter.security.ProxySettings;
import com.sun.jersey.client.urlconnection.HttpURLConnectionFactory;
import java.io.IOException;
import java.net.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Alexander Lomov
 */
public class HttpProxyURLConnectionFactory implements HttpURLConnectionFactory {

    public HttpURLConnection getHttpURLConnection(URL url) throws IOException {
        HttpURLConnection c;
        ProxySettings ps;
        try {
            ps = ProxySettings.getProxySettings(url.toURI().getScheme());
        } catch (URISyntaxException ex) {
            Logger.getLogger(HttpProxyURLConnectionFactory.class.getName()).log(Level.SEVERE, null, ex);
            return null;
        }
        if (ps.isProxyUrlSet()) {
            Proxy p = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(ps.getProxyHost(), Integer.valueOf(ps.getProxyPort())));
            if (ps.isProxyUserSet() && ps.isProxyPasswordSet()) {
                final String user = ps.getProxyUser();
                final String password = ps.getProxyPassword();
                Authenticator.setDefault(new Authenticator() {

                    @Override
                    protected PasswordAuthentication getPasswordAuthentication() {
                        return new PasswordAuthentication(user, password.toCharArray());
                    }
                });
            }
            c = (HttpURLConnection) url.openConnection(p);
        } else {
            c = (HttpURLConnection) url.openConnection();
        }
        return c;
    }
}
