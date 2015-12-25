package com.sun.jbi.httpsoapbc.embedded;

import com.sun.jbi.httpsoapbc.Endpoint;
import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;
import java.util.Set;
import javax.xml.namespace.QName;
import org.glassfish.grizzly.http.server.HttpHandler;
import org.glassfish.grizzly.http.server.Request;
import org.glassfish.grizzly.http.server.Response;

/**
 *
 *
 * @author David BRASSELY (brasseld at gmail.com)
 * @author OpenESB Community
 */
public class EndpointsManagerHttpHandler extends HttpHandler {

    private final HttpSoapBindingLifeCycle lifecycle;
    
    public EndpointsManagerHttpHandler(final HttpSoapBindingLifeCycle lifecycle) {
        this.lifecycle = lifecycle;
    }
    
    @Override
    public void service(Request request, Response response) throws Exception {
        Set<Endpoint> endpoints = lifecycle.getRegisteredEndpoints();
        for(Endpoint endpoint : endpoints) {
            if (endpoint.isInbound()) {
                QName service = endpoint.getServiceName();
                System.out.println("Endpoint : " + endpoint);
            //    response.
            //    endpoint.getC
            }
        }
    }
}